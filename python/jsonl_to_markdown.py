#!/usr/bin/env python3
"""Convert a Claude Code conversation .jsonl file to readable markdown.

Usage:
    python jsonl_to_markdown.py <input.jsonl> [output.md]

If no output path is given, writes to stdout.

E.g.

Print to stdout
python3 scripts/jsonl_to_markdown.py ~/.claude/projects/-Users-danolner-thevault/6544d6a2-37bf-47ec-a6d9-9f4cca8c48e8.jsonl

Write to a file
python3 scripts/jsonl_to_markdown.py ~/.claude/projects/-Users-danolner-thevault/6544d6a2-37bf-47ec-a6d9-9f4cca8c48e8.jsonl output.md

Pipe to less for browsing
python3 scripts/jsonl_to_markdown.py <file>.jsonl | less

"""

import json
import sys
import textwrap
from pathlib import Path


def extract_user_text(content):
    """Extract readable text from user message content."""
    if isinstance(content, str):
        return content

    parts = []
    for block in content:
        if block.get("type") == "text":
            text = block.get("text", "")
            # Strip IDE metadata tags but keep the info
            if "<ide_opened_file>" in text:
                # Extract just the file path info
                import re
                opened = re.findall(r"<ide_opened_file>(.*?)</ide_opened_file>", text)
                for o in opened:
                    parts.append(f"*[Opened file: {o.split('opened the file ')[-1].split(' in the IDE')[0]}]*")
                # Get any remaining text outside the tags
                cleaned = re.sub(r"<ide_opened_file>.*?</ide_opened_file>", "", text).strip()
                if cleaned:
                    parts.append(cleaned)
            elif "<system-reminder>" in text:
                import re
                cleaned = re.sub(r"<system-reminder>.*?</system-reminder>", "", text, flags=re.DOTALL).strip()
                if cleaned:
                    parts.append(cleaned)
            else:
                parts.append(text)
        elif block.get("type") == "tool_result":
            # Summarise tool results compactly
            result_content = block.get("content", "")
            if isinstance(result_content, str):
                preview = result_content[:200].replace("\n", " ")
                if len(result_content) > 200:
                    preview += "..."
                parts.append(f"> **Tool result:** {preview}")
            elif isinstance(result_content, list):
                for sub in result_content:
                    if sub.get("type") == "text":
                        preview = sub.get("text", "")[:200].replace("\n", " ")
                        parts.append(f"> **Tool result:** {preview}")

    return "\n".join(parts)


def extract_assistant_text(content):
    """Extract readable text from assistant message content."""
    parts = []
    for block in content:
        btype = block.get("type", "")

        if btype == "text":
            text = block.get("text", "").strip()
            if text:
                parts.append(text)

        elif btype == "tool_use":
            name = block.get("name", "unknown")
            inp = block.get("input", {})

            if name == "Bash":
                cmd = inp.get("command", "")
                desc = inp.get("description", "")
                label = f" *({desc})*" if desc else ""
                parts.append(f"```bash{label}\n{cmd}\n```")

            elif name == "Read":
                path = inp.get("file_path", "")
                parts.append(f"*[Read: `{path}`]*")

            elif name == "Write":
                path = inp.get("file_path", "")
                content_preview = inp.get("content", "")[:100]
                parts.append(f"*[Write: `{path}`]*")

            elif name == "Edit":
                path = inp.get("file_path", "")
                old = inp.get("old_string", "")[:60]
                parts.append(f"*[Edit: `{path}`]*")

            elif name == "Glob":
                pattern = inp.get("pattern", "")
                parts.append(f"*[Glob: `{pattern}`]*")

            elif name == "Grep":
                pattern = inp.get("pattern", "")
                parts.append(f"*[Grep: `{pattern}`]*")

            elif name == "WebSearch":
                query = inp.get("query", "")
                parts.append(f'*[Web search: "{query}"]*')

            elif name == "WebFetch":
                url = inp.get("url", "")
                parts.append(f"*[Fetch: {url}]*")

            elif name == "TodoWrite":
                todos = inp.get("todos", [])
                items = [f"  - [{t.get('status', '?')}] {t.get('content', '')}" for t in todos]
                parts.append("*[Todo update:]*\n" + "\n".join(items))

            elif name == "Task":
                desc = inp.get("description", "")
                parts.append(f"*[Spawned agent: {desc}]*")

            elif name == "Skill":
                skill = inp.get("skill", "")
                parts.append(f"*[Skill: {skill}]*")

            else:
                parts.append(f"*[Tool: {name}]*")

        elif btype == "thinking":
            # Skip thinking blocks - they're usually empty/redacted
            pass

    return "\n\n".join(parts)


def _has_human_text(content):
    """Check if a user message content contains actual human text blocks
    (as opposed to only tool_result blocks, which are system responses)."""
    if isinstance(content, str):
        return True
    if not isinstance(content, list):
        return False
    return any(block.get("type") == "text" for block in content)


def convert(input_path, output_file=None):
    lines = Path(input_path).read_text().strip().split("\n")

    out = []
    out.append(f"# Claude Code Conversation\n")
    out.append(f"*Source: `{input_path}`*\n")
    out.append("---\n")

    turn_num = 0

    for line in lines:
        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue

        msg_type = msg.get("type", "")

        # Skip non-message lines (snapshots, queue ops, etc.)
        if msg_type not in ("user", "assistant"):
            continue

        user_type = msg.get("userType", "")
        message = msg.get("message", "")

        if msg_type == "user":
            if isinstance(message, dict):
                content = message.get("content", "")
            else:
                content = message

            if _has_human_text(content):
                # Actual human turn — contains text blocks (your input)
                text = extract_user_text(content)
                if text.strip():
                    turn_num += 1
                    out.append(f"\n## Human ({turn_num})\n")
                    out.append(text)
                    out.append("")
            else:
                # Tool results only — system response to a tool call
                text = extract_user_text(content if isinstance(content, list) else [])
                if text.strip():
                    out.append(text)
                    out.append("")

        elif msg_type == "assistant":
            content = message.get("content", []) if isinstance(message, dict) else []
            text = extract_assistant_text(content)
            if text.strip():
                out.append(f"\n## Assistant\n")
                out.append(text)
                out.append("")

    result = "\n".join(out)

    if output_file:
        Path(output_file).write_text(result)
        print(f"Written to {output_file}", file=sys.stderr)
    else:
        print(result)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python jsonl_to_markdown.py <input.jsonl> [output.md]", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else None
    convert(input_path, output_path)
