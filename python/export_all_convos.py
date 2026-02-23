#!/usr/bin/env python3
"""Find and export all Claude Code conversations for a project to markdown.

Usage:
    python export_all_convos.py                     # auto-detect project from cwd
    python export_all_convos.py /path/to/project    # specify project root
    python export_all_convos.py --convo-dir ~/.claude/projects/my-project  # point directly

Outputs markdown files into <project_root>/claudecode_convos/
"""

import json
import platform
import sys
from datetime import datetime
from pathlib import Path

# Import the converter from the sibling script
sys.path.insert(0, str(Path(__file__).parent))
from jsonl_to_markdown import convert


def get_claude_projects_dir():
    """Return the Claude Code projects directory for the current OS."""
    system = platform.system()
    if system == "Darwin":
        return Path.home() / ".claude" / "projects"
    elif system == "Linux":
        return Path.home() / ".claude" / "projects"
    elif system == "Windows":
        return Path.home() / ".claude" / "projects"
    else:
        return Path.home() / ".claude" / "projects"


def project_path_to_folder_name(project_path):
    """Convert an absolute project path to the Claude folder name convention.

    e.g. /Users/danolner/Code/MyProject -> -Users-danolner-Code-MyProject
    """
    resolved = Path(project_path).resolve()
    # Claude Code replaces all path separators AND underscores with hyphens
    import re
    return re.sub(r"[/\\_]", "-", str(resolved))


def find_convo_dir(project_root):
    """Find the Claude Code conversation directory for a given project root."""
    projects_dir = get_claude_projects_dir()
    folder_name = project_path_to_folder_name(project_root)
    convo_dir = projects_dir / folder_name

    if convo_dir.is_dir():
        return convo_dir

    # Fallback: scan for partial match (handles edge cases)
    if projects_dir.is_dir():
        for d in projects_dir.iterdir():
            if d.is_dir() and str(project_root).replace("/", "-").replace("\\", "-") in d.name:
                return d

    return None


def get_convo_timestamp(jsonl_path):
    """Extract the earliest timestamp from a conversation file for naming."""
    try:
        with open(jsonl_path) as f:
            for line in f:
                try:
                    msg = json.loads(line)
                    ts = msg.get("timestamp")
                    if ts:
                        # Handle ISO format or epoch
                        if isinstance(ts, str):
                            dt = datetime.fromisoformat(ts.replace("Z", "+00:00"))
                        else:
                            dt = datetime.fromtimestamp(ts / 1000 if ts > 1e12 else ts)
                        return dt.strftime("%Y-%m-%d_%H%M")
                except (json.JSONDecodeError, ValueError, TypeError):
                    continue
    except Exception:
        pass
    return None


def get_convo_first_message(jsonl_path):
    """Extract a short label from the first human message."""
    try:
        with open(jsonl_path) as f:
            for line in f:
                try:
                    msg = json.loads(line)
                    if msg.get("type") == "user" and msg.get("userType") == "external":
                        message = msg.get("message", "")
                        if isinstance(message, dict):
                            content = message.get("content", "")
                        else:
                            content = message
                        # Extract plain text
                        if isinstance(content, str):
                            text = content
                        elif isinstance(content, list):
                            texts = [b.get("text", "") for b in content if b.get("type") == "text"]
                            text = " ".join(texts)
                        else:
                            continue
                        # Clean and truncate for a filename-safe label
                        import re
                        text = re.sub(r"<[^>]+>", "", text)  # strip tags
                        text = re.sub(r"[^\w\s-]", "", text).strip()
                        words = text.split()[:6]
                        label = "_".join(words)
                        return label[:50] if label else None
                except (json.JSONDecodeError, ValueError):
                    continue
    except Exception:
        pass
    return None


def main():
    # Determine project root and optional convo dir override
    convo_dir_override = None
    project_root = Path.cwd()

    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--convo-dir" and i + 1 < len(args):
            convo_dir_override = Path(args[i + 1])
            i += 2
        else:
            project_root = Path(args[i]).resolve()
            i += 1

    # Find conversation files
    if convo_dir_override:
        convo_dir = convo_dir_override
        if not convo_dir.is_dir():
            print(f"Error: specified convo dir does not exist: {convo_dir}", file=sys.stderr)
            sys.exit(1)
    else:
        convo_dir = find_convo_dir(project_root)
        if convo_dir is None:
            print(f"Error: no Claude Code conversations found for project: {project_root}", file=sys.stderr)
            print(f"  Looked in: {get_claude_projects_dir()}", file=sys.stderr)
            print(f"  Expected folder: {project_path_to_folder_name(project_root)}", file=sys.stderr)
            print(f"\n  Use --convo-dir to point to the folder directly.", file=sys.stderr)
            sys.exit(1)

    jsonl_files = sorted(convo_dir.glob("*.jsonl"))
    if not jsonl_files:
        print(f"No .jsonl files found in {convo_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"Found {len(jsonl_files)} conversation(s) in:", file=sys.stderr)
    print(f"  {convo_dir}", file=sys.stderr)

    # Create output directory
    output_dir = project_root / "claudecode_convos"
    output_dir.mkdir(exist_ok=True)

    # Process each conversation
    for jsonl_path in jsonl_files:
        timestamp = get_convo_timestamp(jsonl_path) or "unknown"
        label = get_convo_first_message(jsonl_path) or jsonl_path.stem[:8]

        filename = f"{timestamp}_{label}.md"
        output_path = output_dir / filename

        print(f"  {jsonl_path.name} -> {filename}", file=sys.stderr)
        convert(str(jsonl_path), str(output_path))

    print(f"\nDone. {len(jsonl_files)} file(s) written to {output_dir}", file=sys.stderr)


if __name__ == "__main__":
    main()
