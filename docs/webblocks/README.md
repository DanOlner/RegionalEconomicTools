# GVA-per-job × jobs — area comparison (web)

A static, mostly-JavaScript version of the block plot in
`selfcontained_rscripts/FORFIDDLINGNOTLIVEjobgva_cumulativeplot.R`. Compare
multiple ITL3 areas side by side and group/filter them by economic similarity.
Served via GitHub Pages at `…/webblocks/`.

## What each block means

Sectors are sorted by GVA per FT job (descending) and stacked cumulatively by job
count, so each block's **area = that sector's total GVA**. Width = GVA per job,
height = job count. Colour is a soft grouping aid only — with 43 SIC sectors it
can't be colourblind-safe, so identity is carried by position, inline labels
(big blocks) and hover/tap tooltips (all blocks), plus cross-area highlight.

## Grouping / similarity (the "Group / find similar" panel)

- **By sector** — filter areas above a job-share or location-quotient threshold in
  one sector (e.g. Warehousing LQ ≥ 1.5 → Thurrock, Doncaster, Milton Keynes…).
- **By archetype** — LQ-based tags (logistics, finance, manufacturing, public
  sector, professional/knowledge, hospitality, retail, real estate). Thresholds
  are data-driven: an area is tagged when its combined LQ across the archetype's
  sectors is above `mean + sdK*SD` of that group's distribution across all areas
  (floored at the national average). Tune via `archetypeConfig` in
  `js/similarity.js` (`sdK` defaults to 1 = "mean + 1 SD").
- **Similar employment mix** — cosine distance on sector job-share vectors vs an
  anchor area (magnitude-based).
- **Similar productivity profile** — Euclidean distance on z-normalised
  GVA-per-job-by-sector vectors vs an anchor area (magnitude-based).
- **Spearman rank methods** (rank-based, so robust to the huge-GVA/job outlier
  sectors, and aligned with the block chart's own encodings):
  - *Similar gva/job ranking* — Spearman ρ of the sectors' GVA-per-job order =
    how similar the left→right block order of two charts is.
  - *Similar jobs ranking* — Spearman ρ of the sectors' job-count order = how
    similar the tall→short block-height order is (ranking by jobs ≡ by share).
  - *Similar ranking both axes, weighted* — `w·ρ(gva/job) + (1−w)·ρ(jobs)`, `w` set by a
    slider (`spearmanConfig.w`, default 0.5). All ρ use average ranks for ties
    and are pairwise-complete over sectors (drops NaN GVA/job values).

All computed in-browser from the JSON (national totals summed from the data
itself); no server.

## Build / update the data

The page reads `data/bresgva2d_web.json`, a slim columnar export of
`data/bresgva2d_2023.rds`. Regenerate it (from the repo root) whenever the source
RDS updates:

```sh
Rscript prepcode/export_bresgva2d_web.R
```

That rewrites `docs/webblocks/data/bresgva2d_web.json` (~1.3 MB, ~330 KB gzipped),
including the sector→colour map (reproduced exactly from the R script's
RColorBrewer palette) and ITL3→ITL2→ITL1 lookup for the region quick-add.

## Local preview

ES modules + `fetch` need HTTP (not `file://`):

```sh
cd docs/webblocks && python3 -m http.server 8791
# open http://localhost:8791/
```

## Files

- `index.html` — page shell + controls
- `css/style.css` — layout, light/dark, mobile single-column stacking
- `js/data.js` — load JSON, reshape, build similarity feature vectors
- `js/blockchart.js` — the D3 cumulative-rect chart
- `js/similarity.js` — the four grouping methods
- `js/ui.js` — orchestration (picker, grid, tooltip, similarity panel)
- `js/vendor/d3.v7.min.js` — vendored D3 (offline-safe)
