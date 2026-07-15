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
- **By archetype** — LQ-based tags for specialisation in a family of sectors. The
  eight archetypes, exactly as they appear in the dropdown, and the sectors each
  pools:
  - *Logistics / warehousing* — Warehousing, Transport, Post
  - *Finance-heavy* — Finance
  - *Manufacturing* — Food manuf, Textiles, Wood/paper, Chemicals, Metals,
    Electronics, Mach/transp manuf, Furniture, Machinery repair/install
  - *Public sector / health / ed* — Public admin, Education, Health, Care, SocialWork
  - *Professional / knowledge* — ICT, Media, Legal, Head office/consult, Arch/Eng,
    Research/Ads
  - *Hospitality / tourism* — Accommodation, Food, Arts/ent, Recreation
  - *Retail / wholesale* — Retail, Wholesale, Motor
  - *Real estate* — Real estate

  The sector groupings are hand-set (edit the `ARCHETYPES` array in
  `js/similarity.js` to change them). The *thresholds*, by contrast, are
  data-driven: an area is tagged when its combined LQ across the archetype's
  sectors is above `mean + sdK*SD` of that group's distribution across all areas
  (floored at the national average). Combined LQ is a pooled share ratio — the
  area's total job share in the group's sectors over the national total, which is
  a national-share-weighted mean of the per-sector LQs, so the larger sectors
  dominate the tag. Tune via `archetypeConfig` in `js/similarity.js` (`sdK`
  defaults to 1 = "mean + 1 SD").
- **Similar employment mix** — cosine distance on sector job-share vectors vs an
  anchor area. Cosine measures the *angle* between the two share vectors, i.e. the
  proportional pattern of the mix (which sectors dominate, relative to each other);
  it is scale-invariant, so it keys on composition rather than overall concentration.
- **Similar productivity profile** — Euclidean (straight-line) distance on
  z-normalised GVA-per-job-by-sector vectors vs an anchor area; unlike cosine this
  is magnitude-sensitive (it compares the actual values, dimension by dimension).
- **Spearman rank methods** (rank-based, so robust to the huge-GVA/job outlier
  sectors, and aligned with the block chart's own encodings):
  - *Similar gva/job ranking* — Spearman ρ of the sectors' GVA-per-job order =
    how similar the left→right block order of two charts is.
  - *Similar jobs ranking* — Spearman ρ of the sectors' job-count order = how
    similar the tall→short block-height order is (ranking by jobs ≡ by share).
  - *Similar ranking both axes* — the two correlations above added together in a
    fixed 50/50 mix, `½·ρ(gva/job) + ½·ρ(jobs)`, so it rewards areas whose block
    charts match on both orderings at once (left→right *and* top→bottom). The mix
    is held at `spearmanConfig.w` (default 0.5); an interactive weight slider is
    built but off by default — flip `SHOW_WEIGHT_SLIDER` in `js/ui.js` to bring it
    back and vary the split. ρ uses average ranks for ties and is pairwise-complete
    over sectors (drops NaN GVA/job values).

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
