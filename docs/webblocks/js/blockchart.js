// The block chart: a cumulative GVA-per-job x job-count "marimekko" for one area.
//
// Direct translation of the ggplot logic in
// selfcontained_rscripts/FORFIDDLINGNOTLIVEjobgva_cumulativeplot.R:
//   - sort sectors by gva/job descending
//   - xmin = cumsum(lag(jobs)); xmax = xmin + jobs; ymax = gva/job
//   - each rect's AREA = that sector's total GVA
//   - coord_flip: jobs run down the vertical axis, gva/job along the horizontal
//
// d3 is loaded as a UMD global (window.d3) by index.html.
import { state, areaRows } from './data.js';

const LABEL_JOB_CUTOFF = 1300; // inline label only for blocks bigger than this
                               // (mirrors textcutoffsize in the R script)

// Build the plot rows for one area/year: sorted + cumulative positions.
export function blockData(regionIdx, yearIdx) {
  const rows = areaRows(regionIdx, yearIdx)
    .filter(r => r.gvaperjob != null && isFinite(r.gvaperjob) && r.gvaperjob > 0)
    .sort((a, b) => b.gvaperjob - a.gvaperjob);
  let cum = 0;
  for (const r of rows) {
    r.xmin = cum;
    r.xmax = cum + r.jobs;
    cum += r.jobs;
  }
  return { rows, totalJobs: cum };
}

// Render/redraw a chart into `container` (a DOM element).
// opts: { regionIdx, yearIdx, width, xMax (gva/job domain max or null=fit),
//         height, onHover, highlightSector }
export function renderBlockChart(container, opts) {
  const {
    regionIdx, yearIdx, width,
    xMax = null, height = 300,
    onSectorEnter = null, onSectorLeave = null,
    highlightSector = null,
  } = opts;

  const { rows, totalJobs } = blockData(regionIdx, yearIdx);
  const region = state.regions[regionIdx];

  const margin = { top: 8, right: 12, bottom: 40, left: 54 };
  const innerW = Math.max(10, width - margin.left - margin.right);
  const innerH = Math.max(10, height - margin.top - margin.bottom);

  // Horizontal axis = GVA per job; vertical axis = cumulative job count.
  const xDomainMax = xMax != null ? xMax
    : (d3.max(rows, r => r.gvaperjob) || 1) * 1.02;
  const x = d3.scaleLinear().domain([0, xDomainMax]).range([0, innerW]);
  const y = d3.scaleLinear().domain([0, totalJobs || 1]).range([0, innerH]);

  d3.select(container).selectAll('*').remove();
  const svg = d3.select(container).append('svg')
    .attr('class', 'blockchart')
    .attr('viewBox', `0 0 ${width} ${height}`)
    .attr('width', '100%')
    .attr('height', height)
    .attr('role', 'img')
    .attr('aria-label', `GVA per job by job count, ${region.name}`);

  const g = svg.append('g')
    .attr('transform', `translate(${margin.left},${margin.top})`);

  // Axes (recessive).
  const xAxis = d3.axisBottom(x).ticks(5).tickSizeOuter(0);
  const yAxis = d3.axisLeft(y).ticks(4).tickSizeOuter(0)
    .tickFormat(d => d >= 1000 ? d3.format('.0s')(d) : d);

  g.append('g')
    .attr('class', 'axis x-axis')
    .attr('transform', `translate(0,${innerH})`)
    .call(xAxis);
  g.append('g')
    .attr('class', 'axis y-axis')
    .call(yAxis);

  // Axis titles.
  g.append('text').attr('class', 'axis-title')
    .attr('x', innerW / 2).attr('y', innerH + 34)
    .attr('text-anchor', 'middle')
    .text('GVA per FT job (£k)');
  g.append('text').attr('class', 'axis-title')
    .attr('transform', 'rotate(-90)')
    .attr('x', -innerH / 2).attr('y', -42)
    .attr('text-anchor', 'middle')
    .text('Job count (cumulative)');

  // Blocks.
  const blocks = g.append('g').selectAll('rect').data(rows).join('rect')
    .attr('class', 'block')
    .attr('data-sector', d => d.sector)
    .attr('x', 0)
    .attr('y', d => y(d.xmin))
    .attr('width', d => Math.max(0, x(Math.min(d.gvaperjob, xDomainMax))))
    .attr('height', d => Math.max(0, y(d.xmax) - y(d.xmin) - 1)) // 1px surface gap
    .attr('fill', d => state.colourFor[d.sector])
    .attr('stroke', 'rgba(0,0,0,0.35)')
    .attr('stroke-width', 0.4);

  // Inline labels for big blocks only; the rest are tooltip-only.
  g.append('g').selectAll('text.blabel')
    .data(rows.filter(d => d.jobs > LABEL_JOB_CUTOFF
      && (y(d.xmax) - y(d.xmin)) > 12))
    .join('text')
    .attr('class', 'blabel')
    .attr('x', d => Math.min(x(d.gvaperjob) / 2, innerW - 4))
    .attr('y', d => (y(d.xmin) + y(d.xmax)) / 2)
    .attr('dy', '0.32em')
    .attr('text-anchor', 'middle')
    .text(d => d.sector);

  // Hover / tap interaction.
  blocks
    .on('mouseenter', (event, d) => onSectorEnter && onSectorEnter(d.sector, d, region))
    .on('mouseleave', () => onSectorLeave && onSectorLeave());

  // Apply cross-area highlight if a sector is active.
  applyHighlight(container, highlightSector);

  return { totalJobs, rows };
}

// Dim all blocks except those matching `sector` (null clears).
export function applyHighlight(container, sector) {
  const sel = d3.select(container).selectAll('rect.block');
  if (!sector) { sel.classed('dim', false).classed('hot', false); return; }
  sel.classed('dim', d => d.sector !== sector)
     .classed('hot', d => d.sector === sector);
}
