// UI orchestration: area picker, year + scale controls, the small-multiples grid,
// the similarity/grouping panel, tooltip and cross-area highlight.
import { state, loadData, buildFeatures, regionIdxByName } from './data.js';
import { renderBlockChart, applyHighlight } from './blockchart.js';
import {
  rankBySectorShare, rankByProductivity, ARCHETYPES, archetypesFor,
  areasByArchetype, areasBySector, archetypeConfig,
} from './similarity.js';

const ui = {
  selected: [],           // array of regionIdx, in display order
  yearIdx: 0,
  xMode: 'shared',        // 'shared' (0..350) | 'fit'
  sharedXMax: 350,        // matches coord_flip(ylim=c(0,350)) in the R script
  activeSector: null,     // cross-area highlight
};

const $ = sel => document.querySelector(sel);
const grid = () => $('#grid');

async function init() {
  await loadData('data/bresgva2d_web.json');
  buildFeatures();
  ui.yearIdx = state.refYearIdx;

  buildAreaPicker();
  buildYearControl();
  buildScaleControl();
  buildSimilarityPanel();
  buildLegend();
  $('#clear-all').onclick = clearAllAreas;

  // Sensible default: a spread of well-known economies.
  const defaults = ['Leeds', 'Sheffield', 'Manchester', 'Nottingham']
    .map(regionIdxByName).filter(i => i != null);
  ui.selected = defaults.length ? defaults : [0, 1, 2, 3];
  renderSelectedChips();
  renderGrid();

  window.addEventListener('resize', debounce(renderGrid, 150));
}

// ---- Area picker ------------------------------------------------------------
function buildAreaPicker() {
  const input = $('#area-search');
  const results = $('#area-results');
  const names = state.regions.map((r, i) => ({ i, name: r.name, itl2: r.itl2, itl1: r.itl1 }));

  function show(list) {
    results.innerHTML = '';
    list.slice(0, 40).forEach(item => {
      const li = document.createElement('li');
      li.textContent = item.name;
      const sub = document.createElement('span');
      sub.className = 'muted';
      sub.textContent = item.itl1 ? ` · ${item.itl1}` : '';
      li.appendChild(sub);
      li.onclick = () => { addArea(item.i); input.value = ''; show([]); };
      results.appendChild(li);
    });
    results.classList.toggle('open', list.length > 0);
  }

  input.addEventListener('input', () => {
    const q = input.value.trim().toLowerCase();
    if (!q) return show([]);
    show(names.filter(n =>
      n.name.toLowerCase().includes(q) ||
      (n.itl1 && n.itl1.toLowerCase().includes(q)) ||
      (n.itl2 && n.itl2.toLowerCase().includes(q))));
  });
  document.addEventListener('click', e => {
    if (!e.target.closest('.area-picker')) show([]);
  });

  // Quick-add by ITL1 region.
  const regionSel = $('#region-quickadd');
  const itl1s = [...new Set(state.regions.map(r => r.itl1).filter(Boolean))].sort();
  regionSel.innerHTML = '<option value="">Add whole region…</option>' +
    itl1s.map(r => `<option>${r}</option>`).join('');
  regionSel.onchange = () => {
    if (!regionSel.value) return;
    state.regions.forEach((r, i) => { if (r.itl1 === regionSel.value) addArea(i); });
    regionSel.value = '';
  };
}

function addArea(idx) {
  if (!ui.selected.includes(idx)) {
    ui.selected.push(idx);
    renderSelectedChips();
    renderGrid();
  }
}
function removeArea(idx) {
  ui.selected = ui.selected.filter(i => i !== idx);
  renderSelectedChips();
  renderGrid();
}
function clearAllAreas() {
  ui.selected = [];
  ui.activeSector = null;
  renderSelectedChips();
  renderGrid();
}

function renderSelectedChips() {
  const box = $('#selected-chips');
  box.innerHTML = '';
  ui.selected.forEach(idx => {
    const chip = document.createElement('span');
    chip.className = 'chip';
    chip.textContent = state.regions[idx].name;
    const x = document.createElement('button');
    x.textContent = '×'; x.title = 'Remove';
    x.onclick = () => removeArea(idx);
    chip.appendChild(x);
    box.appendChild(chip);
  });
  $('#count').textContent = ui.selected.length ? `${ui.selected.length} areas` : '';
  $('#clear-all').hidden = ui.selected.length === 0;
}

// ---- Year + scale controls --------------------------------------------------
function buildYearControl() {
  const sel = $('#year-select');
  sel.innerHTML = state.years.map((y, i) =>
    `<option value="${i}" ${i === ui.yearIdx ? 'selected' : ''}>${y}</option>`).join('');
  sel.onchange = () => { ui.yearIdx = +sel.value; renderGrid(); };
}

function buildScaleControl() {
  const sel = $('#scale-select');
  sel.onchange = () => { ui.xMode = sel.value; renderGrid(); };
}

// ---- Legend -----------------------------------------------------------------
function buildLegend() {
  const box = $('#legend');
  box.innerHTML = '';
  state.sectors.forEach(s => {
    const item = document.createElement('span');
    item.className = 'legend-item';
    item.dataset.sector = s;
    item.innerHTML =
      `<span class="swatch" style="background:${state.colourFor[s]}"></span>${s}`;
    item.onmouseenter = () => setActiveSector(s);
    item.onmouseleave = () => setActiveSector(null);
    box.appendChild(item);
  });
}

// ---- Grid of block charts ---------------------------------------------------
function renderGrid() {
  const g = grid();
  g.innerHTML = '';
  if (!ui.selected.length) {
    g.innerHTML = '<p class="muted pad">Add areas above to compare.</p>';
    return;
  }
  const xMax = ui.xMode === 'shared' ? ui.sharedXMax : null;

  ui.selected.forEach(idx => {
    const card = document.createElement('div');
    card.className = 'card';
    const head = document.createElement('div');
    head.className = 'card-head';
    head.innerHTML = `<span class="card-title">${state.regions[idx].name}</span>`;
    const tags = archetypesFor(idx);
    if (tags.length) {
      const t = document.createElement('span');
      t.className = 'card-tags';
      t.textContent = tags.join(' · ');
      head.appendChild(t);
    }
    const rm = document.createElement('button');
    rm.className = 'card-remove'; rm.textContent = '×'; rm.title = 'Remove';
    rm.onclick = () => removeArea(idx);
    head.appendChild(rm);

    const plot = document.createElement('div');
    plot.className = 'plot';
    card.appendChild(head);
    card.appendChild(plot);
    g.appendChild(card);

    const width = plot.clientWidth || 320;
    renderBlockChart(plot, {
      regionIdx: idx, yearIdx: ui.yearIdx, width, xMax,
      height: 640,
      onSectorEnter: (sector, d, region) => {
        setActiveSector(sector);
        showTooltip(sector, d, region);
      },
      onSectorLeave: () => { setActiveSector(null); hideTooltip(); },
      highlightSector: ui.activeSector,
    });
  });
}

function setActiveSector(sector) {
  ui.activeSector = sector;
  grid().querySelectorAll('.plot').forEach(p => applyHighlight(p, sector));
  document.querySelectorAll('.legend-item').forEach(li =>
    li.classList.toggle('active', sector && li.dataset.sector === sector));
}

// ---- Tooltip ----------------------------------------------------------------
function showTooltip(sector, d, region) {
  const tt = $('#tooltip');
  const totalGva = (d.gva != null) ? `£${Math.round(d.gva)}m`
    : `£${Math.round(d.jobs * d.gvaperjob / 1000)}m (est)`;
  tt.innerHTML =
    `<strong>${sector}</strong><br>${region.name}<br>` +
    `Jobs: ${Math.round(d.jobs).toLocaleString()}<br>` +
    `GVA/job: £${d.gvaperjob.toFixed(1)}k<br>` +
    `Total GVA: ${totalGva}`;
  tt.classList.add('show');
  const move = e => {
    tt.style.left = `${e.clientX + 14}px`;
    tt.style.top = `${e.clientY + 14}px`;
  };
  document.addEventListener('mousemove', move);
  tt._move = move;
}
function hideTooltip() {
  const tt = $('#tooltip');
  tt.classList.remove('show');
  if (tt._move) { document.removeEventListener('mousemove', tt._move); tt._move = null; }
}

// ---- Similarity / grouping panel --------------------------------------------
function buildSimilarityPanel() {
  const method = $('#sim-method');
  const controls = $('#sim-controls');
  const results = $('#sim-results');

  function anchorSelectHtml(id) {
    const opts = state.regions.map((r, i) =>
      `<option value="${i}" ${ui.selected[0] === i ? 'selected' : ''}>${r.name}</option>`).join('');
    return `<label>Anchor area <select id="${id}">${opts}</select></label>`;
  }

  function render() {
    const m = method.value;
    controls.innerHTML = '';
    results.innerHTML = '';
    if (m === 'share' || m === 'productivity') {
      controls.innerHTML = anchorSelectHtml('sim-anchor') +
        `<button id="sim-run">Find similar</button>`;
      $('#sim-run').onclick = () => {
        const a = +$('#sim-anchor').value;
        const res = m === 'share' ? rankBySectorShare(a) : rankByProductivity(a);
        showResults(res, `Most similar to ${state.regions[a].name}`);
      };
    } else if (m === 'archetype') {
      const opts = ARCHETYPES.map(a => `<option>${a.tag}</option>`).join('');
      controls.innerHTML = `<label>Archetype <select id="sim-arc">${opts}</select></label>` +
        `<button id="sim-run">Show areas</button>`;
      $('#sim-run').onclick = () => {
        const tag = $('#sim-arc').value;
        const res = areasByArchetype(tag);
        const thr = res.meta ? res.meta.threshold.toFixed(2) : '';
        showResults(res, `${tag} (LQ ≥ ${thr}, mean+${archetypeConfig.sdK}SD)`);
      };
    } else if (m === 'sector') {
      const sopts = state.sectors.map(s => `<option ${s === 'Warehousing' ? 'selected' : ''}>${s}</option>`).join('');
      controls.innerHTML =
        `<label>Sector <select id="sim-sector">${sopts}</select></label>` +
        `<label>By <select id="sim-metric"><option value="lq">Location quotient</option><option value="share">Job share</option></select></label>` +
        `<label>Min <input id="sim-thresh" type="number" step="0.1" value="1.5" style="width:5em"></label>` +
        `<button id="sim-run">Filter</button>`;
      $('#sim-run').onclick = () => {
        const s = $('#sim-sector').value, metric = $('#sim-metric').value;
        const thr = parseFloat($('#sim-thresh').value) || 0;
        showResults(areasBySector(s, metric, thr),
          `${s}: ${metric === 'lq' ? 'LQ' : 'share'} ≥ ${thr}`);
      };
    }
  }

  function showResults(res, title) {
    const box = results;
    if (!res.items.length) { box.innerHTML = `<p class="muted">${title}: no areas.</p>`; return; }
    const head = `<div class="sim-head"><strong>${title}</strong>` +
      `<button id="sim-addall">Add top ${Math.min(8, res.items.length)}</button></div>`;
    const list = res.items.slice(0, 30).map(it => {
      const inSel = ui.selected.includes(it.regionIdx);
      return `<li data-idx="${it.regionIdx}" class="${inSel ? 'in' : ''}">` +
        `<span>${it.label}</span><span class="muted">${fmtScore(res.kind, it.score)}</span></li>`;
    }).join('');
    box.innerHTML = head + `<ul class="sim-list">${list}</ul>`;
    box.querySelectorAll('li').forEach(li =>
      li.onclick = () => { addArea(+li.dataset.idx); li.classList.add('in'); });
    $('#sim-addall').onclick = () =>
      res.items.slice(0, 8).forEach(it => addArea(it.regionIdx));
  }

  method.onchange = render;
  render();
}

function fmtScore(kind, score) {
  if (kind === 'ranking') return score.toFixed(3);       // distance (lower=closer)
  return score >= 100 ? score.toFixed(0) : score.toFixed(2);
}

// ---- utils ------------------------------------------------------------------
function debounce(fn, ms) {
  let t; return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); };
}

init().catch(err => {
  document.body.innerHTML = `<p style="padding:2rem;color:#b00">Failed to load: ${err.message}</p>`;
  console.error(err);
});
