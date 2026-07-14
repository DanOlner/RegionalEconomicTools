// Data layer: load the slim columnar JSON and reshape into per-area records,
// plus derive the feature vectors the similarity module needs.
//
// The JSON is columnar (parallel arrays) with dictionaries for sectors/regions/
// years — see prepcode/export_bresgva2d_web.R. We expand it once into convenient
// nested structures held in module state.

export const state = {
  raw: null,          // parsed JSON
  sectors: [],        // [name, ...]
  colours: [],        // [hex, ...] aligned to sectors
  colourFor: {},      // name -> hex
  years: [],          // [2016, ...]
  regions: [],        // [{name, itl3, itl2, itl1}, ...]
  // byRegionYear[regionIdx][yearIdx] = [{sector, sectorIdx, jobs, gvaperjob, gva}, ...]
  byRegionYear: [],
  // features per region at the reference year (latest): built lazily
  features: null,
  refYearIdx: 0,
};

export async function loadData(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to load ${url}: ${res.status}`);
  const d = await res.json();
  state.raw = d;
  state.sectors = d.sectors;
  state.colours = d.colours;
  state.years = d.years;
  state.regions = d.regions;
  state.colourFor = {};
  state.sectors.forEach((s, i) => { state.colourFor[s] = state.colours[i]; });
  state.refYearIdx = d.years.length - 1; // latest year

  // Expand columnar rows into byRegionYear[region][year] = [rows]
  const nR = d.regions.length;
  const nY = d.years.length;
  const bry = Array.from({ length: nR }, () =>
    Array.from({ length: nY }, () => []));
  const c = d.cols;
  for (let i = 0; i < c.region.length; i++) {
    bry[c.region[i]][c.year[i]].push({
      sectorIdx: c.sector[i],
      sector: d.sectors[c.sector[i]],
      jobs: c.jobs[i],
      gvaperjob: c.gvaperjob[i],   // may be null
      gva: c.gva[i],               // may be null
    });
  }
  state.byRegionYear = bry;
  return state;
}

// Rows for one area + year, cleaned (drop zero/negative/na jobs).
export function areaRows(regionIdx, yearIdx) {
  const rows = state.byRegionYear[regionIdx][yearIdx] || [];
  return rows.filter(r => r.jobs != null && r.jobs > 0);
}

// Region name -> index (built once).
let nameIndex = null;
export function regionIdxByName(name) {
  if (!nameIndex) {
    nameIndex = new Map();
    state.regions.forEach((r, i) => nameIndex.set(r.name, i));
  }
  return nameIndex.get(name);
}

// ---- Feature vectors for the similarity module -----------------------------
// Built at the reference (latest) year. For each region:
//   share[s]      = jobs in sector s / total jobs           (employment mix)
//   lq[s]         = share[s] / nationalShare[s]              (specialisation)
//   productivity[s] = gva-per-job in sector s (null-safe)   (productivity mix)
// nationalShare is summed from the data itself, keeping this self-contained.
export function buildFeatures() {
  if (state.features) return state.features;
  const nS = state.sectors.length;
  const y = state.refYearIdx;

  const natJobs = new Float64Array(nS);
  let natTotal = 0;
  const perRegionJobs = state.regions.map((_, ri) => {
    const v = new Float64Array(nS);
    let tot = 0;
    for (const row of areaRows(ri, y)) {
      v[row.sectorIdx] += row.jobs;
      tot += row.jobs;
      natJobs[row.sectorIdx] += row.jobs;
      natTotal += row.jobs;
    }
    return { v, tot };
  });

  const natShare = new Float64Array(nS);
  for (let s = 0; s < nS; s++) natShare[s] = natTotal ? natJobs[s] / natTotal : 0;

  // national mean gva-per-job per sector, to impute missing productivity values
  const natProdSum = new Float64Array(nS);
  const natProdN = new Float64Array(nS);
  for (let ri = 0; ri < state.regions.length; ri++) {
    for (const row of areaRows(ri, y)) {
      if (row.gvaperjob != null && isFinite(row.gvaperjob)) {
        natProdSum[row.sectorIdx] += row.gvaperjob;
        natProdN[row.sectorIdx] += 1;
      }
    }
  }
  const natProd = new Float64Array(nS);
  for (let s = 0; s < nS; s++) {
    natProd[s] = natProdN[s] ? natProdSum[s] / natProdN[s] : 0;
  }

  const features = state.regions.map((_, ri) => {
    const { v, tot } = perRegionJobs[ri];
    const share = new Float64Array(nS);
    const lq = new Float64Array(nS);
    for (let s = 0; s < nS; s++) {
      share[s] = tot ? v[s] / tot : 0;
      lq[s] = natShare[s] ? share[s] / natShare[s] : 0;
    }
    const prod = new Float64Array(nS);
    const rowsBySector = new Map(areaRows(ri, y).map(r => [r.sectorIdx, r]));
    for (let s = 0; s < nS; s++) {
      const r = rowsBySector.get(s);
      prod[s] = (r && r.gvaperjob != null && isFinite(r.gvaperjob))
        ? r.gvaperjob : natProd[s];
    }
    return { share, lq, prod, totalJobs: tot };
  });

  state.features = { features, natShare, natProd };
  return state.features;
}
