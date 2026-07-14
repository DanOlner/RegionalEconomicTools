// Similarity & grouping: four selectable ways to group/filter areas by the shape
// of their economy, all computed in-browser from the feature vectors built in
// data.js (share, LQ, productivity, at the latest year).
//
// Each method returns a common shape the UI consumes identically:
//   { kind: 'ranking' | 'list', items: [{regionIdx, label, score}], ... }
import { state, buildFeatures } from './data.js';

function cosineDistance(a, b) {
  let dot = 0, na = 0, nb = 0;
  for (let i = 0; i < a.length; i++) { dot += a[i] * b[i]; na += a[i] * a[i]; nb += b[i] * b[i]; }
  if (!na || !nb) return 1;
  return 1 - dot / (Math.sqrt(na) * Math.sqrt(nb));
}

function euclidean(a, b) {
  let s = 0;
  for (let i = 0; i < a.length; i++) { const d = a[i] - b[i]; s += d * d; }
  return Math.sqrt(s);
}

// z-score normalise a set of vectors column-wise so productivity distances
// aren't dominated by a few huge-value sectors (finance, real estate).
function zNormalise(vectors) {
  const n = vectors.length, dim = vectors[0].length;
  const mean = new Float64Array(dim), sd = new Float64Array(dim);
  for (let j = 0; j < dim; j++) {
    let m = 0; for (let i = 0; i < n; i++) m += vectors[i][j]; m /= n;
    let v = 0; for (let i = 0; i < n; i++) { const d = vectors[i][j] - m; v += d * d; }
    mean[j] = m; sd[j] = Math.sqrt(v / n) || 1;
  }
  return vectors.map(vec => {
    const out = new Float64Array(dim);
    for (let j = 0; j < dim; j++) out[j] = (vec[j] - mean[j]) / sd[j];
    return out;
  });
}

// ---- Method 1: sector-share similarity (ranking vs an anchor) ---------------
export function rankBySectorShare(anchorIdx) {
  const { features } = buildFeatures();
  const a = features[anchorIdx].share;
  return rankByDistance(anchorIdx, features.map(f => f.share), a, cosineDistance);
}

// ---- Method 3: productivity-profile similarity (ranking vs an anchor) --------
export function rankByProductivity(anchorIdx) {
  const { features } = buildFeatures();
  const z = zNormalise(features.map(f => f.prod));
  return rankByDistance(anchorIdx, z, z[anchorIdx], euclidean);
}

function rankByDistance(anchorIdx, vectors, anchorVec, distFn) {
  const items = vectors.map((v, ri) => ({
    regionIdx: ri,
    label: state.regions[ri].name,
    score: distFn(anchorVec, v),
  })).filter(it => it.regionIdx !== anchorIdx)
    .sort((a, b) => a.score - b.score);
  return { kind: 'ranking', anchorIdx, items };
}

// ---- Method 2: LQ archetypes (auto-tag areas by specialisation) --------------
// Each archetype is one or more sectors; an area gets the tag if its combined LQ
// across those sectors clears the threshold. Tuned to be readable, not canonical.
export const ARCHETYPES = [
  { tag: 'Logistics / warehousing', sectors: ['Warehousing', 'Transport', 'Post'], lq: 1.5 },
  { tag: 'Finance-heavy', sectors: ['Finance'], lq: 1.5 },
  { tag: 'Manufacturing', sectors: ['Food manuf', 'Textiles', 'Wood/paper', 'Chemicals', 'Metals', 'Electronics', 'Mach/transp manuf', 'Furniture', 'Machinery repair/install'], lq: 1.3 },
  { tag: 'Public sector / health / ed', sectors: ['Public admin', 'Education', 'Health', 'Care', 'SocialWork'], lq: 1.25 },
  { tag: 'Professional / knowledge', sectors: ['ICT', 'Media', 'Legal', 'Head office/consult', 'Arch/Eng', 'Research/Ads'], lq: 1.3 },
  { tag: 'Hospitality / tourism', sectors: ['Accommodation', 'Food', 'Arts/ent', 'Recreation'], lq: 1.3 },
  { tag: 'Retail / wholesale', sectors: ['Retail', 'Wholesale', 'Motor'], lq: 1.25 },
  { tag: 'Real estate', sectors: ['Real estate'], lq: 1.5 },
];

function sectorIdxSet(names) {
  const idx = new Map(state.sectors.map((s, i) => [s, i]));
  return names.map(n => idx.get(n)).filter(i => i != null);
}

// Combined LQ over a group of sectors = area's share in those sectors /
// national share in those sectors.
function combinedLQ(regionIdx, sectorIdxs) {
  const { features, natShare } = buildFeatures();
  const share = features[regionIdx].share;
  let areaShare = 0, nat = 0;
  for (const s of sectorIdxs) { areaShare += share[s]; nat += natShare[s]; }
  return nat ? areaShare / nat : 0;
}

// Tags for one area.
export function archetypesFor(regionIdx) {
  return ARCHETYPES.filter(a => combinedLQ(regionIdx, sectorIdxSet(a.sectors)) >= a.lq)
    .map(a => a.tag);
}

// All areas carrying a given archetype tag, ranked by how strongly.
export function areasByArchetype(tag) {
  const arc = ARCHETYPES.find(a => a.tag === tag);
  if (!arc) return { kind: 'list', items: [] };
  const sidx = sectorIdxSet(arc.sectors);
  const items = state.regions.map((r, ri) => ({
    regionIdx: ri, label: r.name, score: combinedLQ(ri, sidx),
  })).filter(it => it.score >= arc.lq)
    .sort((a, b) => b.score - a.score);
  return { kind: 'list', items, meta: { tag, threshold: arc.lq } };
}

// ---- Method 4: pick-a-sector filter -----------------------------------------
// metric: 'share' (proportion of jobs) or 'lq' (location quotient).
export function areasBySector(sectorName, metric, threshold) {
  const { features } = buildFeatures();
  const s = state.sectors.indexOf(sectorName);
  if (s < 0) return { kind: 'list', items: [] };
  const items = state.regions.map((r, ri) => ({
    regionIdx: ri, label: r.name,
    score: metric === 'lq' ? features[ri].lq[s] : features[ri].share[s],
  })).filter(it => it.score >= threshold)
    .sort((a, b) => b.score - a.score);
  return { kind: 'list', items, meta: { sectorName, metric, threshold } };
}

// ---- Optional: k-means grouping on sector shares (bonus grouping view) --------
export function kmeansSectorShare(k = 6, iters = 40) {
  const { features } = buildFeatures();
  const X = zNormalise(features.map(f => f.share));
  const n = X.length, dim = X[0].length;
  // k-means++ style seeding, deterministic-ish: spread by index.
  const centroids = [];
  for (let c = 0; c < k; c++) centroids.push(X[Math.floor(c * n / k)].slice());
  const assign = new Int32Array(n);
  for (let it = 0; it < iters; it++) {
    for (let i = 0; i < n; i++) {
      let best = 0, bd = Infinity;
      for (let c = 0; c < k; c++) { const d = euclidean(X[i], centroids[c]); if (d < bd) { bd = d; best = c; } }
      assign[i] = best;
    }
    const sums = Array.from({ length: k }, () => new Float64Array(dim));
    const counts = new Int32Array(k);
    for (let i = 0; i < n; i++) { counts[assign[i]]++; for (let j = 0; j < dim; j++) sums[assign[i]][j] += X[i][j]; }
    for (let c = 0; c < k; c++) if (counts[c]) for (let j = 0; j < dim; j++) centroids[c][j] = sums[c][j] / counts[c];
  }
  return Array.from(assign);
}
