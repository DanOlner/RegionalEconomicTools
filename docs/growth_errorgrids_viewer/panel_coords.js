// Panel coordinates for 3x4 facet grid (3600x4200px images, 12x14in at 300dpi)
// Eyeballed from the rendered ggplot output. Layout is identical for all sectors.
// Grid order: alphabetical by Region_name, row-major, ncol=3
//
// Row 1: East, East Midlands, London
// Row 2: North East, North West, Northern Ireland
// Row 3: Scotland, South East, South West
// Row 4: Wales, West Midlands, Yorkshire and The Humber
//
// Approximate panel bounds (in native pixels of the 3600x4200 image):
//   Left margin (y-axis label + tick labels): ~230px
//   Right margin: ~30px
//   Top margin (title + subtitle): ~190px
//   Bottom margin (x-axis label + legend + caption): ~300px
//   Strip label height: ~55px per row
//   Gap between panel rows: ~80px (strip label + spacing)
//   Gap between panel columns: ~55px
//
//   Usable width: 3600 - 230 - 30 = 3340; per column: (3340 - 2*55) / 3 ≈ 1077
//   Usable height: 4200 - 190 - 300 = 3710; per row: (3710 - 3*80) / 4 ≈ 868

const PANEL_COORDS = [
  // Row 1
  { "region": "East",           "region_safe": "East",          "left": 230, "right": 1290, "top": 245, "bottom": 1085 },
  { "region": "East Midlands",  "region_safe": "EastMidlands",  "left": 1345, "right": 2405, "top": 245, "bottom": 1085 },
  { "region": "London",         "region_safe": "London",        "left": 2460, "right": 3520, "top": 245, "bottom": 1085 },
  // Row 2
  { "region": "North East",     "region_safe": "NorthEast",     "left": 230, "right": 1290, "top": 1165, "bottom": 2005 },
  { "region": "North West",     "region_safe": "NorthWest",     "left": 1345, "right": 2405, "top": 1165, "bottom": 2005 },
  { "region": "Northern Ireland","region_safe": "NorthernIreland","left": 2460, "right": 3520, "top": 1165, "bottom": 2005 },
  // Row 3
  { "region": "Scotland",       "region_safe": "Scotland",      "left": 230, "right": 1290, "top": 2085, "bottom": 2925 },
  { "region": "South East",     "region_safe": "SouthEast",     "left": 1345, "right": 2405, "top": 2085, "bottom": 2925 },
  { "region": "South West",     "region_safe": "SouthWest",     "left": 2460, "right": 3520, "top": 2085, "bottom": 2925 },
  // Row 4
  { "region": "Wales",          "region_safe": "Wales",         "left": 230, "right": 1290, "top": 3005, "bottom": 3845 },
  { "region": "West Midlands",  "region_safe": "WestMidlands",  "left": 1345, "right": 2405, "top": 3005, "bottom": 3845 },
  { "region": "Yorkshire and The Humber", "region_safe": "YorkshireandTheHumber", "left": 2460, "right": 3520, "top": 3005, "bottom": 3845 }
];