# UK Industry-to-Industry Regional Payment Flows Dataset

A mostly-Claude-code-written document with occasional human edits.

## Overview

This dataset captures **inter-industry payment flows** across UK regions, derived from ONS experimental data on industry-to-industry payments (2017-2025). The data enables analysis of economic linkages both within and between regions, at the 2-digit SIC sector level.

**Source**: [ONS Industry to Industry Payment Flows UK](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/industrytoindustrypaymentflowsuk/2017to2024experimentaldata)

## Data Structure

Each row represents a **directed payment flow** from a payer (industry in region A) to a payee (industry in region B), aggregated to yearly totals.

### Variables

| Variable | Description |
|----------|-------------|
| `year` | Year of transaction (2019-2025 in sample) |
| `payer_sic2digit` | 2-digit SIC code of paying industry (0-96) |
| `payer_ITL1` | ITL1 region code of payer |
| `payee_sic2digit` | 2-digit SIC code of receiving industry |
| `payee_ITL1` | ITL1 region code of payee |
| `pounds` | Total value of payments (£) |
| `num_transactions` | Number of transactions (0 indicates suppressed for disclosure) |
| `payer_ITL1name` | Region name of payer |
| `payee_ITL1name` | Region name of payee |
| `sectioncode_payer` | SIC section code of payer (e.g., "C (10-33)") |
| `sectionname_payer` | SIC section name of payer (e.g., "Manufacturing") |
| `sectioncode_payee` | SIC section code of payee |
| `sectionname_payee` | SIC section name of payee |

### Geographic Coverage

The dataset covers all 12 UK ITL1 regions:

| Code | Region |
|------|--------|
| E12000001 | North East |
| E12000002 | North West |
| E12000003 | Yorkshire and The Humber |
| E12000004 | East Midlands |
| E12000005 | West Midlands |
| E12000006 | East of England |
| E12000007 | London |
| E12000008 | South East |
| E12000009 | South West |
| W99999999 | Wales |
| S99999999 | Scotland |
| N99999999 | Northern Ireland |

### SIC Sections

The 2-digit SIC codes are grouped into sections:

| Section | SIC Range | Description |
|---------|-----------|-------------|
| A (1-3) | 1-3 | Agriculture, forestry and fishing |
| B (5-9) | 5-9 | Mining and quarrying |
| C (10-33) | 10-33 | Manufacturing |
| D (35) | 35 | Electricity, gas, steam and air conditioning supply |
| E (36-39) | 36-39 | Water supply; sewerage and waste management |
| F (41-43) | 41-43 | Construction |
| G (45-47) | 45-47 | Wholesale and retail trade; repair of motor vehicles |
| H (49-53) | 49-53 | Transportation and storage |
| I (55-56) | 55-56 | Accommodation and food service activities |
| J (58-63) | 58-63 | Information and communication |
| K (64-66) | 64-66 | Financial and insurance activities |
| L (68) | 68 | Real estate activities |
| M (69-75) | 69-75 | Professional, scientific and technical activities |
| N (77-82) | 77-82 | Administrative and support service activities |
| P (85) | 85 | Education |
| Q (86-88) | 86-88 | Human health and social work activities |
| R (90-93) | 90-93 | Arts, entertainment and recreation |
| S (94-96) | 94-96 | Other service activities |

**Note**: SIC code `0` appears in the data with `NA` section values - this represents transactions where the sector is unknown/unclassified (approximately 4-6% of transactions).

## Data Characteristics

### Scale

- **Full dataset**: ~1.3 million rows
- **Sample**: 300 rows (random sample)
- **Temporal coverage**: 2019-2025 (yearly aggregates)

### Value Distribution

From the sample, payment values range dramatically:
- **Minimum**: £0 (some flows recorded with zero value)
- **Maximum**: £517,949,000 (West Midlands Wholesale/Retail to South East Financial Services, 2020)
- **Typical large flows**: £10-50 million
- **Small flows**: £1,000 - £100,000

### Transaction Counts

- Many rows show `num_transactions = 0`, indicating disclosure control (values too small/identifiable)
- Non-zero transaction counts range from ~100 to ~46,600 in the sample

## Analytical Potential

This dataset enables several input-output and network analysis approaches:

### 1. Regional Input-Output Tables

Construct **regional supply-use tables** showing:
- Which sectors within a region purchase from which other sectors
- Import/export relationships between regions by sector

### 2. Inter-Regional Trade Matrices

For any sector pair, create a 12×12 matrix showing payment flows between all UK regions.

### 3. Network Analysis

- **Nodes**: Region-sector combinations (up to 12 regions × ~80 sectors)
- **Edges**: Payment flows with magnitude weights
- Analyse centrality, clustering, community structure

### 4. Leakage Analysis

Compare **internal** vs **external** spending by region:
- Internal: payer_ITL1 == payee_ITL1
- External: payer_ITL1 != payee_ITL1
- Track changes over time (already explored in the R script)

### 5. Sectoral Dependency Mapping

For a given region, identify:
- Key supplier sectors (both local and from other regions)
- Key customer sectors
- Vulnerability to supply chain disruptions

## Key Observations from Sample

1. **London as a hub**: Many large-value flows involve London, particularly in financial services and professional services

2. **Manufacturing supply chains**: Cross-regional flows between manufacturing sub-sectors (SIC 10-33) show complex supply chain patterns

3. **Within-region dominance**: Many of the largest flows are within the same region (e.g., West Midlands Construction to West Midlands Construction: £26.9m)

4. **Services concentration**: Professional services (M), Administrative services (N), and Financial services (K) appear frequently in high-value flows

## Data Quality Notes

- **Disclosure control**: Zero transaction counts indicate suppressed data
- **Unknown sectors**: ~4-6% of transactions have SIC code 0 (unknown)
- **Experimental data**: ONS labels this as experimental, so methodology may evolve
- **Completeness**: Some region-sector pairs may have missing data

## Next Steps for Analysis

Potential analytical directions:

1. **Build regional IO matrices** - aggregate to section level for manageable 18×18 matrices per region
2. **Calculate multipliers** - derive Type I and Type II multipliers from the flow data
3. **Network visualisation** - chord diagrams or Sankey flows for major payment streams
4. **Temporal analysis** - track structural changes 2019-2025 (including COVID impacts)
5. **Comparative regional analysis** - identify regions with strong internal linkages vs those dependent on external supply chains

---

## Analysis 1: Flow Location Quotients

**Script**: `bits_of_code/ind_to_indpayments_regional_IO.R`

### What It Does

This analysis applies **Location Quotients (LQs)** to inter-industry payment flows rather than to employment or output. Traditional LQs compare a region's sector share to the national share. Here, we extend this to **flow pairs**: how does the proportion of spending from sector A to sector B in a region compare to the same flow proportion nationally?

### The Formula

```
LQ_ij = (Flow_ij in region / Total regional flows) / (Flow_ij in UK / Total UK flows)
```

Where:
- `i` = payer (spending) sector
- `j` = payee (receiving) sector

### Interpretation

| LQ Value | log2(LQ) | Meaning |
|----------|----------|---------|
| 1.0 | 0 | Flow matches UK average |
| 2.0 | +1 | Flow is 2× over-represented |
| 4.0 | +2 | Flow is 4× over-represented |
| 0.5 | -1 | Flow is 2× under-represented |
| 0.25 | -2 | Flow is 4× under-represented |

The log2 transformation creates a symmetric scale where +1 and -1 represent the same magnitude of deviation in opposite directions.

### Reading the Heatmaps

Each heatmap shows a **section × section matrix** for one region:

- **Y-axis (rows)**: Payer section (who is spending)
- **X-axis (columns)**: Payee section (who is receiving)
- **Colour**:
  - **Red**: Over-represented linkage (this flow is proportionally larger in this region than in the UK)
  - **White**: Matches UK average
  - **Blue**: Under-represented linkage (this flow is proportionally smaller in this region)

#### Example Interpretation

If Yorkshire shows a red cell at (Manufacturing → Professional services), it means:
> "When Yorkshire businesses spend money, a larger share of Manufacturing's spending goes to Professional services than you'd expect based on the UK pattern."

This could indicate:
- Strong local professional services supporting manufacturing
- Specialised technical/engineering consultancy clusters
- Different manufacturing sub-sectors with different service needs

#### Diagonal vs Off-Diagonal

- **Diagonal cells** (same sector paying itself): Internal sector transactions (e.g., Construction paying Construction)
- **Off-diagonal cells**: Cross-sector linkages revealing supply chain structure

### Outputs

1. **`flow_lqs`**: Data frame with LQ values for every region × payer section × payee section combination

2. **`lq_matrices`**: Named list of 18×18 matrices, one per region. Access with:
   ```r
   lq_matrices[["Yorkshire and The Humber"]]
   ```

3. **`plot_lq_heatmap()`**: Function to generate heatmap for any region:
   ```r
   plot_lq_heatmap("London", flow_lqs)
   plot_lq_heatmap("Scotland", flow_lqs, use_log = FALSE)  # Raw LQ scale
   ```

4. **`distinctive_linkages`**: Table of the 10 most unusual flows (highest absolute log2 LQ) per region - useful for quickly identifying what makes each regional economy distinctive

### What This Reveals

- **Regional specialisation patterns**: Which inter-industry relationships are unusually strong or weak
- **Supply chain structure**: How sectors connect differently across regions
- **Economic distinctiveness**: Regions with many extreme LQs have unusual economic structures; those with mostly white heatmaps mirror the national pattern
- **Potential vulnerabilities**: Unusually strong dependencies on specific linkages
