# Estimating Error Rates for Regional Economic Statistics

## Project Aims

This project aims to develop a practical framework for estimating uncertainty in UK regional economic statistics, particularly at sub-national geographical scales where official standard errors are not published. The goal is to provide "ballpark" uncertainty estimates that can:

1. **Flag spurious accuracy** in regional economic analyses
2. **Inform policy discussions** about the reliability of regional comparisons
3. **Support growth rate calculations** with appropriate uncertainty propagation
4. **Counter "the lure of incredible certitude"** (Manski's phrase) in economic forecasting and policy evaluation

The ultimate application is to regional growth rates, though this introduces additional complexity through deflators and temporal dependencies.

## Data Source: abs_gva_se_combo.csv

### Structure
- **Rows**: ~10,296 observations
- **Coverage**: UK ITL1 regions (11 regions plus duplicates with both names and codes)
- **Time span**: 2012-2023
- **Sectors**: 2-digit SIC codes (approximately 78 sectors)

### Variables
| Column | Description |
|--------|-------------|
| SIC | 2-digit Standard Industrial Classification code |
| Description | Sector name |
| Country_Code | ITL1 code (TLD, TLE, etc.) |
| Country_and_Region | Region name (North West, Scotland, etc.) |
| Year | 2012-2023 |
| GVA | Gross Value Added estimate (£m) |
| GVA_SE | Standard Error of GVA estimate |
| gva_min95 | Lower 95% confidence bound (GVA - 1.96*SE) |
| gva_max95 | Upper 95% confidence bound (GVA + 1.96*SE) |

### Key Features
- Standard errors are provided at ITL1 level for 2-digit SIC sectors
- Many cells contain NA values (data suppression or unavailable)
- Confidence intervals already calculated using normal approximation
- Some sectors show very high relative standard errors (SE/GVA ratio)

## The "Spurious Accuracy" Problem

### Manski's Critique
Charles Manski's work on "incredible certitude" argues that policy analysis often presents point estimates without acknowledging fundamental uncertainty. His key works include:

- Manski, C.F. (2013). *Public Policy in an Uncertain World*. Harvard University Press.
- Manski, C.F. (2019). "The Lure of Incredible Certitude." *Economics & Philosophy*, 36(2), 216-245.
- Manski, C.F. (2011). "Policy Analysis with Incredible Certitude." *The Economic Journal*, 121(554), F261-F289.

### Application to Regional Statistics
Regional economic statistics suffer from multiple uncertainty sources:
1. **Sampling error** - The ABS is a survey, not a census
2. **Non-response and imputation** - Missing data filled with modelled values
3. **Apportionment** - National totals allocated to regions using auxiliary data
4. **Definitional uncertainty** - Classification of economic activity
5. **Temporal aggregation** - Annual figures from continuous processes

## Approaches to Error Rate Estimation

### 1. Coefficient of Variation (CV) Scaling

The simplest approach: use observed CV (SE/GVA) patterns at ITL1/SIC2 level and scale for smaller geographies.

**Scaling logic:**
- Error typically scales with √(1/n) where n relates to sample size
- Smaller regions have fewer observations → larger relative errors
- More disaggregated sectors have fewer observations → larger relative errors

**Potential heuristic:**
```
CV_small_area ≈ CV_ITL1 × √(Population_ITL1 / Population_small_area)
```

### 2. Hierarchical Decomposition

If ITL1 SE is known, decomposition to ITL2/ITL3 could assume:
- Variance is additive across sub-regions
- Or use a "design effect" multiplier from survey methodology

### 3. Empirical Patterns

Examine the data to find patterns such as:
- How does CV vary with GVA magnitude?
- Are certain sectors consistently more uncertain?
- Do regions with similar characteristics have similar error patterns?

### 4. If/Then Decision Rules

For practical application, simple rules might suffice:

| Condition | Uncertainty Flag |
|-----------|------------------|
| CV > 50% | "Highly uncertain - use with extreme caution" |
| CV 25-50% | "Substantial uncertainty - indicative only" |
| CV 10-25% | "Moderate uncertainty - reasonable for broad comparisons" |
| CV < 10% | "Relatively reliable for most purposes" |

For sub-ITL1 geographies, multiply thresholds accordingly.

## Issues and Complications

### Growth Rate Uncertainty

Calculating growth rates (GVA_t / GVA_{t-1} - 1) compounds uncertainty:

1. **Correlated errors**: Same establishments surveyed across years → errors are correlated
2. **Ratio of uncertain quantities**: Variance of ratio is complex
3. **Deflator uncertainty**: Real growth requires price indices with their own errors
4. **Small differences, large relative errors**: A 2% growth rate with 10% error on each year's GVA is essentially noise

**Approximate variance of growth rate:**
```
Var(growth) ≈ (1/GVA_0)² × [Var(GVA_1) + growth² × Var(GVA_0)]
```
(assuming independence, which understates uncertainty if errors are negatively correlated)

### Deflator Issues

- Regional deflators often don't exist; national deflators applied to regions
- Sector-specific deflators have their own uncertainty
- Compositional effects when aggregating sectors

### Small Area Estimation Literature

Relevant approaches from survey statistics:
- Fay-Herriot models for borrowing strength across areas
- Empirical Bayes estimation
- Synthetic estimation using covariates

## Relevant Links and Sources

### ONS Documentation
- [ABS Quality and Methodology Information](https://www.ons.gov.uk/businessindustryandtrade/business/businessservices/methodologies/annualbusinesssurveyqmi)
- [Regional GVA (Balanced) Methodology](https://www.ons.gov.uk/economy/grossvalueaddedgva/methodologies/regionalgrossvalueaddedbalancedqmi)
- [Sub-regional Productivity](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/labourproductivity/articles/regionalandsubregionalproductivityintheuk/previousReleases)

### Academic Literature
- Manski, C.F. (2013). *Public Policy in an Uncertain World*. Harvard University Press.
- Rao, J.N.K. & Molina, I. (2015). *Small Area Estimation* (2nd ed.). Wiley.
- Pfeffermann, D. (2013). "New Important Developments in Small Area Estimation." *Statistical Science*, 28(1), 40-68.

### Related Projects
- [What Works Centre for Local Economic Growth](https://whatworksgrowth.org/) - Evidence reviews often note data limitations
- NIESR regional modelling work
- Bank of England Agents' reports (qualitative uncertainty acknowledgment)

### Propagation of Uncertainty
- JCGM 100:2008 (GUM) - Guide to the Expression of Uncertainty in Measurement
- Taylor, J.R. (1997). *An Introduction to Error Analysis* (2nd ed.). University Science Books.

## Suggested Next Steps

1. **Exploratory analysis**: Profile CV patterns in the existing data by sector, region, and GVA magnitude
2. **Literature review**: Check if ONS has published any sub-regional error estimation guidance
3. **Simple model**: Fit CV ~ f(GVA, sector, region) to create a prediction surface
4. **Validation**: Where sub-regional data with errors exists, test scaling assumptions
5. **Decision framework**: Create a practical lookup or function for "error flags"

## Notes on Interpretation

Any error estimates derived from this approach should be presented as:
- **Order of magnitude** guidance, not precise confidence intervals
- **Minimum uncertainty** - actual uncertainty is likely higher due to unquantified sources
- **Conditional on model assumptions** - scaling relationships are approximations

The goal is not precise statistical inference but rather to shift the culture of regional economic analysis toward appropriate epistemic humility.
