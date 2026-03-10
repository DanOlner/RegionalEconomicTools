# Estimating Error Rates for Regional Economic Statistics

A mostly-Claude-code-written document with occasional human edits.

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

## Type I and II Errors, The Lost Boys, and the Lure of Incredible Certitude

### A Mnemonic for Type I and II Errors

> "Never confuse Type I and II errors again: Just remember that the Boy Who Cried Wolf caused both Type I & II errors, in that order. First everyone believed there was a wolf, when there wasn't. Next they believed there was no wolf, when there was. Substitute 'effect' for 'wolf' and you're done."

- **Type I error (false positive)**: Rejecting the null hypothesis when it's true — the villagers believed there was a wolf (an effect) when there wasn't one.
- **Type II error (false negative)**: Failing to reject the null hypothesis when it's false — the villagers believed there was no wolf (no effect) when there was one.

### The Lost Boys: Incredible Certitude at the Cliff Edge

> In the movie *The Lost Boys*, David and his west-coast vampire buddies race motorbikes across a foggy landscape. Michael (unaware of the blood-drinking habits of his new associates) tries to keep up with them. They egg him on — but then he spots the faint beam of a lighthouse, puts two and two together and barely avoids hurtling over a cliff edge. One might say David was claiming "incredible certitude" (Manski) about the cliff's location. "Don't worry Michael, it's definitely, definitely miles from here."

#### The Lost Boys in a Type I/II Error Framework

David's false assurance maps neatly onto the error typology:

- **Type I error (false positive) — "There IS safe ground ahead"**: David asserts with incredible certitude that the terrain ahead is safe — that there is solid ground to ride on. This is a false positive: claiming an effect (safety/ground) exists when it doesn't. Anyone who accepts David's claim at face value commits a Type I error — they "detect" safe passage where there is none. In Manski's terms, David is performing **wishful extrapolation**: projecting the observed ground behind them into the unobserved fog ahead, with no evidential basis.

- **Type II error (false negative) — "There is NO cliff"**: The complementary error is failing to detect the cliff — the real danger that is present. By riding confidently into the fog, David's behaviour implicitly communicates "there is no cliff here." This is a Type II error: failing to reject the null hypothesis of "no danger" when danger is very much present. Michael nearly commits this error but is saved by an independent signal (the lighthouse) that cuts through David's manufactured certitude.

#### The Decision Table: What Michael Does and What It Costs

The scene plays out all four cells of a classic decision matrix. But the real payoff is in translating each cell into the policy world — where there is no lighthouse, and you rarely find out you went over the cliff until it's too late.

| Reality | Michael believes / acts as if | Error type | The scene | The policy analogy |
|---------|-------------------------------|------------|-----------|-------------------|
| **Cliff is NOT near** | "Cliff IS near" (brakes hard, swerves) | **Type I (false positive)** | Over-cautious braking when it was actually safe. Embarrassing, slower, but not deadly. | A region's GVA figures look suspiciously volatile, so you hold off on a major investment decision. Turns out the numbers were fine — you were over-cautious, and the opportunity cost is real but recoverable. |
| **Cliff IS near** | "Cliff is NOT near" (keeps speeding) | **Type II (false negative)** | The catastrophic one: he trusts David's "definitely miles away" and goes over the edge. | The numbers say Region A is outperforming Region B by 3%. Nobody checks the error bars. Policy redirects £50m of funding. The difference was noise. The money is spent, the damage is done, and nobody connects the outcome to the original bad number. |
| Cliff NOT near | "Cliff NOT near" | Correct | Keeps up, no harm. | The data is solid, the policy is well-founded, resources flow to the right place. The boring, good outcome. |
| Cliff IS near | "Cliff IS near" | Correct | Sees the lighthouse, updates belief, avoids disaster. | Someone asks "how uncertain is this number?", checks the ABS standard errors, finds the confidence intervals overlap massively, and the policy conversation shifts from "who's winning?" to "what can we actually distinguish from noise?" |

The asymmetry matters: in the scene, the Type II error kills you. In regional policy, it doesn't kill anyone — but it silently misallocates resources, entrenches narratives about "left behind" or "booming" regions based on statistically meaningless differences, and makes it nearly impossible to evaluate whether interventions worked (because the baseline was never reliable). The Type I error — being too cautious about a number — is the kind of mistake you can recover from. The Type II — trusting a number that was fog all along — is the kind that compounds invisibly.

#### The Fog as a Metaphor for Uncertainty

The fog in the scene is a powerful metaphor for the uncertainty surrounding regional economic statistics:

- **The fog** = missing standard errors, data suppression, model dependence, and all the unquantified uncertainty sources listed in this document
- **David's certitude** = point estimates presented without error bars, precise-looking GVA figures reported to spurious decimal places, growth rates quoted as if they are known quantities
- **The lighthouse** = independent checks on data quality — the ABS standard errors, cross-validation against alternative sources, or simply the discipline of asking "how uncertain is this number?"
- **The cliff** = policy decisions made on the basis of numbers that turn out to be noise — misallocating resources between regions, claiming one area is "outperforming" another when the difference is well within the margin of error

The lesson: when someone presents regional economic statistics with incredible certitude, look for the lighthouse before you follow them into the fog.

### Incredible Certitude and Overconfidence: Sources and Examples

Manski's critique sits within a broader literature on overconfidence and false precision in analysis and decision-making. As social psychologist Scott Plous wrote: *"No problem in judgment and decision making is more prevalent and more potentially catastrophic than overconfidence."*

#### Manski's Typology of Incredible Certitude

Manski identifies several recurring patterns ([Manski 2011](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1468-0297.2011.02457.x); [Manski 2019](https://www.cambridge.org/core/journals/economics-and-philosophy/article/abs/lure-of-incredible-certitude/A1F09783377B05F20B83543CD40C7639)):

| Pattern | Description | Regional Statistics Example |
|---------|-------------|----------------------------|
| **Conventional certitude** | Statistics accepted as true by convention | GVA figures treated as exact measurements |
| **Duelling certitudes** | Contradictory point estimates, each presented with full confidence | Competing regional growth league tables that disagree but never acknowledge uncertainty |
| **Wishful extrapolation** | Projecting from limited evidence to broad conclusions | Using short-run ABS data to make claims about long-run regional trends |
| **Conflating science and advocacy** | Analysis aimed at a predetermined conclusion | Selecting the geography or time period that tells the desired story |
| **Illogical certitude** | Logical errors (e.g., treating non-rejection as proof of null) | "No statistically significant difference between regions" interpreted as "regions are equal" |

#### Communicating Uncertainty in Official Statistics

Manski argues ([Manski 2018](https://www.pnas.org/doi/abs/10.1073/pnas.1722389115)) that federal statistical agencies commonly report official economic statistics as point estimates without accompanying measures of error, and users may incorrectly view them as error-free. He notes that UK institutions have been somewhat ahead of the US in this regard, with some official outputs including uncertainty ranges.

Mazzi, Mitchell & Carausu ([2021](https://journals.sagepub.com/doi/abs/10.2478/jos-2021-0013)) examine the challenge of measuring and communicating uncertainty in official economic statistics more broadly, noting the persistent gap between what statisticians know about data quality and what reaches end users.

#### Overprecision and Its Consequences

Research on overconfidence distinguishes three forms ([Moore & Healy](https://learnmoore.org/mooredata/3FOC.pdf)):

1. **Overestimation** — thinking your performance is better than it is
2. **Overplacement** — thinking you're better than others
3. **Overprecision** — unwarranted certainty in the accuracy of your beliefs

Overprecision is the most directly relevant to regional statistics. Studies show that when participants give 90% confidence intervals, actual hit rates are often [as low as 50%](https://learnmoore.org/mooredata/HOC.pdf) — people draw their intervals far too narrowly. In a [study of national security officials](https://tnsr.org/2025/09/the-world-is-more-uncertain-than-you-think-assessing-and-combating-overconfidence-among-2000-national-security-officials/), statements estimated to have a 90% chance of being true were actually true only 58% of the time.

The consequences of overprecision compound in planning and forecasting: if decision-makers are too sure of their forecasts, planning focuses too tightly on a favoured outcome and too little time is spent on contingencies ([Renascence](https://www.renascence.io/journal/overprecision-excessive-confidence-in-precision-of-information)).

#### False Precision ("Spurious Accuracy")

False precision — sometimes called "[spurious accuracy](https://ideas.repec.org/h/pal/palchp/978-0-230-59501-9_7.html)" — is defined as "a pretence to precision that is either unattainable or useless (or both)." When a GVA figure is reported as £12,347m rather than "roughly £12bn, give or take a billion or so", the [implied precision shapes perception](https://orgvitality.com/blog/false-precision) — exact-looking figures are perceived as more trustworthy regardless of underlying uncertainty.



#### A Taxonomy of Type I and II Errors in Regional Economic Policy

Error management theory argues that when the costs of false positives and false negatives are asymmetric, it is rational to be biased toward the less costly error ([Johnson et al., 2013](https://www.sciencedirect.com/science/article/abs/pii/S0169534713001365)). The stick-or-snake heuristic captures this nicely: mistaking a stick for a snake (Type I — false alarm) costs you a fright; mistaking a snake for a stick (Type II — miss) costs you a bite. In regional economic policy, the question is: which errors can we live with, and which ones compound silently into misallocated billions?

##### Type I Errors (False Positives / False Alarms): "Seeing a cliff that isn't there"

These are cases where we *detect* a difference, effect, or problem that doesn't actually exist.

**Tolerable Type I errors — over-caution that is recoverable:**

| Error | What happens | Why it's tolerable |
|-------|-------------|-------------------|
| **Flagging a spurious regional difference** | Region A's GVA growth looks significantly higher than Region B's; further investigation reveals the gap is within the margin of error. | The investigation cost time, but no resources were misallocated. You braked before the non-existent cliff. |
| **Over-estimating uncertainty in a solid figure** | A well-measured sector's GVA is treated as unreliable; policy-makers hedge rather than act decisively. | Opportunity cost is real but recoverable — the data will still be there next quarter. Conservative interpretation of noisy data is defensible. |
| **Demanding more evidence before acting** | A promising regional intervention is delayed because the evaluation evidence doesn't clear a high bar. | The intervention can be piloted later. Delay is frustrating but not catastrophic. Cf. the [precautionary principle](https://en.wikipedia.org/wiki/Precautionary_principle) applied to spending rather than regulation. |
| **False alarm on data quality** | An analyst flags ABS figures as potentially unreliable due to high CV, triggering a review. The data turns out to be fine. | The review consumed resources but built institutional knowledge about data quality. A culture of questioning numbers is a feature, not a bug. |

**Costly Type I errors — false alarms that do real damage:**

| Error | What happens | Why it's costly |
|-------|-------------|----------------|
| **Crying wolf until nobody listens** | Repeated false alarms about data quality cause policy-makers to dismiss *all* uncertainty warnings, including valid ones. This is the Boy Who Cried Wolf completing the cycle: Type I errors breeding Type II errors. | Erodes trust in legitimate quality flags. When a real problem appears, it gets ignored. |
| **Phantom regional crisis triggers intervention** | A noisy dip in a small area's GVA is interpreted as genuine economic decline; emergency funding is redirected from areas with real need. | Resources misallocated based on statistical noise. Opportunity cost falls on the genuinely struggling area that lost funding ([Cappelen et al., 2020](https://www.aeaweb.org/articles?id=10.1257%2Faer.20211015) — the trade-off between false positives and false negatives in fairness). |
| **Spurious "success story" attracts wrong lessons** | A region's growth figure is noise, but it's held up as a policy success. Other regions try to replicate the (non-existent) formula. | Policy learning is corrupted. Resources flow to imitate a mirage. |
| **False positive in programme evaluation** | An intervention is judged to have "worked" based on a noisy before-after comparison. The programme is scaled up nationally. | Scaling a programme that didn't actually work wastes public money and crowds out alternatives that might have been effective. |

##### Type II Errors (False Negatives / Misses): "Missing the cliff that is there"

These are cases where we *fail to detect* a difference, effect, or problem that genuinely exists.

**Tolerable Type II errors — missed signals that don't much matter:**

| Error | What happens | Why it's tolerable |
|-------|-------------|-------------------|
| **Missing a tiny real difference between large regions** | ITL1 regions A and B genuinely differ by 0.3% in GVA growth, but the confidence intervals overlap and we call it "no significant difference." | The real difference is too small to be policy-relevant. Not every true effect deserves a policy response. Statistical significance is not the same as practical significance. |
| **Failing to detect a minor sectoral shift** | A 2-digit SIC sector is slowly declining in one region but the signal is lost in year-to-year noise. | If the decline is small and gradual, it will eventually become detectable. Other indicators (employment, business counts) may pick it up sooner. |
| **Not spotting a successful intervention amid noise** | A genuinely effective local programme doesn't show a statistically significant impact because the area is too small for the effect to clear the noise floor. | Unfortunate, but if the programme is truly effective it can be evaluated at a larger scale or with better methods. The programme continues regardless of the evaluation. |
| **Overlooking a real but modest convergence trend** | Regions are slowly converging in productivity, but the trend is indistinguishable from noise over a 5-year window. | Convergence, if real, will become apparent over longer time horizons. Premature claims of convergence based on noisy short-run data would be worse (a Type I error). |

**Costly Type II errors — the silent, compounding misses:**

| Error | What happens | Why it's costly |
|-------|-------------|----------------|
| **Trusting a false "no difference" between regions** | Two regions' GVA figures look similar, so funding is spread equally. In reality, one region is in serious decline but the data is too noisy to detect it. | The struggling region doesn't get the help it needs. By the time the decline becomes visible in the data, it may be entrenched. This is Michael riding into the fog. |
| **Missing genuine regional divergence** | Regions are pulling apart in productivity, but overlapping confidence intervals lead analysts to report "no statistically significant difference." Policy-makers conclude all is well. | Illogical certitude in Manski's terms — treating non-rejection of the null as evidence for the null. Divergence compounds; early intervention is cheaper than late rescue. |
| **Failing to detect that an intervention didn't work** | A high-profile regional programme absorbs £100m. Evaluation is inconclusive due to noisy data. "Inconclusive" is reported as "no evidence of harm" rather than "we can't tell." The programme continues. | Sunk cost fallacy meets Type II error. Resources continue flowing to an ineffective programme. The fog hides the cliff, and nobody looks for the lighthouse. |
| **Accepting point estimates as ground truth** | GVA figures are published without standard errors at sub-regional level. Users treat them as exact. Real differences between areas are obscured; real similarities are mistaken for differences. | This is the default condition of most regional analysis. The absence of published uncertainty *is* the Type II error — the error that isn't detected is the error in the numbers themselves. Manski's "conventional certitude" at its most pervasive. |
| **Deflator-masked decline** | A region's nominal GVA rises, but after applying an uncertain deflator, real growth is actually negative. The deflator's own error is unacknowledged, so the decline goes undetected. | Policy-makers celebrate nominal growth while real living standards fall. The deflator's uncertainty — rarely quantified at regional level — acts as a second layer of fog. |

##### The Asymmetry: Why Type II Errors Are Usually Worse in This Domain

In the Lost Boys scene, the asymmetry is stark: the Type I error (braking unnecessarily) costs you some embarrassment; the Type II error (missing the cliff) kills you. In regional economic policy, the asymmetry is subtler but similar in structure:

- **Type I errors are usually visible and self-correcting.** If you raise a false alarm about data quality, someone will check and find it's fine. If you hold back funding out of caution, the pressure to spend will reassert itself. False positives generate friction, investigation, and correction.

- **Type II errors are usually invisible and self-reinforcing.** If you miss a real regional decline because the data is too noisy, nobody knows what they didn't see. If a programme doesn't work but the evaluation can't detect this, the programme continues. Misses don't generate their own correction signal — they just compound.

This asymmetry argues for a general bias toward the Type I side in regional statistics: it is better to be over-cautious about point estimates, to flag uncertainty even when you're not sure it matters, and to demand error bars even when convention says they're unnecessary. The cost of a few false alarms is far lower than the cost of riding confidently into the fog.

As [Johnson et al. (2013)](https://www.sciencedirect.com/science/article/abs/pii/S0169534713001365) note in the context of error management theory: when the costs of the two error types are asymmetric, the optimal strategy is not to minimise total errors but to be biased toward the cheaper one. In regional economic policy, that means erring on the side of caution — seeing cliffs that aren't there, rather than missing the ones that are.

#### Key References

- Manski, C.F. (2011). "[Policy Analysis with Incredible Certitude](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1468-0297.2011.02457.x)." *The Economic Journal*, 121(554), F261-F289.
- Manski, C.F. (2018). "[Communicating Uncertainty in Policy Analysis](https://www.pnas.org/doi/abs/10.1073/pnas.1722389115)." *PNAS*, 116(16), 7634-7641.
- Manski, C.F. (2019). "[The Lure of Incredible Certitude](https://www.cambridge.org/core/journals/economics-and-philosophy/article/abs/lure-of-incredible-certitude/A1F09783377B05F20B83543CD40C7639)." *Economics & Philosophy*, 36(2), 216-245.
- Mazzi, G.L., Mitchell, J. & Carausu, F. (2021). "[Measuring and Communicating the Uncertainty in Official Economic Statistics](https://journals.sagepub.com/doi/abs/10.2478/jos-2021-0013)." *Journal of Official Statistics*.
- Moore, D.A. & Healy, P.J. "[The Trouble with Overconfidence](https://learnmoore.org/mooredata/3FOC.pdf)." *Psychological Review*.
- Mandel, D.R. et al. (2025). "[The World Is More Uncertain Than You Think](https://tnsr.org/2025/09/the-world-is-more-uncertain-than-you-think-assessing-and-combating-overconfidence-among-2000-national-security-officials/)." *Texas National Security Review*.
- Plous, S. (1993). *The Psychology of Judgment and Decision Making*. McGraw-Hill.
- Johnson, D.D.P., Blumstein, D.T., Fowler, J.H. & Haselton, M.G. (2013). "[The evolution of error: error management, cognitive constraints, and adaptive decision-making biases](https://www.sciencedirect.com/science/article/abs/pii/S0169534713001365)." *Trends in Ecology & Evolution*, 28(8), 474-481.
- Cappelen, A.W., Cappelen, C. & Tungodden, B. (2020). "[Second-Best Fairness: The Trade-off between False Positives and False Negatives](https://www.aeaweb.org/articles?id=10.1257%2Faer.20211015)." *American Economic Review*.
- Nair, S. & Howlett, M. (2023). "[Beyond precautionary principle: policy-making under uncertainty and complexity](https://www.tandfonline.com/doi/full/10.1080/25741292.2023.2229090)." *Policy Design and Practice*.
- Arrow, K.J. & Fischer, A.C. (1974). "Environmental Preservation, Uncertainty, and Irreversibility." *Quarterly Journal of Economics*, 88(2), 312-319.
