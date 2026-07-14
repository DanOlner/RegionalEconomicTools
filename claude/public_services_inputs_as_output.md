# Inputs as output: public-service measurement and the regional mirror of the telecoms problem

*A companion to [`deflators_real_vs_nominal.md`](deflators_real_vs_nominal.md). That note showed how a measurement choice (the telecoms deflator) can make a sector's "real output" balloon, flattering regions that specialise in it. This note works the **opposite** case: public services, whose output is measured (especially regionally) as little more than **jobs × wages**, so their productivity is pinned near-flat **by construction** — which makes public-sector-heavy regions look like laggards for reasons that have nothing to do with how well the work is done. Both are the same underlying problem: a region's measured productivity is partly an artefact of **which measurement convention its industry mix happens to trigger**. This sits one layer *beneath* [Beatty & Fothergill's](beattyfothergill.qmd) composition argument — it questions whether the numbers they decompose are even commensurable across sectors. Mostly Claude-written from the repo's files plus the cited ONS sources; expects human edits.*

------------------------------------------------------------------------

# Part 1 — Why public-service output is measured the way it is

## 1. The mirror image, stated

In [`deflators_real_vs_nominal.md`](deflators_real_vs_nominal.md) telecoms is a *market* sector: it sells at prices, so its real output is `revenue ÷ a price index`, and the **price index** does the work — a steep (correct) deflator turned ~flat nominal value added into a ~150× real surge. The distortion is **upward**, and it lands on ICT-heavy places.

Public services are the reverse on every axis:

| | Telecoms (market) | Public services (non-market) |
|---|---|---|
| Has a market price? | Yes | No (free at point of use) |
| Real output = | revenue ÷ **price index** | ≈ **volume of inputs** (regionally: jobs × pay) |
| Measured productivity growth | can **explode** (deflator choice) | ~**zero by construction** |
| Regional distortion | **flatters** ICT-heavy regions | **flattens** public-sector-heavy regions |

So the two sectors break the regional productivity league table in **opposite directions**, for the *same* reason: the number isn't an observation about the place, it's an observation about the measurement rule the place's sector mix invokes.

## 2. The non-market problem and the "output = inputs" convention

A hospital or a state school has no market price, so there's no revenue and no quantity to deflate. The historical fallback (in the UK, [until the late 1990s](https://www.ons.gov.uk/economy/economicoutputandproductivity/publicservicesproductivity/methodologies/publicserviceproductivityestimatestotalpublicservicesqmi)) was to set **output (in volume terms) equal to the sum of deflated inputs** — staff, goods, capital. The fatal feature: if output is *defined* as inputs, then **productivity (output per input) cannot change by definition** — a service could double its effectiveness per worker and the accounts would record nothing.

The [**Atkinson Review (2005)**](https://www.semanticscholar.org/paper/The-Atkinson-Review:-Final-Report:-Measurement-of-Atkinson/aa515465288bbe305ccf2c79b18450344626e78e) (Tony Atkinson, *Measurement of Government Output and Productivity for the National Accounts*) set out nine principles to break this — measure **output directly** (treatments, pupils, cases) and **adjust for quality** (outcomes, attainment). This created the UK's [**public service productivity**](https://www.ons.gov.uk/economy/economicoutputandproductivity/publicservicesproductivity/methodologies/publicserviceproductivityestimatestotalpublicservicesqmi) estimates — direct, partly quality-adjusted output volumes for health, education and more — and the UK is unusually advanced internationally in doing this. A current [National Statistician's Independent Review of the Measurement of Public Services Productivity](https://uksa.statisticsauthority.gov.uk/publication/national-statisticians-independent-review-of-the-measurement-of-public-services-productivity/) is pushing these measures further into the National Accounts ([ONS Public Services Productivity Review](https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/publicserviceproductivityreview)).

**But — and this is the whole regional point — that progress is mostly national.**

## 3. The regional catch: regionally, it's still jobs × wage

The Atkinson-style direct, quality-adjusted measures live in the **national** public-service-productivity workstream. They do **not** flow through to the **regional** GVA numbers this repo uses. The [Regional GVA (production approach) QMI](https://www.ons.gov.uk/economy/grossvalueaddedgva/methodologies/regionalgrossvalueaddedproductionapproachqmi) states the method plainly: for the public-dominated industries — **public administration (O), education (P), health (Q)** — non-market output is apportioned to regions by

> **public-sector employee numbers ([BRES](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/businessregisterandemploymentsurveybresqmi)) × average public-sector earnings ([ASHE](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/methodologies/annualsurveyofhoursandearningsashemethodologyandguidance))**

— exactly the "jobs × wage and nothing else" you described. Three consequences:

1. **Regional public-service real growth ≈ employment growth.** Deflate a wage bill by a wage index and you're left with headcount. A region that holds service quality constant while cutting staff (austerity) records *falling real output*; one that adds staff to a failing service records *rising real output*. Effort and outcomes are invisible.
2. **Inter-regional productivity differences in public services are essentially manufactured by the apportionment.** Because regional GVA(B) is *constrained to the national total*, a region's public-service "productivity" can barely deviate from the national average except through its **earnings mix** — so comparing regions on public-service productivity is close to comparing their public-pay structures, not their efficiency.
3. **No regional quality adjustment.** ONS itself flags the gap: see [*Measuring subnational education output* (2021)](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/measuringsubnationaleducationoutput/2021-12-20), which is explicitly about *developing* the direct subnational measures that don't yet exist — confirming that the current regional figures rest on the input/apportionment method.

------------------------------------------------------------------------

# Part 2 — How it shows up differently in different regions

## 4. The two poles of the same distortion

Put the two notes together and you get a map of *measurement incidence*:

- **ICT/telecoms-heavy regions** (London, the Thames Valley/Reading–Slough, Bristol, Cambridge, pockets of Manchester and Leeds) carry a slab of GVA — SIC 61–63 — whose real growth is **inflated** by the (now corrected, steep) telecoms deflator. They look like productivity stars partly *because of the deflator*.
- **Public-sector-heavy regions** (the North East, Wales, Northern Ireland, much of the South West; and within Yorkshire & Humber, places leaning on NHS trusts, universities and local government) carry a slab of GVA — SIC O/P/Q — whose real growth is **pinned to headcount** by the inputs-as-output convention. They look like laggards partly *because of the method*.

The same national productivity-gap statistic therefore means different things in different places: in one region it's a deflator artefact, in another a public-sector-measurement artefact, and only in the residual is it anything like "how efficiently identical work is done." We can quantify the exposure directly from the repo's own employment data — the BRES/jobs pipeline ([`regionalgva_n_bres.qmd`](../regionalgva_n_bres.qmd), [`prepcode/combine_regGVA_and_BRES.R`](../prepcode/combine_regGVA_and_BRES.R)) already holds employment by SIC and region, so a region's SIC 61–63 share and its O/P/Q share are computable today.

## 5. The level-vs-growth twist, and the link to living standards

Two further wrinkles, both regionally uneven:

- **Levels** of public-service GVA partly track **local public pay**, which is relatively nationally-set and compressed across regions — so the *level* of measured public output per worker is squeezed into a narrow band regardless of real service intensity, while *growth* tracks staffing.
- This compounds the [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md) theme from the other end. There, telecoms' "real output" grew 150× but reached no one's wage (it left as consumer surplus). Here, public services *are* substantially wages — they're a real income floor for exactly the left-behind regions — yet the accounts grant them **no productivity growth and a compressed level**, so a region's biggest stabiliser of household income is also its biggest drag on measured productivity. The "many jobs, low measured productivity" pattern flagged for warehousing in that note has a public-sector twin, and it is concentrated in the same kinds of places.

## 6. Why this is *prior* to Beatty & Fothergill

[`beattyfothergill.qmd`](../beattyfothergill.qmd) and the [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md) §2 discussion take the productivity numbers as given and decompose the regional gap into **composition** — industry mix, occupations, hours, demographics, worklessness — concluding most of it isn't pure inefficiency. That argument is right and important, but it runs **on top of** the numbers. This note (with its sibling) raises the layer underneath: *are the per-sector productivity numbers being decomposed even measuring the same kind of thing?* For telecoms they're deflator-inflated; for public services they're convention-flattened. Some of what B&F attribute to "industrial mix" is therefore not just *which* industries a place has, but **which measurement regimes those industries drag in**. The composition story and the measurement story need to be told together.

------------------------------------------------------------------------

# Part 3 — Ways forward (planning)

1. **Map the exposure.** From the repo's BRES employment data, compute each region's **SIC 61–63 share** (telecoms/ICT, deflator-inflated) against its **O/P/Q share** (public services, input-measured), and plot the two together. This is the single most clarifying artefact: it shows, region by region, which measurement convention is doing the most to its headline productivity — and it complements the deflator-sensitivity re-ranking proposed in [`deflators_real_vs_nominal.md`](deflators_real_vs_nominal.md) §12C.
2. **Flag the public-sector share** whenever reporting regional productivity, and treat public-service real growth as **employment-driven, not efficiency-driven**. Don't rank regions on public-service productivity at all without this caveat.
3. **Watch the national PSP work as a future regional shock.** When ONS's [subnational education output](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/measuringsubnationaleducationoutput/2021-12-20) and wider [public-services productivity](https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/publicserviceproductivityreview) measures reach the regional accounts, public-sector-heavy regions' figures could move sharply — just as the telecoms deflator correction already moved ICT-heavy regions (see [`deflators_real_vs_nominal.md`](deflators_real_vs_nominal.md) §12A).
4. **Extend the §12D house rule.** To "always name the deflator" add: **always name the *method*** — market price-deflated, or non-market inputs-as-output — behind any sector's regional real-output figure, and never compare across the two as if they were the same measurement.

------------------------------------------------------------------------

## Sources

**Public-service / non-market measurement**
- [Atkinson, A.B. (2005), *Atkinson Review: Final Report — Measurement of Government Output and Productivity for the National Accounts*](https://www.semanticscholar.org/paper/The-Atkinson-Review:-Final-Report:-Measurement-of-Atkinson/aa515465288bbe305ccf2c79b18450344626e78e)
- [ONS, *Public service productivity: total, UK QMI*](https://www.ons.gov.uk/economy/economicoutputandproductivity/publicservicesproductivity/methodologies/publicserviceproductivityestimatestotalpublicservicesqmi) · [ONS Public Services Productivity Review](https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/publicserviceproductivityreview) · [National Statistician's Independent Review (UKSA)](https://uksa.statisticsauthority.gov.uk/publication/national-statisticians-independent-review-of-the-measurement-of-public-services-productivity/)

**Regional method**
- [ONS, *Regional GVA (production approach) QMI*](https://www.ons.gov.uk/economy/grossvalueaddedgva/methodologies/regionalgrossvalueaddedproductionapproachqmi) — the employees × earnings apportionment for O/P/Q
- [ONS, *Regional accounts methodology guide: June 2019*](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/methodologies/regionalaccountsmethodologyguidejune2019)
- [ONS, *Measuring subnational education output* (2021)](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/measuringsubnationaleducationoutput/2021-12-20) — ONS developing the direct regional measures that don't yet exist

**In this repo**
- [`deflators_real_vs_nominal.md`](deflators_real_vs_nominal.md) — the market-sector mirror (telecoms deflator)
- [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md) — output ≠ income ≠ welfare; "many jobs, low productivity"
- [`beattyfothergill.qmd`](../beattyfothergill.qmd) — the composition decomposition this note sits beneath
- [`econ_error_rates.md`](econ_error_rates.md) — measurement uncertainty / epistemic humility
- [`regionalgva_n_bres.qmd`](../regionalgva_n_bres.qmd) · [`prepcode/combine_regGVA_and_BRES.R`](../prepcode/combine_regGVA_and_BRES.R) — employment by SIC and region (to compute exposure shares)

------------------------------------------------------------------------

***In one line:*** *Telecoms measurement makes ICT-heavy regions look like productivity stars (a steep deflator inflates real output); public-service measurement makes public-sector-heavy regions look like laggards (output ≈ jobs × wage, so productivity can't grow by construction) — so the regional league table is, in good part, a map of which measurement rule each place's industry mix triggers, a distortion that sits underneath Beatty & Fothergill's composition story rather than beside it.*
