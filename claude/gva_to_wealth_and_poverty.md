# GVA → regional wealth and poverty: the value-to-living-standards link in this work

*A collation of where the GVA-to-wealth/poverty link already shows up in this repo (Part 1), followed by research on the frameworks and datasets that would let it be made explicit (Part 2). Mostly Claude-written from the repo's own files plus external sources; expects human edits.*

## The question, stated precisely

"Does GVA connect through to regional wealth and poverty?" is really a question about a **chain of translations**, each of which leaks:

> **production in a place (GVA)** → *who captures the value (labour share vs profit)* → **income available to residents (GDHI / earnings)** → *what accumulates (wealth stocks: property, pensions, assets)* → **living standards, deprivation, poverty**

This repo is strong at the **first** node — it measures regional GVA, productivity, jobs and sector structure in detail — and it has a genuinely unusual instrument for the **second** (the inter-industry payment-flow / "leakage" work). The later nodes (residents' income, wealth, deprivation) are mostly *implicit*: gestured at, occasionally named, but not yet wired to the GVA data. That gap is the whole point of this document.

A recurring theme below: the headline measure — **GVA per head** — is the worst-behaved link in the chain. It is neither a clean productivity measure nor an income measure, and the ways it diverges from both are exactly where the wealth-and-poverty story lives.

------------------------------------------------------------------------

# Part 1 — What's already in this repo

## 1. The GVA-per-hour vs GVA-per-capita distinction — the hinge of the whole question

[`gdp_gaps.qmd`](../gdp_gaps.qmd) is the core GVA-gap analysis (South Yorkshire vs the rest of the UK, ITL2/ITL3, 1998–2023). Buried in its introduction is the single most important conceptual move in the repo for this topic:

> "successfully moving people from inactivity into work (into most likely below average pay jobs) or creating new jobs that are below average productivity per hour, will *lower* the 'GVA per hour' number but *increase* GVA per capita."

This is the pivot on which "productivity" and "people's living standards" come apart. A policy that is unambiguously *good* for poverty — getting more residents into work — **worsens** the preferred ONS productivity headline (GVA per hour) while **improving** output per head. The two metrics the document tracks side by side are not two views of the same thing; under anti-poverty interventions they move in opposite directions.

The document also surfaces the **commuting wedge**: it notes that "core cities" show the *largest* GVA-per-capita gaps partly because of "larger numbers of commuters in other core cities, and parallel lower resident populations increasing the 'per capita' GVA averages there." GVA per head is measured on *workplace* output but *residence* population — so it is contaminated by who commutes in, and is therefore **not** a measure of how well-off residents are. This is the cleanest in-repo statement of why GVA per head ≠ income per resident.

Empirically the doc finds South Yorkshire \~17.7% below the non-London UK average on GVA per hour; Barnsley/Doncaster/Rotherham \~20% below on per-capita while Sheffield's gap is 5–10% and narrowing — i.e. the gap is *internally uneven*, which already foreshadows the within-region inequality theme of Part 2.

## 2. Beatty & Fothergill: the "productivity gap" is mostly compositional — and shades straight into poverty

[`beattyfothergill.qmd`](../beattyfothergill.qmd) scrapes Table 1 of Sheffield Hallam CRESR's [*The Productivity of Industries and Places* (2020)](https://www.shu.ac.uk/centre-regional-economic-social-research/publications/the-productivity-of-industries-and-places) and visualises it. The headline finding is decisive for the wealth/poverty question:

-   On **GVA per head**, London is 177% of the UK average and Southern Scotland 59% — a **3×** spread.
-   Once you control for **industry, occupation and hours worked**, the spread collapses: the lowest sub-region is "just below two-thirds of the best."

So most of the apparent regional "productivity gap" is **composition** — what industries a place has, its occupational mix, how many hours people work, demographics, unemployment and inactivity — not pure inefficiency of identical work. The qmd makes the sharp policy observation that industrial mix is "exactly one of the things one might want to change," but the deeper implication for *this* topic is that **low GVA per head in left-behind places is substantially a worklessness-and-mix story, which is to say a poverty story**, not a "they're bad at their jobs" story.

That connects directly to Beatty & Fothergill's wider, decades-long CRESR body of work on **hidden unemployment** — the deindustrialisation → withdrawal-into-inactivity → low-output-per-head channel — which is the most concrete GVA-to-poverty mechanism in UK regional economics (picked up again in Part 2G). The repo currently uses only one table from one of their reports; the link to their poverty work is latent.

## 3. Supply-chain locality / "leakage" — community wealth building, measured

[`ind_to_ind_regionalpayments_IO.md`](ind_to_ind_regionalpayments_IO.md) documents the ONS/HMRC **industry-to-industry payment-flows** dataset (\~1.3M rows, region × region × SIC2, 2019–2025) and the analysis in [`bits_of_code/ind_to_indpayments_regional_IO.R`](../bits_of_code/ind_to_indpayments_regional_IO.R). Section 4 of that doc names the relevant move explicitly — **"Leakage Analysis"**: internal (payer region == payee region) vs external spending, tracked over time.

The substantive results are written up in Section C of [`llm_output/yorkshire_humber_sector_highlights_summary.md`](../llm_output/yorkshire_humber_sector_highlights_summary.md), and they are, in effect, **community-wealth-building metrics derived from real payment data**:

-   **Regional self-sufficiency:** Y&H sources \~21.3% of its B2B inputs from within the region (6th of 11 GB regions); London is in a league of its own at **45.9%**. "Keeping money local" is something London achieves through sheer density, not policy.
-   **Extractive sectors:** **finance/insurance and energy/power** are the universal non-local sectors — money flows out of the region. Y&H finance receives just 4.3% of its payments locally (vs 14% UK mean): Leeds operates as a *national* back-office, so its GVA is real but its **local multiplier is thin**.
-   **The foundational-economy complication (crucial):** retail, food service and accommodation *employ* locally and *serve* local customers, but their *intermediate purchasing is largely non-local*. So — in the summary's own words — this "complicates the 'foundational economy' argument that investing in everyday services necessarily strengthens local economic circuits — it may strengthen local employment without strengthening local supply chains."

That last point is a genuine, data-backed **contribution to** (and partial rebuttal of) the foundational-economy and CWB literatures, not just an application of them. It is the most directly wealth-retention-relevant empirical asset in the repo.

## 4. "Inclusive growth" already named in the client-facing work

[`quarto_docs/LeedsBradford_allresults.qmd`](../quarto_docs/LeedsBradford_allresults.qmd) is where the distributional framing surfaces in delivered analysis:

-   Creative/cultural industries framed as a "**catalyst for inclusive growth**" (citing the [Inclusive Growth Network](https://www.thersa.org/), Jan 2024) — and the linked point that "poor transport connectivity can act as a barrier to people *participating* in culture," i.e. a distribution-of-access argument, not just an output argument.
-   It cites **Stansbury, Turner & Balls (2023), *Tackling the UK's regional economic inequality: binding constraints and avenues for policy intervention*** — the most prominent recent academic statement of the regional-inequality problem, used here to argue transport constrains core-city labour markets.
-   It explicitly "**challenges any simple core/periphery take**," showing Leeds–Bradford convergence — a distributional reading of who benefits from agglomeration, rather than a winner-takes-all one.

## 5. The foundational-economy critique of the Industrial Strategy, and within-region inequality

[`uk_industrial_strategy_since_launch.md`](uk_industrial_strategy_since_launch.md) carries two threads that bear directly on the GVA→poverty link:

-   **§ on the RSA Regions critique:** the IS-8's high-tech focus "marginalises the 'everyday/foundational economy' (SMEs, traditional manufacturing, logistics, care, tourism, local services) that dominates many regions" — i.e. the strategy chases the high-GVA frontier while the sectors most people actually work in (and where in-work poverty concentrates) are treated as residual.
-   **§8 (agglomeration vs distributed growth):** the explicit warning that an agglomeration strategy "can lift the North's *aggregate* GVA (the £40bn headline) while leaving its **towns** … relatively further behind" — within-region inequality as the price of headline growth. It links this to [McCann's "geography of discontent"](https://www.tandfonline.com/doi/full/10.1080/00343404.2019.1619928) and the IPPR critique of city-centric, implicitly trickle-down models. This is the GVA-aggregate-vs-distribution problem stated as geography.

## 6. Anchor institutions, ownership and patient capital

Two further docs supply the *institutional* half of the wealth-retention story:

-   [`universities_industrial_strategy_devolution.md`](universities_industrial_strategy_devolution.md) repeatedly frames universities (and Y-PERN) as "**anchor institutions**" — which is a term of art from community wealth building (Part 2A). It also flags the distributional bite of the HE funding crisis falling hardest on regional/post-92 institutions — hollowing out exactly the dispersed capacity poorer regions rely on.
-   [`mittelstand.md`](mittelstand.md) is, read through this lens, a **continental cousin of community wealth building**: unity of ownership and management, retained earnings over equity extraction, deep regional rootedness, *Sparkassen*/*Landesbanken* local banking, and "social responsibility to the town." Its citation of the [Leibniz small-towns work](https://leibniz-ifl.de/en/home/research/project/hidden-champions-stabilisation-and-development-factors-of-small-towns-in-peripheral-regions) — that rooted firms measurably *stabilise* the towns that host them (human capital, demographics, tax base) — is a wealth-retention claim about firm ownership and locality. The IDE selection that prompted this note ("the Mittelstand story is fundamentally a *distributed* small-town and mid-town cluster story") is precisely a claim about *where* economic value lands and stays.

## What the repo does **not** yet have (and what's sitting unused)

The repo measures **production and jobs** richly but has **no explicit residents'-income, wealth-stock, or deprivation layer joined to the GVA data**. Notably, two relevant datasets are *already on disk but unused for this purpose*:

-   [`local/ashe-tables-3-time-series-v7.csv`](../local/ashe-tables-3-time-series-v7.csv) — **ASHE** gross annual/weekly pay by geography, occupation (SOC), sex, working pattern and **percentile** (incl. median). This is an earnings-distribution layer — the obvious bridge from workplace output to what workers actually take home.
-   [`census2021-ts063-*`](../census2021-ts063-utla.csv) / [`ts064`](../census2021-ts064-utla.csv) (occupation, major and detailed SOC) — the occupational-mix data that Beatty & Fothergill show *drives* the headline gap.

What's missing entirely from the repo: **GDHI** (residents' disposable income), any **wealth-stock** measure, and any **deprivation/IMD** overlay. Part 2 is about those.

------------------------------------------------------------------------

# Part 2 — What else connects: frameworks and the missing data

These are the named frameworks that would turn "GVA gap" findings into "wealth and poverty" findings, plus the datasets that close the chain. They are **not rivals** so much as different entry points into the same translation chain set out at the top.

## A. Community Wealth Building (CLES / the Preston Model)

[Community Wealth Building](https://cles.org.uk/expertise/community-wealth-building/) (CWB), developed by the **Centre for Local Economic Strategies (CLES)** and made famous by the **Preston Model**, is the framework the repo's leakage work most directly serves. Its [**five pillars**](https://cles.org.uk/blog/community-wealth-building-a-history/) map onto repo capabilities almost one-to-one:

| CWB pillar | Repo connection |
|----|----|
| **Progressive procurement / shorter supply chains** | The inter-industry payment-flow leakage analysis (Part 1.3) is *literally a measurement instrument* for this pillar — it quantifies how much spend stays in-region by sector. |
| **Plural ownership of the economy** (co-ops, municipal, locally-rooted SMEs) | The Mittelstand ownership analysis ([`mittelstand.md`](mittelstand.md)) and the Companies House firm-level work (ownership, HQ location, succession risk). |
| **Making financial power work for local places** | The *Sparkassen*/*Landesbanken*/NPIF discussion in the Mittelstand and IS notes. |
| **Fair employment / just labour markets ("good jobs")** | The "many jobs, poor productivity" warehousing/logistics finding (Wakefield, Doncaster) — job growth without value growth is the empirical face of low-pay, low-quality work. |
| **Socially productive use of land & property** | Not yet touched in the repo. |

CWB is now statutory in Scotland (the [Community Wealth Building (Scotland) Bill, 2025](https://www.parliament.scot/chamber-and-committees/research-prepared-for-parliament/research-briefings/2025/5/27/sb-2522)), so this is live policy, not just a think-tank frame. See also the [Democracy Collaborative's reflection on Preston and the future of CWB](https://www.democracycollaborative.org/blogs/the-preston-model-and-future-of-cwb).

**The honest tension** (already half-discovered in the repo): CWB assumes localising supply chains retains wealth — but the payment-flow data shows many "local" everyday sectors buy non-locally, and London retains most wealth through *scale*, not policy. The repo can *test* CWB's central premise rather than merely illustrate it.

## B. The Foundational Economy (the Welsh tradition)

The [**foundational economy**](https://foundationaleconomy.com/research-reports/) — the "taken-for-granted" goods and services essential to everyday life (food, housing, energy, care, retail) — was developed by Froud, Williams and Moran at Manchester's CRESC and has become explicit [Welsh Government policy](https://research.senedd.wales/research-articles/the-foundational-economy/) (it employs \~40% of the Welsh workforce and \~60% of Wales-headquartered firms; see the Senedd's ["Fixing the foundations?"](https://research.senedd.wales/research-articles/fixing-the-foundations-the-foundational-economy-in-wales/)).

This matters here because it is the **counter-narrative to GVA-maximising industrial strategy**: it argues regional wellbeing depends less on chasing high-GVA frontier sectors than on the quality, security and local ownership of the mundane economy most people actually work in and depend on. The repo's payment-flow finding (Part 1.3) is a *direct empirical intervention* in this literature — it shows that investing in everyday services strengthens local *employment* without necessarily strengthening local *supply circuits*. That is a publishable nuance the foundational-economy collective would care about.

## C. Inclusive Growth and "Quality GVA"

The [**RSA Inclusive Growth Commission** (2017)](https://www.thersa.org/reports/final-report-of-the-inclusive-growth-commission/) is the framework that most explicitly attacks the metric problem the repo keeps bumping into. Its core claim: "GDP or at a regional level **GVA** are a poor guide to social and economic welfare and do not show how the opportunities and benefits of growth are distributed."

Its constructive proposals are strikingly close to what the repo's per-hour/per-capita distinction is *already gesturing at*:

-   **"Quality GVA"** — a wider measurement frame capturing not just aggregate growth but changes in inequality and the reach of prosperity into deprived populations.
-   A **multidimensional living-standards measure** — household real disposable income *adjusted for inequality* (the gap between the average and lower deciles), plus health and unemployment.

This is, in effect, a recipe for the analytical layer the repo lacks. The Leeds–Bradford work (Part 1.4) already cites the allied **Inclusive Growth Network**, so the vocabulary is in the door.

## D. The missing dataset: GVA → GDHI (Gross Disposable Household Income)

This is the **single most important next step**, and the cleanest one technically. **GVA** measures production in a place; [**GDHI**](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/bulletins/regionalgrossdisposablehouseholdincomegdhi/1997to2023) measures the income actually available to residents to spend or save. The *difference* between them is the leakage the whole chain is about — commuting, profits flowing to non-resident owners, and tax-and-benefit redistribution.

The numbers make the case:

-   London's **GVA per head** is \~177% of the UK average (Part 1.2), but its **GDHI per head** gap is far smaller (£35,361 in 2023 vs the North East's £19,977 — under 2×, not 3×). Redistribution, commuting and profit outflows compress the production gap into a much narrower *income* gap.
-   The within-London spread is enormous (Westminster/City \~£79.6k vs Barking & Dagenham \~£24.4k) — so even GDHI needs sub-regional resolution to mean anything, exactly the resolution the repo specialises in.
-   The Resolution Foundation's [**Uneven ground** (2024)](https://www.resolutionfoundation.org/app/uploads/2024/08/Uneven-ground.pdf) is the current definitive synthesis of UK geographic economic inequality and a natural anchor reference.

**Feasibility:** ONS publishes [regional GDHI at ITL1/2/3](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomegdhi) — the *same geography* as the existing GVA pipeline. A GVA-vs-GDHI-per-head join, plotted as a ratio over time per ITL2, would be a few hours' work and would convert the repo's "GVA gap" story into a "does production translate into residents' income?" story. **This is the recommended first build.**

## E. Productivity ≠ pay: decoupling, labour share, and who captures the value

Even where GVA per hour *does* rise, it doesn't automatically reach workers — the labour-share question. The evidence is nuanced:

-   The [CEP/POID work, *Have productivity and pay decoupled in the UK?*](https://cep.lse.ac.uk/pubs/download/dp1812.pdf) finds **no net mean decoupling** nationally (mean compensation tracked productivity 1981–2019, helped by the minimum wage and graduate premia) — **but a large divergence between *median* wage growth and productivity**, i.e. the gains skewed up the distribution. The corrected labour share fell \~3.5pp over four decades. See also [ONS, *Trends in the UK labour share* (2024)](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/trendsintheuklabourshare1997to2023/2024-11-25).
-   The GLA's [*The weak link between productivity and wages in London*](https://www.london.gov.uk/sites/default/files/the_weak_link_between_productivity_and_wages_in_london_-_full_report.pdf) shows firm-level productivity and pay can be only loosely connected even in the highest-GVA city — reinforcing that high regional GVA need not mean high regional *pay*.

This is the lens for the repo's own oddities — e.g. the YH summary's note that Doncaster finance GVA "jumped 125% with no jobs growth," and that finance is the least supply-chain-connected sector with "capital leaving regions." Value can rise locally while local incomes don't. The repo's unused **ASHE percentile** data (Part 1, "unused") is the instrument for pairing GVA-per-hour against *median* local pay over time.

## F. Wealth as a stock, not a flow

The brief said *wealth* and poverty. GVA and GDHI are **flows**; wealth is a **stock** (property, pensions, financial and physical assets) — and it behaves very differently:

-   Wealth is far more unequal than income: 2022/23 wealth Gini \~**0.59** vs income Gini \~**0.35** ([ONS Wealth and Assets Survey](https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/debt/methodologies/wealthandassetssurveyqmi)).
-   It is more regionally skewed and *path-dependent*: property is 51% of household wealth in London vs 30% in the North East; the North East's median real wealth **fell \~17% since 2006**. See the Resolution Foundation's [*Before the fall* (2025)](https://www.resolutionfoundation.org/app/uploads/2025/10/Before-the-fall.pdf).

The implication for regional policy is sharp: a region can raise its GVA *flow* for years while its residents' *wealth stock* stagnates or falls (because the assets — land, housing, firm equity — are owned elsewhere). This is precisely why CWB's land/property and plural-ownership pillars (Part 2A) exist: **building local asset stocks** is a different objective from raising local output, and arguably the truer meaning of "regional wealth." The repo has nothing on stocks today; it is the furthest-out but conceptually most important extension.

## G. Hidden unemployment — the labour-market floor under the poverty story

Closing the loop back to Part 1.2: Beatty & Fothergill's [*persistence of hidden unemployment among incapacity claimants* (2023)](https://journals.sagepub.com/doi/10.1177/02690942231184815) and the long-running [*Real Level of Unemployment*](https://www.shu.ac.uk/-/media/home/research/cresr/reports/t/the-real-level-unemployment-2007.pdf) series argue that as many as \~900,000 people are "hidden" unemployed on incapacity-related benefits, **concentrated in older industrial areas, coalfields and coastal towns** (South Wales, Merseyside, the North East, Clydeside — and South Yorkshire's coalfield belt). This is the demographic that simultaneously **depresses GVA per head** (low employment rate) **and constitutes the regional poverty problem** — the two are the same population. It is the most concrete single mechanism connecting the repo's per-capita gaps to actual deprivation, and the repo already touches its authors.

A deprivation overlay (**Index of Multiple Deprivation**) on the existing ITL/LA GVA and jobs data — combined with the census occupation data already in the repo — would let this be shown directly rather than inferred.

------------------------------------------------------------------------

## The through-line

The repo owns the **production** end of the chain and has built a rare, defensible instrument — the inter-industry **leakage** analysis — for the **value-capture/retention** node, which is exactly where community wealth building and the foundational economy make their claims. The chain then runs out: it has no **GDHI** (residents' income), no **earnings-distribution** read (despite ASHE sitting on disk), no **wealth-stock** layer, and no **deprivation** overlay. The frameworks differ mainly in *which leak they target* — CWB and the foundational economy target value-retention and ownership; inclusive growth targets the metric and its distribution; the labour-share literature targets value-capture; the wealth literature targets stock accumulation; hidden-unemployment work targets the employment floor.

The ranked, build-it sequence:

1.  **GVA-vs-GDHI per head over time, by ITL2** — same geography as the existing pipeline; turns "GVA gap" into "does production reach residents?". *(Lowest effort, highest payoff.)*
2.  **GVA-per-hour vs median ASHE pay** — pair the unused earnings percentiles against productivity to test local productivity→pay translation.
3.  **IMD / census-occupation overlay** on the GVA and jobs data — make the Beatty–Fothergill compositional/poverty mechanism visible, not inferred.
4.  **Extend the leakage analysis explicitly as a CWB metric** — frame the payment-flow self-sufficiency scores as progressive-procurement evidence, and publish the foundational-economy nuance as a finding.
5.  **A regional wealth-stock read** (property/pensions via WAS) — hardest, but the only one that addresses "wealth" rather than "income," and the truest test of whether growth is *retained*.

## The reading-in-one-line

This repo measures where economic value is *produced* better than almost any open regional resource, and uniquely measures how much of it *leaks out* — but "wealth and poverty" lives three translations further down the chain (residents' income → wealth stocks → deprivation), and the fastest way to reach it is to join the existing GVA pipeline to ONS GDHI and the ASHE earnings data already sitting unused in the repo.
