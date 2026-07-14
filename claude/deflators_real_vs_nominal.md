# Real vs nominal: deflators, "true" output, and what it means for regional productivity

*A planning and research note. Part 1 states the puzzle the way it already shows up in this repo; Part 2 surveys the Abdirahman–Coyle–Heys–Stewart telecoms-deflator paper and the ONS follow-up that built on it; Part 3 works out why the problem is sharper at the **regional** level and sets out critical, statistically-sane ways forward. A companion to [`econ_error_rates.md`](econ_error_rates.md) (deflators as a source of error) and [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md) (does "real output" reach anyone's living standards?). Mostly Claude-written from the repo's own files plus the cited sources; expects human edits.*

------------------------------------------------------------------------

# Part 1 — The puzzle, as it already appears in this repo

## 1. One sector, two completely different stories

[`bits_of_code/currentprices_v_chainedvolume.R`](../bits_of_code/currentprices_v_chainedvolume.R) plots UK telecoms, computer programming and information services (SIC 61/62/63) two ways from the ONS [regional GVA (balanced) by industry](https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry) file:

-   **Current prices, as a share of the UK economy** ([line 116](../bits_of_code/currentprices_v_chainedvolume.R#L116)): telecoms (SIC 61) stays a roughly **flat slice of the cash economy** across 1998–2022.
-   **Chained volume, real growth index (2022 = 100)** ([line 199](../bits_of_code/currentprices_v_chainedvolume.R#L199)): telecoms sits at **\~0.7 in 1998** and **\~106 by 2023** — a **\~150× real increase** ([lines 207–211](../bits_of_code/currentprices_v_chainedvolume.R#L207-L211)), versus \~5× for the next-fastest (manufacture of computer/electronic products).

The script's own comment on the 150× figure is one word: `Err.` That `Err.` is the whole of this note. The same sector, the same data file, the same years — and depending on which column you read, telecoms either barely moved or grew more than a hundred-fold. The framing question is right at the top of the script ([lines 1–5](../bits_of_code/currentprices_v_chainedvolume.R#L1-L5)): *"how do we make sense of this difference, actually? If we're thinking about where value is coming from for people and their jobs?"*

## 2. Why the two stories diverge: the deflator does all the work

Real (chained-volume) GVA is just nominal GVA divided by a price index:

```         
real output_t  =  nominal output_t  ×  (P_base / P_t)
```

So **the entire gap between the two panels is the deflator.** If telecoms revenue is roughly flat in cash terms but the price of what telecoms sells is judged to have collapsed, then "real volume" must have exploded to compensate — that is arithmetically what a 150× chained-volume index *means*. The number is not an observation about telecoms; it is an observation about the price index ONS applied. Change the deflator and you change the apparent productivity of the sector by orders of magnitude, with no new data about telecoms at all.

This is exactly the point Diane Coyle and colleagues made forcefully — and it is the subject of the paper sitting in [`local/`](../local/07-ES_517-518-519_Abdirahman-et-al_ENWeb.pdf).

------------------------------------------------------------------------

# Part 2 — The Coyle et al. telecoms-deflator paper, surveyed

**Abdirahman, M., Coyle, D., Heys, R. & Stewart, W. (2020), "A Comparison of Deflators for Telecommunications Services Output," *Économie et Statistique / Economics and Statistics* 517-518-519, 103–122.** [DOI](https://doi.org/10.24187/ecostat.2020.517t.2017) · local copy: [`local/07-ES_517-518-519_Abdirahman-et-al_ENWeb.pdf`](../local/07-ES_517-518-519_Abdirahman-et-al_ENWeb.pdf). (Authors span the [IPO](https://www.gov.uk/government/organisations/intellectual-property-office), Cambridge/[ESCoE](https://escoe.ac.uk/), [ONS](https://www.ons.gov.uk/) and the [IET](https://www.theiet.org/) — i.e. economists *and* a telecoms engineer in the same room, which turns out to be the crux.)

## 3. The headline disconnect

> "Data usage in the UK expanded by nearly **2,300%** between 2010 and 2017, yet real Gross Value Added for the telecommunications services industry **fell by 8%** between 2010 and 2016, while the industry experienced one of the slowest rates of recorded productivity growth." (Abstract)

The current ONS deflator for telecoms services (CPA 61) showed prices **rising \~3%** over 2010–2017 (their Figure I) — so nominal revenue, roughly flat, deflated into *falling* real output. To a telecoms engineer watching 3G→4G, fibre rollout and exponential traffic growth, "negative productivity growth" is absurd. The [Bean Review (2016)](https://www.gov.uk/government/publications/independent-review-of-uk-economic-statistics-final-report) had already flagged that official deflators may understate true price declines in fast-moving tech, so real growth is understated.

## 4. Engineers vs economists: one product, or a basket?

The paper's organising insight is that two disciplines see telecoms differently:

-   **Economists** see a *basket* of distinct services (voice, text, fixed broadband, mobile data) with different prices and revenue weights, and build a price index over that basket.
-   **Engineers** see a *single* product — **bits of data transported** — whose cost-per-bit has fallen relentlessly. Data rate on a single installed fibre rose \~10¹⁰× (1960–2015); widely-installed systems rose \~10⁶× (1980–2015), roughly 5,000–6,000% per decade. (The paper's sugar analogy: the gains since 1980, if they were sugar, would bury the UK under four extra metres a year.)

This is the [Nordhaus (1994, 2007)](https://doi.org/10.1017/S0022050707000058) "price of light / price of computation" move: measure the price of the *underlying service characteristic* (lumens, computations, bits), not the price of the marketed good. Do that, and measured prices collapse.

## 5. The two alternative deflators — and why they bracket the truth

The paper constructs two replacements for the current deflator and treats them as **upper and lower bounds** on some ideal constant-utility index:

| Method | Weighting | What it captures | Price change 2010–2017 |
|----|----|----|----|
| Current ONS deflator (⅔ CPI + ⅓ SPPI) | revenue | status quo | **+3%** |
| **Option A** — improved SPPI (adds broadband + mobile data, business-to-all, chain-linked) | **revenue** | cautious, internationally comparable | **−37%** |
| **Option B** — data-usage / aggregate unit value (total revenue ÷ total petabytes) | **volume** | engineering view, cost-per-bit | **−96%** |

(Their Figures III and VI.) Swapping a **+3%** deflator for a **−37% to −96%** one turns telecoms from a productivity laggard into one of the fastest-growing real-output sectors in the economy. That is the "orders of magnitude" swing — and it is a *methodological choice*, not a measurement.

## 6. Why the A–B gap is so wide: revenue weights vs volume weights

The whole 37%-vs-96% spread comes from **what you weight by**:

-   By **volume**, data is \~**99.8%** of everything telecoms moves by 2017.
-   By **revenue**, broadband + mobile data are only \~**40%** of the total; voice and text still earn most of the money (Appendix Table A1-2: total volume 2,553 → 61,254 petabytes 2010–17, while revenue stayed \~£40bn / actually fell \~6%).

A **revenue-weighted** index (Option A) barely notices the data explosion, because cheap-per-bit data earns little. A **volume-weighted** index (Option B) is dominated by it. The paper's drug-pricing analogy (Griliches & Cockburn): when generics enter, the *price index* hardly moves because incumbents keep the *revenue* even as generics take the *volume*. Telecoms has the same structure.

## 7. The deep problem (the part that doesn't have a clean answer)

This is where the paper is most useful and most honest, and where this whole agenda stops being a technical fix:

-   **Index-number theory assumes a fixed basket.** [Diewert (1998)](https://doi.org/10.1257/jep.12.1.47): "like can be compared to like" — but in telecoms the goods themselves dissolve (SMS → WhatsApp; voice → VoIP). You can't price a 2017 basket against a 1998 one when the 2017 basket didn't exist.
-   **New goods / consumer surplus.** [Feldstein (2017)](https://doi.org/10.1257/jep.31.2.145) argues the failure to value *new goods* is an even bigger bias than quality change. Price indices, even hedonic ones, miss the surplus from a good that simply wasn't there before.
-   **Hedonics is the "gold standard" but barely used.** Quality-adjusting via regression on characteristics (speed, RAM, bandwidth…) is principled but data-hungry; only \~0.39% of the UK CPI basket is hedonically adjusted, and telecoms quality (latency, coverage, throttling, advertised-vs-actual speed) resists it.
-   **The bikini fork (the philosophical crux).** The paper closes with Milton Gilbert's jibe that if quality adjustment fully reflected utility, a bikini would count as equivalent output to a Victorian bathing costume, and "should this trend reach its limit of no costumes at all, we would have to say that swimsuit production had not fallen, even though the industry was out of business." Griliches' reply: goods make no sense independent of a utility framework. **The unresolved question is whether we are measuring production/efficiency or consumer welfare — and the right deflator differs depending on which.**

> The paper's own bottom line: a sensible improvement to the telecoms deflator shows real output "significantly understated in recent years," with "consequential implications for the sector distribution of output, but potentially also for real GDP," and "similar considerations may apply to other service sectors experiencing rapid digital innovations."

## 8. What ONS did next — and the sanity check that deflates the excitement

ONS, [*Improvements to the measurement of UK GDP: an update on progress*](https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/articles/improvementstothemeasurementofukgdp/anupdateonprogress) (6 July 2020), built on exactly this ESCoE work. Two moves:

1.  **An improved telecoms deflator** for Blue Book 2021 — correcting "the under-representation of internet services" and the handling of access charges; it shows a stronger price decline.
2.  **Double deflation** of industry GVA (deflate inputs and outputs separately, not just outputs) — the method the US/Australia/Netherlands use.

The crucial distinction — and the easy thing to get wrong — is **what gets washed out, and where.** There are two levels, and only one of them washes out:

> "**any increase to the volume measure of GDP will be significantly less than that driven by the increased output**" — because telecoms is largely an **intermediate input** to other industries. Under double deflation, a steeper telecoms *output* deflator raises *consuming* industries' real telecoms **inputs**, which *lowers their* real GVA — offsetting the telecoms gain **in the GDP total**. And "at the headline level, GDP will be largely unaffected by the introduction of double deflation itself," since the expenditure measure already embeds it.

But note what is **not** washed out: the **telecoms sector's own real GVA** — which is exactly what [`currentprices_v_chainedvolume.R`](../bits_of_code/currentprices_v_chainedvolume.R) plots, and the \~150× stands. Value added is `real gross output − real intermediate inputs`; steepening the telecoms *output* deflator inflates the first term while leaving the second (electricity, equipment, business services, deflated by *their* prices, not the telecoms price) roughly untouched. So the sector's own real GVA balloons. **The offset lands on *other* industries and on the *GDP aggregate*, never on telecoms itself** — intermediate consumption is already netted out of the number on the chart.

So fixing the telecoms deflator dramatically changes the **sector distribution** of real output (which industries and regions look productive) while moving **headline GDP** very little. The action is entirely in *allocation* — precisely the regional question. And it *sharpens* the puzzle rather than resolving it: we are left with a sector whose **nominal value added — ≈ the income (wages, profits, taxes) it generates — is roughly flat**, yet whose **real value added grew \~150×**. "Real value added" has come unmoored from "income anyone received" (see §11). *(Caveat: the exact multiple is a fragile ratio — `106.1/0.7`, a tiny 1998 denominator on a 2022=100 index — so handle it gently even though it now checks out empirically at **151.6×** (§12A). Its size also hinges on the deflator vintage, which §12A confirms is the corrected, post-2021 one.)*

ONS flagged the same treatment is coming for **computer hardware/software**, **financial & insurance services** and **tourism** (Blue Books 2021–22).

## 9. Double deflation, intuitively: why cheaper inputs don't raise a buyer's real GVA

A natural objection to §8: *if telecoms gets that much cheaper, surely the industries that buy it gain too — they make the same output with cheaper bits, so their margins (and GVA) should rise?* That intuition is right about **nominal** terms and wrong about **real** terms, and seeing why is the key to the whole regional argument. The resolving word: **real GVA is a *volume* measure, not a cash margin.** Double deflation never asks "did the buyer save cash?" — it asks "how do we split the buyer's spend into a price part and a volume part?" Under that split, "cheaper bits" doesn't become "extra margin"; it becomes **"the same cash now counts as a far larger *volume* of input."**

Take a firm buying connectivity:

-   **Case 1 — same cash, more bits (the realistic one).** It spends £1m on connectivity in both 2010 and 2017 but gets \~100× the data. Nominal intermediate consumption is unchanged. Deflate that £1m by a telecoms deflator that has fallen to \~1% of its 2010 level → the firm's *real* telecoms input is up \~100×. Real GVA = real output − real input; with output flat and input *volume* up 100×, **the firm's real GVA falls.** It is recorded as consuming a vast volume of cheap bits to make the same output. *That* is the wash-out: telecoms' real output ↑, the buyer's real GVA ↓, and across the whole economy the two offset.
-   **Case 2 — same bits, less cash.** It buys the same data but pays £0.5m. Nominal II ↓ → nominal GVA ↑ (the margin intuition — and it's right, *nominally*). But real II = £0.5m ÷ (half the price) = the *same real volume* → **real GVA flat.** The cash saving is booked as a *price* effect, not a volume gain.

Either way, cheaper bits **do not raise the buyer's *real* GVA** — margin ≠ value-added volume. (Per firm the Case-1 drag is small, since telecoms is a tiny share of most firms' costs; but summed across every buyer it is exactly what offsets telecoms' gain. Telecoms' 100× is weighted by \~100% of *its* output; each buyer's 100× is weighted by telecoms' sliver of *their* costs.)

**Where the intuition is right — and it's the important bit.** If a firm uses the cheaper bits to make **more or better real output**, then *its* real output rises and *its* real GVA rises: the productivity gain is real, but it lands **in the consuming sector, not telecoms.** That is the genuine spillover channel — cheap digital infrastructure is *supposed* to make banks, retailers and logistics more productive. The national accounts only let the telecoms revolution lift *GDP* through two doors: (1) more real **final** demand — households and exports getting far more for the same broadband bill (consumer surplus, much of it unmeasured); or (2) higher real **output in the sectors that buy telecoms** (the spillover). The intermediate-input portion cancels *by construction*; doors (1) and (2) are precisely where measurement is weakest.

So "it balances elsewhere" never meant "nobody benefited." It means the benefit either **left as consumer surplus we don't capture**, or it **should be sitting in consuming sectors' real output and stubbornly isn't** — which is the UK [productivity puzzle](../quarto_docs/productivity_bits.qmd) in one move. Either way the gain is real; the framework just files it where it is hardest to see. This is also why Part 3 is really about *allocation*: the deflator decides which sector — and so which place — gets credited with output that, in welfare terms, mostly flowed to bill-payers everywhere.

------------------------------------------------------------------------

# Part 3 — Why this is sharper at the regional level, and where to go

## 10. The regional kicker: one national deflator, applied to every region

This is the bit that matters for *this* repo and isn't in the Coyle paper. As [`econ_error_rates.md`](econ_error_rates.md) already notes ([lines 119–123](econ_error_rates.md#L119-L123)):

> "Regional deflators often don't exist; national deflators applied to regions. Sector-specific deflators have their own uncertainty. Compositional effects when aggregating sectors."

Three consequences follow directly:

1.  **Inherited error.** Whatever the national telecoms deflator gets wrong, *every* region's telecoms real-GVA inherits identically. A region's "real telecoms growth" is its nominal growth pushed through a single national price series — so cross-region comparisons of *real* sector output are comparisons of *nominal* output wearing a shared, contested coat of paint.
2.  **Compositional swing.** Regions heavy in telecoms/ICT (the script notes the ICT difference "shows up in Leeds"; nationally, London, Reading/Thames Valley, Bristol) have their *aggregate* real-growth and productivity numbers swung hardest by the deflator choice. A −96% vs +3% deflator doesn't just move telecoms — it reweights the whole regional league table through sector mix. This is the same compositional logic as the [Beatty & Fothergill productivity-gap finding](beattyfothergill.qmd) in [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md): much of the regional "productivity gap" is *what industries a place has*, not efficiency — and the deflator decides how much each industry counts. (The **opposite** measurement bias — public services measured as inputs, so productivity can't grow by construction — is the subject of the companion note [`public_services_inputs_as_output.md`](public_services_inputs_as_output.md).)
3.  **Double deflation flows through supply chains.** Because the telecoms correction mostly redistributes output to *consuming* industries (§8–9), the regional incidence depends on *who buys telecoms services where* — which is exactly what the inter-industry payment-flow / leakage work measures ([`ind_to_ind_regionalpayments_IO.md`](ind_to_ind_regionalpayments_IO.md)). The deflator story and the leakage story are the same story.

## 11. …and does any of this "real growth" reach people?

A 150× real-output increase that shows up in **nobody's wage** and barely in the **sector's revenue share** is the purest possible case of the [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md) thesis: **GVA, "real output," income and welfare are different things that diverge exactly where it matters.** Telecoms' "real productivity" is overwhelmingly **consumer surplus** (we get vastly more data for our money) — a genuine welfare gain, but one that accrues to *users* as cheaper service, not to the *region* as wages, profits or tax base. For regional policy aimed at jobs and living standards, "real telecoms output grew 150×" is almost the wrong sentence: the relevant facts are flat revenue, flat-to-falling employment, and a big consumer-surplus transfer to bill-payers everywhere.

This connects to the "deflator-masked decline" failure mode already logged in [`econ_error_rates.md`](econ_error_rates.md) ([line 302](econ_error_rates.md#L302)): the deflator's own (unquantified, regionally-uniform) error is a second layer of fog over every real-terms regional claim.

## 12. Critical ways forward (the planning part)

**A. The vintage question — checked, and resolved: the corrected deflator is already in.** The repo's 150× comes from the *current* (April 2025) ONS regional release, and it **does** incorporate the post-Blue-Book-2021 improved telecoms deflator. Verified (2026-06-19) by recomputing the *implied* telecoms deflator (current price ÷ chained volume) for SIC 61, UK, straight from the release file:

| SIC 61 (Telecommunications), UK | 1998 | 2010 | 2017 | 2023  |
|---------------------------------|------|------|------|-------|
| Chained-volume index (2022=100) | 0.7  | 9.3  | 41.3 | 106.1 |
| Implied GVA deflator (2023=100) | 7327 | 829  | 239  | 100   |

The decisive test is Coyle's own **2010–2017** window: their paper shows the **old** deflator *rose \~+3%* and real telecoms GVA *fell \~8%*. In the current file the implied deflator **falls \~71%** over those years and real GVA **rises \~344%** (cv 9.3→41.3). Same sector, same window, opposite sign — that flip *is* the BB2021 correction. Across 1998–2023: real output ×**151.6**, nominal ×**2.07**, so the deflator does \~**98.6%** of the work, and the real growth is spread across *both* halves of the period (×13 to 2010, ×11 after) — not concentrated pre-2008, as an earlier draft of this note guessed.

So we are firmly in the *post-Coyle* world: the 150× is the *corrected* number, which makes §11's welfare point sharper, not weaker. *(Caveat: this is an **implied value-added** deflator — a double-deflation composite — not the **output** deflator Coyle reports, so don't map the −71% onto his Option A (−37%) or B (−96%) precisely; the robust claim is the regime, not the figure. The workbook also carries a "Correction" sheet, consistent with the ongoing deflation/chain-linking fixes noted in [`ONSregionalGVA_currentsituation.md`](ONSregionalGVA_currentsituation.md). Verification script: [`bits_of_code/check_telecoms_deflator.R`](../bits_of_code/check_telecoms_deflator.R).)*

**B. Decide which question we're asking before computing anything.** The bikini fork (§7) is not academic. For this repo's purposes: - *Production/efficiency* ("is the sector growing, is it productive?") → chained volume is the right tool, but must be reported **with its deflator dependence flagged**, and treated as **order-of-magnitude** in fast-tech sectors. - *Living standards / poverty* (the repo's real interest) → lean on **nominal shares, jobs, and pay** (ASHE), not chained-volume real output. Reserve "real growth" for narrow sectoral questions.

**C. Concrete empirical moves, smallest first:** - Extend [`currentprices_v_chainedvolume.R`](../bits_of_code/currentprices_v_chainedvolume.R) to **ICT-heavy vs ICT-light regions** — show how much a region's apparent real-growth ranking depends on its SIC 61–63 share. - A **deflator sensitivity analysis**: re-rank regions' real GVA growth under the current (+3%), Option A (−37%) and Option B (−96%) telecoms deflators. How much does the league table move? This is the single most decision-relevant number we could produce. - Cross-walk to the [productivity per-hour pipeline](../prepcode/productivity_perhourworkedONS.R): which regions' GVA-per-hour figures are most exposed to the telecoms deflator choice? - Tie the deflator incidence to the [supply-chain-locality flows](../quarto_docs/industry_to_industry_supply_chain_locality_2025.qmd) — where do cheaper telecoms inputs land?

**D. A house rule for "statistically sane" regional productivity.** Three principles, consistent with [`econ_error_rates.md`](econ_error_rates.md)'s epistemic-humility line: 1. **Always name the deflator.** Any real-terms regional claim should say which price series did the work and that it is national, not regional. 2. **Separate "volume of stuff" from "value to people."** Report data/output volume, revenue, employment and pay as distinct series; don't let chained volume stand in for welfare. 3. **Treat fast-tech real-growth as a range, not a point.** Where a sector's deflator is contested by an order of magnitude, carry the range through to the regional total rather than reporting a single deflated figure.

------------------------------------------------------------------------

## Sources

**Core** - Abdirahman, Coyle, Heys & Stewart (2020), *A Comparison of Deflators for Telecommunications Services Output* — [DOI](https://doi.org/10.24187/ecostat.2020.517t.2017) · [`local/07-ES_517-518-519_Abdirahman-et-al_ENWeb.pdf`](../local/07-ES_517-518-519_Abdirahman-et-al_ENWeb.pdf) - ONS (2020), [*Improvements to the measurement of UK GDP: an update on progress*](https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/articles/improvementstothemeasurementofukgdp/anupdateonprogress) - [Bean, C. (2016), *Independent Review of UK Economic Statistics*](https://www.gov.uk/government/publications/independent-review-of-uk-economic-statistics-final-report)

**Theory referenced by the paper** - [Diewert (1998)](https://doi.org/10.1257/jep.12.1.47) · [Feldstein (2017)](https://doi.org/10.1257/jep.31.2.145) · [Nordhaus (2007)](https://doi.org/10.1017/S0022050707000058) · [Hausman (2003)](https://doi.org/10.1257/089533003321164930) · [Coyle (2017), *Do-it-yourself digital*](https://www.escoe.ac.uk/wp-content/uploads/2017/02/ESCoE-DP-2017-01.pdf)

**In this repo** - [`bits_of_code/currentprices_v_chainedvolume.R`](../bits_of_code/currentprices_v_chainedvolume.R) — the telecoms current-price vs chained-volume comparison - [`econ_error_rates.md`](econ_error_rates.md) — deflators as a source of error; "deflator-masked decline" - [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md) — output ≠ income ≠ welfare - [`public_services_inputs_as_output.md`](public_services_inputs_as_output.md) — the non-market mirror: public-service output measured (regionally) as jobs × wage, so productivity is flat by construction - [`ind_to_ind_regionalpayments_IO.md`](ind_to_ind_regionalpayments_IO.md) — supply-chain leakage (where intermediate-input gains land) - [`ONSregionalGVA_currentsituation.md`](ONSregionalGVA_currentsituation.md) — current state of the regional GVA release (notes deflation/chain-linking corrections) - [`prepcode/productivity_perhourworkedONS.R`](../prepcode/productivity_perhourworkedONS.R) · [`gdp_gaps.qmd`](../gdp_gaps.qmd)

------------------------------------------------------------------------

***In one line:*** *Telecoms' "real output" can be \~flat or \~150× from the same data depending only on the deflator; Coyle et al. show the honest range is −37% to −96% prices (vs ONS's old +3%); the fix mostly re-slices output between sectors rather than growing GDP — so for UK regions, where one national deflator is bolted onto every area, the deflator choice silently rewrites the productivity league table while the welfare gain quietly leaves the region as consumer surplus.*
