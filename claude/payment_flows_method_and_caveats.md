# Payment Flows: Method Notes and Interpretation Caveats

A mostly-Claude-code-written document with occasional human edits.

Companion to [`ind_to_ind_regionalpayments_IO.md`](ind_to_ind_regionalpayments_IO.md) (which documents the dataset and the Location Quotient / IO analyses). This doc is about **how to read the locality results without being misled** — the places where the numbers say one thing and the real economy is doing another.

**Script**: `bits_of_code/ind_to_indpayments_regional_IO.R`
**Data**: `local/data/payments_itl2_sic2_yearly_w_sections.rds` (ONS experimental industry-to-industry payment flows, 2019–2025)

## What the locality method measures

The script builds several metrics for "how locally embedded is a sector":

- **Internal vs external split** — a flow is *internal* if `payer_ITL1name == payee_ITL1name`, else *external*. This underlies the self-sufficiency and `regional_share` metrics (sections 1 & 4 of the script).
- **`payer_locality` / `payee_locality`** — for each sector in each region, the share of its spending (payer) or income (payee) that stays within the region. Plotted as the sourcing-vs-serving scatters, crosshairs and arrows.
- **The "London pull" decomposition** (section 6) — splits every flow three ways: **own region / London / rest of UK**, for regions *outside* London, on both the spending and income sides. Outputs: `07_finance_london_pull.png`, `07_finance_london_scatter.png`.

All of these are **share** measures computed on **business-to-business** payments, with households and extraterritorial bodies filtered out (`!qg('households|extraterr', ...)`).

## The core interpretation problem: geography is the *registered entity*, not the activity

Each payment is attributed to the region of the **paying and receiving legal entity as registered**, not to where the underlying economic activity physically happens. Wherever a payment is settled, aggregated, or routed through an intermediary — or wherever a firm books activity through a headquartered legal entity — the money is attributed to that entity's region. Because so many financial, payment and platform intermediaries are registered in **London**, London's inflows and outflows are **systematically inflated** relative to real economic geography.

This is compounded by two dataset choices:

1. **Households are excluded.** All direct consumer spend — the genuinely local part of consumer-facing sectors — is absent. What remains is inter-business flows only.
2. **The unit is the paying entity, not the transaction's economic origin.** Aggregated consumer card takings appear as a single business-to-business flow *from the card acquirer* (a bank), not from the thousands of local consumers behind them.

### Worked example: accommodation & food service (hospitality)

Hospitality is about as locally-demanded as a sector gets — people eat and sleep near where they are. Yet the income-side plot shows **77% of its income "from London"** and only ~11% from its own region. This looks wrong, and in an economic sense it is — but the arithmetic is correct. The reason is entirely the caveat above.

Food/service income, regions **outside** London, most recent year:

| Origin | £ | Share | Rows | Transactions |
|--------|---|-------|------|--------------|
| **London** | £20.2bn | 77.4% | 681 | **15.0m** |
| Own region | £2.8bn | 10.8% | 1,005 | 1.2m |
| Rest of UK | £3.1bn | 11.9% | 3,080 | 1.1m |

The London money is dominated by a handful of **intermediary SICs**:

| Payer SIC | £ | Transactions | What it is |
|-----------|---|--------------|------------|
| **64** (financial service activities, exc. insurance/pension) | £10.9bn | 7.6m | card acquirers / payment settlement |
| **63** (information service activities) | £3.5bn | 3.8m | online travel agents / booking platforms |
| **82** (office admin & business support) | £3.3bn | 2.5m | reservation / booking agencies |

The **transaction signature** is the giveaway: ~15 million tiny payments averaging **~£1,300** arrive "from London", versus ~1 million larger ones (~£2,300–2,800) from own-region and rest-of-UK. That pattern — enormous counts of small, uniform payments — is aggregated consumer card takings and platform payouts being settled by London-registered processors. It is local consumers' money, routed through London payment rails and *labelled* London.

| Origin | Total £ | Transactions | Avg £/transaction |
|--------|---------|--------------|-------------------|
| London | £20.2bn | 15.0m | £1,347 |
| Own region | £2.8bn | 1.2m | £2,315 |
| Rest of UK | £3.1bn | 1.1m | £2,803 |

### Why this matters for the finance headline

The same effect **partly inflates the finance "London pull"** in the section-6 plots. Regional finance genuinely does keep very little locally — it is the least locally-embedded sector on both sides (own-region spending 22.9%, own-region income 15.6%, both the lowest of any sector). That direction of travel is robust. But some of the large *London* share (spending ~41%, income ~36%) is registration geography — regional branches settling through, or booking activity via, London-registered financial entities — rather than genuinely new economic flow to the capital.

**Rule of thumb:** the **own-region share is the trustworthy half** of any locality finding — when both parties are local, no intermediary geography can distort it. Treat high **London** shares with suspicion until the payer/payee SIC mix and transaction granularity have been checked.

## Time trends can be capture artefacts: the Wales & West Midlands "localisation"

The intermediary problem above distorts *cross-sectional* geography. A second, separate problem distorts *trends over time*. It surfaced in the sector-locality **arrow plot** (section 5 of the IO script, `06_arrow_plot_change.png`), where Wales and the West Midlands show a near-universal drift to the "north-east" — almost every broad SIC section becoming more local as **both** payer and payee across the plotted years. A move that uniform across sectors is suspicious: is it real localisation, or an artefact of how the data is captured?

**Diagnosis script**: `bits_of_code/ind_to_indpayments_localisation_diagnosis.R`

### It is not single large transactions

An earlier investigation had found one cell inflated by a single deal (Wales advanced-manufacturing chemicals, SIC 20→20, £0 → £30.7m in 2024). This is *not* that. Wales's internal pounds **and** its internal transaction **counts** both roughly tripled 2019→2025, while the **average** internal transaction size stayed flat — the signature of *more links*, not *bigger deals*:

| Wales | 2019 | 2025 | Growth |
|-------|------|------|--------|
| Internal £m | 1,108 | 3,014 | ×2.7 |
| Internal transactions | 290k | 810k | ×2.8 |
| Avg internal transaction | £3.8k | £3.7k | flat |

The growth is also spread across sectors, not concentrated: in 2025 the top payee section is 24% and the top three 55% of Wales's internal pounds (across 19 sections), and the 2022→2025 rise shows up in health (+£261m), construction (+£193m), wholesale (+£158m), professional services (+£86m), finance (+£71m) and more. Manufacturing — where the chemicals cell sits — added only +£42m, so that earlier one-off is a small part of a much broader pattern.

### The arrow plot's two axes are really one number

The plot looks like ~40 independent signals (19 sections × 2 axes), but both axes are the **same quantity** — the payer-region's internal spend share — merely sliced by payer sector (x) and by payee sector (y). (In the IO script, `payee_locality` is grouped by `payer_ITL1name` as well as by payee sector.) So "everything moves north-east" is **one aggregate fact shown many ways**: the region's overall internal spend share rose. Within Wales, 95% of payer sections moved the same way (West Midlands 84%) — confirming a broad, single-signal shift rather than sector-specific stories. And the aggregate rose most exactly where it started lowest:

| Region | 2019 | 2025 | Change |
|--------|------|------|--------|
| Wales | 15.0 | 22.0 | +7.0 |
| North West | 22.7 | 27.6 | +4.9 |
| West Midlands | 19.7 | 22.6 | +2.9 |
| North East | 15.7 | 18.5 | +2.8 |
| South West | 18.8 | 20.6 | +1.8 |
| Yorkshire and The Humber | 19.6 | 21.3 | +1.7 |
| East Midlands | 16.7 | 18.2 | +1.5 |
| East of England | 18.8 | 20.2 | +1.4 |
| London | 45.0 | 45.9 | +0.9 |
| South East | 29.8 | 30.4 | +0.6 |
| Scotland | 33.2 | 32.1 | −1.1 |

*(Internal share of payer spend, %.)* Wales moved most, from the lowest base; the already-internal regions — London, Scotland, the South East — barely moved or fell. (The North West is a partial exception, rising strongly from a middling base.)

### Coverage growth vs real localisation — the discriminating test

Here is the key statistical point. A pure *proportional* increase in coverage **cannot** move a share: 100 firms split 50/50 and 200 firms split 50/50 both give a 50% internal share. Capturing more of the same mix leaves the ratio untouched. The internal share can only rise if internal and external flows are captured at **different** rates — and they are:

| Transaction-count growth, 2019→2025 | Internal | External |
|-------------------------------------|----------|----------|
| Wales | ×2.79 | ×1.40 |
| West Midlands | ×1.73 | ×1.45 |

Transaction counts nearly tripling in six years is not the real economy — the number of B2B payments in Wales did not triple. It is the **captured panel** growing, and growing disproportionately in *within-region* links, which is exactly what pushes the internal share up across every sector at once. (This argument is deflator-free: it rests on transaction *counts*, which prices do not inflate — cf. [`deflators_real_vs_nominal.md`](deflators_real_vs_nominal.md).)

Why would the *extra* coverage be internally skewed rather than neutral? The most likely mechanism is **edge observation**: a link is only counted and geolocated once *both* ends are in the panel. A Welsh firm's external counterparty is drawn from the huge rest-of-UK pool (probably already captured even when coverage is thin); its internal counterparty is drawn from the small Welsh pool (often *not* yet captured when the panel is thin). So internal links in small, sparse regions are systematically under-observed at low coverage and "switch on" as the panel fills — a strong effect for Wales and the North East, negligible for large, dense London. That gradient is exactly what the region table shows.

### But it is not proven

This is a **hypothesis**, not a result. The same internally-skewed growth is equally consistent with **genuine local supply-chain deepening**, and one fact cuts against the crude coverage story: the average internal transaction size is flat (Wales ~£3.7k throughout), whereas filling in lots of previously-missed *small* internal links should drag it down.

The deeper problem is that this dataset **cannot** separate the two. Cells are SIC × SIC, not firm × firm, so "new firm relationships appearing in the panel" (coverage) and "existing relationships growing" (real) are indistinguishable inside a section-level cell. Resolving it needs evidence from outside this dataset:

- **firm-level link data**, or
- the data provider's **coverage / onboarding metadata** for 2019–2025 — e.g. how the count of distinct captured Welsh counterparties, and Wales's share of the whole panel, changed over the period.

**Until then, treat every over-time locality reading as unresolved between a region-skewed capture artefact and real localisation.** This applies to the arrow plot *and* the multi-year self-sufficiency bars (section 1 of the IO script), not just to Wales and the West Midlands.

> **Note on the plotted years.** The arrow plot's start year is `unique(i2i.yr$year)[4]` = **2022**, not the 2019 start of the data, so its arrows already *understate* the full-period shift (Wales's internal share rose 15.0→22.0 across 2019–2025, but only 17.6→22.0 across the plotted 2022–2025).

## Other issues to keep in mind

- **Disclosure suppression** — `num_transactions == 0` marks flows suppressed for disclosure control. Small local linkages are the most likely to be suppressed, which could bias *down* measured local embeddedness (small local flows dropped, large intermediary flows retained).
- **Unknown sector (SIC 0)** — ~4–6% of transactions have SIC code 0 / `NA` section, dropped by the `!is.na(section...)` filters. If unknown-sector activity is not geographically random, dropping it is not neutral.
- **Experimental status** — ONS labels this experimental; the entity-to-region attribution methodology may change between vintages.
- **Enterprise vs local unit** — the crux of the whole caveat is whether a payment is attributed to a firm's *registered/HQ address* (enterprise level) or to the *local unit* doing the activity. This determines how bad the London-inflation is and needs confirming against ONS methodology (see next steps).
- **VAT-group and HQ booking effects** — large groups may route payments through a single registered entity, concentrating flows on wherever that entity sits (often London or the South East).

## Next investigation steps

1. **Strip / flag intermediary SICs and recompute locality.** Recompute the section-6 splits excluding (or separately reporting) SIC **64, 66, 63, 82** as payers/payees. Compare the "with" and "without" London shares per sector — the gap *is* the intermediary artefact. Start with hospitality and finance, then run across all sectors.
2. **Build a transaction-granularity diagnostic.** Add a per-flow metric of average payment size (`pounds / num_transactions`) and/or transaction count share. Flag flows dominated by huge counts of tiny payments (card-settlement signature) vs few large payments (genuine B2B trade). This can be a systematic filter rather than eyeballing.
3. **Confirm the ONS geography convention.** Read the ONS methodology for whether region is assigned by registered/enterprise address or local unit. This decides how much of the London effect is artefact vs real. (Source article linked in the companion doc.)
4. **Test the "City of London" concentration.** If the London inflows concentrate in specific London ITL2 areas (e.g. the City), that strengthens the registered-HQ / financial-intermediary interpretation. The data is ITL2-capable (`payments_itl2_...`) even though the current analysis aggregates to ITL1.
5. **Cross-check against independent sources.** Compare implied London shares against ONS regional supply-use / GVA and known sector geographies. Where the payment data says "London" but GVA/employment says "local", that divergence localises the artefact.
6. **Sensitivity on the finance finding.** Re-run the finance London-pull with intermediary sectors removed and with a "both parties non-financial" restriction, to isolate how much of finance's London dependence survives.
7. **Optional "final demand" view.** For consumer-facing sectors, a version that *re-includes* households would answer a different but useful question (where does ultimate demand sit) — worth a clearly-labelled separate cut, not mixed into the B2B locality metrics.
8. **Resolve the coverage-vs-real question for over-time trends.** Obtain firm-level link data, or the provider's coverage/onboarding metadata (distinct captured counterparties per region; each region's share of the panel, 2019–2025), to test the edge-observation hypothesis behind the Wales/West Midlands "localisation" (see section above). Failing that, caveat all over-time locality readings. A partial in-dataset check: track the count of *distinct SIC×SIC internal cells* observed per region per year — if that cell count climbs in step with the internal share, it points to coverage fill-in rather than existing links growing.

---

*Related notes:* [`ind_to_ind_regionalpayments_IO.md`](ind_to_ind_regionalpayments_IO.md) · [`ONSregionalGVA_currentsituation.md`](ONSregionalGVA_currentsituation.md) · [`gva_to_wealth_and_poverty.md`](gva_to_wealth_and_poverty.md)
