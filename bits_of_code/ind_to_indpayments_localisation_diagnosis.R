# ============================================================================
# DIAGNOSIS: why do Wales & the West Midlands show near-universal "localisation"
# (almost every broad SIC section moving NE - more local as both payer & payee)
# in the SECTOR-LOCALITY ARROW PLOT at ind_to_indpayments_regional_IO.R line ~1304?
#
# Prompted by the observation that the move is across ~all sectors at once, so it
# can't plausibly be the single-large-transaction effect we found earlier for one
# cell (Wales Adv Manuf chemicals, SIC20->SIC20, GBP0 -> GBP30.7m in 2024).
#
# ----------------------------------------------------------------------------
# SHORT ANSWER
#
# 1. It is NOT single huge transactions (the intuition is correct). The internal-
#    pound growth is spread across ~all 19 sections (2025 top-3 payee sections =
#    only 55% of internal pounds), internal transaction COUNTS roughly TRIPLED
#    (Wales 290k -> 810k, 2019-2025) while the AVERAGE internal transaction size
#    stayed FLAT (~GBP3.7k throughout). That is the signature of MORE LINKS, not
#    bigger deals - the opposite of the chemicals cell.
#
# 2. The plot looks like ~40 independent signals (19 sections x 2 axes) but BOTH
#    axes are the SAME quantity: payer-region internal-spend share, just sliced by
#    payer sector (x) and by payee sector (y). (See lines 1242-1257 of the IO
#    script: payee_locality is grouped by payer_ITL1name too.) So "everything goes
#    NE" is really ONE fact shown many ways: the region's overall internal spend
#    share rose - Wales 15% -> 22%, West Midlands 19.7% -> 22.6% (2019-2025).
#
# 3. WHAT we can and cannot conclude about WHY the aggregate internal share rose.
#    Robust facts:
#      - captured transaction COUNTS grew far faster than the real economy (Wales
#        internal x2.8 in six years; the number of B2B payments did not triple) -
#        so the CAPTURE PROCESS / panel changed over 2019-2025, not just prices.
#      - internal counts grew far faster than external (x2.8 vs x1.4); that
#        differential is exactly what raises the internal share.
#
#    IMPORTANT - this differential is NOT explained by uniform coverage growth.
#    100 firms at 50/50 and 200 firms at 50/50 both give a 50% internal share;
#    simply capturing MORE of the same mix does not move the ratio. For coverage
#    to raise the internal share the EXTRA data must be internally skewed, and
#    there is essentially one mechanism that does that without real change:
#      EDGE OBSERVATION ("both ends must be in the panel"). A link is only counted
#      and classified once BOTH payer and payee are captured + geocoded. A Welsh
#      firm's EXTERNAL counterparty is drawn from the huge rest-of-UK pool (likely
#      already captured even at low coverage); its INTERNAL counterparty is drawn
#      from the SMALL Welsh pool (often not yet captured when the panel is thin).
#      So internal links for SMALL regions are systematically under-observed at low
#      coverage and "switch on" as coverage fills in - strongest for small/sparse
#      regions (Wales, NE), negligible for large dense ones (London). This fits the
#      gradient: lowest-internal-share regions rose most; London/Scotland barely
#      moved. (It is a quadratic-ish "both nodes present" effect, not per-firm.)
#
#    BUT this is a HYPOTHESIS, not proven. The same internally-skewed growth is
#    equally consistent with GENUINE local supply-chain deepening, and one thing
#    cuts against the crude coverage story: average internal transaction size is
#    FLAT (~GBP3.7k), whereas filling in lots of previously-missed small internal
#    links would drag it down.
#
#    => We CANNOT attribute the Wales/W-Mids localisation to coverage vs real from
#       THIS dataset: cells are SIC x SIC, not firms, so "new firm links appearing"
#       (coverage) cannot be separated from "existing relationships growing" (real).
#       Distinguishing them needs firm-level link data, or the provider's coverage/
#       onboarding metadata for 2019-2025 (e.g. how the count of distinct captured
#       Welsh counterparties, and Wales's share of the whole panel, changed).
#       Treat the across-the-board localisation as UNRESOLVED between a region-
#       skewed edge-observation artefact and real localisation - not as established
#       economic fact - until checked against that external evidence.
#
# NB the plot's start year is unique(i2i.yr$year)[4] = 2022 (not the 2019 start of
#    the data), so its arrows already understate the full-period move.
# ============================================================================

library(tidyverse)
source('functions/misc_functions.R')   # qg()
options(pillar.sigfig = 4)

i2i = readRDS('local/data/payments_itl2_sic2_yearly_w_sections.rds') %>%
  mutate(year = as.integer(format(year, "%Y")),
         internal = payer_ITL1name == payee_ITL1name)

# Same base filters as the plot (drop SIC-0/NA sections, households/extraterr, NI)
base = i2i %>%
  filter(!is.na(sectionname_payer), !is.na(sectionname_payee),
         !qg('households|extraterr', sectionname_payer),
         !qg('households|extraterr', sectionname_payee),
         payer_ITL1name != "Northern Ireland")


# 0. WHICH YEARS DOES THE PLOT ACTUALLY USE? -------------------------------
# IO script line 1186 sets locality_years = c(unique(i2i.yr$year)[4], max(year)).
# unique() returns years in order of first appearance = 2019,2020,2021,2022,...
# so the plotted "start" is 2022, the "end" is 2025. The arrows therefore measure
# 2022->2025 only, understating the full 2019->2025 shift.
print(unique(readRDS('local/data/payments_itl2_sic2_yearly_w_sections.rds')$year))


# 1. STRUCTURAL POINT: BOTH AXES ARE ONE PAYER-REGION INTERNAL SHARE --------
# x = internal share of a payer-sector's spend; y = internal share of spend going
# to a payee-sector; both restricted to payer_ITL1name == region. So both axes are
# slices of the region's overall internal spend share. If that aggregate rises,
# essentially every section drifts NE on both axes by construction.
#
# FINDING - region-wide internal spend share rose most exactly where it started
# lowest (Wales 15.0 -> 22.0, North East 15.7 -> 18.5, West Midlands 19.7 -> 22.6),
# and barely moved where already high (London 45.0 -> 45.9, Scotland 33.2 -> 32.1).
reg_int_share = base %>%
  group_by(region = payer_ITL1name, year) %>%
  summarise(int_share = round(100 * sum(pounds * internal) / sum(pounds), 1), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = int_share)
print(reg_int_share, width = 200, n = 20)

# How uniform is the NE move? Share of a region's payer sections whose internal
# share rose 2022->2025. FINDING: Wales 95%, West Midlands 84% (vs Scotland 16%,
# Yorkshire 42%) - confirms the near-universal move is specific to these regions.
pct_sections_up = base %>%
  filter(year %in% c(2022, 2025)) %>%
  group_by(payer_ITL1name, sectionname_payer, year) %>%
  summarise(s = sum(pounds[internal]) / sum(pounds), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = s, names_prefix = 'y') %>%
  group_by(payer_ITL1name) %>%
  summarise(n_sec = n(), pct_up = round(100 * mean(y2025 > y2022, na.rm = TRUE)), .groups = 'drop') %>%
  arrange(desc(pct_up))
print(pct_sections_up, n = 20)


# 2. IS IT SINGLE BIG TRANSACTIONS? NO -------------------------------------
# Internal vs external pounds, transaction counts, and average transaction size.
# FINDING (Wales): internal pounds x2.7 (1108 -> 3014 GBPm) AND internal txns x2.8
# (290k -> 810k) while avg internal txn size is FLAT (~GBP3.7-3.8k) across all
# years. Money rose because the NUMBER of internal transactions rose, not their
# size => broad-based, many links - not a handful of big deals. West Midlands the
# same (internal txns x1.7, avg size flat ~GBP4k).
intext = base %>%
  filter(payer_ITL1name %in% c("Wales", "West Midlands")) %>%
  group_by(payer_ITL1name, year) %>%
  summarise(int_m = round(sum(pounds[internal]) / 1e6),
            ext_m = round(sum(pounds[!internal]) / 1e6),
            int_share = round(100 * sum(pounds[internal]) / sum(pounds), 1),
            int_txn = sum(num_transactions[internal]),
            ext_txn = sum(num_transactions[!internal]),
            avg_int_k = round(sum(pounds[internal]) / sum(num_transactions[internal]) / 1e3, 1),
            avg_ext_k = round(sum(pounds[!internal]) / sum(num_transactions[!internal]) / 1e3, 1),
            .groups = 'drop')
print(intext, width = 200, n = 30)

# Spread across sectors? Wales internal pounds by payee section, 2022 vs 2025.
# FINDING: growth is broad - Health (+261m), Construction (+193m), Wholesale
# (+158m), Professional (+86m), Finance (+71m), Real estate (+54m)... many sectors
# rising, not one. (Manufacturing only +42m total - so the earlier chemicals
# +30.7m cell is a small part of a much bigger, broad pattern.)
wales_by_payee = base %>%
  filter(payer_ITL1name == "Wales", internal, year %in% c(2022, 2025)) %>%
  group_by(sectionname_payee, year) %>%
  summarise(m = sum(pounds) / 1e6, .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = m, names_prefix = 'y', values_fill = 0) %>%
  mutate(jump = round(y2025 - y2022, 1), y2022 = round(y2022, 1), y2025 = round(y2025, 1)) %>%
  arrange(desc(y2025))
print(wales_by_payee, width = 200, n = 25)

# Concentration of Wales internal pounds across payee sections (2025).
# FINDING: top-1 section = 24%, top-3 = 55% across 19 sections => NOT concentrated
# in one cell/relationship; genuinely spread.
wales_conc = base %>%
  filter(payer_ITL1name == "Wales", internal, year == 2025) %>%
  group_by(sectionname_payee) %>%
  summarise(m = sum(pounds), .groups = 'drop') %>%
  arrange(desc(m)) %>% mutate(share = m / sum(m)) %>%
  summarise(n_sections = n(), top1_pct = round(100 * share[1], 1), top3_pct = round(100 * sum(share[1:3]), 1))
print(wales_conc)


# 3. COVERAGE / SAMPLE GROWTH vs REAL LOCALISATION -------------------------
# The discriminating test. A pure proportional coverage increase would NOT move
# the internal share (numerator and denominator scale together). The ratio only
# moves if internal and external are captured at DIFFERENT rates. They are:
#
# FINDING - internal transaction counts grow much faster than external:
#   Wales:         internal txns x2.79 vs external x1.40   (2019->2025)
#   West Midlands: internal txns x1.73 vs external x1.45
# Transaction counts tripling in six years is not real economic activity (the
# number of B2B payments did not triple); it is the captured panel growing, and
# growing disproportionately in within-region links - which is precisely what
# pushes internal share up across every sector at once.
growth = base %>%
  filter(payer_ITL1name %in% c("Wales", "West Midlands"), year %in% c(2019, 2025)) %>%
  group_by(payer_ITL1name, year) %>%
  summarise(int_txn = sum(num_transactions[internal]),
            ext_txn = sum(num_transactions[!internal]),
            int_m = sum(pounds[internal]) / 1e6,
            ext_m = sum(pounds[!internal]) / 1e6, .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = c(int_txn, ext_txn, int_m, ext_m)) %>%
  mutate(int_txn_growth = round(int_txn_2025 / int_txn_2019, 2),
         ext_txn_growth = round(ext_txn_2025 / ext_txn_2019, 2),
         int_pounds_growth = round(int_m_2025 / int_m_2019, 2),
         ext_pounds_growth = round(ext_m_2025 / ext_m_2019, 2)) %>%
  select(payer_ITL1name, int_txn_growth, ext_txn_growth, int_pounds_growth, ext_pounds_growth)
print(growth, width = 200)

# CONCLUSION (see header): the Wales/West-Midlands across-the-board localisation is
# real in the data and is NOT single transactions. Two things are certain: the two
# plot axes are one payer-region quantity (so it is ~1 aggregate signal, not 19),
# and the captured panel grew fast and internally-skewed. What is NOT certain is
# WHY the internal share rose: uniform coverage growth cannot do it, so it is
# either (a) a region-skewed edge-observation artefact (internal links under-seen
# in small regions when the panel is thin, recovered as it fills) or (b) genuine
# local supply-chain deepening. This dataset (SIC x SIC, not firms) cannot separate
# them; needs firm-level links or the provider's coverage metadata for 2019-2025.
