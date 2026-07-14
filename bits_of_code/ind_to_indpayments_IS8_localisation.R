# ============================================================================
# IS-8 INDUSTRIAL STRATEGY SECTORS APPLIED TO REGIONAL INDUSTRY-TO-INDUSTRY FLOWS
# ============================================================================
#
# Combines two existing analyses:
#   (i)  the IS-8 industrial-strategy SIC definitions / jobs LQ work
#        (prepcode/industrial_strategy_datalinkage.R +
#         prepcode/yorkshire_n_humber_sectors_dataprep.R "IS-8 LQs attempt 2", line ~2320)
#   (ii) the regional industry-to-industry PAYMENT flows
#        (bits_of_code/ind_to_indpayments_regional_IO.R)
#
# Two deliverables:
#   1. ARROW PLOT  - how locally embedded each IS-8 sector's supply chain is, and
#      how that is changing over time. Direct analogue of the section-level
#      "SECTOR LOCALITY CHANGE OVER TIME" arrow plot at
#      ind_to_indpayments_regional_IO.R line ~1179.
#   2. CONNECTIVITY MATRICES - how IS-8 categories pay each other, and how much
#      regions are connected through IS-8 activity (IS-8 x IS-8 and region x
#      region flow matrices).
#
# ---------------------------------------------------------------------------
# THE GRANULARITY PROBLEM AND THE DOUBLE-COUNTING STRATEGY (read before editing)
# ---------------------------------------------------------------------------
# The payments data (i2i.yr) is published at 2-DIGIT SIC only. The IS-8
# definitions (data/industrialstrategy2025sectordefs.csv) are at 2-, 3- and
# 4-digit, plus "frontier" rows with no SIC at all. To map IS-8 onto the flows
# we TRUNCATE every IS-8 SIC to 2 digits. This is lossy: a 3/4-digit IS-8 code
# pulls in the WHOLE 2-digit division (e.g. Creative's 6201/6202 "computer
# games / programming" become all of division 62 "computer programming &
# consultancy"). Fidelity, best to worst:
#   FIN (64,65,66) & PBS (69-74,77,78,82)            -> clean (truncation barely distorts)
#   ADV MANUF (20,26,27,28,29,30,33)                 -> good
#   LIFESCI (21,26,32,72), FOUNDATION (20,23,24,35,41,42,43,52) -> lossy
#   DEFENCE (25,30), DIGITAL (26,27,46,58-63,71,72,95),
#     CREATIVE (32,58,59,60,62,70,71,73,74,85,90,91) -> very lossy
#   Clean Energy                                     -> not representable (all frontier, no SIC)
# This 2-digit coarseness is a hard limit of the published payments source, not
# a modelling choice - there is no finer SIC breakdown of the flows to fall back on.
#
# Once truncated, there are TWO distinct double-counting mechanisms:
#
# Mechanism 1 - the same division appears twice WITHIN one IS-8.
#   e.g. Creative lists both 6201 and 6202, both -> 62. Left as-is, a division-62
#   flow would be counted twice inside Creative.
#   FIX (always, both deliverables): distinct(indstrat_code, sic2) so each IS-8
#   holds each 2-digit division at most once. This is the 2-digit analogue of the
#   nesting fix the jobs code does with droplist_foreachIS8
#   (industrial_strategy_datalinkage.R line ~783).
#
# Mechanism 2 - the same division is claimed by DIFFERENT IS-8s.
#   14 of the 40 truncated divisions are shared (20,26,27,30,32,58,59,60,62,70,
#   71,72,73,74). A single flow from a shared division then lands in multiple
#   (payer-IS-8, payee-IS-8) cells. At 2-digit we cannot split the pounds between
#   claimants (we can't tell creative-62 from general-IT-62), so row sums, column
#   sums and the grand total of a naive IS-8 x IS-8 matrix are inflated.
#
# We handle Mechanism 2 with a DIFFERENT mapping for each deliverable, for
# principled reasons:
#
#   ARROW PLOT -> strategy (A) OVERLAP. Each IS-8 is processed INDEPENDENTLY and
#   its localities are never summed across IS-8s. An IS-8's locality is a
#   self-contained internal/total ratio, so a division shared between (say)
#   Creative and Digital does NOT double-count within either ratio - it simply
#   contributes to both sectors' own ratios, which is correct. This also keeps the
#   method consistent with the jobs IS-8 LQ analysis (indstratsums_perIS8), which
#   likewise keeps each IS-8's full (overlapping) SIC set and never totals across
#   IS-8s. The ONLY rule: do not add the IS-8 localities together.
#
#   CONNECTIVITY MATRIX -> strategy (B) PARTITION. Every 2-digit division is
#   assigned to EXACTLY ONE IS-8 (is8_partition below). Each flow then lands in
#   exactly one (payer_is8, payee_is8) cell, so the matrix is summable with zero
#   double counting - which is what a "flows between categories" accounting needs.
#   The partition keeps manufacturing divisions with ADV MANUF, content (58/59/60)
#   with CREATIVE, IT/electronics (26/62) with DIGITAL, and professional services
#   (70-74) with PBS. Four rows are genuinely ambiguous at 2-digit and are flagged
#   inline: 20 chemicals (FOUNDATION vs ADV MANUF), 32 other-mfg (LIFESCI vs
#   CREATIVE), 72 R&D (PBS vs LIFESCI/DIGITAL), 73 advertising (PBS vs CREATIVE).
# ============================================================================

library(tidyverse)
library(patchwork)
source('functions/misc_functions.R')   # provides qg()

theme_set(theme_light())


# LOAD ----

# Industry-to-industry payment flows: ITL1 region x 2-digit SIC, yearly.
# Columns: year (Date, 2019-2025), payer_sic2digit, payee_sic2digit (numeric),
# payer_ITL1name, payee_ITL1name, pounds, sectionname_payer/payee, ...
i2i.yr = readRDS('local/data/payments_itl2_sic2_yearly_w_sections.rds')

# Integer year for labels / pivots (source 'year' is a Date: 2019-01-01 ...)
i2i.yr = i2i.yr %>% mutate(year_int = as.integer(format(year, "%Y")))

# Northern Ireland is dropped to mirror the section-level arrow plot
# (coef_regions_exclude in ind_to_indpayments_regional_IO.R) - its cross-border
# flows with Ireland behave oddly. Set to character(0) to keep all 12 regions.
# NB this drops a region from the flows ENTIRELY (as payer AND payee), so it also
# stops being a trading counterpart - only use it for regions you want gone from
# the analysis altogether.
regions_to_drop = c("Northern Ireland")

# DIFFERENT to regions_to_drop: regions excluded only from the LQ scoring and the
# leave-one-out benchmark (section 5), but KEPT as trading counterparts in the
# flows. London is a ~outlier hub (its IS-8 flows are ~50% internal vs ~20%
# elsewhere); kept as a counterpart, Y&H->London still counts as external (so
# locality stays honest), but its extreme internal share no longer inflates every
# other region's "typical region" benchmark. Set to character(0) to keep London
# in the benchmark. Do NOT move London into regions_to_drop - that would remove it
# as a counterpart and artificially inflate every remaining region's internal share.
lq_exclude_regions = c("London")


# IS-8 -> 2-DIGIT SIC LOOKUPS ----

indsic = read_csv('data/industrialstrategy2025sectordefs.csv')

# (A) OVERLAP lookup - used for the arrow plot.
# Reuses the short IS-8 code from industrial_strategy_datalinkage.R (lines 27-36).
# 'Clean Energy' has no SIC rows, so it drops out once we remove NA sic_code.
is8_lookup_overlap = indsic %>%
  filter(!is.na(sic_code)) %>%
  mutate(
    indstrat_code = case_when(
      qg('manufactur', sector_name) ~ "ADV MANUF",
      qg('business',   sector_name) ~ "PBS",
      qg('financ',     sector_name) ~ "FIN",
      qg('creative',   sector_name) ~ "CREATIVE",
      qg('foundation', sector_name) ~ "FOUNDATION",
      qg('digital',    sector_name) ~ "DIGITAL",
      qg('life sci',   sector_name) ~ "LIFESCI",
      qg('defence',    sector_name) ~ "DEFENCE"
    ),
    # Truncate each IS-8 SIC to 2 digits (e.g. 6201 -> 62, 283 -> 28, 29 -> 29)
    sic2 = as.integer(substr(as.character(sic_code), 1, 2))
  ) %>%
  # Mechanism-1 fix: each IS-8 holds each 2-digit division at most once
  distinct(indstrat_code, sic2) %>%
  arrange(indstrat_code, sic2)

# Show the 2-digit expansion per IS-8 (sanity check vs the header table)
is8_lookup_overlap %>%
  group_by(indstrat_code) %>%
  summarise(sic2s = paste(sort(sic2), collapse = " "), .groups = 'drop') %>%
  print(n = Inf)


# (B) PARTITION lookup - used for the connectivity matrices.
# Shared divisions get a single owner via the overrides below; unique-owner
# divisions keep their sole owner. See header for the rationale per row.
is8_partition_overrides = tribble(
  ~sic2,  ~indstrat_code,
  20L,    "FOUNDATION",   # chemicals      - DEBATABLE vs ADV MANUF
  26L,    "DIGITAL",      # electronics mfg (ADV MANUF / LIFESCI also claim)
  27L,    "ADV MANUF",    # electrical equip (DIGITAL wants cables 273x/279 only)
  30L,    "ADV MANUF",    # transport equip (DEFENCE wants 304 mil. vehicles only)
  32L,    "LIFESCI",      # other mfg      - DEBATABLE vs CREATIVE (medical instr 325 vs jewellery 3212)
  58L,    "CREATIVE",     # publishing     (DIGITAL lists it secondarily)
  59L,    "CREATIVE",     # film / music
  60L,    "CREATIVE",     # broadcasting
  62L,    "DIGITAL",      # computing      (CREATIVE wants games 6201/62011 only)
  70L,    "PBS",          # mgmt consultancy (CREATIVE wants PR 7021 only)
  71L,    "PBS",          # arch / eng     (CREATIVE=7111, DIGITAL=7112 sub-parts)
  72L,    "PBS",          # R&D            - DEBATABLE vs LIFESCI / DIGITAL
  73L,    "PBS",          # advertising    - DEBATABLE vs CREATIVE
  74L,    "PBS"           # other professional (CREATIVE wants design/photo 741/742/743)
)

# Divisions claimed by more than one IS-8 in the overlap lookup
shared_sics = is8_lookup_overlap %>% count(sic2) %>% filter(n > 1) %>% pull(sic2)

# Guard: the overrides must cover exactly the shared set (no more, no less)
stopifnot(setequal(shared_sics, is8_partition_overrides$sic2))

is8_partition = bind_rows(
  is8_lookup_overlap %>% filter(!sic2 %in% shared_sics),  # unique owners, kept as-is
  is8_partition_overrides                                 # shared divisions, one owner each
) %>%
  distinct(sic2, indstrat_code) %>%
  arrange(sic2)

# Guard: every division now has exactly one owner
stopifnot(is8_partition %>% count(sic2) %>% filter(n > 1) %>% nrow() == 0)


# Common flow base: drop SIC-0 / NA-section flows and households / extraterritorial,
# matching the section-level IO analysis. internal = within-region flow.
flows_base = i2i.yr %>%
  filter(
    !is.na(sectionname_payer),
    !is.na(sectionname_payee),
    !qg('households|extraterr', sectionname_payer),
    !qg('households|extraterr', sectionname_payee),
    !payer_ITL1name %in% regions_to_drop,
    !payee_ITL1name %in% regions_to_drop
  ) %>%
  mutate(internal = payer_ITL1name == payee_ITL1name)


# ============================================================================
# 1. ARROW PLOT: IS-8 SECTOR LOCALITY CHANGE OVER TIME  (strategy A: OVERLAP)
# ============================================================================
# Analogue of ind_to_indpayments_regional_IO.R line ~1179. Two timepoints, one
# arrow per IS-8 per region. Sector-centric, each axis anchored on the IS-8's OWN
# region (consistent with sections 5-6, so it's the absolute-shares twin of the
# section-6 LQ arrows):
#   payer_locality = of what the IS-8 BUYS (it is the payer), share from suppliers
#                    in its own region -> "how local are its suppliers"
#   payee_locality = of what the IS-8 SELLS (it is the payee), share from customers
#                    in its own region -> "how local are its customers"
# Each IS-8 is computed independently (overlap), so shared divisions are fine.

# Two timepoints to compare. Default = full span; swap to c(2021, 2025) to skip
# the COVID-onset years if preferred.
locality_years = c(min(i2i.yr$year_int), max(i2i.yr$year_int))
# locality_years = c(2021, 2025)

# payer_locality (SUPPLIERS): IS-8 as the PAYER, anchored on the IS-8's own
# (payer) region. internal = supplier in same region. Join attaches payer-division
# IS-8 membership(s); grouping by indstrat_code keeps each IS-8 independent.
payer_loc = flows_base %>%
  filter(year_int %in% locality_years) %>%
  # many-to-many is intended: a shared division attaches to each IS-8 that claims it
  inner_join(is8_lookup_overlap, by = c('payer_sic2digit' = 'sic2'), relationship = "many-to-many") %>%
  group_by(indstrat_code, region = payer_ITL1name, year_int) %>%
  summarise(
    payer_locality = sum(pounds * internal) / sum(pounds),
    .groups = 'drop'
  )

# payee_locality (CUSTOMERS): IS-8 as the PAYEE, anchored on the IS-8's own
# (payee) region. internal = customer in same region.
payee_loc = flows_base %>%
  filter(year_int %in% locality_years) %>%
  # many-to-many is intended: a shared division attaches to each IS-8 that claims it
  inner_join(is8_lookup_overlap, by = c('payee_sic2digit' = 'sic2'), relationship = "many-to-many") %>%
  group_by(indstrat_code, region = payee_ITL1name, year_int) %>%
  summarise(
    payee_locality = sum(pounds * internal) / sum(pounds),
    .groups = 'drop'
  )

# Join into the shape the section-level arrow code expects (region = IS-8's region)
sector_locality_multiyear = payer_loc %>%
  inner_join(payee_loc, by = c('indstrat_code', 'region', 'year_int')) %>%
  rename(section = indstrat_code, year = year_int)

# Pivot to start/end positions and classify direction of change
sector_locality_arrows = sector_locality_multiyear %>%
  mutate(timepoint = ifelse(year == min(year), "start", "end")) %>%
  select(region, section, timepoint, payer_locality, payee_locality) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = c(payer_locality, payee_locality)
  ) %>%
  mutate(
    payer_change = payer_locality_end - payer_locality_start,
    payee_change = payee_locality_end - payee_locality_start,
    compass = case_when(
      payer_change > 0 & payee_change > 0 ~ "NE",  # more local on both
      payer_change > 0 & payee_change < 0 ~ "SE",
      payer_change < 0 & payee_change > 0 ~ "NW",
      payer_change < 0 & payee_change < 0 ~ "SW",  # less local on both
      TRUE ~ "NC"
    )
  )

# Bounding box across both timepoints (for the faint reference rectangle)
arrows_bbox = list(
  xmin = min(c(sector_locality_arrows$payer_locality_start, sector_locality_arrows$payer_locality_end), na.rm = TRUE),
  xmax = max(c(sector_locality_arrows$payer_locality_start, sector_locality_arrows$payer_locality_end), na.rm = TRUE),
  ymin = min(c(sector_locality_arrows$payee_locality_start, sector_locality_arrows$payee_locality_end), na.rm = TRUE),
  ymax = max(c(sector_locality_arrows$payee_locality_start, sector_locality_arrows$payee_locality_end), na.rm = TRUE)
)

is8_arrow_plot = ggplot(sector_locality_arrows) +
  annotate("rect",
           xmin = arrows_bbox$xmin, xmax = arrows_bbox$xmax,
           ymin = arrows_bbox$ymin, ymax = arrows_bbox$ymax,
           fill = "grey90", alpha = 0.5, colour = "grey70", linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_segment(
    aes(x = payer_locality_start, y = payee_locality_start,
        xend = payer_locality_end, yend = payee_locality_end, colour = compass),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    linewidth = 0.6, alpha = 0.85
  ) +
  geom_point(aes(x = payer_locality_start, y = payee_locality_start),
             size = 0.6, colour = "grey40", alpha = 0.5) +
  ggrepel::geom_text_repel(
    aes(x = payer_locality_end, y = payee_locality_end, label = section, colour = compass),
    size = 2.4, max.overlaps = 20,
    segment.colour = "grey70", segment.alpha = 0.5,
    box.padding = unit(0.2, "lines"), show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("NE" = "#2ca02c", "SE" = "#ff7f0e", "NW" = "#1f77b4",
               "SW" = "#d62728", "NC" = "grey50"),
    labels = c("NE" = "Buying & selling nearer", "SE" = "Buying nearer, selling farther",
               "NW" = "Buying farther, selling nearer", "SW" = "Buying & selling farther",
               "NC" = "No change"),
    name = "Direction of change"
  ) +
  facet_wrap(~region, ncol = 3) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = paste0("IS-8 sector locality change: ", min(locality_years), " -> ", max(locality_years)),
    subtitle = "Each arrow = one IS-8 growth sector; overlap mapping, each IS-8 independent (not summable)",
    x = "How local are its suppliers? (share it buys from within region)",
    y = "How local are its customers? (share it sells within region)",
    caption = "2-digit SIC truncation: Creative/Digital/Defence coarse, Finance/PBS clean (see header)"
  ) +
  theme(plot.caption = element_text(hjust = 0), legend.position = "bottom",
        legend.text = element_text(size = 7))

is8_arrow_plot
ggsave('llm_output/io_plots/is8_arrow_plot_change.png', is8_arrow_plot, width = 11, height = 12)
saveRDS(sector_locality_multiyear, 'local/data/is8_flow_locality_multiyear.rds')


# ============================================================================
# 2. IS-8 x IS-8 FLOW MATRIX  (strategy B: PARTITION)
# ============================================================================
# With the partition, each division has one owner, so each flow lands in exactly
# one (payer_is8, payee_is8) cell - no double counting. Both ends must be in an
# IS-8 (inner joins), so this is the "IS-8 economy talking to itself".

flows_is8 = flows_base %>%
  inner_join(is8_partition %>% rename(payer_is8 = indstrat_code), by = c('payer_sic2digit' = 'sic2')) %>%
  inner_join(is8_partition %>% rename(payee_is8 = indstrat_code), by = c('payee_sic2digit' = 'sic2'))

matrix_year = max(flows_is8$year_int)

is8_x_is8 = flows_is8 %>%
  filter(year_int == matrix_year) %>%
  group_by(payer_is8, payee_is8) %>%
  summarise(pounds = sum(pounds), .groups = 'drop') %>%
  # Row share: of each IS-8's outbound spend (to other IS-8s), where does it go?
  group_by(payer_is8) %>%
  mutate(row_share = pounds / sum(pounds)) %>%
  ungroup()

is8_matrix_plot = ggplot(is8_x_is8, aes(x = payee_is8, y = payer_is8, fill = row_share)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(row_share, accuracy = 1)), size = 2.6) +
  scale_fill_distiller(palette = 'Blues', direction = 1, labels = scales::percent,
                       name = "Share of\npayer's\nIS-8 spend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = paste0("IS-8 -> IS-8 payment flows, ", matrix_year),
    subtitle = "Row share: of each IS-8's spending on other IS-8 sectors, where it goes (partition mapping)",
    x = "Payee IS-8 (receiving)", y = "Payer IS-8 (spending)"
  )

is8_matrix_plot
ggsave('llm_output/io_plots/is8_x_is8_flow_matrix.png', is8_matrix_plot, width = 9, height = 7)


# ============================================================================
# 3. REGION x REGION CONNECTIVITY THROUGH IS-8 ACTIVITY  (strategy B: PARTITION)
# ============================================================================
# Restricting to IS-8<->IS-8 flows, how much are regions connected? Row share =
# of each region's outbound IS-8 spend, what fraction goes to each region
# (diagonal = stays internal).

region_x_region = flows_is8 %>%
  filter(year_int == matrix_year) %>%
  group_by(payer_ITL1name, payee_ITL1name) %>%
  summarise(pounds = sum(pounds), .groups = 'drop') %>%
  group_by(payer_ITL1name) %>%
  mutate(row_share = pounds / sum(pounds)) %>%
  ungroup()

region_matrix_plot = ggplot(region_x_region,
                            aes(x = payee_ITL1name, y = payer_ITL1name, fill = row_share)) +
  geom_tile() +
  scale_fill_distiller(palette = 'Blues', direction = 1, labels = scales::percent,
                       name = "Share of\npayer region's\nIS-8 spend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  labs(
    title = paste0("Region-to-region connectivity via IS-8 flows, ", matrix_year),
    subtitle = "Row share: of each region's IS-8 spending, what fraction goes to each region (diagonal = internal)",
    x = "Payee region (receiving)", y = "Payer region (spending)"
  )

region_matrix_plot
ggsave('llm_output/io_plots/is8_region_connectivity_matrix.png', region_matrix_plot, width = 9, height = 8)


# ============================================================================
# 4. IS-8 ECONOMY INTERNAL SHARE OVER TIME, PER REGION  (strategy B: PARTITION)
# ============================================================================
# Of each region's total IS-8<->IS-8 flows, what share stays within the region,
# and is that rising or falling? (On-theme with "how connected are regions".)

region_internal_overtime = flows_is8 %>%
  filter(year_int %in% locality_years) %>%
  group_by(payer_ITL1name, year_int) %>%
  summarise(internal_share = sum(pounds * internal) / sum(pounds), .groups = 'drop') %>%
  mutate(year = factor(year_int))

region_internal_plot = ggplot(region_internal_overtime,
                              aes(x = reorder(payer_ITL1name, internal_share),
                                  y = internal_share, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Internal share of IS-8 flows, by region, over time",
    subtitle = paste0("Share of each region's IS-8<->IS-8 payments kept within region (",
                      paste(locality_years, collapse = " vs "), ")"),
    x = "", y = "Internal share of IS-8 flows", fill = "Year"
  ) +
  theme(legend.position = "bottom")

region_internal_plot
ggsave('llm_output/io_plots/is8_region_internal_share_overtime.png', region_internal_plot, width = 8, height = 6)


# ============================================================================
# 5. FLOW-LOCALITY LOCATION QUOTIENTS  (two-axis behavioural; strategy A: OVERLAP)
# ============================================================================
# Re-express each IS-8 sector's locality as an LQ against the TYPICAL region's
# same IS-8, so sectors with very different baseline localities (Finance is
# intrinsically local, Adv Manuf intrinsically traded) become comparable.
#   LQ = 1   -> same as the typical region
#   LQ = 1.3 -> 1.3x the typical region's IS-8  (e.g. "Y&H Adv Manuf sources
#               locally 1.3x the typical region's Adv Manuf")
#
# The four internal/external flow shares collapse to TWO axes, because external
# share = 1 - internal share, so an external LQ is just the mirror of its
# internal LQ. The two independent axes:
#   spend-locality   = IS/(IS+ES) - of what S-in-R BUYS, share from within R
#                      (S is the payer; anchored on S's own = PAYER region)
#   receive-locality = IR/(IR+ER) - of what S-in-R SELLS, share from within R
#                      (S is the payee; anchored on S's own = PAYEE region)
# NB the receive side is anchored on the PAYEE region (S's own region) - the
# intuitive "how locally is S supplied" reading. This differs from the arrow plot
# (section 1), which mirrored the section-level IO plot by anchoring BOTH axes on
# the payer region; hence the distinct names spend_locality / receive_locality.
#
# Benchmark = LEAVE-ONE-OUT unweighted mean across the OTHER regions' same IS-8
# (matches "the typical region"; stops a region contaminating its own benchmark).
# Overlap mapping, each IS-8 independent - never summed across IS-8s.

# Reusable: compute the two-axis behavioural flow-locality LQs for one year.
# Overlap mapping, each IS-8 independent. lq_exclude_regions are kept as trading
# counterparts (still count as external) but dropped from scoring and from the
# leave-one-out "typical region" benchmark. Benchmark is recomputed within the year.
compute_flow_locality_lq = function(yr) {

  yr_flows = flows_base %>% filter(year_int == yr)

  # spend-locality: S as PAYER, anchored on payer region (the group). The
  # lq_exclude filter drops the SCORED region only; excluded regions remain as
  # payees, so they still count as external counterparts.
  spend_loc = yr_flows %>%
    inner_join(is8_lookup_overlap, by = c('payer_sic2digit' = 'sic2'), relationship = "many-to-many") %>%
    group_by(indstrat_code, region = payer_ITL1name) %>%
    summarise(spend_locality = sum(pounds * internal) / sum(pounds), .groups = 'drop') %>%
    filter(!region %in% lq_exclude_regions)

  # receive-locality: S as PAYEE, anchored on payee region (S's own region).
  # Excluded regions remain as payers, so they still count as external counterparts.
  receive_loc = yr_flows %>%
    inner_join(is8_lookup_overlap, by = c('payee_sic2digit' = 'sic2'), relationship = "many-to-many") %>%
    group_by(indstrat_code, region = payee_ITL1name) %>%
    summarise(receive_locality = sum(pounds * internal) / sum(pounds), .groups = 'drop') %>%
    filter(!region %in% lq_exclude_regions)

  spend_loc %>%
    full_join(receive_loc, by = c('indstrat_code', 'region')) %>%
    group_by(indstrat_code) %>%
    mutate(
      # leave-one-out mean of the OTHER regions' same IS-8 (the "typical region")
      spend_loc_typical   = (sum(spend_locality, na.rm = TRUE)   - coalesce(spend_locality, 0)) /
                            (sum(!is.na(spend_locality))   - !is.na(spend_locality)),
      receive_loc_typical = (sum(receive_locality, na.rm = TRUE) - coalesce(receive_locality, 0)) /
                            (sum(!is.na(receive_locality)) - !is.na(receive_locality)),
      spend_LQ       = spend_locality   / spend_loc_typical,
      receive_LQ     = receive_locality / receive_loc_typical,
      spend_LQ_log   = log2(spend_LQ),
      receive_LQ_log = log2(receive_LQ)
    ) %>%
    ungroup() %>%
    mutate(year = yr)
}

lq_year = max(i2i.yr$year_int)
flow_locality_lq = compute_flow_locality_lq(lq_year)

saveRDS(flow_locality_lq, 'local/data/is8_flow_locality_lq.rds')

# The headline reads (e.g. for Y&H): LQ of 1.3 = "sources locally 1.3x typical"
flow_locality_lq %>%
  filter(region == "Yorkshire and The Humber") %>%
  transmute(indstrat_code,
            spend_locality = round(spend_locality, 3), spend_LQ = round(spend_LQ, 2),
            receive_locality = round(receive_locality, 3), receive_LQ = round(receive_LQ, 2)) %>%
  arrange(-spend_LQ) %>%
  print(n = Inf)


## 5a. Situate every region against all others, faceted by IS-8 ----

region_abbrevs = c(
  "North East" = "NE", "North West" = "NW", "Yorkshire and The Humber" = "Yorks",
  "East Midlands" = "EM", "West Midlands" = "WM", "East of England" = "East",
  "London" = "Lon", "South East" = "SE", "South West" = "SW",
  "Wales" = "Wal", "Scotland" = "Scot", "Northern Ireland" = "NI"
)

lq_plotdat = flow_locality_lq %>%
  mutate(reg_ab = region_abbrevs[region],
         is_ynh = region == "Yorkshire and The Humber")

# Note for plot subtitles: which regions (if any) are excluded from the benchmark
bench_note = if (length(lq_exclude_regions) > 0)
  paste0(" (", paste(lq_exclude_regions, collapse = ", "), " excluded from benchmark)") else ""

is8_lq_situate_plot = ggplot(lq_plotdat, aes(x = spend_LQ_log, y = receive_LQ_log)) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.3) +
  geom_point(aes(colour = is_ynh, size = is_ynh)) +
  ggrepel::geom_text_repel(aes(label = reg_ab, colour = is_ynh), size = 2.3,
                           max.overlaps = 20, segment.colour = "grey75", show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "grey55", `TRUE` = "red"), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 1.4, `TRUE` = 2.8), guide = "none") +
  facet_wrap(~indstrat_code, ncol = 3) +
  labs(
    title = paste0("IS-8 flow-locality LQs by region, ", lq_year, " (Y&H in red)"),
    subtitle = paste0("Each point = one region's IS-8 vs the typical region (leave-one-out mean)", bench_note),
    x = "Suppliers: how local vs typical region (LQ, log2: 0 = typical, +1 = 2x more local buying)",
    y = "Customers: how local vs typical region (LQ, log2: 0 = typical, +1 = 2x more local selling)",
    caption = "Top-right = both suppliers AND customers more local than the typical region; bottom-left = more traded at both ends"
  ) +
  theme(plot.caption = element_text(hjust = 0))

is8_lq_situate_plot
ggsave('llm_output/io_plots/is8_flow_locality_lq_situate.png', is8_lq_situate_plot, width = 11, height = 9)


## 5b. Y&H headline dot plot: LQ per IS-8, spend vs receive ----

ynh_lq = flow_locality_lq %>%
  filter(region == "Yorkshire and The Humber") %>%
  select(indstrat_code, `Suppliers (buys from)` = spend_LQ, `Customers (sells to)` = receive_LQ) %>%
  pivot_longer(c(`Suppliers (buys from)`, `Customers (sells to)`), names_to = "side", values_to = "LQ")

ynh_lq_plot = ggplot(ynh_lq, aes(x = LQ, y = fct_reorder(indstrat_code, LQ), colour = side)) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.6) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  scale_x_log10() +
  scale_colour_manual(values = c("Suppliers (buys from)" = "#1f77b4", "Customers (sells to)" = "#d62728")) +
  labs(
    title = "Yorkshire & Humber IS-8 flow-locality vs the typical region",
    subtitle = paste0(lq_year, ": LQ > 1 = more locally embedded than the typical region's same IS-8", bench_note),
    x = "Location quotient (log scale; 1 = typical region)", y = "", colour = ""
  ) +
  theme(legend.position = "bottom")

ynh_lq_plot
ggsave('llm_output/io_plots/is8_flow_locality_lq_ynh.png', ynh_lq_plot, width = 8, height = 6)


# ============================================================================
# 6. FLOW-LOCALITY LQ CHANGE OVER TIME (ARROW PLOT)  (strategy A: OVERLAP)
# ============================================================================
# Same axes as section 5, but an arrow per region x IS-8 from the earliest to the
# latest year (faceted BY REGION, IS-8s as arrows - mirrors the section-1 layout).
#
# IMPORTANT - these arrows are in LQ (relative) space, NOT absolute locality.
# The benchmark (the "typical region") is recomputed for EACH year, so an arrow
# shows movement RELATIVE TO PEERS: a sector can point up-right here (gaining
# ground) even if its raw local share fell, as long as peers fell faster. For
# ABSOLUTE local-share movement see the section-1 arrow plot instead.

lq_arrow_years = c(min(i2i.yr$year_int), max(i2i.yr$year_int))
# lq_arrow_years = c(2021, 2025)   # alternative: skip COVID-onset years

# LQs at each timepoint (benchmark recomputed per year inside the function)
flow_lq_overtime = map(lq_arrow_years, compute_flow_locality_lq) %>% bind_rows()

flow_lq_arrows = flow_lq_overtime %>%
  mutate(timepoint = ifelse(year == min(year), "start", "end")) %>%
  select(region, indstrat_code, timepoint, spend_LQ_log, receive_LQ_log) %>%
  pivot_wider(names_from = timepoint, values_from = c(spend_LQ_log, receive_LQ_log)) %>%
  mutate(
    spend_change   = spend_LQ_log_end   - spend_LQ_log_start,
    receive_change = receive_LQ_log_end - receive_LQ_log_start,
    compass = case_when(
      spend_change > 0 & receive_change > 0 ~ "NE",  # gaining ground on both (vs peers)
      spend_change > 0 & receive_change < 0 ~ "SE",
      spend_change < 0 & receive_change > 0 ~ "NW",
      spend_change < 0 & receive_change < 0 ~ "SW",  # losing ground on both (vs peers)
      TRUE ~ "NC"
    )
  )

flow_lq_arrow_plot = ggplot(flow_lq_arrows) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.3) +
  geom_segment(
    aes(x = spend_LQ_log_start, y = receive_LQ_log_start,
        xend = spend_LQ_log_end, yend = receive_LQ_log_end, colour = compass),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.6, alpha = 0.85
  ) +
  geom_point(aes(x = spend_LQ_log_start, y = receive_LQ_log_start),
             size = 0.5, colour = "grey40", alpha = 0.5) +
  ggrepel::geom_text_repel(
    aes(x = spend_LQ_log_end, y = receive_LQ_log_end, label = indstrat_code, colour = compass),
    size = 2, max.overlaps = 20, segment.colour = "grey75",
    box.padding = unit(0.15, "lines"), show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("NE" = "#2ca02c", "SE" = "#ff7f0e", "NW" = "#1f77b4",
               "SW" = "#d62728", "NC" = "grey50"),
    labels = c("NE" = "Buying & selling nearer (vs peers)", "SE" = "Buying nearer, selling farther",
               "NW" = "Buying farther, selling nearer", "SW" = "Buying & selling farther (vs peers)",
               "NC" = "No change"),
    name = "Direction of change (vs peers)"
  ) +
  facet_wrap(~region, ncol = 3) +
  labs(
    title = paste0("IS-8 flow-locality LQ change: ", min(lq_arrow_years), " -> ", max(lq_arrow_years)),
    subtitle = paste0("Arrows = movement in LQ space, RELATIVE to the typical region (recomputed each year)", bench_note),
    x = "Suppliers: how local vs typical region (LQ, log2; 0 = typical)",
    y = "Customers: how local vs typical region (LQ, log2; 0 = typical)",
    caption = "Relative to peers, not absolute locality (cf section 1 for absolute). Green = buying & selling nearer than peers; red = both farther."
  ) +
  theme(plot.caption = element_text(hjust = 0), legend.position = "bottom",
        legend.text = element_text(size = 7))

flow_lq_arrow_plot
ggsave('llm_output/io_plots/is8_flow_locality_lq_arrows.png', flow_lq_arrow_plot, width = 11, height = 12)


## 6b. Free-scales version: each region zoomed to its own arrows ----
# Same plot, but scales = "free" so each region panel uses its own axis range -
# makes small movers (e.g. Y&H) legible. The 0/typical reference lines are kept,
# so each panel still includes the "typical region" benchmark. NB axis ranges now
# DIFFER between panels, so arrow lengths are NOT comparable across regions.
flow_lq_arrow_plot_free = flow_lq_arrow_plot +
  facet_wrap(~region, ncol = 3, scales = "free") +
  labs(subtitle = paste0("FREE axis scales: each region zoomed to its own arrows (lengths not comparable across panels)", bench_note))

flow_lq_arrow_plot_free
ggsave('llm_output/io_plots/is8_flow_locality_lq_arrows_freescales.png', flow_lq_arrow_plot_free, width = 11, height = 12)


# ============================================================================
# 7. YEARLY TRAJECTORIES: WHEN did localisation change?  (strategy A: OVERLAP)
# ============================================================================
# The arrow plots show only the 2019 vs 2025 endpoints. This traces every year so
# we can see WHEN a shift happened - e.g. abrupt at the pandemic onset (2019->2020)
# vs gradual through the recovery (2021->2025).
#
# CAVEAT: the payments data starts in 2019, so there is only ONE pre-pandemic year.
# We can locate when within 2019-2025 a change occurred, but cannot establish a
# pre-pandemic TREND (no run-up years before 2019).
#
# Layout = compare-regions: facet by IS-8, every region a faint grey line, with
# Y&H and Wales highlighted. Two measures x two sides = four plots:
#   absolute locality share (did it ACTUALLY localise) and
#   LQ vs the typical region (did it move RELATIVE to peers, benchmark per year).

all_years = sort(unique(i2i.yr$year_int))

# Reuse the per-year LQ function (gives absolute spend/receive_locality AND LQs).
# Benchmark (and London exclusion) are recomputed within each year, as in section 6.
flow_locality_timeseries = map(all_years, compute_flow_locality_lq) %>% bind_rows()

saveRDS(flow_locality_timeseries, 'local/data/is8_flow_locality_timeseries.rds')

highlight_regions = c("Yorkshire and The Humber", "Wales")

# Helper: facet-by-IS-8 trajectory, all regions grey, highlight_regions coloured.
make_ts_plot = function(df, yvar, ylab, title, ref = NA_real_, ref_is_percent = FALSE) {
  hl = df %>% filter(region %in% highlight_regions)
  p = ggplot(df, aes(x = year, y = .data[[yvar]], group = region)) +
    geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey50", alpha = 0.7) +
    geom_line(colour = "grey80", linewidth = 0.4) +
    geom_line(data = hl, aes(colour = region), linewidth = 0.9) +
    geom_point(data = hl, aes(colour = region), size = 1.3) +
    scale_colour_manual(
      values = c("Yorkshire and The Humber" = "#d62728", "Wales" = "#1f77b4"),
      name = NULL
    ) +
    scale_x_continuous(breaks = all_years) +
    facet_wrap(~indstrat_code, ncol = 4) +
    labs(
      title = title, x = NULL, y = ylab,
      caption = paste0("Grey = other regions; dashed line = 2020 (pandemic onset). ",
                       "Data starts 2019 - one pre-pandemic year only.", bench_note)
    ) +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(size = 6))
  if (!is.na(ref)) p = p + geom_hline(yintercept = ref, linetype = "dotted", alpha = 0.5)
  if (ref_is_percent) p = p + scale_y_continuous(labels = scales::percent)
  p
}

yr_lab = paste0(min(all_years), "-", max(all_years))

# Absolute locality shares over time
p_abs_sup = make_ts_plot(flow_locality_timeseries, "spend_locality",
  "Suppliers: local share of purchases", paste0("How local are each IS-8's SUPPLIERS over time, ", yr_lab),
  ref_is_percent = TRUE)
p_abs_cus = make_ts_plot(flow_locality_timeseries, "receive_locality",
  "Customers: local share of sales", paste0("How local are each IS-8's CUSTOMERS over time, ", yr_lab),
  ref_is_percent = TRUE)

# LQ (relative to typical region) over time; reference line at 1 = typical
p_lq_sup = make_ts_plot(flow_locality_timeseries, "spend_LQ",
  "Suppliers locality LQ (1 = typical region)", paste0("SUPPLIERS locality vs typical region (LQ) over time, ", yr_lab),
  ref = 1)
p_lq_cus = make_ts_plot(flow_locality_timeseries, "receive_LQ",
  "Customers locality LQ (1 = typical region)", paste0("CUSTOMERS locality vs typical region (LQ) over time, ", yr_lab),
  ref = 1)

p_abs_sup
p_abs_cus
p_lq_sup
p_lq_cus

ggsave('llm_output/io_plots/is8_locality_timeseries_abs_suppliers.png', p_abs_sup, width = 12, height = 8)
ggsave('llm_output/io_plots/is8_locality_timeseries_abs_customers.png', p_abs_cus, width = 12, height = 8)
ggsave('llm_output/io_plots/is8_locality_timeseries_lq_suppliers.png',  p_lq_sup,  width = 12, height = 8)
ggsave('llm_output/io_plots/is8_locality_timeseries_lq_customers.png',  p_lq_cus,  width = 12, height = 8)
