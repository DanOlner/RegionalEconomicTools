# See ind_to_indpayments_region_by_SIC... for earlier code wrangling into this more manageable shape
# Here, let's do some IO digging
library(tidyverse)
library(patchwork)
source('functions/misc_functions.R')

theme_set(theme_light())


i2i.yr = readRDS('local/data/payments_itl2_sic2_yearly_w_sections.rds')


# LOCATION QUOTIENTS ON FLOWS ----

# Calculate flow-based LQs at the SIC section level
# LQ_ij = (Flow_ij in region / Total flows in region) / (Flow_ij in UK / Total UK flows)
# LQ > 1: this linkage is over-represented in the region
# LQ < 1: this linkage is under-represented

# Optional: subset to specific regions for pairwise comparison
# If NULL, uses all regions (compares each to UK average)
# If set to a vector of region names, compares those regions to their combined total
# e.g. for Yorkshire vs North West comparison:

regions_to_compare = c("Yorkshire and The Humber", "North West")
# regions_to_compare = NULL

# First, aggregate to section level (dropping NA sections i.e. SIC code 0)
# Using most recent year for now - could extend to all years
i2i.sections = i2i.yr %>%
  filter(
    !is.na(sectionname_payer),
    !is.na(sectionname_payee),
    year == max(year)  # Most recent year
  ) %>%
  # Apply region filter if specified

  {if(!is.null(regions_to_compare)) filter(., payer_ITL1name %in% regions_to_compare) else .} %>%
  group_by(payer_ITL1name, sectionname_payer, sectionname_payee) %>%
  summarise(
    pounds = sum(pounds, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate UK-wide totals for each section-to-section flow
uk_section_flows = i2i.sections %>%
  group_by(sectionname_payer, sectionname_payee) %>%
  summarise(
    uk_pounds = sum(pounds),
    .groups = 'drop'
  )

uk_total = sum(uk_section_flows$uk_pounds)

# Add UK share for each flow type
uk_section_flows = uk_section_flows %>%
  mutate(uk_share = uk_pounds / uk_total)

# Calculate regional totals
regional_totals = i2i.sections %>%
  group_by(payer_ITL1name) %>%
  summarise(regional_total = sum(pounds), .groups = 'drop')

# Join and calculate LQs
flow_lqs = i2i.sections %>%
  left_join(regional_totals, by = 'payer_ITL1name') %>%
  mutate(regional_share = pounds / regional_total) %>%
  left_join(uk_section_flows, by = c('sectionname_payer', 'sectionname_payee')) %>%
  mutate(
    LQ = regional_share / uk_share,
    LQ_log = log2(LQ),  # Log2 for symmetric interpretation: +1 = 2x over, -1 = 2x under
    # Add short section names for cleaner plots
    section_payer_short = reduceSICnames(sectionname_payer, 'section'),
    section_payee_short = reduceSICnames(sectionname_payee, 'section')
  )

# Quick check - LQs should average around 1 (weighted by flow size)
flow_lqs %>%
  group_by(payer_ITL1name) %>%
  summarise(
    weighted_mean_LQ = weighted.mean(LQ, pounds, na.rm = TRUE),
    median_LQ = median(LQ, na.rm = TRUE)
  )


# CREATE SECTION x SECTION LQ MATRICES PER REGION ----

# Function to create LQ matrix for a single region
make_lq_matrix = function(region_name, lq_data) {

  region_data = lq_data %>%
    filter(payer_ITL1name == region_name) %>%
    select(sectionname_payer, sectionname_payee, LQ)

  # Pivot to matrix form
  lq_matrix = region_data %>%
    pivot_wider(
      names_from = sectionname_payee,
      values_from = LQ,
      values_fill = NA
    ) %>%
    column_to_rownames('sectionname_payer') %>%
    as.matrix()

  return(lq_matrix)
}

# Create list of LQ matrices for all regions
regions = unique(flow_lqs$payer_ITL1name)
lq_matrices = map(regions, ~make_lq_matrix(.x, flow_lqs)) %>%
  set_names(regions)

# Check one
lq_matrices[["Yorkshire and The Humber"]]


# VISUALISE LQ MATRICES ----

# Heatmap function for a single region
plot_lq_heatmap = function(region_name, lq_data, use_log = TRUE, use_short_names = TRUE) {

  plot_data = lq_data %>%
    filter(payer_ITL1name == region_name)

  # Use log2 LQ for symmetric colour scale, or raw LQ
  y_var = if(use_log) "LQ_log" else "LQ"

  # Use short or full section names
  x_var = if(use_short_names) "section_payee_short" else "sectionname_payee"
  y_axis_var = if(use_short_names) "section_payer_short" else "sectionname_payer"

  p = ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_axis_var]], fill = .data[[y_var]])) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      name = if(use_log) "log2(LQ)" else "LQ",
      limits = if(use_log) c(-3, 3) else c(0, 4),
      oob = scales::squish
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7)
    ) +
    labs(
      title = region_name,
      x = "Payee Section (receiving)",
      y = "Payer Section (spending)"
    )

  return(p)
}

# Plot for one region
plot_lq_heatmap("Yorkshire and The Humber", flow_lqs)

# Plot all regions in a grid
all_region_plots = map(regions, ~plot_lq_heatmap(.x, flow_lqs))
wrap_plots(all_region_plots, ncol = 3) +
  plot_annotation(title = "Flow Location Quotients by Region (log2 scale)")


# TOP OVER/UNDER-REPRESENTED LINKAGES PER REGION ----

# What are the most distinctive linkages for each region?
distinctive_linkages = flow_lqs %>%
  group_by(payer_ITL1name) %>%
  slice_max(order_by = abs(LQ_log), n = 10) %>%
  arrange(payer_ITL1name, desc(LQ_log)) %>%
  select(payer_ITL1name, sectionname_payer, sectionname_payee, pounds, LQ, LQ_log)

# View top linkages for Yorkshire
distinctive_linkages %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  print(n = 10)


# INTERNAL VS EXTERNAL FLOW TRENDS ----

# Analyse how spending patterns differ for flows staying within a region vs leaving it
# Using log-transformed OLS to get comparable growth rates across regions

# Select sector pair to analyse (using section names)
# Start with Finance → Finance (section K)
payer_section_filter = "Financial and insurance activities"
payee_section_filter = "Financial and insurance activities"

# Prepare data: split into internal (same region) vs external (different region)
flow_trends = i2i.yr %>%
  filter(
    !is.na(sectionname_payer),
    !is.na(sectionname_payee),
    sectionname_payer == payer_section_filter,
    sectionname_payee == payee_section_filter
  ) %>%
  mutate(
    flow_type = ifelse(payer_ITL1name == payee_ITL1name, "internal", "external")
  ) %>%
  group_by(payer_ITL1name, year, flow_type) %>%
  summarise(
    pounds = sum(pounds, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate slopes using log transform for comparable % change interpretation
# Slope on log(pounds) ~ year gives approximate annual % change
flow_slopes = get_slope_and_se_safely(
  data = flow_trends,
  payer_ITL1name, flow_type,
  y = log(pounds),
  x = year
)

# Convert log slopes to annual % change
flow_slopes = flow_slopes %>%
  mutate(
    annual_pct_change = (exp(slope) - 1) * 100,
    # 95% CI bounds
    ci_lower = (exp(slope - 1.96 * se) - 1) * 100,
    ci_upper = (exp(slope + 1.96 * se) - 1) * 100,
    # Flag if significantly different from zero
    sig = sign(ci_lower) == sign(ci_upper)
  )

# View results
flow_slopes %>%
  arrange(flow_type, desc(annual_pct_change)) %>%
  print(n = 24)

# Pivot to compare internal vs external directly
flow_slopes_wide = flow_slopes %>%
  select(payer_ITL1name, flow_type, annual_pct_change, ci_lower, ci_upper, sig) %>%
  pivot_wider(
    names_from = flow_type,
    values_from = c(annual_pct_change, ci_lower, ci_upper, sig)
  ) %>%
  mutate(
    # Difference: positive means external growing faster than internal
    external_minus_internal = annual_pct_change_external - annual_pct_change_internal
  ) %>%
  arrange(desc(external_minus_internal))

flow_slopes_wide

# Visualise: dot plot comparing internal vs external growth rates
flow_slopes_plot = flow_slopes %>%
  mutate(
    payer_ITL1name = fct_reorder(payer_ITL1name, annual_pct_change)
  ) %>%
  ggplot(aes(x = annual_pct_change, y = payer_ITL1name, colour = flow_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    height = 0.2, alpha = 0.5
  ) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("internal" = "blue", "external" = "red")) +
  labs(
    title = paste0(payer_section_filter, " → ", payee_section_filter),
    subtitle = "Annual % change in payment flows (log-linear OLS)",
    x = "Annual % change",
    y = "",
    colour = "Flow type"
  ) +
  theme(legend.position = "bottom")

flow_slopes_plot

