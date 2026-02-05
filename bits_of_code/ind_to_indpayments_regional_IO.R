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
payer_section_filter = "Manufacturing"
payee_section_filter = "Administrative and support service activities"
# payee_section_filter = "Manufacturing"
# payer_section_filter = "Financial and insurance activities"
# payee_section_filter = "Financial and insurance activities"

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
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    width = 0.2, alpha = 0.5,
    orientation = "y",
    position = position_dodge(width = 0.5)
  ) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
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


# ALL SECTION PAIRS: INTERNAL VS EXTERNAL SLOPES ----

# Calculate internal vs external slopes for ALL section pairs across all regions
# This allows us to find which flows show the largest divergence between internal/external growth

# Prepare data for all section pairs
all_flow_trends = i2i.yr %>%
  filter(
    !is.na(sectionname_payer),
    !is.na(sectionname_payee)
  ) %>%
  mutate(
    flow_type = ifelse(payer_ITL1name == payee_ITL1name, "internal", "external")
  ) %>%
  group_by(payer_ITL1name, sectionname_payer, sectionname_payee, year, flow_type) %>%
  summarise(
    pounds = sum(pounds, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate slopes for all combinations
# This may take a moment as it's fitting many models
all_flow_slopes = get_slope_and_se_safely(
  data = all_flow_trends,
  payer_ITL1name, sectionname_payer, sectionname_payee, flow_type,
  y = log(pounds),
  x = year
)

# Convert to annual % change and add significance flags
all_flow_slopes = all_flow_slopes %>%
  mutate(
    annual_pct_change = (exp(slope) - 1) * 100,
    ci_lower = (exp(slope - 1.96 * se) - 1) * 100,
    ci_upper = (exp(slope + 1.96 * se) - 1) * 100,
    sig = sign(ci_lower) == sign(ci_upper),
    # Add short names for display
    section_payer_short = reduceSICnames(sectionname_payer, 'section'),
    section_payee_short = reduceSICnames(sectionname_payee, 'section')
  )

# Pivot to wide format for direct comparison
all_flow_slopes_wide = all_flow_slopes %>%
  select(payer_ITL1name, sectionname_payer, sectionname_payee,
         section_payer_short, section_payee_short,
         flow_type, annual_pct_change, ci_lower, ci_upper, sig) %>%
  pivot_wider(
    names_from = flow_type,
    values_from = c(annual_pct_change, ci_lower, ci_upper, sig)
  ) %>%
  mutate(
    # Difference: positive = external growing faster than internal
    external_minus_internal = annual_pct_change_external - annual_pct_change_internal,
    # Create flow label for display
    flow_label = paste0(section_payer_short, " → ", section_payee_short)
  )

# Find flows with largest divergence (external outpacing internal)
top_divergent_flows = all_flow_slopes_wide %>%
  filter(!is.na(external_minus_internal)) %>%
  group_by(payer_ITL1name) %>%
  slice_max(order_by = external_minus_internal, n = 20) %>%
  arrange(payer_ITL1name, desc(external_minus_internal))

# View top divergent flows for a specific region
top_divergent_flows %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  select(flow_label, annual_pct_change_internal, annual_pct_change_external, external_minus_internal) %>%
  print(n = 20)



# Find flows where internal is outpacing external (negative divergence)
top_internal_growth = all_flow_slopes_wide %>%
  filter(!is.na(external_minus_internal)) %>%
  group_by(payer_ITL1name) %>%
  slice_min(order_by = external_minus_internal, n = 20) %>%
  arrange(payer_ITL1name, external_minus_internal)

# View top divergent flows for a specific region
top_internal_growth %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  select(flow_label, annual_pct_change_internal, annual_pct_change_external, external_minus_internal) %>%
  # arrange(desc(annual_pct_change_internal)) %>% 
  arrange(external_minus_internal) %>% 
  print(n = 20)




# Summary across all regions: which section pairs show consistent patterns?
section_pair_summary = all_flow_slopes_wide %>%
  filter(!is.na(external_minus_internal)) %>%
  group_by(sectionname_payer, sectionname_payee, section_payer_short, section_payee_short) %>%
  summarise(
    mean_divergence = mean(external_minus_internal, na.rm = TRUE),
    median_divergence = median(external_minus_internal, na.rm = TRUE),
    n_regions_external_faster = sum(external_minus_internal > 0, na.rm = TRUE),
    n_regions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    flow_label = paste0(section_payer_short, " → ", section_payee_short)
  ) %>%
  arrange(desc(mean_divergence))

# Top flows where external is consistently outpacing internal across regions
section_pair_summary %>%
  filter(n_regions >= 6) %>%  # Only flows present in at least half the regions
  slice_max(order_by = mean_divergence, n = 20) %>%
  select(flow_label, mean_divergence, median_divergence, n_regions_external_faster, n_regions)






# REGIONAL COEFFICIENT MATRICES ----

# Technical coefficients: a_ij = purchases from sector j by sector i / total output of sector i
# Since we don't have true output data, we use total sales (row sums) as a proxy
# This gives us "input shares" - what proportion of a sector's purchases come from each supplier

# Optional: filter to specific regions (or exclude regions)
# If NULL, uses all regions
# e.g. exclude Northern Ireland:
coef_regions_exclude = c("Northern Ireland")
# coef_regions_exclude = NULL

# Use most recent year for coefficient matrices
coef_year = max(i2i.yr$year)

# Aggregate to section level for the chosen year
# Keep internal vs external separate so we can compare "recipes"
# Drop a couple of sectors too
i2i_for_coefs = i2i.yr %>%
  filter(
    !is.na(sectionname_payer),
    !is.na(sectionname_payee),
    year == coef_year,
    !qg('households|extraterr',sectionname_payer),
    !qg('households|extraterr',sectionname_payee)
  ) %>%
  # Apply region exclusion filter if specified
  {if(!is.null(coef_regions_exclude)) filter(., !payer_ITL1name %in% coef_regions_exclude) else .} %>%
  mutate(
    flow_type = ifelse(payer_ITL1name == payee_ITL1name, "internal", "external")
  ) %>%
  group_by(payer_ITL1name, sectionname_payer, sectionname_payee, flow_type) %>%
  summarise(
    pounds = sum(pounds, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate total purchases by each payer sector in each region (proxy for "output")
# This is the column sum - total intermediate inputs purchased by sector i
sector_total_purchases = i2i_for_coefs %>%
  group_by(payer_ITL1name, sectionname_payer) %>%
  summarise(
    total_purchases = sum(pounds, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate coefficients: purchases from j / total purchases by i
# Do this separately for internal and external
regional_coefficients = i2i_for_coefs %>%
  left_join(sector_total_purchases, by = c("payer_ITL1name", "sectionname_payer")) %>%
  mutate(
    coefficient = pounds / total_purchases,
    section_payer_short = reduceSICnames(sectionname_payer, 'section'),
    section_payee_short = reduceSICnames(sectionname_payee, 'section')
  )

# Pivot to get internal and external coefficients side by side
coefficients_wide = regional_coefficients %>%
  select(payer_ITL1name, sectionname_payer, sectionname_payee,
         section_payer_short, section_payee_short, flow_type, coefficient) %>%
  pivot_wider(
    names_from = flow_type,
    values_from = coefficient,
    values_fill = 0
  ) %>%
  mutate(
    # Total coefficient (internal + external)
    total = internal + external,
    # Regional purchase coefficient: what share is sourced locally?
    regional_share = ifelse(total > 0, internal / total, NA),
    flow_label = paste0(section_payer_short, " → ", section_payee_short)
  )


# 1. WHICH REGIONS HAVE STRONGER INTERNAL SUPPLY CHAINS? ----

# Sum internal coefficients across all linkages for each region
regional_self_sufficiency = coefficients_wide %>%
  group_by(payer_ITL1name) %>%
  summarise(
    total_internal_coef = sum(internal, na.rm = TRUE),
    total_external_coef = sum(external, na.rm = TRUE),
    total_coef = sum(total, na.rm = TRUE),
    overall_regional_share = total_internal_coef / total_coef,
    .groups = 'drop'
  ) %>%
  arrange(desc(overall_regional_share))

regional_self_sufficiency

# Visualise regional self-sufficiency
ggplot(regional_self_sufficiency,
       aes(x = reorder(payer_ITL1name, overall_regional_share), y = overall_regional_share)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = mean(regional_self_sufficiency$overall_regional_share),
             linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(
    title = "Regional Self-Sufficiency in Inter-Industry Purchases",
    subtitle = paste0("Share of intermediate purchases sourced within region (", coef_year, ")"),
    x = "",
    y = "Internal share of total purchases",
    caption = "Red line = UK average"
  ) +
  scale_y_continuous(labels = scales::percent)

# Self-sufficiency by purchasing sector within each region
sector_self_sufficiency = coefficients_wide %>%
  group_by(payer_ITL1name, sectionname_payer, section_payer_short) %>%
  summarise(
    internal_coef = sum(internal, na.rm = TRUE),
    external_coef = sum(external, na.rm = TRUE),
    total_coef = sum(total, na.rm = TRUE),
    regional_share = internal_coef / total_coef,
    .groups = 'drop'
  )

# Heatmap of self-sufficiency by region and sector
ggplot(sector_self_sufficiency,
       aes(x = section_payer_short, y = payer_ITL1name, fill = regional_share)) +
  geom_tile() +
  # scale_fill_gradient2(
  #   low = "red", mid = "lightyellow", high = "darkgreen",
  #   midpoint = 0.5,
  #   name = "Internal\nshare",
  #   labels = scales::percent
  # ) +
  scale_fill_distiller(palette = 'Blues', direction = 1, name = "Internal\nshare", labels = scales::percent) +
  # scale_fill_distiller(palette = 'PuBuGn', direction = 1, name = "Internal\nshare", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  labs(
    title = "Self-Sufficiency by Region and Purchasing Sector",
    subtitle = "Share of each sector's purchases sourced within region",
    x = "Purchasing sector",
    y = ""
  )


# 2. COMPARING "RECIPES" ACROSS REGIONS ----

# Function to extract and compare coefficient vectors for a specific purchasing sector
compare_sector_recipes = function(purchasing_sector, coef_data = coefficients_wide) {

  # Filter to the purchasing sector and get total coefficients
  sector_coefs = coef_data %>%
    filter(sectionname_payer == purchasing_sector) %>%
    select(payer_ITL1name, sectionname_payee, section_payee_short, total) %>%
    pivot_wider(
      names_from = payer_ITL1name,
      values_from = total,
      values_fill = 0
    )

  return(sector_coefs)
}

# Example: Compare Manufacturing "recipes" across regions
manufacturing_recipes = compare_sector_recipes("Manufacturing")
manufacturing_recipes

# Calculate coefficient matrix for a single region (total coefficients)
make_coefficient_matrix = function(region_name, coef_data = coefficients_wide) {

  region_coefs = coef_data %>%
    filter(payer_ITL1name == region_name) %>%
    select(sectionname_payer, sectionname_payee, total)

  coef_matrix = region_coefs %>%
    pivot_wider(
      names_from = sectionname_payee,
      values_from = total,
      values_fill = 0
    ) %>%
    column_to_rownames('sectionname_payer') %>%
    as.matrix()

  return(coef_matrix)
}

# Create coefficient matrices for all regions
coef_matrices = map(unique(coefficients_wide$payer_ITL1name),
                    ~make_coefficient_matrix(.x, coefficients_wide)) %>%
  set_names(unique(coefficients_wide$payer_ITL1name))

# Compare two regions' recipes for a specific sector
compare_two_regions = function(sector, region1, region2, coef_data = coefficients_wide) {

  comparison = coef_data %>%
    filter(sectionname_payer == sector,
           payer_ITL1name %in% c(region1, region2)) %>%
    select(payer_ITL1name, section_payee_short, total) %>%
    pivot_wider(
      names_from = payer_ITL1name,
      values_from = total,
      values_fill = 0
    ) %>%
    mutate(
      difference = .data[[region1]] - .data[[region2]],
      ratio = ifelse(.data[[region2]] > 0, .data[[region1]] / .data[[region2]], NA)
    ) %>%
    arrange(desc(abs(difference)))

  return(comparison)
}

# Example: How does Yorkshire Manufacturing source differently from West Midlands?
compare_two_regions("Manufacturing", "Yorkshire and The Humber", "West Midlands")

# Visualise recipe comparison for a sector across all regions
plot_recipe_comparison = function(purchasing_sector, coef_data = coefficients_wide) {

  plot_data = coef_data %>%
    filter(sectionname_payer == purchasing_sector) %>%
    select(payer_ITL1name, section_payee_short, total)

  ggplot(plot_data, aes(x = section_payee_short, y = payer_ITL1name, fill = total)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkblue", name = "Coefficient") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
    labs(
      title = paste0("Input 'Recipes' for ", purchasing_sector),
      subtitle = "Technical coefficients by supplying sector and region",
      x = "Supplying sector",
      y = ""
    )
}

plot_recipe_comparison("Manufacturing")
plot_recipe_comparison("Financial and insurance activities")


# 3. WHICH LINKAGES SHOW MOST REGIONAL VARIATION IN LOCAL SOURCING? ----

# For each sector pair, calculate variation in regional_share across regions
linkage_variation = coefficients_wide %>%
  filter(!is.na(regional_share), total > 0) %>%
  group_by(sectionname_payer, sectionname_payee, section_payer_short, section_payee_short) %>%
  summarise(
    mean_regional_share = mean(regional_share, na.rm = TRUE),
    sd_regional_share = sd(regional_share, na.rm = TRUE),
    cv_regional_share = sd_regional_share / mean_regional_share,  # Coefficient of variation
    min_regional_share = min(regional_share, na.rm = TRUE),
    max_regional_share = max(regional_share, na.rm = TRUE),
    range_regional_share = max_regional_share - min_regional_share,
    n_regions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    flow_label = paste0(section_payer_short, " → ", section_payee_short)
  )

# Linkages with highest variation in local sourcing
linkage_variation %>%
  filter(n_regions >= 6) %>%
  arrange(desc(range_regional_share)) %>%
  select(flow_label, mean_regional_share, range_regional_share, min_regional_share, max_regional_share) %>%
  print(n = 20)

# Linkages that are mostly local everywhere
linkage_variation %>%
  filter(n_regions >= 6, mean_regional_share > 0.5) %>%
  arrange(desc(mean_regional_share)) %>%
  select(flow_label, mean_regional_share, sd_regional_share) %>%
  print(n = 20)

# Linkages that are mostly imported everywhere
linkage_variation %>%
  filter(n_regions >= 6, mean_regional_share < 0.2) %>%
  arrange(mean_regional_share) %>%
  select(flow_label, mean_regional_share, sd_regional_share) %>%
  print(n = 20)


















