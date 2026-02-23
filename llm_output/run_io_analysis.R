suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  source("functions/misc_functions.R")
})
theme_set(theme_light())

i2i.yr = readRDS("local/data/payments_itl2_sic2_yearly_w_sections.rds")
outdir = "llm_output/io_plots"

# ---- REGIONAL COEFFICIENT MATRICES ----
coef_regions_exclude = c("Northern Ireland")
coef_year = max(i2i.yr$year)

i2i_for_coefs = i2i.yr %>%
  filter(
    !is.na(sectionname_payer),
    !is.na(sectionname_payee),
    year == coef_year,
    !qg("households|extraterr",sectionname_payer),
    !qg("households|extraterr",sectionname_payee)
  ) %>%
  {if(!is.null(coef_regions_exclude)) filter(., !payer_ITL1name %in% coef_regions_exclude) else .} %>%
  mutate(flow_type = ifelse(payer_ITL1name == payee_ITL1name, "internal", "external")) %>%
  group_by(payer_ITL1name, sectionname_payer, sectionname_payee, flow_type) %>%
  summarise(pounds = sum(pounds, na.rm = TRUE), .groups = "drop")

sector_total_purchases = i2i_for_coefs %>%
  group_by(payer_ITL1name, sectionname_payer) %>%
  summarise(total_purchases = sum(pounds, na.rm = TRUE), .groups = "drop")

regional_coefficients = i2i_for_coefs %>%
  left_join(sector_total_purchases, by = c("payer_ITL1name", "sectionname_payer")) %>%
  mutate(
    coefficient = pounds / total_purchases,
    section_payer_short = reduceSICnames(sectionname_payer, "section"),
    section_payee_short = reduceSICnames(sectionname_payee, "section")
  )

coefficients_wide = regional_coefficients %>%
  select(payer_ITL1name, sectionname_payer, sectionname_payee,
         section_payer_short, section_payee_short, flow_type, coefficient) %>%
  pivot_wider(names_from = flow_type, values_from = coefficient, values_fill = 0) %>%
  mutate(
    total = internal + external,
    regional_share = ifelse(total > 0, internal / total, NA),
    flow_label = paste0(section_payer_short, " -> ", section_payee_short)
  )

# ---- 1. REGIONAL SELF-SUFFICIENCY ----
cat("=== 1. REGIONAL SELF-SUFFICIENCY (latest year) ===\n")
regional_self_sufficiency = coefficients_wide %>%
  group_by(payer_ITL1name) %>%
  summarise(
    total_internal_coef = sum(internal, na.rm = TRUE),
    total_external_coef = sum(external, na.rm = TRUE),
    total_coef = sum(total, na.rm = TRUE),
    overall_regional_share = total_internal_coef / total_coef,
    .groups = "drop"
  ) %>%
  arrange(desc(overall_regional_share))

print(regional_self_sufficiency, n = 20)

# ---- MULTI-YEAR SELF-SUFFICIENCY ----
cat("\n=== MULTI-YEAR SELF-SUFFICIENCY ===\n")
available_years = sort(unique(i2i.yr$year))
selected_years = available_years[c(1, ceiling(length(available_years)/2), length(available_years))]
cat("Selected years:", as.character(as.Date(selected_years, origin="1970-01-01")), "\n")

regional_self_sufficiency_multiyear = i2i.yr %>%
  filter(
    !is.na(sectionname_payer), !is.na(sectionname_payee),
    year %in% selected_years,
    !qg("households|extraterr", sectionname_payer),
    !qg("households|extraterr", sectionname_payee)
  ) %>%
  {if(!is.null(coef_regions_exclude)) filter(., !payer_ITL1name %in% coef_regions_exclude) else .} %>%
  mutate(flow_type = ifelse(payer_ITL1name == payee_ITL1name, "internal", "external")) %>%
  group_by(payer_ITL1name, year, flow_type) %>%
  summarise(pounds = sum(pounds, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = flow_type, values_from = pounds, values_fill = 0) %>%
  mutate(
    total = internal + external,
    overall_regional_share = internal / total,
    year_label = format(as.Date(year, origin="1970-01-01"), "%Y")
  )

regional_self_sufficiency_multiyear %>%
  select(payer_ITL1name, year_label, overall_regional_share) %>%
  pivot_wider(names_from = year_label, values_from = overall_regional_share) %>%
  print(n = 20, width = Inf)

# ---- SECTOR SELF-SUFFICIENCY ----
cat("\n=== SECTOR SELF-SUFFICIENCY: Y&H ===\n")
sector_self_sufficiency = coefficients_wide %>%
  group_by(payer_ITL1name, sectionname_payer, section_payer_short) %>%
  summarise(
    internal_coef = sum(internal, na.rm = TRUE),
    external_coef = sum(external, na.rm = TRUE),
    total_coef = sum(total, na.rm = TRUE),
    regional_share = internal_coef / total_coef,
    .groups = "drop"
  )

sector_self_sufficiency %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  arrange(desc(regional_share)) %>%
  select(section_payer_short, regional_share) %>%
  print(n = 25)

# ---- SECTOR LOCALITY ----
linkage_variation = coefficients_wide %>%
  filter(!is.na(regional_share), total > 0) %>%
  group_by(sectionname_payer, sectionname_payee, section_payer_short, section_payee_short) %>%
  summarise(
    mean_regional_share = mean(regional_share, na.rm = TRUE),
    sd_regional_share = sd(regional_share, na.rm = TRUE),
    n_regions = n(),
    .groups = "drop"
  ) %>%
  mutate(flow_label = paste0(section_payer_short, " -> ", section_payee_short))

cat("\n=== PAYEE LOCALITY (how locally payments received) ===\n")
linkage_variation %>%
  filter(n_regions >= 6) %>%
  group_by(sectionname_payee, section_payee_short) %>%
  summarise(mean_local_share = mean(mean_regional_share, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_local_share)) %>%
  print(n = 25)

cat("\n=== PAYER LOCALITY (how locally inputs sourced) ===\n")
linkage_variation %>%
  filter(n_regions >= 6) %>%
  group_by(sectionname_payer, section_payer_short) %>%
  summarise(mean_local_share = mean(mean_regional_share, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_local_share)) %>%
  print(n = 25)

# ---- PER-REGION LOCALITY ----
payer_locality_by_region = coefficients_wide %>%
  filter(total > 0) %>%
  group_by(payer_ITL1name, sectionname_payer, section_payer_short) %>%
  summarise(payer_locality = sum(internal, na.rm = TRUE) / sum(total, na.rm = TRUE), .groups = "drop")

payee_locality_by_region = coefficients_wide %>%
  filter(total > 0) %>%
  group_by(payer_ITL1name, sectionname_payee, section_payee_short) %>%
  summarise(payee_locality = sum(internal, na.rm = TRUE) / sum(total, na.rm = TRUE), .groups = "drop")

sector_ranges = payer_locality_by_region %>%
  group_by(section_payer_short) %>%
  summarise(payer_min = min(payer_locality), payer_max = max(payer_locality),
            payer_mean = mean(payer_locality), .groups = "drop") %>%
  rename(section = section_payer_short) %>%
  inner_join(
    payee_locality_by_region %>%
      group_by(section_payee_short) %>%
      summarise(payee_min = min(payee_locality), payee_max = max(payee_locality),
                payee_mean = mean(payee_locality), .groups = "drop") %>%
      rename(section = section_payee_short),
    by = "section"
  )

cat("\n=== SECTOR LOCALITY SUMMARY ===\n")
sector_ranges %>%
  arrange(desc(payer_mean)) %>%
  select(section, payer_mean, payee_mean) %>%
  print(n = 25)

# ---- Y&H vs MEANS ----
sector_locality_by_region = payer_locality_by_region %>%
  rename(section = section_payer_short) %>%
  inner_join(
    payee_locality_by_region %>% rename(section = section_payee_short),
    by = c("payer_ITL1name", "section")
  ) %>%
  left_join(sector_ranges %>% select(section, payer_mean, payee_mean), by = "section")

cat("\n=== Y&H SECTOR LOCALITY vs UK MEANS ===\n")
sector_locality_by_region %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  mutate(
    payer_diff = payer_locality - payer_mean,
    payee_diff = payee_locality - payee_mean
  ) %>%
  arrange(desc(payer_diff)) %>%
  select(section, payer_locality, payer_mean, payer_diff, payee_locality, payee_mean, payee_diff) %>%
  print(n = 25, width = Inf)

# ---- MOSTLY LOCAL / MOSTLY IMPORTED LINKAGES ----
cat("\n=== LINKAGES MOSTLY LOCAL EVERYWHERE (mean > 50%) ===\n")
linkage_variation %>%
  filter(n_regions >= 6, mean_regional_share > 0.5) %>%
  arrange(desc(mean_regional_share)) %>%
  select(flow_label, mean_regional_share) %>%
  head(15) %>%
  print(n = 15)

cat("\n=== LINKAGES MOSTLY IMPORTED EVERYWHERE (mean < 15%) ===\n")
linkage_variation %>%
  filter(n_regions >= 6, mean_regional_share < 0.15) %>%
  arrange(mean_regional_share) %>%
  select(flow_label, mean_regional_share) %>%
  head(15) %>%
  print(n = 15)

# ---- SAVE KEY PLOTS ----
cat("\n=== SAVING PLOTS ===\n")

# Plot 1: Regional self-sufficiency bar chart
p1 = ggplot(regional_self_sufficiency,
       aes(x = reorder(payer_ITL1name, overall_regional_share), y = overall_regional_share)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = mean(regional_self_sufficiency$overall_regional_share),
             linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(title = "Regional Self-Sufficiency in Inter-Industry Purchases",
       subtitle = paste0("Share of intermediate purchases sourced within region (", format(as.Date(coef_year, origin="1970-01-01"), "%Y"), ")"),
       x = "", y = "Internal share of total purchases") +
  scale_y_continuous(labels = scales::percent)

ggsave(file.path(outdir, "01_regional_self_sufficiency.png"), p1, width = 10, height = 6, dpi = 150)
cat("Saved 01_regional_self_sufficiency.png\n")

# Plot 2: Sector self-sufficiency heatmap
p2 = ggplot(sector_self_sufficiency,
       aes(x = section_payer_short, y = payer_ITL1name, fill = regional_share)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "Internal\nshare", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  labs(title = "Self-Sufficiency by Region and Purchasing Sector",
       x = "Purchasing sector", y = "")

ggsave(file.path(outdir, "02_sector_self_sufficiency_heatmap.png"), p2, width = 12, height = 7, dpi = 150)
cat("Saved 02_sector_self_sufficiency_heatmap.png\n")

# Plot 3: Locality scatter with crosshairs
p3 = ggplot(sector_ranges) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0.5, alpha = 0.3) +
  geom_hline(yintercept = 0.5, alpha = 0.3) +
  geom_segment(aes(x = payer_min, xend = payer_max, y = payee_mean, yend = payee_mean, colour = section),
               linewidth = 0.6, alpha = 0.7) +
  geom_segment(aes(x = payer_mean, xend = payer_mean, y = payee_min, yend = payee_max, colour = section),
               linewidth = 0.6, alpha = 0.7) +
  geom_point(aes(x = payer_mean, y = payee_mean, colour = section), size = 3) +
  ggrepel::geom_label_repel(aes(x = payer_mean, y = payee_mean, label = section, colour = section),
    size = 2.5, label.padding = unit(0.15, "lines"), fill = "white", alpha = 0.85, show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sector Locality: Sourcing vs Serving",
       subtitle = "Crosshairs show min-max range across UK regions",
       x = "Local share of inputs purchased", y = "Local share of payments received") +
  theme(legend.position = "none")

ggsave(file.path(outdir, "03_sector_locality_crosshairs.png"), p3, width = 10, height = 8, dpi = 150)
cat("Saved 03_sector_locality_crosshairs.png\n")

# Plot 4: Faceted by sector with region abbreviations
region_abbrevs = c(
  "North East" = "NE", "North West" = "NW",
  "Yorkshire and The Humber" = "Yorks", "East Midlands" = "E Mid",
  "West Midlands" = "W Mid", "East of England" = "East",
  "London" = "Lon", "South East" = "SE", "South West" = "SW",
  "Wales" = "Wal", "Scotland" = "Scot", "Northern Ireland" = "NI"
)

sector_locality_by_region_abbrev = sector_locality_by_region %>%
  mutate(region_abbrev = region_abbrevs[payer_ITL1name])

means_bbox = list(
  xmin = min(sector_ranges$payer_mean), xmax = max(sector_ranges$payer_mean),
  ymin = min(sector_ranges$payee_mean), ymax = max(sector_ranges$payee_mean)
)

p4 = ggplot(sector_locality_by_region_abbrev, aes(x = payer_locality, y = payee_locality)) +
  annotate("rect", xmin = means_bbox$xmin, xmax = means_bbox$xmax,
           ymin = means_bbox$ymin, ymax = means_bbox$ymax,
           fill = "grey90", alpha = 0.5, colour = "grey70", linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0.5, alpha = 0.2) +
  geom_hline(yintercept = 0.5, alpha = 0.2) +
  geom_point(data = sector_ranges, aes(x = payer_mean, y = payee_mean),
             size = 4, colour = "red", alpha = 0.7) +
  geom_point(size = 2, colour = "steelblue", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = region_abbrev), size = 2, max.overlaps = 15,
                           segment.colour = "grey70", segment.alpha = 0.5) +
  facet_wrap(~section, ncol = 4) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sector Locality by Region",
       subtitle = "Blue = individual regions; Red = sector mean",
       x = "Local share of inputs purchased", y = "Local share of payments received") +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))

ggsave(file.path(outdir, "04_facet_by_sector.png"), p4, width = 14, height = 16, dpi = 150)
cat("Saved 04_facet_by_sector.png\n")

# Plot 5: Faceted by region
p5 = ggplot(sector_locality_by_region_abbrev, aes(x = payer_locality, y = payee_locality)) +
  annotate("rect", xmin = means_bbox$xmin, xmax = means_bbox$xmax,
           ymin = means_bbox$ymin, ymax = means_bbox$ymax,
           fill = "grey90", alpha = 0.5, colour = "grey70", linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0.5, alpha = 0.2) +
  geom_hline(yintercept = 0.5, alpha = 0.2) +
  geom_point(size = 2, colour = "steelblue", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = section), size = 2, max.overlaps = 20,
                           segment.colour = "grey70", segment.alpha = 0.5) +
  facet_wrap(~payer_ITL1name, ncol = 4) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Sector Locality by Region",
       subtitle = "Each panel shows one region's sectors",
       x = "Local share of inputs purchased", y = "Local share of payments received") +
  theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))

ggsave(file.path(outdir, "05_facet_by_region.png"), p5, width = 14, height = 10, dpi = 150)
cat("Saved 05_facet_by_region.png\n")

# Plot 6: Arrow plot (change over time)
locality_years = c(min(i2i.yr$year), max(i2i.yr$year))
cat("\nArrow plot years:", format(as.Date(locality_years, origin="1970-01-01"), "%Y"), "\n")

i2i_for_coefs_multiyear = i2i.yr %>%
  filter(!is.na(sectionname_payer), !is.na(sectionname_payee),
         year %in% locality_years,
         !qg("households|extraterr", sectionname_payer),
         !qg("households|extraterr", sectionname_payee)) %>%
  {if(!is.null(coef_regions_exclude)) filter(., !payer_ITL1name %in% coef_regions_exclude) else .} %>%
  mutate(flow_type = ifelse(payer_ITL1name == payee_ITL1name, "internal", "external")) %>%
  group_by(payer_ITL1name, sectionname_payer, sectionname_payee, flow_type, year) %>%
  summarise(pounds = sum(pounds, na.rm = TRUE), .groups = "drop")

sector_total_purchases_multiyear = i2i_for_coefs_multiyear %>%
  group_by(payer_ITL1name, sectionname_payer, year) %>%
  summarise(total_purchases = sum(pounds, na.rm = TRUE), .groups = "drop")

coefficients_wide_multiyear = i2i_for_coefs_multiyear %>%
  left_join(sector_total_purchases_multiyear, by = c("payer_ITL1name", "sectionname_payer", "year")) %>%
  mutate(coefficient = pounds / total_purchases,
         section_payer_short = reduceSICnames(sectionname_payer, "section"),
         section_payee_short = reduceSICnames(sectionname_payee, "section")) %>%
  select(payer_ITL1name, sectionname_payer, sectionname_payee,
         section_payer_short, section_payee_short, flow_type, coefficient, year) %>%
  pivot_wider(names_from = flow_type, values_from = coefficient, values_fill = 0) %>%
  mutate(total = internal + external, regional_share = ifelse(total > 0, internal / total, NA))

payer_loc_my = coefficients_wide_multiyear %>%
  filter(total > 0) %>%
  group_by(payer_ITL1name, sectionname_payer, section_payer_short, year) %>%
  summarise(payer_locality = sum(internal) / sum(total), .groups = "drop")

payee_loc_my = coefficients_wide_multiyear %>%
  filter(total > 0) %>%
  group_by(payer_ITL1name, sectionname_payee, section_payee_short, year) %>%
  summarise(payee_locality = sum(internal) / sum(total), .groups = "drop")

sector_locality_multiyear = payer_loc_my %>%
  rename(section = section_payer_short) %>%
  inner_join(payee_loc_my %>% rename(section = section_payee_short),
             by = c("payer_ITL1name", "section", "year"))

sector_locality_arrows = sector_locality_multiyear %>%
  mutate(timepoint = ifelse(year == min(year), "start", "end")) %>%
  select(payer_ITL1name, section, timepoint, payer_locality, payee_locality) %>%
  pivot_wider(names_from = timepoint, values_from = c(payer_locality, payee_locality)) %>%
  mutate(
    payer_change = payer_locality_end - payer_locality_start,
    payee_change = payee_locality_end - payee_locality_start,
    compass = case_when(
      payer_change > 0 & payee_change > 0 ~ "NE",
      payer_change > 0 & payee_change < 0 ~ "SE",
      payer_change < 0 & payee_change > 0 ~ "NW",
      payer_change < 0 & payee_change < 0 ~ "SW",
      TRUE ~ "NC"
    )
  )

cat("\n=== Y&H LOCALITY CHANGE OVER TIME ===\n")
sector_locality_arrows %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  arrange(desc(payer_change)) %>%
  select(section, payer_locality_start, payer_locality_end, payer_change,
         payee_locality_start, payee_locality_end, payee_change, compass) %>%
  print(n = 25, width = Inf)

cat("\n=== COMPASS DIRECTION SUMMARY (all regions) ===\n")
sector_locality_arrows %>%
  count(compass) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print()

cat("\n=== COMPASS DIRECTION SUMMARY (Y&H only) ===\n")
sector_locality_arrows %>%
  filter(payer_ITL1name == "Yorkshire and The Humber") %>%
  count(compass) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print()

# Arrow plot
p6 = ggplot(sector_locality_arrows) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0.5, alpha = 0.2) +
  geom_hline(yintercept = 0.5, alpha = 0.2) +
  geom_segment(
    aes(x = payer_locality_start, y = payee_locality_start,
        xend = payer_locality_end, yend = payee_locality_end, colour = compass),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    linewidth = 0.6, alpha = 0.8
  ) +
  geom_point(aes(x = payer_locality_start, y = payee_locality_start),
             size = 1, colour = "grey40", alpha = 0.5) +
  ggrepel::geom_text_repel(
    aes(x = payer_locality_end, y = payee_locality_end, label = section, colour = compass),
    size = 2, max.overlaps = 15, show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("NE" = "#2ca02c", "SE" = "#ff7f0e", "NW" = "#1f77b4", "SW" = "#d62728", "NC" = "grey50"),
    labels = c("NE" = "More local (both)", "SE" = "More sourcing, less payments",
               "NW" = "Less sourcing, more payments", "SW" = "Less local (both)", "NC" = "No change"),
    name = "Direction"
  ) +
  facet_wrap(~payer_ITL1name, ncol = 4) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0("Change in Sector Locality: ",
       format(as.Date(min(locality_years), origin="1970-01-01"), "%Y"), " to ",
       format(as.Date(max(locality_years), origin="1970-01-01"), "%Y")),
       x = "Local share of inputs purchased", y = "Local share of payments received") +
  theme(legend.position = "bottom", strip.text = element_text(size = 7), axis.text = element_text(size = 6))

ggsave(file.path(outdir, "06_arrow_plot_change.png"), p6, width = 14, height = 10, dpi = 150)
cat("Saved 06_arrow_plot_change.png\n")

# Plot 7: Local vs tradeable linkages heatmap
p7 = ggplot(linkage_variation %>% filter(n_regions >= 6),
       aes(x = section_payee_short, y = section_payer_short, fill = mean_regional_share)) +
  geom_tile() +
  scale_fill_gradient2(low = "coral", mid = "white", high = "steelblue",
                       midpoint = 0.5, name = "Local\nshare", labels = scales::percent, limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), axis.text.y = element_text(size = 7)) +
  labs(title = "Local vs Tradeable Linkages",
       subtitle = "Mean share of payments sourced within region",
       x = "Payee sector (receiving)", y = "Payer sector (spending)")

ggsave(file.path(outdir, "07_local_vs_tradeable_heatmap.png"), p7, width = 12, height = 10, dpi = 150)
cat("Saved 07_local_vs_tradeable_heatmap.png\n")

cat("\n=== DONE ===\n")
