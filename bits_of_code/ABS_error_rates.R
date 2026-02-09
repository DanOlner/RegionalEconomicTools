# Playing around with standard errors for the ABS component of regional GVA
# How much are places actually separarable for sectors?
library(tidyverse)
library(zoo)

source('functions/misc_functions.R')
source('functions/data_process_functions.R')

options(scipen = 999)

# DAN CODE----

## Get data----

# From here for regional, including quality measures
# https://www.ons.gov.uk/businessindustryandtrade/business/businessservices/bulletins/nonfinancialbusinesseconomyukandregionalannualbusinesssurvey/2023results?utm_source=chatgpt.com

# Just 2 digit for regional level

# Actual values first
# https://www.ons.gov.uk/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveyregionalresultssectionsas
url1 <- 'https://www.ons.gov.uk/file?uri=/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveyregionalresultssectionsas/current/absregionalsectionas2023.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

# Each region is in its own sheet. We'll need to read each separately to get all the data for that
# Get sheet names first from the contents
# All ITL1 subregions...
# https://www.reddit.com/r/rstats/comments/nr3xhj/removing_rest_of_string_after_a_certain_character/
sheetnames = readxl::read_excel(path = p1f,range = "Contents!A5:A16") %>%
  pull(`Table 1: North East`) %>%
  str_split_i(., ':', i = 2) %>% #https://stackoverflow.com/a/74727577
  trimws()

# Add North East back in
sheetnames = c(sheetnames,'North East')
  
# getregion = function(region){
#     region = readxl::read_excel(path = p1f,range = paste0(region,"!A8:I1575"))
# }

allregions = map(sheetnames, ~ readxl::read_excel(path = p1f,range = paste0(.,"!A7:I1575"))) %>% 
  bind_rows()

#More process-able names with no spaces
names(allregions) <- gsub(x = names(allregions), pattern = ' ', replacement = '_')

# Replace any non-numeric with NA and make numeric
# Those would be e.g. disclosure issues
# NAs introduced is exactly what we want
allregions = allregions %>% 
  mutate(
    across(6:9, as.numeric)
  )

# Better names plz
names(allregions)[1] = 'SIC'
names(allregions)[6:9] = c('turnover','GVA','goods_materials_services_purchased','employment_costs')


# Then get the error rates from the quality measures excel sheet
# Different structure, single sheet this time
# From https://www.ons.gov.uk/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveyregionalresultsqualitymeasures
url1 <- 'https://www.ons.gov.uk/file?uri=/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveyregionalresultsqualitymeasures/2023/absregionalqualitymeasures2023.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

# Some of this won't be numeric cos of other codes, will need to fix
qual = readxl::read_excel(path = p1f,range = ("Section Division by Region!A7:M16471"))

names(qual) <- gsub(x = names(qual), pattern = ' ', replacement = '_')

# Replace any non-numeric with NA and make numeric
# Those would be e.g. disclosure issues
# NAs introduced is exactly what we want
qual = qual %>% 
  mutate(
    across(6:13, as.numeric)
  )

# naaaaaames
names(qual)[1] = 'SIC'
names(qual)[6:13] = c(
  'turnover_SE',
  'GVA_SE',
  'goods_materials_services_purchased_SE',
  'employment_costs_SE',
  'turnover_CoV',
  'GVA_CoV',
  'goods_materials_services_purchased_CoV',
  'employment_costs_CoV'
  )



## Find error rates across regions and sectors----

# What sectors have we got? Let's just keep 2 digit
# Use this to keep 2 digit without too much faff
siclookup = read_csv('data/SIClookup.csv')

allregions = allregions %>% 
  mutate(SIC = str_sub(SIC,1,2)) %>% 
  filter(SIC %in% unique(siclookup$SIC_2DIGIT_CODE))

qual = qual %>% 
  mutate(SIC = str_sub(SIC,1,2)) %>% 
  filter(SIC %in% unique(siclookup$SIC_2DIGIT_CODE))

# Tick
unique(allregions$SIC)
unique(qual$SIC)


# Stick to finding GVA error rates for the mo
# Add gva in...
# NOTE: ONLY HAVE QUAL FROM 2012 VS 2008 FOR THE NOMINAL DATA
abs_gva = allregions %>% 
  select(-c(goods_materials_services_purchased,turnover,employment_costs)) %>% 
  filter(Year >= 2012) %>% 
  left_join(
    qual %>% select(-contains(c('turnover','goods_','employment','_CoV','Description','Country_and_Region'))),
    by = c('SIC','Country_Code','Year')
  ) %>% 
  mutate(
    gva_min95 = GVA - (GVA_SE * 1.96),
    gva_max95 = GVA + (GVA_SE * 1.96)
  )



# Add in coeff of variation to compare...
# And 95% CI equivalents... hmm nope
abs_gva = abs_gva %>% 
  mutate(
    CoV100 = (GVA_SE / GVA) * 100
    # CoV100_min95 = (GVA_SE / gva_min95) * 100,
    # CoV100_max95 = (GVA_SE / gva_max95) * 100,
  )

# Save for use elsewhere
write_csv(abs_gva,'data/abs_gva_se_combo.csv')


# Quite a lot of missing values - but let's see for a specific year
abs_gva2023 = abs_gva %>% 
  filter(Year == max(Year)) 

# Check sector diffs across regions
dodgewidth = 1

# ggplot(abs_gva2023, aes(y = Description, x = GVA, colour = Country_and_Region)) +
#   geom_point(position = position_dodge()) +
#   geom_errorbar(aes(xmin = gva_min95, xmax = gva_max95), position = position_dodge())

# Try one sector at a time!
ggplot(
  abs_gva2023 %>% filter(qg('fabricated',Description)),
  aes(y = fct_reorder(Country_and_Region,GVA), x = GVA)
  ) +
  geom_point() +
  geom_errorbar(aes(xmin = gva_min95, xmax = gva_max95), width = 0.3)


# And how about how it's changed over time in a few places?
setwidth = 1

ggplot(
  abs_gva %>% filter(qg('construction of buildings',Description)),
  # abs_gva %>% filter(qg('fabricated',Description)),
  aes(y = factor(Year), x = GVA)
) +
  geom_point() +
  geom_errorbar(aes(xmin = gva_min95, xmax = gva_max95), width = 0.3) +
  scale_color_brewer(palette = 'Paired') +
  facet_wrap(~Country_and_Region, ncol=1)
  
# ggplot(
#   abs_gva %>% filter(qg('fabricated',Description)),
#   aes(y = fct_reorder(Country_and_Region,GVA), x = GVA, colour = factor(Year))
# ) +
#   geom_point(position = position_dodge(width = setwidth)) +
#   geom_errorbar(aes(xmin = gva_min95, xmax = gva_max95), width = 0.3, position = position_dodge(width = setwidth)) +
#   scale_color_brewer(palette = 'Paired')



# We want proportions really.
# But that's tricky with error rates
# Partial prob the way forward - 
# If others at 'expected' value, how much can x sector vary?

# Or just use coeffs of variation, obv. 

# Oh also - we can't get sums because a lot of values are missing.
# Do we have totals in the orig sheet?
# Yes, in the first bunch of rows
# Except...

# Let's look at how much different sectors vary, percent wise, for one place
# Drop sectors we have no values for

# Keep only top value sectors to viz
sectorstokeep = abs_gva2023 %>% filter(
  qg('yorkshire',Country_and_Region),
  !is.na(GVA)
) %>% 
  arrange(-GVA) %>%
  slice(1:16) %>% 
  pull(Description)
  

ggplot(
  abs_gva2023 %>% filter(
    qg('yorkshire',Country_and_Region),
    !is.na(GVA),
    Description %in% sectorstokeep
    ),
  aes(y = fct_reorder(Description,GVA), x = GVA)
) +
  geom_point() +
  geom_errorbar(aes(xmin = gva_min95, xmax = gva_max95), width = 0.3) +
  ylab('')



# Version using COV100 95% bounds, for comparison
# Not quite sure this makes sense!
# ggplot(
#   abs_gva2023 %>% filter(
#     qg('yorkshire',Country_and_Region),
#     !is.na(GVA),
#     Description %in% sectorstokeep
#   ),
#   aes(y = fct_reorder(Description,CoV100), x = CoV100)
# ) +
#   geom_point() +
#   geom_errorbar(aes(xmin = CoV100_min95, xmax = CoV100_max95), width = 0.3) +
#   ylab('')




# OK. What's the 2023 GVA from the actual region by industry counts?
# Slightly different sector lists but some overlap
# Might be better to check against sections, no?



## Process data for applying those error rates to ITL1 CP and CV values----

# Outputted these to CSV in yorkshire_n_humber_sectors_dataprep.R
itl1.cp = read_csv('data/regionalGVA/regionalGVA_currentprices_ITL1_SIC_2DIGIT_WIDE_2023.csv')
itl1.cv = read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL1_SIC_2DIGIT_WIDE_2023.csv')

# Check sector match. Will have to make numeric, not matching digits
# Quick other checks
abs_gva %>% select(SIC,Description) %>% distinct() %>% View
itl1.cp %>% select(SIC07_code,SIC07_description) %>% distinct() %>% View

table(as.numeric(abs_gva$SIC))

# Of course we have the differing categorisations in the GVA data
# But also a function for that...

# Those will be a the same SICs for both CV and CP
itl1.cp.longsics = make.GVA.SICs.long(itl1.cp)

# Readload abs gva here and sum by those bespoke categories
# We just want a coefficient of variation we can apply to the new numbers...
abs_gva = read_csv('data/abs_gva_se_combo.csv') %>% 
  rename(Region_name = Country_and_Region) %>% 
  mutate(
    SIC07_code_numeric = as.numeric(SIC)#To match longenated GVA SICs
    ) %>% 
  mutate(
    Region_name = ifelse(Region_name == 'East of England', 'East', Region_name)#To match other data
  )

abs_gva.shorterSIClist = abs_gva %>% 
  left_join(
    itl1.cp.longsics %>% select(SIC07_description_fromGVAdata = SIC07_description,SIC07_code_fromGVAdata,SIC07_code_numeric),
    by = 'SIC07_code_numeric'
  )

# Keep and use only the CoV
abs_gva.shorterSIClist = abs_gva.shorterSIClist %>% 
  group_by(Region_name,Year,SIC07_code_fromGVAdata) %>% 
  summarise(
    CoV100 = weighted.mean(CoV100,GVA)
    # across(GVA:gva_max95, sum, .names = "{col}_sum")
    # across(GVA:gva_max95, ~sum(., na.rm = T), .names = "{col}_sum")
  ) %>% ungroup()

# The NAs there we should probably keep - incidates missing values, so sector totals won't be correct
# Merge names from reg GVA data back in
# abs_gva.shorterSIClist = abs_gva.shorterSIClist %>% 
#   left_join(
#     itl1.cp %>% select(SIC07_code_fromGVAdata = SIC07_code,SIC07_description) %>% distinct(),
#     by = 'SIC07_code_fromGVAdata'
#   )

# We should then be able to join back to the itl1.cp and cv data
# Let's just check ITL1 name matches... some fixes, done
table(unique(itl1.cp$Region_name) %in% abs_gva.shorterSIClist$Region_name)
unique(itl1.cp$Region_name)[!unique(itl1.cp$Region_name) %in% abs_gva.shorterSIClist$Region_name]



# Inner join to keep only matches, especially on year
itl1.cp.linked = itl1.cp %>% 
  inner_join(
    abs_gva.shorterSIClist,
    by = c('Region_name','year' = 'Year','SIC07_code' = 'SIC07_code_fromGVAdata')
  ) %>% 
  mutate(
    SE = value * (CoV100/100),#A little unnecessary there!
    gva_min95 = value - (SE * 1.96),
    gva_max95 = value + (SE * 1.96)
  )

# Save...
write_csv(itl1.cp.linked,'data/itl1_cp_withestimatederrorratefromABS.csv')

itl1.cv.linked = itl1.cv %>% 
  inner_join(
    abs_gva.shorterSIClist,
    by = c('Region_name','year' = 'Year','SIC07_code' = 'SIC07_code_fromGVAdata')
  ) %>% 
  mutate(
    SE = value * (CoV100/100),
    gva_min95 = value - (SE * 1.96),
    gva_max95 = value + (SE * 1.96)
  )

# Save...
write_csv(itl1.cv.linked,'data/itl1_cv_withestimatederrorratefromABS.csv')







# CLAUDE CODE SECTION:----

## Look at error rates----

# Start with growth in some key sectors, shall we?
# Let's pick on fabricate metal again

# Load the linked data with error rates
itl1.cv.linked = read_csv('data/itl1_cv_withestimatederrorratefromABS.csv')

# Function to plot sector GVA with error bars across regions over time
plot_sector_gva_with_errors <- function(data, sector_pattern, title_suffix = "") {

  # Filter to sector using pattern matching (case insensitive)
  sector_data <- data %>%
    filter(grepl(sector_pattern, SIC07_description, ignore.case = TRUE))

  if(nrow(sector_data) == 0) {
    stop(paste("No data found for sector pattern:", sector_pattern))
  }

  # Get the actual sector name for the title
  sector_name <- unique(sector_data$SIC07_description)[1]

  # Create the plot - faceted by region, time on x-axis
  p <- ggplot(sector_data, aes(x = year, y = value)) +
    geom_ribbon(aes(ymin = gva_min95, ymax = gva_max95),
                alpha = 0.3, fill = "steelblue") +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_point(colour = "steelblue", size = 1.5) +
    facet_wrap(~Region_name, scales = "free_y", ncol = 3) +
    labs(
      title = paste0("GVA with 95% CI: ", sector_name),
      subtitle = "Shaded area shows uncertainty from ABS coefficient of variation",
      x = "Year",
      y = "GVA (£m, chained volume)",
      caption = "Source: ONS Regional GVA with error rates from Annual Business Survey"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )

  return(p)
}

# Plot fabricated metal products
plot_sector_gva_with_errors(itl1.cv.linked, "fabricated metal")
plot_sector_gva_with_errors(itl1.cv.linked, "construction of buildings")
plot_sector_gva_with_errors(itl1.cv.linked, "telecom")
plot_sector_gva_with_errors(itl1.cv.linked, "computer programming")


## Year-on-year growth rates with bounds----

# Calculate YoY growth for central value and bounds
# Option (a): growth of each series separately
# Option (b) would be bounds on growth rate itself (more pessimistic)
itl1.cv.growth <- itl1.cv.linked %>%
  arrange(Region_name, SIC07_code, year) %>%
  group_by(Region_name, SIC07_code, SIC07_description) %>%
  mutate(
    # Central value growth
    growth = (value / lag(value)) - 1,
    # Growth of the bounds themselves
    growth_min95 = (gva_min95 / lag(gva_min95)) - 1,
    growth_max95 = (gva_max95 / lag(gva_max95)) - 1,
    # Also calculate "bounds on growth" (option b) - more conservative
    # Worst case low: this year's min vs last year's max
    # Worst case high: this year's max vs last year's min
    growth_bound_low = (gva_min95 / lag(gva_max95)) - 1,
    growth_bound_high = (gva_max95 / lag(gva_min95)) - 1,
    # Boolean: does CI exclude zero? (i.e. growth is distinguishable from no change)
    # Less conservative: based on growth of bounds
    excludes_zero = (growth_min95 > 0 & growth_max95 > 0) | (growth_min95 < 0 & growth_max95 < 0),
    # Conservative: based on bounds on growth rate
    excludes_zero_conservative = (growth_bound_low > 0) | (growth_bound_high < 0)
  ) %>%
  ungroup()


# Function to plot YoY growth rates with uncertainty bounds across regions
plot_sector_growth_with_bounds <- function(data, sector_pattern, use_conservative_bounds = TRUE) {

  # Filter to sector

  sector_data <- data %>%
    filter(grepl(sector_pattern, SIC07_description, ignore.case = TRUE)) %>%
    filter(!is.na(growth))  # Remove first year (no growth calc possible)

  if(nrow(sector_data) == 0) {
    stop(paste("No data found for sector pattern:", sector_pattern))
  }

  sector_name <- unique(sector_data$SIC07_description)[1]

  # Choose which bounds to use
  if(use_conservative_bounds) {
    sector_data <- sector_data %>%
      mutate(ymin = growth_bound_low, ymax = growth_bound_high)
    bounds_label <- "Conservative bounds (min/max vs max/min)"
  } else {
    sector_data <- sector_data %>%
      mutate(ymin = growth_min95, ymax = growth_max95)
    bounds_label <- "Growth of 95% CI bounds"
  }

  # Plot with facets by region
  p <- ggplot(sector_data, aes(x = year, y = growth)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = ymin, ymax = ymax),
                alpha = 0.3, fill = "steelblue") +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_point(aes(colour = ymin > 0 | ymax < 0), size = 2.5) +
    scale_colour_manual(
      values = c("FALSE" = "steelblue", "TRUE" = "green"),
      labels = c("FALSE" = "Includes zero", "TRUE" = "Excludes zero"),
      name = "Growth CI"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~Region_name, scales = "free_y", ncol = 3) +
    labs(
      title = paste0("Year-on-year GVA growth: ", sector_name),
      subtitle = bounds_label,
      x = "Year",
      y = "Growth rate",
      caption = "Green points: 95% CI excludes zero (distinguishable growth/decline)"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom"
    )

  return(p)
}

# Plot YoY growth for fabricated metal products
plot_sector_growth_with_bounds(itl1.cv.growth, "fabricated metal")
plot_sector_growth_with_bounds(itl1.cv.growth, "construction of buildings")
plot_sector_growth_with_bounds(itl1.cv.growth, "computer programming")

# Compare with less conservative bounds
# plot_sector_growth_with_bounds(itl1.cv.growth, "fabricated metal", use_conservative_bounds = FALSE)


# DAN CODING: checks on some of the data above----

# Save West Midlands computer prog as example
write_csv(
  itl1.cv.growth %>% 
    filter(
      qg('west mid', Region_name),
      qg('computer prog', SIC07_description)
      ),
  'claude/wm_computerprog_itl1cv.csv')

# For the 'excludes zero' in the year on year growth trends
# Which sectors manage that the most over the years?
itl1.cv.growth %>%
  filter(!is.na())


# Let's also find the slopes so we can look at fastest growing/shrinkaging sectors overall
cv_slopes = get_slope_and_se_safely(
  itl1.cv.linked,
  Region_name,SIC07_description, 
  y = log(value), 
  x = year
)

# Find sector slope averages...
meansectorslopes = cv_slopes %>% 
  group_by(SIC07_description) %>% 
  summarise(meanslope = mean(slope)) %>% 
  arrange(meanslope)


# BACK TO CLAUDE----  

## Pairwise year comparison heatmaps----

# Function to create pairwise year CI overlap matrix for a sector across regions
# Shows which years are distinguishably different from which other years
plot_pairwise_year_heatmap <- function(data, sector_pattern, region_pattern = NULL) {


  # Filter to sector
  sector_data <- data %>%
    filter(grepl(sector_pattern, SIC07_description, ignore.case = TRUE))

  if(!is.null(region_pattern)) {
    sector_data <- sector_data %>%
      filter(grepl(region_pattern, Region_name, ignore.case = TRUE))
  }

  if(nrow(sector_data) == 0) {
    stop(paste("No data found for sector pattern:", sector_pattern))
  }

  sector_name <- unique(sector_data$SIC07_description)[1]

  # For each region, create pairwise year comparisons
  # Two years are "distinguishable" if their 95% CIs don't overlap
  pairwise_results <- sector_data %>%
    select(Region_name, year, value, gva_min95, gva_max95) %>%
    # Self-join to get all year pairs
    inner_join(
      sector_data %>% select(Region_name, year2 = year, value2 = value,
                             gva_min95_2 = gva_min95, gva_max95_2 = gva_max95),
      by = "Region_name",
      relationship = "many-to-many"
    ) %>%
    # Check if CIs overlap: overlap if max of mins < min of maxs
    mutate(
      ci_overlap = pmax(gva_min95, gva_min95_2) < pmin(gva_max95, gva_max95_2),
      distinguishable = !ci_overlap,
      # Also calculate the direction of difference
      direction = case_when(
        !distinguishable ~ "Not distinguishable",
        value > value2 ~ "Year 1 higher",
        value < value2 ~ "Year 1 lower",
        TRUE ~ "Equal"
      )
    )

  # Create heatmap - faceted by region
  p <- ggplot(pairwise_results, aes(x = factor(year), y = factor(year2), fill = distinguishable)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_manual(
      values = c("FALSE" = "grey85", "TRUE" = "steelblue"),
      labels = c("FALSE" = "CIs overlap", "TRUE" = "CIs don't overlap"),
      name = ""
    ) +
    facet_wrap(~Region_name, ncol = 3) +
    labs(
      title = paste0("Pairwise year distinguishability: ", sector_name),
      subtitle = "Blue = 95% confidence intervals do not overlap (years are distinguishable)",
      x = "Year",
      y = "Year",
      caption = "Based on 95% CIs from ABS coefficient of variation"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      panel.grid = element_blank()
    ) +
    coord_fixed()

  return(p)
}

# Plot pairwise heatmaps
plot_pairwise_year_heatmap(itl1.cv.linked, "computer programming")
plot_pairwise_year_heatmap(itl1.cv.linked, "fabricated metal")

# Single region version
plot_pairwise_year_heatmap(itl1.cv.linked, "computer programming", "west midlands")


# Version with direction: shows which year is higher when CIs don't overlap
plot_pairwise_year_heatmap_direction <- function(data, sector_pattern, region_pattern = NULL) {

  # Filter to sector
  sector_data <- data %>%
    filter(grepl(sector_pattern, SIC07_description, ignore.case = TRUE))

  if(!is.null(region_pattern)) {
    sector_data <- sector_data %>%
      filter(grepl(region_pattern, Region_name, ignore.case = TRUE))
  }

  if(nrow(sector_data) == 0) {
    stop(paste("No data found for sector pattern:", sector_pattern))
  }

  sector_name <- unique(sector_data$SIC07_description)[1]

  # For each region, create pairwise year comparisons
  pairwise_results <- sector_data %>%
    select(Region_name, year, value, gva_min95, gva_max95) %>%
    inner_join(
      sector_data %>% select(Region_name, year2 = year, value2 = value,
                             gva_min95_2 = gva_min95, gva_max95_2 = gva_max95),
      by = "Region_name",
      relationship = "many-to-many"
    ) %>%
    mutate(
      # Check if either year has missing CI data
      missing_data = is.na(gva_min95) | is.na(gva_max95) | is.na(gva_min95_2) | is.na(gva_max95_2),
      ci_overlap = pmax(gva_min95, gva_min95_2) < pmin(gva_max95, gva_max95_2),
      # Four-way comparison: no data, overlap, column year higher, column year lower
      # Note: x-axis (columns) = year, y-axis (rows) = year2
      # value is for 'year' (column), value2 is for 'year2' (row)
      comparison = case_when(
        missing_data ~ "No CI data",
        ci_overlap ~ "CIs overlap",
        value > value2 ~ "Column year higher",
        value < value2 ~ "Column year lower",
        TRUE ~ "CIs overlap"  # Equal case (unlikely)
      )
    )

  # Create heatmap with four colours
  p <- ggplot(pairwise_results, aes(x = factor(year), y = factor(year2), fill = comparison)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    # Add crosses for missing data cells
    geom_text(data = pairwise_results %>% filter(comparison == "No CI data"),
              aes(label = "×"), colour = "grey50", size = 3) +
    scale_fill_manual(
      values = c("No CI data" = "grey95",
                 "CIs overlap" = "grey75",
                 "Column year higher" = "steelblue",
                 "Column year lower" = "coral"),
      name = ""
    ) +
    facet_wrap(~Region_name, ncol = 3) +
    labs(
      title = paste0("Pairwise year comparison: ", sector_name),
      subtitle = "Blue = column > row, Coral = column < row, Grey = indistinguishable, × = no CI data",
      x = "Year (column)",
      y = "Year (row)",
      caption = "Colour shows direction when 95% CIs don't overlap"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      panel.grid = element_blank()
    ) +
    coord_fixed()

  return(p)
}

# Plot with direction
plot_pairwise_year_heatmap_direction(itl1.cv.linked, "computer programming")
plot_pairwise_year_heatmap_direction(itl1.cv.linked, "fabricated metal")
plot_pairwise_year_heatmap_direction(itl1.cv.linked, "land transport")#Identified with OLS slopes as shrinking on av

# Single region
plot_pairwise_year_heatmap_direction(itl1.cv.linked, "computer programming", "west midlands")




# DAN CODE----

## How much does the introduction of error rates change LQs at ITL1 level?----
itl1.cp.linked = read_csv('data/itl1_cp_withestimatederrorratefromABS.csv')

# Repeat LQ-finding code for central estimate and upper/lower bounds then re-combine
# Let's use the average of the most recent three years
itl1.cp.smoothed = itl1.cp.linked %>% 
  group_by(Region_name,SIC07_description) %>%
  mutate(
    across(c(value,gva_min95,gva_max95), ~rollapply(.,3,mean,align='center',fill=NA), .names = "{col}_3yr_av")
  ) %>%
  ungroup() 
  # filter(!is.na(value_3yr_av)) #Keep only smoothed data years


# Find LQs for the central estimate and 95% bounds
itl1.cp.centralestLQ = itl1.cp.smoothed %>% 
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>%
  bind_rows() 

itl1.cp.min = itl1.cp.smoothed %>% 
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = gva_min95) %>%
  bind_rows() 

itl1.cp.max = itl1.cp.smoothed %>% 
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = gva_max95) %>%
  bind_rows() 


# No point trying to keep all the 3 different sets of values here...
itl1.cp.3yrLQs = itl1.cp.centralestLQ %>%
  select(ITL_code,Region_name,SIC07_description,year,LQ_centralestimate = LQ) %>% 
  left_join(
    itl1.cp.min %>% select(ITL_code,SIC07_description,year,LQ_min95 = LQ),
    by = c('ITL_code','SIC07_description','year')
  ) %>% 
  left_join(
    itl1.cp.max %>% select(ITL_code,SIC07_description,year,LQ_max95 = LQ),
    by = c('ITL_code','SIC07_description','year')
  )
  

# Let's look at some sectors/places...
ggplot(
  itl1.cp.3yrLQs %>% filter(qg('fabricated',SIC07_description), year == max(year)),
  aes(y = fct_reorder(Region_name,LQ_centralestimate), x = LQ_centralestimate)
) +
  geom_point() +
  geom_errorbar(aes(xmin = LQ_min95, xmax = LQ_max95), width = 0.3)



# No, this is wrong. For LQs, it's tricky: 
# I was trying to find LQs separately for central, min and max but I realise that actually doesn't make sense (having seen the batty CIs) - they won't sum correctly, will they? Think this through a bit.
# - The central GVA estimate gets summed across all places, and against other sector sizes too both regionally and nationally. So the variation in the sizes of those is going to wamp out totals isn't it? Though I'm struggling a bit to visualise how.
# - Possibly I should be **holding all other values constant**, not using mins and maxes for everything

# Or more likely, using the CIs values to simulate what the values could be. That's probably a better idea.
# Let's give Claude a go at that...


# BACK TO CLAUDE----

## Monte Carlo simulation of LQs with uncertainty ----

# APPROACH:
# The naive method (computing LQs separately on central, min95, max95 values) produces
# nonsensical results because LQs are ratios of sums — when you set ALL sectors to their
# min or max simultaneously, the denominators (regional total, national total) shift in
# unrealistic ways that distort the proportions.
#
# Instead, we use Monte Carlo simulation:
# 1. For each iteration, draw a plausible GVA value for every sector/region/year combo
#    from N(value, SE), using a log-normal distribution to avoid negative draws.
# 2. Where SE is missing (NA), hold the value fixed at the central estimate.
# 3. Run the existing add_location_quotient_and_proportions function on each simulated dataset.
# 4. Collect the LQ from each iteration and summarise across iterations
#    (median, 2.5th/97.5th percentiles) to get simulated CIs on the LQ.
#
# This correctly propagates uncertainty through the sums that LQs depend on:
# in any single draw, some sectors are above their mean, some below, producing
# realistic variation in totals rather than the extreme all-high or all-low scenario.
#
# Log-normal note: we parameterise so that the mean of the log-normal equals the
# observed GVA value and the SD on the log scale corresponds to the observed SE.
# This prevents negative draws which are an issue for small sectors with large SEs.

# Reload data (in case running from here)
itl1.cp.linked = read_csv('data/itl1_cp_withestimatederrorratefromABS.csv')

set.seed(42)
n_sims <- 500

# Pre-compute log-normal parameters for each row
# For log-normal: if we want mean = value and sd = SE,
# then on the log scale: sigma^2 = log(1 + (SE/value)^2), mu = log(value) - sigma^2/2
itl1.cp.sim_params <- itl1.cp.linked %>%
  mutate(
    has_se = !is.na(SE) & SE > 0 & value > 0,
    # Log-normal parameters (only meaningful where has_se is TRUE)
    lnorm_sigma2 = ifelse(has_se, log(1 + (SE / value)^2), NA),
    lnorm_sigma = ifelse(has_se, sqrt(lnorm_sigma2), NA),
    lnorm_mu = ifelse(has_se, log(value) - lnorm_sigma2 / 2, NA)
  )

# Run simulations — for each iteration, draw GVA values then compute LQs per year
# Store results in a list for efficiency
sim_LQs <- vector("list", n_sims)

for(i in 1:n_sims) {

  if(i %% 50 == 0) cat("Simulation", i, "of", n_sims, "\n")

  # Draw simulated GVA values
  sim_data <- itl1.cp.sim_params %>%
    mutate(
      sim_value = ifelse(
        has_se,
        rlnorm(n(), meanlog = lnorm_mu, sdlog = lnorm_sigma),
        value  # No SE available: hold at central estimate
      )
    )

  # Compute LQs for each year using the existing function
  sim_LQ_result <- sim_data %>%
    group_split(year) %>%
    map(add_location_quotient_and_proportions,
        regionvar = Region_name,
        lq_var = SIC07_description,
        valuevar = sim_value) %>%
    bind_rows()

  # Store just the identifying columns and the LQ
  sim_LQs[[i]] <- sim_LQ_result %>%
    select(ITL_code, Region_name, SIC07_description, year, LQ) %>%
    mutate(sim_id = i)
}

# Combine all simulations
all_sim_LQs <- bind_rows(sim_LQs)

# Summarise: median and 95% interval across simulations
LQ_sim_summary <- all_sim_LQs %>%
  group_by(ITL_code, Region_name, SIC07_description, year) %>%
  summarise(
    LQ_median = median(LQ, na.rm = TRUE),
    LQ_p025 = quantile(LQ, 0.025, na.rm = TRUE),
    LQ_p975 = quantile(LQ, 0.975, na.rm = TRUE),
    LQ_sd = sd(LQ, na.rm = TRUE),
    .groups = 'drop'
  )

# Also compute the central-estimate LQs for comparison
LQ_central <- itl1.cp.linked %>%
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>%
  bind_rows() %>%
  select(ITL_code, Region_name, SIC07_description, year, LQ_central = LQ)

# Merge central and simulated
LQ_with_sim_CIs <- LQ_central %>%
  left_join(LQ_sim_summary, by = c('ITL_code', 'Region_name', 'SIC07_description', 'year'))


## Visualise simulated LQ uncertainty ----

# Pick a year and sector to inspect
plot_simulated_LQ <- function(data, sector_pattern, plot_year = NULL) {

  sector_data <- data %>%
    filter(grepl(sector_pattern, SIC07_description, ignore.case = TRUE))

  if(is.null(plot_year)) plot_year <- max(sector_data$year)

  sector_data <- sector_data %>% filter(year == plot_year)
  sector_name <- unique(sector_data$SIC07_description)[1]

  ggplot(sector_data, aes(y = fct_reorder(Region_name, LQ_central), x = LQ_central)) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = LQ_p025, xmax = LQ_p975), height = 0.3) +
    geom_vline(xintercept = 1, linetype = 'dashed', colour = 'blue', alpha = 0.5) +
    labs(
      title = paste0("Location Quotient with simulated 95% CI: ", sector_name),
      subtitle = paste0("Year: ", plot_year, " | ", n_sims, " Monte Carlo draws using log-normal on GVA"),
      x = "Location Quotient",
      y = "",
      caption = "Error bars: 2.5th-97.5th percentile from simulation. Dashed line = LQ of 1 (national average)."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40")
    )
}

# Example plots
plot_simulated_LQ(LQ_with_sim_CIs, "fabricated metal")
plot_simulated_LQ(LQ_with_sim_CIs, "computer programming")
plot_simulated_LQ(LQ_with_sim_CIs, "construction of buildings")
plot_simulated_LQ(LQ_with_sim_CIs, "pharmaceutical")


# Dancode: Save all sectors...
map(unique(itl1.cp$SIC07_description), 
    ~{
      ggsave(
        filename = paste0('local/outputs/LQ_errorbars_SIC2s/',gsub('[[:punct:]]| ','',.),'.png'),
        plot = plot_simulated_LQ(LQ_with_sim_CIs, .),width = 10, height = 10
    })





# Time series version: LQ over time for a single region/sector with simulated CIs
plot_simulated_LQ_timeseries <- function(data, sector_pattern, region_pattern) {

  plot_data <- data %>%
    filter(
      grepl(sector_pattern, SIC07_description, ignore.case = TRUE),
      grepl(region_pattern, Region_name, ignore.case = TRUE)
    )

  sector_name <- unique(plot_data$SIC07_description)[1]
  region_name <- unique(plot_data$Region_name)[1]

  ggplot(plot_data, aes(x = year, y = LQ_central)) +
    geom_ribbon(aes(ymin = LQ_p025, ymax = LQ_p975), alpha = 0.3, fill = "steelblue") +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_point(colour = "steelblue", size = 1.5) +
    geom_hline(yintercept = 1, linetype = 'dashed', colour = 'blue', alpha = 0.5) +
    labs(
      title = paste0("LQ over time: ", sector_name),
      subtitle = paste0(region_name, " | Shaded = simulated 95% CI from ", n_sims, " MC draws"),
      x = "Year",
      y = "Location Quotient",
      caption = "Dashed line = LQ of 1 (national average). Log-normal draws on GVA with ABS standard errors."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40")
    )
}

# Example time series
plot_simulated_LQ_timeseries(LQ_with_sim_CIs, "fabricated metal", "yorkshire")
plot_simulated_LQ_timeseries(LQ_with_sim_CIs, "pharmaceutical", "north east")
plot_simulated_LQ_timeseries(LQ_with_sim_CIs, "computer programming", "west midlands")


# Faceted version: all regions for one sector over time
plot_simulated_LQ_allregions <- function(data, sector_pattern) {

  plot_data <- data %>%
    filter(grepl(sector_pattern, SIC07_description, ignore.case = TRUE))

  sector_name <- unique(plot_data$SIC07_description)[1]

  ggplot(plot_data, aes(x = year, y = LQ_central)) +
    geom_ribbon(aes(ymin = LQ_p025, ymax = LQ_p975), alpha = 0.3, fill = "steelblue") +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_point(colour = "steelblue", size = 1) +
    geom_hline(yintercept = 1, linetype = 'dashed', colour = 'blue', alpha = 0.5) +
    facet_wrap(~Region_name, ncol = 3) +
    labs(
      title = paste0("LQ over time with simulated 95% CI: ", sector_name),
      subtitle = paste0(n_sims, " Monte Carlo draws using log-normal on GVA with ABS standard errors"),
      x = "Year",
      y = "Location Quotient"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
}

plot_simulated_LQ_allregions(LQ_with_sim_CIs, "fabricated metal")
plot_simulated_LQ_allregions(LQ_with_sim_CIs, "computer programming")
plot_simulated_LQ_allregions(LQ_with_sim_CIs, "pharmaceutical")



# DAN CHECKS ON THAT CLAUDE OUTPUT----

## Checking the central LQ values look correct----

# Already calculated that
# And yes, they're all looking fine
itl1.cp.centralestLQ %>% 
  # filter(qg('computer prog',SIC07_description), year == max(year)) %>% 
  filter(qg('pharmaceutical',SIC07_description), year == max(year)) %>% 
  # filter(qg('fabricated',SIC07_description), year == max(year)) %>% 
  arrange(-LQ) %>% 
  select(Region_name,LQ)






























