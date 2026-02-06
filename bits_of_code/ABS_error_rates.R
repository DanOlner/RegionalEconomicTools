# Playing around with standard errors for the ABS component of regional GVA
# How much are places actually separarable for sectors?
library(tidyverse)

source('functions/misc_functions.R')
source('functions/data_process_functions.R')

# Get data (dan code)----

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



# Find error rates across regions and sectors----

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



# Process data for applying those error rates to ITL1 CP and CV values (dan code)----

# Outputted these to CSV in yorkshire_n_humber_sectors_dataprep.R
itl1.cp = read_csv('data/regionalGVA/regionalGVA_currentprices_ITL1_SIC_2DIGIT_WIDE_2023.csv')
itl1.cv = read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL1_SIC_2DIGIT_WIDE_2023.csv')

# Check sector matcch. Will have to make numeric, not matching digits
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





# CLAUDE CODE SECTION: Look at error rates----

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


# Year-on-year growth rates with bounds----

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
    growth_bound_high = (gva_max95 / lag(gva_min95)) - 1
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
    geom_point(aes(colour = ymin > 0 | ymax < 0), size = 1.5) +
    scale_colour_manual(
      values = c("FALSE" = "steelblue", "TRUE" = "darkgreen"),
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

# Compare with less conservative bounds
# plot_sector_growth_with_bounds(itl1.cv.growth, "fabricated metal", use_conservative_bounds = FALSE)













