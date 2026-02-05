# Playing around with standard errors for the ABS component of regional GVA
# How much are places actually separarable for sectors?
library(tidyverse)

# Get data----
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

# Save for use elsewhere



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
  abs_gva %>% filter(qg('fabricated',Description)),
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



# OK. What's the 2023 GVA from the actual region by industry counts?
# Slightly different sector lists but some overlap
# Might be better to check against sections, no?


























