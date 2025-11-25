#ONS LOCAL SESSION 27 NOV 2025: TEST CODE
#PIPELINE ESSENTIALS
library(tidyverse)
library(xml2)
source('functions/misc_functions.R')

#Set ggplot theme
theme_set(theme_light())


# Get zip list from nomis website for bulk census download----


website_basename = "https://www.nomisweb.co.uk"

doc = read_html(paste0(website_basename,"/sources/census_2021_bulk"))

# Extract all <a> tags with hrefs containing "census2021*.zip"
zip_links = xml_attr(
  xml_find_all(doc, "//a[contains(@href, 'census2021') and contains(@href, '.zip')]"),
  "href"
)

#Set global timeout to very bigly indeed
#ten hours per file covers all eventualities!
options(timeout = 36000)

url1 = paste0(website_basename,zip_links[1])
p1f = tempfile(fileext=".zip")
download.file(url1, p1f, mode="wb")

# List what's in that but don't unzip
filenames = unzip(p1f, list = TRUE)

#Stick into dataframe
census2021data = read_csv(unzip(p1f, files = getdistinct('utla',filenames$Name)))




# Keep this simple plz!

# PRODUCTIVITY: CURRENT PRICE INDEX (PPT DIFF TO UK AVERAGE)----

url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

# "Table A2: Current Price (unsmoothed) GVA (B) per hour worked indices; ITL2 and ITL3 subregions, 2004 - 2023"
# cp.indices <- readxl::read_excel(path = p1f,range = "A2!A5:W247")

# "Table A1: Current Price (smoothed) GVA (B) per hour worked indices; ITL2 and ITL3 subregions, 2004 - 2023"
cp.indices <- readxl::read_excel(path = p1f,range = "A1!A5:W247")

# RESHAPE
# Get just ITL3 level and pivot years into their own column
cp.indices = cp.indices %>%
  filter(ITL_level == 'ITL3') %>% 
  pivot_longer(Index_2004:Index_2023, names_to = 'year', values_to = 'index_value') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)

# subset years
# cp.indices = cp.indices %>% 
#   filter(year >= 2014)


# p = ggplot(
#   cp.indices,
#   aes(x = year, y = index_value, group = Region_name)
#   ) +
#   geom_line(alpha = 0.2) +
#   geom_hline(yintercept = 100)
# 
# plotly::ggplotly(p, tooltip = 'Region_name')


# Get core cities
# corecities = readRDS('data/corecitiesvector.rds')
# 
# table(corecities %in% cp.indices$Region_name)
# corecities[!corecities %in% cp.indices$Region_name]
# 
# # Replace and save
# corecities[corecities == 'Cardiff'] = 'Cardiff and Vale of Glamorgan'
# corecities[corecities == 'Newcastle upon Tyne'] = 'Tyneside'
# 
# # Save that version
# saveRDS(corecities,'data/corecitiesvector_itl3_2025.rds')

corecities = readRDS('data/corecitiesvector_itl3_2025.rds')

table(corecities %in% cp.indices$Region_name)

ggplot(
    cp.indices %>% filter(Region_name %in% corecities),
    aes(x = year, y = index_value, colour = fct_reorder(Region_name,index_value,.desc = T))
    ) +
  geom_line() +
  geom_hline(yintercept = 100) +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank())


# Simple way without finding slopes to get most extreme changes
# Use standard deviation of points over time
cp.indices.sd = cp.indices %>% 
  group_by(Region_name) %>% 
  summarise(
    index_sd = sd(index_value)
  ) %>% 
  arrange(desc(index_sd))

toptwelvechangers = cp.indices.sd %>% 
  slice(1:12) %>% 
  pull(Region_name)
  
ggplot(
  cp.indices %>% filter(Region_name %in% toptwelvechangers),
  aes(x = year, y = index_value, colour = fct_reorder(Region_name,index_value,.desc = T))
) +
  geom_line() +
  geom_hline(yintercept = 100) +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank())


# OK, slopes!
slopes = get_slope_and_se_safely(
  cp.indices, 
  Region_name, 
  y = log(index_value), 
  x = year
  )

# Order by annualised slope
slopes = slopes %>% 
  arrange(desc(slope))

# Keep top and bottom six
topandtail = cp.indices %>% 
  filter(
    Region_name %in% c(
      slopes$Region_name[1:6]
      # slopes$Region_name[(nrow(slopes)-5):nrow(slopes)]
      )
    )

ggplot(
  topandtail,
  aes(x = year, y = index_value, colour = fct_reorder(Region_name,index_value,.desc = T))
) +
  geom_line() +
  geom_hline(yintercept = 100) +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank())





# POSSIBLE OTHER MATERIAL----

## PRODUCTIVITY AND GVA COMBINED----

# Getting % change in GVA / percent change in hours? Maybe?
# Taking from productivity_bits.qmd

#So need the number of hours and chained volume GVA
#Hours count is in the per hour productivity sheet
#GVA in the region by industry sheet

#hours worked first
url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

#"Productivity Hours Worked per Week; ITL2 and ITL3 subregions (constrained to ITL1), 2004 - 2023"
hoursworked <- readxl::read_excel(path = p1f,range = "Productivity Hours!A5:W247")

names(hoursworked) <- gsub(x = names(hoursworked), pattern = ' ', replacement = '_')

# Get just ITL3 level
itl3.hoursworked = hoursworked %>%
  filter(ITL_level == 'ITL3') %>% 
  pivot_longer(Hours_2004:Hours_2023, names_to = 'year', values_to = 'hoursworkedperweek') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)


# Get UK level as a whole to compare to average
uk.hoursworked = hoursworked %>%
  filter(ITL_level == 'UK') %>% 
  pivot_longer(Hours_2004:Hours_2023, names_to = 'year', values_to = 'hoursworkedperweek') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)



#Then ITL3 CV for all industries (so has to include imputed rent as we can't sum CV)
url2 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p2f <- tempfile(fileext=".xlsx")
download.file(url2, p2f, mode="wb") 

#"Table 3b: ITL3, chained volume measures in 2019 money value, pounds million [note 2]"
itl3.cv <- readxl::read_excel(path = p2f, range = "Table 3b!A2:AD11468")

names(itl3.cv) <- gsub(x = names(itl3.cv), pattern = ' ', replacement = '_')

itl3.cv = itl3.cv %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl3.cv)[length(names(itl3.cv))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year)) %>% 
  rename(gva_cv = value)


#Check link between two is valid... tick
# table(unique(hoursworked$ITL_code) %in% itl3.all$ITL_code)

# Matching years between the two
#2004:2023
# unique(hoursworked$year)[unique(hoursworked$year) %in% unique(itl3.all$year)]

# What we could do: not percent change (which is something of a faff)
# But diff to UK average
# which is what the hours worked index already does, but we could test how that changes with some other assumptions
# E.g. take out imputed rent; take out London



#Then ITL3 CP
# "Table 3c: ITL3 current price estimates, pounds million"
url2 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p2f <- tempfile(fileext=".xlsx")
download.file(url2, p2f, mode="wb") 

#"Table 3b: ITL3, chained volume measures in 2019 money value, pounds million [note 2]"
itl3.cp <- readxl::read_excel(path = p2f, range = "Table 3c!A2:AD11468")

names(itl3.cp) <- gsub(x = names(itl3.cp), pattern = ' ', replacement = '_')

itl3.cp = itl3.cp %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl3.cp)[length(names(itl3.cp))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year)) %>% 
  rename(gva_cv = value)


# Get a version that's summed to 'all industries minus imputed rent'
















