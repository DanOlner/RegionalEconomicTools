#ONS LOCAL SESSION 27 NOV 2025: TEST CODE
#PIPELINE ESSENTIALS
library(tidyverse)
library(xml2)
library(nomisr)
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

url = paste0(website_basename,zip_links[1])
temp_path = tempfile(fileext=".zip")
download.file(url, temp_path, mode="wb")

# List what's in that but don't unzip
filenames = unzip(temp_path, list = TRUE)

#Stick into dataframe
census2021data = read_csv(unzip(temp_path, files = getdistinct('utla',filenames$Name)))




# Keep this simple plz!

# PRODUCTIVITY: CURRENT PRICE INDEX (PPT DIFF TO UK AVERAGE)----

url <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
temp_path <- tempfile(fileext=".xlsx")
download.file(url, temp_path, mode="wb")

# "Table A2: Current Price (unsmoothed) GVA (B) per hour worked indices; ITL2 and ITL3 subregions, 2004 - 2023"
# cp.indices <- readxl::read_excel(path = temp_path,range = "A2!A5:W247")

# Or keep a local version in case it changes later...
# Might not use this here, don't want to be syncing all these other versions?
# download.file(
#   url, 
#   # paste0('data_downloads/ONS_outputperhourdata-',format(Sys.Date(),'%b-%d-%Y'),'.xlsx'), 
#   paste0('data_downloads/ONS_outputperhourdata-',format(Sys.Date(),'%b-%Y'),'.xlsx'),
#   mode="wb") 




# "Table A1: Current Price (smoothed) GVA (B) per hour worked indices; ITL2 and ITL3 subregions, 2004 - 2023"
cp.indices <- readxl::read_excel(path = temp_path,range = "A1!A5:W247")

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

# Actually, let's do that from scratch, so people can see how to add their own places
# cp.indices %>% 
#   filter(
#     qg('',Region_name)
#   )

# Hmm - if we add in one more, there won't be enough colours will there? Great

ggplot(
    cp.indices %>% filter(Region_name %in% corecities),
    aes(x = year, y = index_value, colour = fct_reorder(Region_name,index_value,.desc = T))
    ) +
  geom_line() +
  geom_hline(yintercept = 100) +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank()) +
  ylab('Output per hour: 100 = UK av')


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
  theme(legend.title=element_blank()) +
  ylab('Output per hour: 100 = UK average')



# NOMISR----

# Need to show how to find this number

#BRES code, get "concepts" we can use to specify download 
a <- nomis_get_metadata(id = "NM_189_1")

#Pick on some of those (some of which don't seem to be working.)
#Note, MEASURE in the actual downloaded data is "MEASURE_NAME" column...
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
geogz = nomis_get_metadata(id = "NM_189_1", concept = "GEOGRAPHY", type = "type")

print(geogz, n = 60)

#Point of confusion here - 
#Look at the full column range and how it's broken down:
#(for some sample data)
# placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% 
#   filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

# Core city / ITL3 2021 match?
# Tick - Belfast won't be there cos BRES is GB but otherwise good
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% 
  filter(label.en %in% corecities) %>% select(id) %>% pull

# For the exercise here, let's just check on Edinburgh
# placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% 
#   filter(label.en == 'City of Edinburgh') %>% select(id) %>% pull


# USE LOCAL AUTHORITIES TO GET ALL TIMEPOINTS
# "TYPE424 local authorities: district / unitary (as of April 2023)"
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE424") %>% 
  filter(label.en %in% c(corecities,'Cardiff','Newcastle upon Tyne')) %>% select(id) %>% pull



bres <- nomis_get_data(id = "NM_189_1",  time = "latest", geography = placeid,
                    # MEASURE = 1,#Count of jobs
                    MEASURE = 2,#'Industry percentage'
                    MEASURES = 20100,#Just gives value (of percent) - redundant but lowers data download
                    EMPLOYMENT_STATUS = 2,#Full time jobs
                    select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

# Out of interest, how long to get all timepoints? Can we do in one here?
# Aaah - all time requires using different geography, local authorities
# ITL3 2021 none prior to 2022?
bres <- nomis_get_data(id = "NM_189_1",  geography = placeid,
                    # MEASURE = 1,#Count of jobs
                    MEASURE = 2,#'Industry percentage'
                    MEASURES = 20100,#Just gives value (of percent) - redundant but lowers data download
                    EMPLOYMENT_STATUS = 2,#Full time jobs
                    select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

# That'd fit in the 1GB easy enough
# (But I'll provide a blue peter version too!)
pryr::object_size(bres)


# Save this just in case NOMIS falls over when everyone trying to use
saveRDS(bres,'data/bresLAdownload.rds')

# Save as CSV for ease of loading
write_csv(bres,'data/bresLAdownload.csv')



unique(z$INDUSTRY_TYPE)

# Quick looksee
bres %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') %>% View

# Check industry percentages sum to 100 per ITL3 zone
# Mostly - some roundings-down build up to not quite 100%
bres %>% 
  filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') %>% 
  group_by(GEOGRAPHY_NAME,DATE) %>% 
  summarise(totalpercent = sum(OBS_VALUE))


# OK, let's do a combined chart? Or do by sector. Start with finannce jobs.
# 2 digit SICS are 64,65 and 66: sum those
# Pick out just 2 digit first as well
finance.percent = bres %>% 
  filter(
    INDUSTRY_TYPE == 'SIC 2007 division (2 digit)',
    qg('64|65|66', INDUSTRY_NAME)
  ) %>% 
  group_by(GEOGRAPHY_NAME,DATE) %>% 
  summarise(
    percent = sum(OBS_VALUE)
  ) 

# Plot. May need to smooth
ggplot(
  finance.percent, 
  aes(x = DATE, y = percent, colour = fct_reorder(GEOGRAPHY_NAME,percent,.desc = TRUE))
  ) +
  geom_line() +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank()) +
  scale_x_continuous(breaks = unique(finance.percent$DATE)) +
  ylab('Percent') +
  ggtitle('Percent jobs in finance sectors, GB core cities')



# TEST ZIP SCRAPING AND LOADING----

# From NOMIS webpage bulk Census download
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

url = paste0(website_basename,zip_links[qg('TS063', zip_links)][1])
temp_path = tempfile(fileext=".zip")
download.file(url, temp_path, mode="wb")

# LOOK INSIDE ZIP FOLDER - We can see separate files for different geographical scales
# With list = TRUE, this DOES NOT unzip - it just lists the zip folder's contents
filenames = unzip(temp_path, list = TRUE)

# Take a look
filenames

#Stick into dataframe
# census2021data = read_csv(unzip(temp_path, files = getdistinct('utla',filenames$Name)))

# Pick the index we want
occ = read_csv(unzip(temp_path, files = filenames$Name[3]))

colnames(occ)               
               
# # Get lookup to add geog names in there
# utla_lookup = read_csv('data/WD21_PCON21_LAD21_UTLA21_UK_LU.csv') %>% 
#   select(contains('UTLA')) %>% 
#   distinct()
# 
# table(occ$`geography code` %in% utla_lookup$UTLA21CD)
# 
# # Census data is only England and wales...
# # Non matches are Scot/NI?
# utla_lookup %>% filter(!UTLA21CD %in% occ$`geography code`) %>% View

# Don't need name lookup - the names *are* in the LTLAs (not in UTLAs)
# Which is what we want if we're after core cities

occ = occ %>% 
  pivot_longer(
    4:13, names_to = 'occupation', values_to = 'count'
  )

# Split: get 'all usual 18+ residents in employment
# totals = occ %>% 
#   filter(qg('all usual',occupation))
# 
# # Keep just separate occupations and merge in the totals to get percentages
# occ = occ %>% 
#   filter(
#     !qg('all usual',occupation)
#   ) %>%
#   left_join(
#     totals %>% select(`geography code`, all_in_employment = count),
#     by = 'geography code'
#   ) %>% 
#   mutate(
#     percent_occ = (count / all_in_employment) * 100
#   )
#   
# # Just confirming the totals do match
# occ %>% 
#   group_by(geography) %>% 
#   summarise(
#     chksum = sum(count),
#     all_in_employment = max(all_in_employment)
#   ) %>% 
#   mutate(
#     chksum == all_in_employment
#   ) %>% 
#   summarise(mean(chksum))

# Don't need all of that - can just do sums per group given we've confirmed matches
occ = occ %>% 
  filter(
    !qg('all usual',occupation)
    ) %>%
  group_by(geography) %>% 
  mutate(
    percent_occ = (count / sum(count)) * 100
    ) %>% 
  ungroup()

# Confirm sums to 100% per place... tick
occ %>% group_by(geography) %>% 
  summarise(percentsum = sum(percent_occ))



# Nicer occupation names
occ = occ %>% 
  mutate(
    occupation = case_when(
      qg('Managers', occupation) ~ 'Managers, directors & senior officials',
      qg('Professional occ', occupation) ~ 'Professionals',
      qg('Associate', occupation) ~ 'Associate professional & technical',
      qg('secretar', occupation) ~ 'Admin & secretarial',
      qg('trades', occupation) ~ 'Skilled trades',
      qg('caring', occupation) ~ 'Caring, leisure & other service',
      qg('sales', occupation) ~ 'Sales & customer service',
      qg('process', occupation) ~ 'Process, plant & machine operatives',
      qg('elementary', occupation) ~ 'Elementary'
    )
  )

# Filter to core cities
table(corecities %in% occ$geography)
table(c(corecities,'Cardiff','Newcastle upon Tyne') %in% occ$geography)

# Aaaand plot!
occ.core = occ %>% 
  filter(
    geography %in% c(corecities,'Cardiff','Newcastle upon Tyne')
  )

# English and Welsh core cities
unique(occ.core$geography)
unique(occ.core$geography)[!unique(occ.core$geography) %in% corecities]




# Might be that a filled bar is the way forward? Might not have needed percentages but let's see.
# Could potentially also illustrate ggplotly for hovering for %s
ggplot(
  occ.core,
  aes(y = geography, x = percent_occ, fill = occupation)
) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_brewer(palette = 'Paired')



# Get occupation values for England outside London
# To compare differences
occ.all = read_csv(unzip(temp_path, files = filenames$Name[6])) %>% 
  filter(geography != 'London') %>% 
  pivot_longer(
    4:13, names_to = 'occupation', values_to = 'count'
  )

# Sum for rest of England/Wales outside London
# By occupation
occ.all = occ.all %>% 
  group_by(occupation) %>% 
  summarise(count = sum(count)) %>% 
  filter(!qg('usual',occupation)) %>% 
  mutate(percent_occ = (count/sum(count)) * 100)

occ.all = occ.all %>% 
  mutate(
    occupation = case_when(
      qg('Managers', occupation) ~ 'Managers, directors & senior officials',
      qg('Professional occ', occupation) ~ 'Professionals',
      qg('Associate', occupation) ~ 'Associate professional & technical',
      qg('secretar', occupation) ~ 'Admin & secretarial',
      qg('trades', occupation) ~ 'Skilled trades',
      qg('caring', occupation) ~ 'Caring, leisure & other service',
      qg('sales', occupation) ~ 'Sales & customer service',
      qg('process', occupation) ~ 'Process, plant & machine operatives',
      qg('elementary', occupation) ~ 'Elementary'
    )
  )


# Merge those in to get difference to other regions minus London
# occ = occ %>% 
#   left_join(
#     occ.all %>% select(-count,percent_occ_engwalesnolondon = percent_occ),
#     by = 'occupation'
#   ) 

# Add in percentage point (PPT) difference

# Actually, let's just try plotting this together
# Which would be a slightly different structure...

occ.compare = bind_rows(
  occ.core %>% select(-date,-`geography code`),
  occ.all %>% mutate(geography = 'E+W minus London')
)

# Order factor by E + W minus London amount
factororder = occ.all %>% 
  arrange(-percent_occ) %>% 
  pull(occupation)

occ.compare = occ.compare %>% 
  mutate(
    geography = factor(geography, levels = )
  )
  

# Can now see both. Facetting per sector might show better but let's see
ggplot() +
  geom_jitter(
    data = occ.compare %>% filter(!qg('e\\+w', geography)), 
    aes(x = percent_occ, y = occupation, colour = geography),
    size = 3,
    shape = 17,
    height = 0.1
    ) +
  scale_color_brewer(palette = 'Paired') +
  geom_point(
    data = occ.compare %>% filter(qg('e\\+w', geography)), 
    aes(x = percent_occ, y = fct_reorder(occupation,percent_occ)),
    size = 7, shape = 3, colour = 'black'
  ) +
  xlab('Occupation % of all employed (18+)')


  # scale_size_manual(values = c(rep(1,9),3)) +
  # scale_shape_manual(values = c(rep(1,9),3)) +

# Possibly add Edinburgh back in, if not too much faff
# Actuall probably is, if we want UK averages...




# POSSIBLE OTHER MATERIAL----

## PRODUCTIVITY AND GVA COMBINED----

# Getting % change in GVA / percent change in hours? Maybe?
# Taking from productivity_bits.qmd

#So need the number of hours and chained volume GVA
#Hours count is in the per hour productivity sheet
#GVA in the region by industry sheet

#hours worked first
url <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
temp_path <- tempfile(fileext=".xlsx")
download.file(url, temp_path, mode="wb")

#"Productivity Hours Worked per Week; ITL2 and ITL3 subregions (constrained to ITL1), 2004 - 2023"
hoursworked <- readxl::read_excel(path = temp_path,range = "Productivity Hours!A5:W247")

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
















