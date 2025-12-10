# Comparing current prices and chained volume values
# E.g. ICT stays a fairly stable money-value proportion of the economy
# But has apparently grown hugely - e.g. value of a MB.
# But but how do we make sense of this difference, actually?
# If we're thinking about where value is coming from for people and their jobs, for example?
library(tidyverse)
library(zoo)
source('functions/misc_functions.R')

# I know the difference in ICT shows up in Leeds
# But let's see if we can get at national level


# Get ONS regional productivity data----

url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

## Current prices first, find proportions too so can see them change over time----

# Note: section J 'information and comms' has a chunk of non-computer-y things:
# 58: Publishing activities
# 59: Motion picture, video and television programme production, sound recording and music publishing activities
# 60: Programming and broadcasting activities
# 61: Telecommunications
# 62: Computer programming, consultancy and related activities
# 63: Information service activities

# We prob want to just look at 61-3

# Nabbed from yorkshire_n_humber_sectors_dataprep.R
#Table 1c is current prices with ITL1 zones
gva.2digit <- readxl::read_excel(path = p1f,range = "Table 1c!A2:AD1730") 

#More process-able names with no spaces
names(gva.2digit) <- gsub(x = names(gva.2digit), pattern = ' ', replacement = '_')

#WARNING: ONLY CORRECT LIST TO REMOVE FOR ITL1 (only one change from ITL2 though, E (36-39))
#SICs to remove to leave just unique SIC values
#Still works for 2025 as well as 2024 to leave correct highest res SICs
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'B (5-9)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'E (36-39)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

gva.2digit <- gva.2digit %>% 
  filter(
    !SIC07_code %in% SICremoves,
    Region_name == 'United Kingdom'
  ) 

# Looking OK
unique(gva.2digit$SIC07_code)
unique(gva.2digit$Region_name)

# Enlongen
gva.2digit <- gva.2digit %>%  
  pivot_longer(`1998`:names(gva.2digit)[length(names(gva.2digit))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# Tick
# gva.2digit %>% filter(qg('yorkshire', Region_name)) %>% View

# Add smoothed vals
# smoothband = 3
# 
# gva.2digit = gva.2digit %>% 
#   arrange(year) %>% 
#   group_by(Region_name,SIC07_description) %>% 
#   mutate(
#     gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
#   ) %>% 
#   ungroup()

# Just for UK as a whole, find sector proportions for each year
gva.2digit = gva.2digit %>% 
  group_by(year) %>% 
  mutate(
    sector_percent = (value / sum(value)) * 100
  )

# How have 61-3 percents changed over time? No smoothing at national level should be fine?
ggplot(
  gva.2digit %>% filter(SIC07_code %in% c('61','62','63')),
  aes(x = year, y = sector_percent, colour = fct_reorder(SIC07_description,sector_percent,.desc = T))
) +
  geom_line() +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank()) +
  ylab('telecoms, programming, info services: percent of UK economy over time')




# Repeat for chained volume version----

# Using index version: 2022 = 100
gva.2dig.cv <- readxl::read_excel(path = p1f,range = "Table 1a!A2:AD1730") 

#More process-able names with no spaces
names(gva.2dig.cv) <- gsub(x = names(gva.2dig.cv), pattern = ' ', replacement = '_')

#WARNING: ONLY CORRECT LIST TO REMOVE FOR ITL1 (only one change from ITL2 though, E (36-39))
#SICs to remove to leave just unique SIC values
#Still works for 2025 as well as 2024 to leave correct highest res SICs
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'B (5-9)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'E (36-39)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

gva.2dig.cv <- gva.2dig.cv %>% 
  filter(
    !SIC07_code %in% SICremoves,
    Region_name == 'United Kingdom'
  ) 

# Looking OK
unique(gva.2dig.cv$SIC07_code)
unique(gva.2dig.cv$Region_name)

# Enlongen
gva.2dig.cv <- gva.2dig.cv %>%  
  pivot_longer(`1998`:names(gva.2dig.cv)[length(names(gva.2dig.cv))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# Tick
# gva.2dig.cv %>% filter(qg('yorkshire', Region_name)) %>% View

# Add smoothed vals
# smoothband = 3
# 
# gva.2dig.cv = gva.2dig.cv %>% 
#   arrange(year) %>% 
#   group_by(Region_name,SIC07_description) %>% 
#   mutate(
#     gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
#   ) %>% 
#   ungroup()


# How have 61-3 percents changed over time? No smoothing at national level should be fine?
ggplot(
  gva.2dig.cv %>% filter(SIC07_code %in% c('61','62','63')),
  aes(x = year, y = value, colour = fct_reorder(SIC07_description,value,.desc = T))
) +
  geom_line() +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.title=element_blank()) +
  ylab('telecoms, programming, info services: real growth index, 2022 = 100')


# Index values quite helpful - 
# Can see which has lowest in 1998, that immediately tells us which has grown in real terms the most
# And yep, it's telecoms - vastly different to the next one (manuf of computer/electronic, 5* value increase)
# Telecoms has a 106/0.7 = 150* value increase. Err.
gva.2dig.cv %>% filter(year == 1998) %>% arrange(value) %>% View






















