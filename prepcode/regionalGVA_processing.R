#ONS regional GVA process from URL
#Though to separate sheets for different geographical scales and SIC levels
#For both current prices and chained volume
library(tidyverse)

# GET LATEST REGIONAL GVA DATA----

#Latest release here:
#https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry
#April 2024 release for data up to 2022

#Workaround for lack of URL download native in readxl package
#Via https://stackoverflow.com/a/79311678/5023561
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")



# ITL2 ZONES----

## SIC SECTIONS----

### 1. CURRENT PRICES AT ITL2 LEVEL, SIC SECTIONS, WITH/WITHOUT IMPUTED RENT----

#Table 2c is current prices with ITL2 zones
gva <- readxl::read_excel(path = p1f,range = "Table 2c!A2:AC3938") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#Keep SIC sections
#This gets all the letters, saves having to manually filter
SIC_sections <- gva$SIC07_code[substr(gva$SIC07_code,2,2) == ' '] %>% unique


#TWO VERSIONS - ONE THAT KEEPS IMPUTED RENT, ONE THAT REMOVES
#To do the latter for SIC sections, just need to replace the SIC section that includes it with the lower level SIC that does not
#This is also then the quickest way to remove imputed rent from the national total and regional totals
#(Reminder - which we can only do using 'current prices' because only those can be re-summed, unlike chained volume measures)

#REPLACE "L (68)" REAL ESTATE ACTIVITIES (WHICH INCLUDES IMPUTED RENT) WITH JUST 68 "Real estate activities, excluding imputed rental"  
SIC_sections_minusImputedRent = SIC_sections
SIC_sections_minusImputedRent[SIC_sections_minusImputedRent == 'L (68)'] <- '68'


#Filter down to SIC section rows and make long by year
#Also convert year to numeric
gva.all <- gva %>% 
  filter(SIC07_code %in% SIC_sections) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# unique(gva.all$Region_name)
# unique(gva.all$SIC07_description)


#And non imputed rent version
gva.minusimputedrent <- gva %>% 
  filter(SIC07_code %in% SIC_sections_minusImputedRent) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #Get most recent year
  mutate(year = as.numeric(year))

# unique(gva.minusimputedrent$Region_name)
# unique(gva.minusimputedrent$SIC07_description)


#Check that gva.all total does equal the total for each ITL2 in the current prices sheet...
# gva.totals <- gva %>% 
#   filter(SIC07_code == 'Total') %>% 
#   pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
#   mutate(year = as.numeric(year))
# 
# gva.totals.summedfromsections <- gva.all %>% 
#   group_by(Region_name,year) %>% 
#   summarise(value = sum(value)) %>% 
#   ungroup()

#Yep, all looks within rounding errors on the individual sectors
# chk <- gva.totals %>% 
#   left_join(
#     gva.totals.summedfromsections %>% rename(valuefromsum = value),
#     by = c('Region_name','year')
#   ) %>% 
#   mutate(diff = value - valuefromsum)

#Check total differences between those... 10,000th of a percent difference, fine
# chk %>% summarise(across(value:valuefromsum,sum))

#Can use gva.totals for anything including imputed rent (which I think let's just not do at the moment)


#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_currentprices_ITL2_SICsections_',names(gva)[length(names(gva))],'.csv'))
write_csv(gva.minusimputedrent, paste0('data/regionalGVA_currentprices_ITL2_SICsections_MINUSimputedrent_',names(gva)[length(names(gva))],'.csv'))




### 2. CHAINED VOLUME AT ITL2 LEVEL, SIC SECTIONS----

#Reminder: can't remove imputed rent here as CV measures can't be summed across places
#Have to keep each as is

#Table 2b is chained volume prices with ITL2 zones
gva <- readxl::read_excel(path = p1f,range = "Table 2b!A2:AC3938") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#Already have SIC sections defined...

#Filter down to SIC section rows and make long by year
#Also convert year to numeric
gva.all <- gva %>% 
  filter(SIC07_code %in% SIC_sections) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# unique(gva.all$Region_name)
# unique(gva.all$SIC07_description)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_chainedvolume_ITL2_SICsections_',names(gva)[length(names(gva))],'.csv'))






## SIC CODES AT LOWEST AVAILABLE LEVEL (BELOW SECTIONS)----

#Which are bespoke to the regional GVA data and vary between geographical level

#NOTE: MINUS VALUES IN WATER AND LAND TRANSPORT FOR SOME OF THE CELLS


### 1. CURRENT PRICES AT ITL2 LEVEL, LOWEST AVAILABLE SIC LEVEL----

#Don't need to recalculate removing imputed rent - it's in here as a separate category, can be removed later

#Table 2c is current prices with ITL2 zones
gva <- readxl::read_excel(path = p1f,range = "Table 2c!A2:AC3938") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#WARNING: ONLY CORRECT LIST TO REMOVE FOR ITL2 - ITL3 has a different list of SICs
#SICs to remove to leave just unique SIC values
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
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


#Filter down to SIC rows and make long by year - remove ones from the list above, just leaving the ones we want
#Also convert year to numeric
gva.all <- gva %>% 
  filter(!SIC07_code %in% SICremoves) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

#Confirmed match by visual check against xls spreadsheet codes
# unique(gva.all$SIC07_code)
# unique(gva.all$SIC07_description)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_currentprices_ITL2_allavailableSICs_',names(gva)[length(names(gva))],'.csv'))




### 2. CHAINED VOLUME AT ITL2 LEVEL, LOWEST AVAILABLE SIC LEVEL----

#Reminder: can't remove imputed rent here as CV measures can't be summed across places
#Have to keep each as is

#Table 2b is chained volume prices with ITL2 zones
gva <- readxl::read_excel(path = p1f,range = "Table 2b!A2:AC3938") 

#Filter down to SIC rows and make long by year - remove ones from the list above, just leaving the ones we want
#Also convert year to numeric
gva.all <- gva %>% 
  filter(!SIC07_code %in% SICremoves) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# unique(gva.all$SIC07_code)
# unique(gva.all$SIC07_description)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_chainedvolume_ITL2_allavailableSICs_',names(gva)[length(names(gva))],'.csv'))










# ITL3 ZONES----

## SIC SECTIONS----

### 1. CURRENT PRICES AT ITL3 LEVEL, SIC SECTIONS, WITH/WITHOUT IMPUTED RENT----

#Table 3c is current prices with ITL3 zones
gva <- readxl::read_excel(path = p1f,range = "Table 3c!A2:AC11458") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')



#SIC section def needs to be a bit different at ITL3 level -
#Two of them are combined, so aren't single letters:
#AB (1-9) is "Agriculture, forestry and fishing; mining and quarrying" combined
#DE (35-39) is "Electricity, gas, water; sewerage and waste management"

#We can use previous definition, but combine with bespoke...
SIC_sections_ITL3 <- c(
  'AB (1-9)',#Unique to ITL3
  'C (10-33)',
  'DE (35-39)',#Unique to ITL3
  SIC_sections[6:length(SIC_sections)]#rest the same
)

#And minus imputed rent... (adding in "real estate excluding imputed rent" in place of including)
SIC_sections_ITL3_minusImputedRent <- SIC_sections_ITL3
SIC_sections_ITL3_minusImputedRent[SIC_sections_ITL3_minusImputedRent == 'L (68)'] <- '68'
  
  

#Filter down to SIC section rows and make long by year
#Also convert year to numeric
gva.all <- gva %>% 
  filter(SIC07_code %in% SIC_sections_ITL3) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# unique(gva.all$SIC07_code)
# unique(gva.all$SIC07_description)

#And non imputed rent version
gva.minusimputedrent <- gva %>% 
  filter(SIC07_code %in% SIC_sections_ITL3_minusImputedRent) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #Get most recent year
  mutate(year = as.numeric(year))

# unique(gva.minusimputedrent$SIC07_code)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_currentprices_ITL3_SICsections_',names(gva)[length(names(gva))],'.csv'))
write_csv(gva.minusimputedrent, paste0('data/regionalGVA_currentprices_ITL3_SICsections_MINUSimputedrent_',names(gva)[length(names(gva))],'.csv'))





### 2. CHAINED VOLUME AT ITL3 LEVEL, SIC SECTIONS----

#Reminder: No point removing imputed rent here as CV measures can't be summed across places
#Have to keep each as is

#Table 3b is chained volume prices with ITL3 zones
gva <- readxl::read_excel(path = p1f,range = "Table 3b!A2:AC11458") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#Already have SIC sections defined...

#Filter down to SIC section rows and make long by year
#Also convert year to numeric
gva.all <- gva %>% 
  filter(SIC07_code %in% SIC_sections_ITL3) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# unique(gva.all$Region_name)
# unique(gva.all$SIC07_code)
# unique(gva.all$SIC07_description)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_chainedvolume_ITL3_SICsections_',names(gva)[length(names(gva))],'.csv'))






## SIC CODES AT LOWEST AVAILABLE LEVEL (BELOW SECTIONS)----

#Which are bespoke to the regional GVA data and vary between geographical level
#48 SIC categories for ITL3

#NOTE: MINUS VALUES IN WATER AND LAND TRANSPORT FOR SOME OF THE CHAINED VOLUME CELLS


### 1. CURRENT PRICES AT ITL3 LEVEL, LOWEST AVAILABLE SIC LEVEL----

#Don't need to recalculate removing imputed rent - it's in here as a separate category, can be removed later

#Table 3c is current prices with ITL3 zones
gva <- readxl::read_excel(path = p1f,range = "Table 3c!A2:AC11458") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#WARNING: ONLY CORRECT LIST TO REMOVE FOR ITL3 - ITL3 has a different list of SICs
#SICs to remove to leave just unique SIC values
ITL3_SICremoves = c(
  'Total',
  'A-E',
  'C (10-33)',
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

#Filter down to SIC rows and make long by year - remove ones from the list above, just leaving the ones we want
#Also convert year to numeric
gva.all <- gva %>% 
  filter(!SIC07_code %in% ITL3_SICremoves) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

#Confirmed match by visual check against xls spreadsheet codes
# unique(gva.all$SIC07_code)
# unique(gva.all$SIC07_description)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_currentprices_ITL3_allavailableSICs_',names(gva)[length(names(gva))],'.csv'))




### 2. CHAINED VOLUME AT ITL3 LEVEL, LOWEST AVAILABLE SIC LEVEL----

#Reminder: can't remove imputed rent here as CV measures can't be summed across places
#Have to keep each as is

#Table 3b is chained volume prices with ITL3 zones
gva <- readxl::read_excel(path = p1f,range = "Table 3b!A2:AC11458") 

#More process-able names with no spaces
names(gva) <- gsub(x = names(gva), pattern = ' ', replacement = '_')

#Filter down to SIC rows and make long by year - remove ones from the list above, just leaving the ones we want
#Also convert year to numeric
gva.all <- gva %>% 
  filter(!SIC07_code %in% ITL3_SICremoves) %>% 
  pivot_longer(`1998`:names(gva)[length(names(gva))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# unique(gva.all$SIC07_code)
# unique(gva.all$SIC07_description)

#Save as CSV, with latest year as name
write_csv(gva.all, paste0('data/regionalGVA_chainedvolume_ITL3_allavailableSICs_',names(gva)[length(names(gva))],'.csv'))










