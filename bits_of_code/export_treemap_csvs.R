#Export CSV of counts for plotly output in python
library(tidyverse)
library(sf)
source('functions/misc_functions.R')

# From BRES data----

bres.5digit.ft = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv") %>% 
  filter(qg('bradford',GEOGRAPHY_NAME), DATE == 2023)
# filter(qg('leeds',GEOGRAPHY_NAME), DATE == 2023)
# filter(qg('sheffield',GEOGRAPHY_NAME), DATE == 2023)
# filter(qg('barnsley',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data
# filter(qg('kirklees',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data
# filter(qg('bristol',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data

SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match between lookup and BRES... tick!
# table(unique(bres.5digit.ft$SIC_5DIGIT_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bres.ft <- bres.5digit.ft %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
  )




#Make shorter names, use those.
#Make lookup so can be merged in.
names.sections = unique(bres.ft$SIC_SECTION_NAME)
shortnames.sections = reduceSICnames(unique(bres.ft$SIC_SECTION_NAME),'section')

chk.sections = data.frame(names = names.sections, shortnames = shortnames.sections)


names.2digit = unique(bres.ft$SIC_2DIGIT_NAME)
shortnames.2digit = reduceSICnames(unique(bres.ft$SIC_SECTION_NAME),'2 digit')

chk.2digit = data.frame(names = names.2digit, shortnames = shortnames.2digit)


#And 729 categories in the 5 digit...
names.5digit = unique(bres.ft$SIC_5DIGIT_NAME)
#NOTE: THE ORDER HERE IS BASED ON THE ORDER IN bres.FT
#We should make an actual lookup to make sure we don't lose that order
shortnames.5digit = reduceSICnames(unique(bres.ft$SIC_5DIGIT_NAME),'5 digit')

chk.5digit = data.frame(names = names.5digit, shortnames = shortnames.5digit)


#Merge into bres.ft
bres.ft.shorts = bres.ft %>% 
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>% 
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>% 
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names')) 






#Code nabbed from https://github.com/DanOlner/FirmAnalysis/blob/bcf06e46849eb7e08501c596955a247e5dadbe00/Fame_processing.R#L512
bres.ft.shorts %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = JOBCOUNT) %>%
  # count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_treemap(sort_by_n = T)
# count_to_sunburst(sort_by_n = T)


bres.ft.shorts %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = JOBCOUNT) %>% 
  write_csv('local/data/backup/count_output.csv')




#Repeat for multiple places
bres.5digit.ft = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv") %>% 
  filter(qg('bradford|kirklees|leeds|wakefield|calderdale',GEOGRAPHY_NAME), DATE == 2023)
# filter(qg('leeds',GEOGRAPHY_NAME), DATE == 2023)
# filter(qg('sheffield',GEOGRAPHY_NAME), DATE == 2023)
# filter(qg('barnsley',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data
# filter(qg('kirklees',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data
# filter(qg('bristol',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data

SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match between lookup and BRES... tick!
# table(unique(bres.5digit.ft$SIC_5DIGIT_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bres.ft <- bres.5digit.ft %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
  )

bres.ft.shorts = bres.ft %>% 
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>% 
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>% 
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names')) 

bres.ft.shorts %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(GEOGRAPHY_NAME,SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = JOBCOUNT) %>% 
  write_csv('local/data/backup/count_output_las.csv')







# From Companies House data----

ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_July2025.rds')

#Add in nicer SIC names
ch = ch %>% 
  left_join(
    bres.ft.shorts %>% select(SIC_5DIGIT_CODE,SIC_SECTION_NAME_SHORT:SIC_5DIGIT_NAME_SHORT),
    by = 'SIC_5DIGIT_CODE'
  )



#Keep only firms with at least 1 employee in the most recent year
ch.emp1 <- ch %>% filter(Employees_thisyear >= 1)

#Just for bradford...
ch.la <- ch.emp1 %>% 
  # filter(qg('bradford',localauthority_name))
  filter(qg('sheffield',localauthority_name))
# filter(qg('leicester',localauthority_name))
# filter(qg('barnsley',localauthority_name))
# filter(qg('rotherham',localauthority_name))
# filter(qg('kirklees',localauthority_name))
# filter(qg('bristol',localauthority_name))


#Multiple places
ch.la <- ch.emp1 %>% 
  # filter(qg('barnsley|sheffield|rotherham|doncaster',localauthority_name))
  filter(qg('bradford|kirklees|leeds|wakefield|calderdale',localauthority_name))

#Ooo yes but could do with more control over colours...
#Which we now have in Python, huzzah!
ch.la %>%
  st_set_geometry(NULL) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(localauthority_name,SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = Employees_thisyear) %>%
  filter(!is.na(SIC_SECTION_NAME_SHORT)) %>% 
  write_csv('local/data/plotly_dataexportsfromR/CH_count_output.csv')

#Check that...
# chk <- ch.la %>%
#   st_set_geometry(NULL) %>% 
#   mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
#   count(localauthority_name,SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = Employees_thisyear) 
# 
# 
# #Pick one example - a single row - and check that summed correctly
# #Tick
# ch.la %>% 
#   st_set_geometry(NULL) %>% 
#   select(localauthority_name,SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, Employees_thisyear) %>% 
#   filter(qg('brad',localauthority_name), SIC_5DIGIT_NAME_SHORT == 'Facilities') %>% 
#   summarise(
#     tot = sum(Employees_thisyear)
#   )
# 
# 
# #Now, is it summing correctly in treemap to higher SICs?
# #E.g. Bradford manufacturing section is 11659
# #Tick tick
# ch.la %>% 
#   st_set_geometry(NULL) %>% 
#   select(localauthority_name,SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, Employees_thisyear) %>% 
#   filter(qg('brad',localauthority_name), SIC_SECTION_NAME_SHORT == 'Manuf') %>% 
#   summarise(
#     tot = sum(Employees_thisyear)
#   )











