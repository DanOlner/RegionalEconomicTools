#Explore Leeds Bradford data
library(tidyverse)
# library(spdep)
library(sf)#for geo stuff
library(tmap)#for mapping 
library(zoo)#for making smoothed moving averages
library(patchwork)#combine ggplots easily
library(plotly)#For interactive plots
library(plotme)#For sunburst output
# library(ggdist)
# library(tidyr)
# library(distributional)
library(RColorBrewer)

source('functions/misc_functions.R')
options(scipen = 999)


#Reminder of category numbers in each SIC hierarchy in the BRES data----

#Pick random
bres.levels = readRDS("local/data/BRES/INDIV_YEARS/BRES_TYPE429_internationalterritoriallevelslevel2asofJan2021_2_Fulltimeemployees_2023.rds")

#5 4 3 and 2 digit
unique(bres.levels$INDUSTRY_TYPE)

#5 = 729
#4 = 616
#3 = 273
#2 = 88
#Then 20 sections...
bres.levels %>% 
  select(INDUSTRY_TYPE,INDUSTRY_NAME) %>% 
  group_by(INDUSTRY_TYPE) %>% 
  summarise_all(n_distinct)





# Test sunburst for various SIC hierarchy nestings----

#Get separate BRES SIC values for Bradford and a specific year
#Then combine into single DF with different sector levels

#Because just for Bradford, no need for geography harmonising at this point
# bradford.2digit.ft = read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_2DIGIT.csv") %>% filter(qg('bradford',GEOGRAPHY_NAME))

# bradford.section.ft = read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_SECTION.csv")
# 

#For the sunburst code, we only need to keep the jobcounts for the highest hierarchical level - 5 digit
#For the rest, we'll need to add in a join field on 5 digit

#Actually, just realised, we don't need anything BUT the 5 digit.
#Can then just join the other SIC lookup and we're more or less done.
bradford.5digit.ft = read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv") %>% 
  filter(qg('bradford',GEOGRAPHY_NAME), DATE == 2023)
  # filter(qg('leeds',GEOGRAPHY_NAME), DATE == 2023)
  # filter(qg('sheffield',GEOGRAPHY_NAME), DATE == 2023)
  # filter(qg('barnsley',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data
  # filter(qg('kirklees',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data
  # filter(qg('bristol',GEOGRAPHY_NAME), DATE == 2023)#will get BDR in this data

SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match between lookup and BRES... tick!
# table(unique(bradford.5digit.ft$SIC_5DIGIT_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bradford.ft <- bradford.5digit.ft %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
    )




#Make shorter names, use those.
#Make lookup so can be merged in.
names.sections = unique(bradford.ft$SIC_SECTION_NAME)
shortnames.sections = reduceSICnames(unique(bradford.ft$SIC_SECTION_NAME),'section')

chk.sections = data.frame(names = names.sections, shortnames = shortnames.sections)


names.2digit = unique(bradford.ft$SIC_2DIGIT_NAME)
shortnames.2digit = reduceSICnames(unique(bradford.ft$SIC_SECTION_NAME),'2 digit')

chk.2digit = data.frame(names = names.2digit, shortnames = shortnames.2digit)


#And 729 categories in the 5 digit...
names.5digit = unique(bradford.ft$SIC_5DIGIT_NAME)
#NOTE: THE ORDER HERE IS BASED ON THE ORDER IN BRADFORD.FT
#We should make an actual lookup to make sure we don't lose that order
shortnames.5digit = reduceSICnames(unique(bradford.ft$SIC_5DIGIT_NAME),'5 digit')

chk.5digit = data.frame(names = names.5digit, shortnames = shortnames.5digit)


#Merge into bradford.ft
bradford.ft.shorts = bradford.ft %>% 
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>% 
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>% 
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names')) 
  
#Saving that in case I somehow lose the order...
# saveRDS(bradford.ft.shorts,'local/data/bradford_SICs_withshortnames.rds')


#Failed attempts to be clever!
# chk = map2(list(chk.sections,chk.2digit,chk.5digit), 
#            list("SIC_SECTION_NAME","SIC_2DIGIT_NAME","SIC_5DIGIT_NAME"),
#            function(brad_df) {
#   bradford.ft %>%
#     reduce(~ .x %>% inner_join(
#       .y,
#       by = c('GEOGRAPHY_NAME', 'DATE', 'SIC_CODE')),
#       .init = gva_df %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
#                                 SIC_CODE = SIC07_code ,gva = value)) 
# })
# 
# 
#   
# chk = map2(list(chk.sections,chk.2digit,chk.5digit), list("SIC_SECTION_NAME","SIC_2DIGIT_NAME","SIC_5DIGIT_NAME"), ~ {
#   
#   .x %>% write_csv(file = .y)
# })


#Code nabbed from https://github.com/DanOlner/FirmAnalysis/blob/bcf06e46849eb7e08501c596955a247e5dadbe00/Fame_processing.R#L512
bradford.ft.shorts %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = JOBCOUNT) %>%
  # count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_treemap(sort_by_n = T)
  # count_to_sunburst(sort_by_n = T)


bradford.ft.shorts %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = JOBCOUNT) %>% 
  write_csv('local/data/backup/count_output.csv')




#Test for earliest year in the data (I wonder if there are other earlier sources for Bradford...?)
bradford.nuts.5digit.ft = read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE437_nuts2016level3_2_Fulltimeemployees_2015_2022_SIC_5DIGIT.csv") %>% 
  filter(qg('bradford',GEOGRAPHY_NAME), DATE == 2015)

#Check 5 digit name match between lookup and BRES... tick!
table(unique(bradford.5digit.ft$SIC_5DIGIT_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bradford.ft.2015 <- bradford.nuts.5digit.ft %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
  )

#Add in short names
bradford.ft.shorts.2015 = bradford.ft.2015 %>% 
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>% 
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>% 
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names')) 


bradford.ft.shorts.2015 %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = JOBCOUNT) %>%
  # count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_treemap(sort_by_n = T)
  # count_to_sunburst(sort_by_n = T)




# REPEAT SUNBURST FOR COMPANIES HOUSE DATA FROM LAST YEAR----

ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_July2025.rds')

#Add in nicer SIC names
ch = ch %>% 
  left_join(
    bradford.ft.shorts %>% select(SIC_5DIGIT_CODE,SIC_SECTION_NAME_SHORT:SIC_5DIGIT_NAME_SHORT),
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

#Aaand
ch.la %>%
  st_set_geometry(NULL) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = Employees_thisyear) %>%
  filter(!is.na(SIC_SECTION_NAME_SHORT)) %>% 
  # count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_treemap(sort_by_n = T)
# count_to_sunburst(sort_by_n = T)



#Can we do treemap for range of places to compare easily? Kinda...
ch.la <- ch.emp1 %>% 
  # filter(qg('bradford',localauthority_name))
  # filter(qg('sheffield',localauthority_name))
  # filter(qg('leicester',localauthority_name))
  # filter(qg('barnsley|sheffield|rotherham|doncaster',localauthority_name))
  filter(qg('bradford|kirklees|leeds|wakefield|calderdale',localauthority_name))

#Ooo yes but could do with more control over colours
ch.la %>%
  st_set_geometry(NULL) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  count(localauthority_name,SIC_SECTION_NAME_SHORT,SIC_2DIGIT_NAME_SHORT,SIC_5DIGIT_NAME_SHORT, wt = Employees_thisyear) %>%
  # count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_treemap(sort_by_n = T)



