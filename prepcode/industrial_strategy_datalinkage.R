# UK INDUSTRIAL STRATEGY DATA LINKAGES
# To BRES and Companies House data
# Used in e.g. SouthYorkshire_creativeindustries qml report
library(tidyverse)
library(plotly)
library(nomisr)
library(stringdist)
library(sf)
library(zoo)
library(stringr)
library(RColorBrewer)
source('functions/misc_functions.R')
source('functions/adhoc_functions.R')
source('functions/data_process_functions.R')

options(scipen = 99)



# BRES 2024 LOCAL AUTHORITY LEVEL IndStrat----

indsic <- read_csv('data/industrialstrategy2025sectordefs.csv')

# Keep only indstrat cats with SIC codes to match to
strategy_df <- indsic %>% filter(!is.na(SIC_name)) %>% 
  mutate(
    toplevelindstrat = case_when(
      qg('manufactur', sector_name) ~ "ADV MANUF",
      qg('business', sector_name) ~ "PBS",
      qg('financ', sector_name) ~ "FIN",
      qg('creative', sector_name) ~ "CREATIVE",
      qg('foundation', sector_name) ~ "FOUNDATION",
      qg('digital', sector_name) ~ "DIGITAL",
      qg('life sci', sector_name) ~ "LIFESCI",
      qg('defence', sector_name) ~ "DEFENCE"
    ),
    sic_namefrom_indstrat_short = removecommonSICnameelements(SIC_name, removemanuf = T, removeactivities = T)
  )
    
  #   ,
  #   sic_namefrom_indstrat_short = ifelse(qg('Motion picture video/television programme productio',sic_namefrom_indstrat_short),
  #                                        "Film/TV prog production sound/music publishing",sic_namefrom_indstrat_short),
  #   sic_namefrom_indstrat_combo = paste0(toplevelindstrat,": ",sic_namefrom_indstrat_short, " (",sic_code,")")
  # )

# Some extra tweaks
strategy_df = strategy_df %>% 
  mutate(
    sic_namefrom_indstrat_short = case_when(
      qg('ready-made interactive', sic_namefrom_indstrat_short) ~ "interactive entertainment software",
      qg('other financial services', sic_namefrom_indstrat_short) ~ "other fin (not insur/pensions)",
      qg('basic pharmaceutical products', sic_namefrom_indstrat_short) ~ "basic pharma / pharma prep",
      qg('experimental development on natural', sic_namefrom_indstrat_short) ~ "research/experiment sciences/engineering",
      qg('measuring testing navigation', sic_namefrom_indstrat_short) ~ "electronic insts / measure test navigation",
      qg('insurance reinsurance', sic_namefrom_indstrat_short) ~ "insurance/pensions",
      qg('Motion picture video/television programme productio',sic_namefrom_indstrat_short) ~
      "Film/TV prog production sound/music publishing",
      .default = sic_namefrom_indstrat_short
    ),
    sic_namefrom_indstrat_combo = paste0(toplevelindstrat,": ",sic_namefrom_indstrat_short, " (",sic_code,")")
  )

unique(strategy_df$sic_namefrom_indstrat_combo)

# This one is esp. long, enshorten it! (Do above)
# strategy_df %>% select(sic_namefrom_indstrat_short) %>% filter(qg('Motion picture video/television programme productio',sic_namefrom_indstrat_short)) %>% pull


#LOAD BRES 5 DIGIT
bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE423_localauthoritiescountyunitaryasofApril2023_2_Fulltimeemployees_2015_2024_SIC_5DIGIT.csv")

# Need to repeat LQ calcs for 5,4,3 and 2 digit level
# Summing jobs for each at each of those levels first
# We've got 5 already so that can just get LQ'd immediately

# Version to function up
# bres4 = bres %>% 
#   mutate(SIC4 = str_sub(SIC_5DIGIT_CODE,1,4)) %>% 
#   group_by(DATE,GEOGRAPHY_NAME,SIC4) %>% 
#   summarise(
#     JOBCOUNT = sum(JOBCOUNT)
#   )

# Funtioned up - get bres job counts for 2 to 4 digit SICs
# Actually, just adding 5 digit in here rather than tack on
breses = map(2:5,bres_countjobs_by_SICdigitlevel,bres)

# Check... tick
# breses[[1]] %>% View
# breses[[4]] %>% View

# Each of those can now separately get LQd up, along with the 5 digit
# Have functioned this up to get all the various bits needed
# I *think* this should be OK - each can attach separately to different SIC levels in IndStrat?
# Let's see

# Checking how to pass in by column index to the LQ function
# levelcolname = names(breses[[1]])[3]
# breses[[1]] %>% 
#   group_split(DATE) %>% 
#   map(
#     add_location_quotient_and_proportions,
#     regionvar = GEOGRAPHY_NAME,
#     # lq_var = sic2,
#     lq_var = !!sym(levelcolname),
#     valuevar = JOBCOUNT
#   ) %>% 
#   bind_rows() %>% View

# Test function... tick
# getLQs_and_attachedstuff(breses[[1]])

lq_results = map(breses,getLQs_and_attachedstuff)

# Looking OK, though note actually using smoothed vals might be a good idea
# i.e. cos yeartoplot is the final year values, prob a bit volatile esp at 5 digit
# Let's see
chk = lq_results[[1]]
chk = lq_results[[2]] %>% g


# So now we need to pull out relevant bits from each for the different IndStrat SICs
# Explicit join will work, no need for fuzzy join
# Except... think it requires combining the 2 to 5 digit lq results into a single DF to then match codes on
# Separately for year results and whole results

# What we want:
# A single islq and yeartoplot for the ind strat sectors
# Pulled from the appropriate SIC level BRES data

# Actually fine
pryr::object_size(lq_results)

# Pull out the first dfs from each and process
lqs <- map(lq_results[1:4], ~ .x$lqs %>% 
             mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
             rename(sic = names(.)[3])) %>% bind_rows

# check
lqs %>% filter(siclevel == 'sic5') %>% View

# Same for yeartoplots
yeartoplots <- map(lq_results[1:4], ~ .x$yeartoplot %>% 
                     mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
                     rename(sic = names(.)[3])) %>% bind_rows

# check
yeartoplots %>% filter(siclevel == 'sic5') %>% View



# So now in theory we can just do a join on the indstrat sic codes, yes?
# Ignore many to many warnings, that's what we want
lqs_for_indstrat = lqs %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
lqs_for_indstrat %>% filter(sic == 60, DATE == 2023, GEOGRAPHY_NAME == 'Sheffield') %>% View
lqs_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2023, GEOGRAPHY_NAME == 'Sheffield') %>% View

# Repeat for yeartoplots
yeartoplots_for_indstrat = yeartoplots %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
yeartoplots_for_indstrat %>% filter(sic == 60, DATE == 2024, GEOGRAPHY_NAME == 'Sheffield') %>% View
yeartoplots_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2024, GEOGRAPHY_NAME == 'Sheffield') %>% View


# Ticks all round
# In theory, we might be able to save those and use them. Let's see.
saveRDS(lqs_for_indstrat,'local/lqs_for_indstrat.rds')
saveRDS(yeartoplots_for_indstrat,'local/yeartoplots_for_indstrat.rds')







# BRES 2024 ITL2 LEVEL IndStrat----

#LOAD BRES 5 DIGIT for ITL2
bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_2_Fulltimeemployees_2022_2024_SIC_5DIGIT.csv")

# Need to repeat LQ calcs for 5,4,3 and 2 digit level
# Summing jobs for each at each of those levels first
# We've got 5 already so that can just get LQ'd immediately

# Version to function up
# bres4 = bres %>% 
#   mutate(SIC4 = str_sub(SIC_5DIGIT_CODE,1,4)) %>% 
#   group_by(DATE,GEOGRAPHY_NAME,SIC4) %>% 
#   summarise(
#     JOBCOUNT = sum(JOBCOUNT)
#   )

# Funtioned up - get bres job counts for 2 to 4 digit SICs
# Actually, just adding 5 digit in here rather than tack on
breses = map(2:5,bres_countjobs_by_SICdigitlevel,bres)

# Check... tick
# breses[[1]] %>% View
# breses[[4]] %>% View

# Each of those can now separately get LQd up, along with the 5 digit
# Have functioned this up to get all the various bits needed
# I *think* this should be OK - each can attach separately to different SIC levels in IndStrat?
# Let's see

# Checking how to pass in by column index to the LQ function
# levelcolname = names(breses[[1]])[3]
# breses[[1]] %>% 
#   group_split(DATE) %>% 
#   map(
#     add_location_quotient_and_proportions,
#     regionvar = GEOGRAPHY_NAME,
#     # lq_var = sic2,
#     lq_var = !!sym(levelcolname),
#     valuevar = JOBCOUNT
#   ) %>% 
#   bind_rows() %>% View

# Test function... tick
# getLQs_and_attachedstuff(breses[[1]])

lq_results = map(breses,getLQs_and_attachedstuff)

# Looking OK, though note actually using smoothed vals might be a good idea
# i.e. cos yeartoplot is the final year values, prob a bit volatile esp at 5 digit
# Let's see
chk = lq_results[[1]]
chk = lq_results[[2]] %>% g


# So now we need to pull out relevant bits from each for the different IndStrat SICs
# Explicit join will work, no need for fuzzy join
# Except... think it requires combining the 2 to 5 digit lq results into a single DF to then match codes on
# Separately for year results and whole results

# What we want:
# A single islq and yeartoplot for the ind strat sectors
# Pulled from the appropriate SIC level BRES data

# Actually fine
# pryr::object_size(lq_results)

# Pull out the first dfs from each and process
lqs <- map(lq_results[1:4], ~ .x$lqs %>% 
             mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
             rename(sic = names(.)[3])) %>% bind_rows

# check
# lqs %>% filter(siclevel == 'sic5') %>% View

# Same for yeartoplots
yeartoplots <- map(lq_results[1:4], ~ .x$yeartoplot %>% 
                     mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
                     rename(sic = names(.)[3])) %>% bind_rows

# check
# yeartoplots %>% filter(siclevel == 'sic5') %>% View



# So now in theory we can just do a join on the indstrat sic codes, yes?
# Ignore many to many warnings, that's what we want
lqs_for_indstrat = lqs %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
# lqs_for_indstrat %>% filter(sic == 60, DATE == 2023, GEOGRAPHY_NAME == 'South Yorkshire') %>% View
# lqs_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2023, GEOGRAPHY_NAME == 'South Yorkshire') %>% View

# Repeat for yeartoplots
yeartoplots_for_indstrat = yeartoplots %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
yeartoplots_for_indstrat %>% filter(sic == 60, DATE == 2024, GEOGRAPHY_NAME == 'South Yorkshire') %>% View
yeartoplots_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2024, GEOGRAPHY_NAME == 'South Yorkshire') %>% View


# Ticks all round
# In theory, we might be able to save those and use them. Let's see.
saveRDS(lqs_for_indstrat,'local/ITL2_lqs_for_indstrat.rds')
saveRDS(yeartoplots_for_indstrat,'local/ITL2_yeartoplots_for_indstrat.rds')




# PROCESS COMPANIES HOUSE FOR INDSTRAT WITH CORRECT METHOD USED IN BRES ABOVE----

# See below for the original (incorrect) code. Redo.
# Don't need SIC codes put in for now.
ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds')

#Somewhere I've got timepoint change between last two already done
#Taking from BradfordExplore.R here
#https://github.com/DanOlner/RegionalEconomicTools/blob/a517bdd5b37d3696de080160c68d772921afe4fa/bits_of_code/BradfordExplore.R#L1139

# Get 5 digit job sums per local authority (matching BRES structure)
ch.5digitsums <- ch %>% 
  st_set_geometry(NULL) %>% #don't forget this!!
  filter(!is.na(Employees_thisyear) & !is.na(Employees_lastyear)) %>% #Keep only firms with employees in BOTH years even if it's zero
  select(CompanyName,CompanyNumber,accountcode,CompanyCategory,
         incorporationdate_formatted,age_of_firm_years,localauthority_code:ITL221NM,
         Employees_thisyear,Employees_lastyear,SIC_5DIGIT_CODE) %>% 
  group_by(SIC_5DIGIT_CODE,localauthority_name) %>% 
  summarise(
    employeecount_thisyear = sum(Employees_thisyear),
    employeecount_lastyear = sum(Employees_lastyear)
  ) %>% ungroup()

#Make those into pseudo dates in an order we can get an LQ size change from
# Rename columns to more closely match BRES structure, so we can re-use functions
ch.5digitsums.long <- ch.5digitsums %>% 
  pivot_longer(employeecount_thisyear:employeecount_lastyear, names_to = 'timepoint', values_to = 'JOBCOUNT') %>% 
  mutate(
    DATE = ifelse(timepoint == 'employeecount_lastyear', 1,2)
  ) %>% 
  rename(GEOGRAPHY_NAME = localauthority_name)


# Now create summed versions for 2 to 5 digit groupings as with BRES
ches = map(2:5,bres_countjobs_by_SICdigitlevel,ch.5digitsums.long)

# Tick!
# ches[[4]] %>% View

# LQ that up
lq_results = map(ches,getLQs_and_attachedstuff)

# Looking OK
lq_results[[1]] 
lq_results[[2]] %>% g

# Pull out the first dfs from each and process
lqs <- map(lq_results[1:4], ~ .x$lqs %>% 
             mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
             rename(sic = names(.)[3])) %>% bind_rows

# check
lqs %>% filter(siclevel == 'sic5') %>% View
lqs %>% filter(siclevel == 'sic2') %>% View

# Same for yeartoplots
yeartoplots <- map(lq_results[1:4], ~ .x$yeartoplot %>% 
                     mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
                     rename(sic = names(.)[3])) %>% bind_rows

# check
yeartoplots %>% filter(siclevel == 'sic2') %>% View

# Join to indstrat!
# Ignore many to many warnings, that's what we want
lqs_for_indstrat = lqs %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
# lqs_for_indstrat %>% filter(sic == 60, DATE == 1, GEOGRAPHY_NAME == 'Sheffield') %>% View
# lqs_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 1, GEOGRAPHY_NAME == 'Sheffield') %>% View

# Repeat for yeartoplots
yeartoplots_for_indstrat = yeartoplots %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
# yeartoplots_for_indstrat %>% filter(sic == 60, DATE == 2, GEOGRAPHY_NAME == 'Sheffield') %>% View
# yeartoplots_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2, GEOGRAPHY_NAME == 'Sheffield') %>% View


# Ticks all round
# In theory, we might be able to save those and use them. Let's see.
saveRDS(lqs_for_indstrat,'local/CH_lqs_for_indstrat.rds')
saveRDS(yeartoplots_for_indstrat,'local/CH_yeartoplots_for_indstrat.rds')





# ITL2 level: PROCESS COMPANIES HOUSE FOR INDSTRAT WITH CORRECT METHOD USED IN BRES ABOVE----

# See below for the original (incorrect) code. Redo.
# Don't need SIC codes put in for now.
ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds')

#Somewhere I've got timepoint change between last two already done
#Taking from BradfordExplore.R here
#https://github.com/DanOlner/RegionalEconomicTools/blob/a517bdd5b37d3696de080160c68d772921afe4fa/bits_of_code/BradfordExplore.R#L1139

# Get 5 digit job sums per local authority (matching BRES structure)
ch.5digitsums <- ch %>% 
  st_set_geometry(NULL) %>% #don't forget this!!
  filter(!is.na(Employees_thisyear) & !is.na(Employees_lastyear)) %>% #Keep only firms with employees in BOTH years even if it's zero
  select(CompanyName,CompanyNumber,accountcode,CompanyCategory,
         incorporationdate_formatted,age_of_firm_years,localauthority_code:ITL221NM,
         Employees_thisyear,Employees_lastyear,SIC_5DIGIT_CODE) %>% 
  group_by(SIC_5DIGIT_CODE,ITL221NM) %>% 
  summarise(
    employeecount_thisyear = sum(Employees_thisyear),
    employeecount_lastyear = sum(Employees_lastyear)
  ) %>% ungroup()

#Make those into pseudo dates in an order we can get an LQ size change from
# Rename columns to more closely match BRES structure, so we can re-use functions
ch.5digitsums.long <- ch.5digitsums %>% 
  pivot_longer(employeecount_thisyear:employeecount_lastyear, names_to = 'timepoint', values_to = 'JOBCOUNT') %>% 
  mutate(
    DATE = ifelse(timepoint == 'employeecount_lastyear', 1,2)
  ) %>% 
  rename(GEOGRAPHY_NAME = ITL221NM)


# Now create summed versions for 2 to 5 digit groupings as with BRES
ches = map(2:5,bres_countjobs_by_SICdigitlevel,ch.5digitsums.long)

# Tick!
# ches[[4]] %>% View

# LQ that up
lq_results = map(ches,getLQs_and_attachedstuff)

# Looking OK
# lq_results[[1]] 
# lq_results[[2]] %>% g

# Pull out the first dfs from each and process
lqs <- map(lq_results[1:4], ~ .x$lqs %>% 
             mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
             rename(sic = names(.)[3])) %>% bind_rows

# check
# lqs %>% filter(siclevel == 'sic5') %>% View
# lqs %>% filter(siclevel == 'sic2') %>% View

# Same for yeartoplots
yeartoplots <- map(lq_results[1:4], ~ .x$yeartoplot %>% 
                     mutate(siclevel = names(.)[3], LQ = ifelse(is.nan(LQ),0,LQ)) %>% 
                     rename(sic = names(.)[3])) %>% bind_rows

# check
# yeartoplots %>% filter(siclevel == 'sic2') %>% View

# Join to indstrat!
# Ignore many to many warnings, that's what we want
lqs_for_indstrat = lqs %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
# lqs_for_indstrat %>% filter(sic == 60, DATE == 1, GEOGRAPHY_NAME == 'Sheffield') %>% View
# lqs_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 1, GEOGRAPHY_NAME == 'Sheffield') %>% View

# Repeat for yeartoplots
yeartoplots_for_indstrat = yeartoplots %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
# yeartoplots_for_indstrat %>% filter(sic == 60, DATE == 2, GEOGRAPHY_NAME == 'South Yorkshire') %>% View
# yeartoplots_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2, GEOGRAPHY_NAME == 'South Yorkshire') %>% View


# Ticks all round
# In theory, we might be able to save those and use them. Let's see.
saveRDS(lqs_for_indstrat,'local/ITL2_CH_lqs_for_indstrat.rds')
saveRDS(yeartoplots_for_indstrat,'local/ITL2_CH_yeartoplots_for_indstrat.rds')





# SOME CHECKS----

lqs_for_indstrat = readRDS('local/lqs_for_indstrat.rds')
lqs_for_indstrat %>% filter(GEOGRAPHY_NAME == 'Sheffield', qg('sound rec',sic_namefrom_indstrat_combo)) %>% View

lqs_for_indstrat.ch = readRDS('local/CH_lqs_for_indstrat.rds')
lqs_for_indstrat.ch %>% filter(GEOGRAPHY_NAME == 'Sheffield', qg('sound rec',sic_namefrom_indstrat_combo)) %>% View



# TEST PLOTS----

# Using the data above. Proportion plots, including comp to core cities (which would require LQing those up)
# Let's see about LQing up all core cities, using the functions above to get the different indstrat levels

# BRES first


## Reducing base plot just to core cities----

greplsector = 'CREATIVE'

islq = readRDS('local/lqs_for_indstrat.rds') %>% filter(qg(greplsector,sic_namefrom_indstrat_combo))
yeartoplot = readRDS('local/yeartoplots_for_indstrat.rds') %>% filter(qg(greplsector,sic_namefrom_indstrat_combo))

place = "Sheffield"

sectorLQorder <- islq %>% filter(
  DATE == max(DATE),#use latest data
  GEOGRAPHY_NAME == place) %>% 
  arrange(-LQ) %>% 
  select(sic_namefrom_indstrat_combo) %>% 
  pull()

#Turn the sector column into a factor and order by LQs
yeartoplot$sic_namefrom_indstrat_combo <- factor(yeartoplot$sic_namefrom_indstrat_combo, levels = sectorLQorder, ordered = T)

#Remove some LQs
yeartoplot.lqfiltered <- yeartoplot %>% filter(LQ > 0 & LQ < 100, JOBCOUNT >= minjobs)

#Drop any sectors that Bradford now doesn't have after that filter
sectorstokeep <- yeartoplot.lqfiltered %>% filter(GEOGRAPHY_NAME == place) %>% select(sic_namefrom_indstrat_combo) %>% pull

yeartoplot.lqfiltered <- yeartoplot.lqfiltered %>% filter(sic_namefrom_indstrat_combo %in% sectorstokeep)

# Keep only core cities in base plot, make more prominent
corecities <- yeartoplot.lqfiltered$GEOGRAPHY_NAME[grepl(x = yeartoplot.lqfiltered$GEOGRAPHY_NAME, pattern = 'sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Tyne|Nottingham|Edinburgh', ignore.case = T)] %>% unique

#Yes, all in there, need to remove a few...
corecities <- corecities[!grepl(x = corecities, pattern = 'Greater|shire|north tyne|south tyne', ignore.case = T)]
corecities <- corecities[order(corecities)]

# Add in Belfast! It'll get filtered out but need for completion
corecities = c(corecities,'Belfast')

# Save that list for ease of loading
saveRDS(corecities,'data/corecitiesvector.rds')

# Remove sheffield also, don't want that labelled
corecities = corecities[!qg('sheffield',corecities)]


corecities_baselayer = yeartoplot.lqfiltered %>% filter(GEOGRAPHY_NAME %in% corecities)

# Add in placename abbreviations
corecities_baselayer = corecities_baselayer %>% 
  mutate(
    placename_short = str_sub(GEOGRAPHY_NAME,1,2)
  )

# Unique but will need a key!
unique(corecities_baselayer$placename_short)

# debugonce(LQ_baseplot)
p <- LQ_baseplot(df = corecities_baselayer %>% filter(is_frontier == 1), alpha = 0.8, shape = 0, sector_name = sic_namefrom_indstrat_combo, LQ_column = LQ, change_over_time = slope, labelcolumn = placename_short)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = JOBCOUNT, sector_regional_proportion = sector_regional_proportion,
                            region_name = GEOGRAPHY_NAME,
                            sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = F)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat frontier sector")

p


## CCI AS A WHOLE CHANGE OVER TIME IN BRES----

# Of relative concentration levels since % is easier to track
lqs_for_indstrat = readRDS('local/lqs_for_indstrat.rds')

# Job percents of economy will sum correctly here, they're each from the right overall count
# But we just have to make sure to remove any nested SICs...

# Just for CCIs
cci = lqs_for_indstrat %>% filter(qg('creative', sic_namefrom_indstrat_combo))

# Nested SICs to remove:
# 5821 is covered by 58
# 591 is covered by 59
# 592 is covered by 59
# 602 is covered by 60
# 62011 is covered by 6201
unique(cci$sic)

# Filter accordingly
cci = cci %>% filter(!sic %in% c('5821','602','591','592','62011'))

# Actually, we do have region total size here if we want to repeat
# Or can just sum sector_regional_proportion...
# Having plotted, it does need smoothing...
ccisums = cci %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  summarise(
    percent_indstrat_cci = sum(sector_regional_proportion) * 100,
    totaljobs = sum(JOBCOUNT)
    ) %>% 
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    percent_indstrat_cci_movingav = rollapply(percent_indstrat_cci,3,mean,align='center',fill=NA),
    totaljobs_movingav = rollapply(totaljobs,3,mean,align='center',fill=NA)
  ) %>% 
  ungroup()

# May want to smooth, let's see. But...
corecities = readRDS('data/corecitiesvector.rds')

cciplot = ccisums %>% 
  filter(GEOGRAPHY_NAME %in% c(corecities,c('Barnsley','Doncaster','Rotherham')))


# Random pastel colours
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(cciplot$GEOGRAPHY_NAME))
set.seed(13)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[10:(10+(n-1))]

# Pick final smoothed year to order factor
placeorder = cciplot %>% 
  filter(DATE == 2023) %>% #centre of 3 year smooth final point
  arrange(-percent_indstrat_cci_movingav) %>% 
  select(GEOGRAPHY_NAME) %>% 
  pull

cciplot = cciplot %>% 
  mutate(
    GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME, levels = placeorder)
  )

# Save for qml
saveRDS(cciplot,'local/cciplot.rds')

# PLOOOOT
ggplot(cciplot %>% filter(!is.na(percent_indstrat_cci_movingav)),
       aes(x = DATE, 
           y = percent_indstrat_cci_movingav, 
                   colour = GEOGRAPHY_NAME
                   # colour = fct_reorder(GEOGRAPHY_NAME,-percent_indstrat_cci_movingav) 
           )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank())

# Version with all data, non smoothed...
# Doesn't pick up on any job drop
ggplot(cciplot,
       aes(x = DATE, y = percent_indstrat_cci,
           colour = GEOGRAPHY_NAME
           # colour = fct_reorder(GEOGRAPHY_NAME,-percent_indstrat_cci_movingav)
       )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank())






# Check whether that's actually jobs growth or shrinkage by looking at log actual job numbers
# Break down into facets if nec
ggplot(cciplot %>% filter(!is.na(percent_indstrat_cci_movingav)),
       aes(x = DATE, y = totaljobs_movingav, 
                   colour = GEOGRAPHY_NAME
                   # colour = fct_reorder(GEOGRAPHY_NAME,-percent_indstrat_cci_movingav) 
           )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank()) +
  scale_y_log10()



## ALL INDSTRAT AS PROPORTION: CHANGE OVER TIME IN BRES----

# Of relative concentration levels since % is easier to track
lqs_for_indstrat = readRDS('local/lqs_for_indstrat.rds')

# Job percents of economy will sum correctly here, they're each from the right overall count
# But we just have to make sure to remove any nested or repeated SICs...
# Get unique SICs
# Check if any have higher code values
# If duplicates, 
# We'll need to manually check which indstrat grouping we want to keep
uniquesics = unique(lqs_for_indstrat$sic)

# Check with one we know... tick
# ^ is regex for begins with...
# uniquesics[which(qg(paste0('^',59),uniquesics))]

# Function moved to adhoc_functions
map(uniquesics, checksic)

# Have to do this separately for frontier vs non-frontier
# As well as a combo that includes both / excludes overlap / includes foundation sectors

# To get correct %s for each
# These check each subset against the full list of SICs set above
# map(unique(lqs_for_indstrat$sic[lqs_for_indstrat$is_frontier == 0]), checksic)
# map(unique(lqs_for_indstrat$sic[lqs_for_indstrat$is_frontier == 1]), checksic)
# map(unique(lqs_for_indstrat$sic[lqs_for_indstrat$is_frontier == 2]), checksic)


# What's a nice way to keep only e.g. 26 given we've got 261,262... 26701?
# We could drop the first element of each list item then NOT keep the others?
dropfirst = map(uniquesics, checksic)
dropfirst = map(dropfirst, ~.x[-1])
# Removing these will leave only ones we can sum to get total IndStrat jobs
dropthese = unlist(dropfirst)


# ISSUE!
# If we're grouping by IS-8 industrial strategy codes
# Some SICs duplicated that I've removed when keeping just unique values
# Should NOT be dropped, because they're unique to each IS-8 - can't get total without them

# So how to keep unique sectors per IS-8?
# Create a list of uniques for each, then combine
# Then when we summarise below per IS-8, it should sum correctly

# So let's add in the IS-8 code again...
# Add indstrat grouping for option to plot each separately
# https://stackoverflow.com/questions/41321768/regex-to-match-a-string-after-colon
lqs_for_indstrat$indstrat_code = gsub(pattern = ":(.*)", replacement = "", lqs_for_indstrat$sic_namefrom_indstrat_combo)

# Then redo the function so it gets unique SICs per IS-8
# We can then keep uniques from that
# Stuck into ad hoc functions to debug

# debugonce(droplist_foreachIS8)
# Give top level the is8 names so we can access directly
# Use purrr method...
is8drops = unique(lqs_for_indstrat$indstrat_code) %>% set_names() %>% map(droplist_foreachIS8)

# ... rather than this
# names(is8drops) <- sapply(is8drops, `[[`, "is8")

# Some checks 
unique(lqs_for_indstrat$sic_namefrom_indstrat_combo)[qg('creative',unique(lqs_for_indstrat$sic_namefrom_indstrat_combo))]

# Filter accordingly... just testing one IS-8 to start with
# uniqueindstrat = lqs_for_indstrat %>% filter(!sic %in% dropthese)
uniqueindstrat = lqs_for_indstrat %>% 
  filter(
    indstrat_code == is8drops[['CREATIVE']]$is8,
    !sic %in% is8drops[['CREATIVE']]$dropthese
  )

# Test - should be no more nested SICs... tick
# uniquesics = unique(uniqueindstrat$sic)
# map(unique(uniqueindstrat$sic), checksic)

# Look at what unique combos are left
# unique(uniqueindstrat$sic_namefrom_indstrat_combo)

 
# All looks OK (creative now matching previous version used in the CCI analysis)
# To be sure we get the right sums, best to repeat for each IS-8 separately then recombine

indstratsums_perIS8 = function(is8name){

  lqs_for_indstrat %>% 
    filter(
      indstrat_code == is8name,
      !sic %in% is8drops[[is8name]]$dropthese
      ) %>% 
    group_by(DATE,GEOGRAPHY_NAME) %>% 
    summarise(
      percent_all_indstrat = sum(sector_regional_proportion) * 100,
      totaljobs = sum(JOBCOUNT)
    ) %>% 
    arrange(DATE) %>% 
    group_by(GEOGRAPHY_NAME) %>% 
    mutate(
      percent_all_indstrat_movingav = rollapply(percent_all_indstrat,3,mean,align='center',fill=NA),
      totaljobs_movingav = rollapply(totaljobs,3,mean,align='center',fill=NA)
    ) %>% 
    ungroup() %>% 
    mutate(indstrat_code = is8name)

}

# Just check again with single IS8 we already know... tick
# indstrat_sums = indstratsums_perIS8('CREATIVE')

# Repeat for all
indstrat_sums = bind_rows(
  map(names(is8drops), indstratsums_perIS8)
)

# Save that for use elsewhere
saveRDS(indstrat_sums,'local/indstrat_sums.rds')


# May want to smooth, let's see. But...
corecities = readRDS('data/corecitiesvector.rds')

indstrat_plot = indstrat_sums %>% 
  filter(GEOGRAPHY_NAME %in% c(corecities,c('Barnsley','Doncaster','Rotherham')))


# Random pastel colours
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(indstrat_plot$GEOGRAPHY_NAME))
set.seed(13)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[14:(14+(n-1))]

# Pick final smoothed year to order factor
placeorder = indstrat_plot %>% 
  filter(DATE == 2023) %>% #centre of 3 year smooth final point
  arrange(-percent_all_indstrat_movingav) %>% 
  select(GEOGRAPHY_NAME) %>% 
  distinct() %>% 
  pull

indstrat_plot = indstrat_plot %>% 
  mutate(
    GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME, levels = placeorder)
  )

# Save for qml
# saveRDS(indstrat_plot,'local/allindstrat_plot.rds')

# PLOOOOT
p = ggplot(indstrat_plot %>% filter(!is.na(percent_all_indstrat_movingav)) %>% 
             mutate(selectplace = GEOGRAPHY_NAME %in% c('Rotherham','Doncaster','Barnsley','Sheffield')),
       aes(x = DATE, 
           y = percent_all_indstrat_movingav, 
           colour = GEOGRAPHY_NAME,
           size = selectplace
           # colour = fct_reorder(GEOGRAPHY_NAME,-percent_indstrat_cci_movingav) 
       )) +
  geom_line() +
  geom_point() +
  scale_size_manual(values = c(0.3,1)) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~indstrat_code, scales = 'free_y', ncol = 2)

gp = ggplotly(p, tooltip = 'GEOGRAPHY_NAME')

htmlwidgets::saveWidget(gp, "docs/miscimages/indstrat_groups_BRESjobs.html")

# Version with all data, non smoothed...
# Doesn't pick up on any job drop
ggplot(indstrat_plot,
       aes(x = DATE, y = percent_all_indstrat,
           colour = GEOGRAPHY_NAME
           # colour = fct_reorder(GEOGRAPHY_NAME,-percent_indstrat_cci_movingav)
       )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank())



# Some batty things going on there
# Quick look at all data for some places
roth = uniqueindstrat %>% filter(GEOGRAPHY_NAME == 'Rotherham')
# roth = uniqueindstrat %>% filter(GEOGRAPHY_NAME == 'Salford') 

# Break into groups based on latest year's numbers
roth.groups = roth %>% filter(DATE == 2024) %>% 
  mutate(
    jobsizegroups = as.numeric(cut_number(JOBCOUNT,3))
  )

roth = roth %>% 
  left_join(
    roth.groups %>% select(jobsizegroups,sic_namefrom_indstrat_combo),
    by = 'sic_namefrom_indstrat_combo'
  )




# All indstrat sectors job counts
p = ggplot(roth, aes(x = DATE, y = JOBCOUNT, colour = sic_namefrom_indstrat_combo)) +
  geom_line() +
  geom_point() +
  facet_wrap(~jobsizegroups, scales = 'free_y')
  # scale_y_log10()

ggplotly(p, tooltip = 'sic_namefrom_indstrat_combo')





# Look at defence - lots of places without job count, right?
indstrat_sums %>% filter(qg('defence',indstrat_code)) %>% View

# Check same just for core cities / SY
indstrat_sums %>% filter(qg('defence',indstrat_code), GEOGRAPHY_NAME %in% c(corecities,'Rotherham','Doncaster','Barnsley')) %>% View


# Proportion of places with zero jobs in those SICs
propwithzero = indstrat_sums %>% filter(qg('defence',indstrat_code)) %>% 
  group_by(DATE) %>% 
  summarise(
    percent_with_zero = mean(totaljobs == 0) * 100,
    percent_withmorethan100 = mean(totaljobs > 100) * 100
    ) %>% 
  ungroup()

# So we can do a log plot to see who / where




## GVA for ARTS/ENT SECTION----

# Let's just try and quickly get GVA for the appropriate SIC section as well for those...
# Taken from Leeds/Bradford linkage doc
itl3.sections.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_SECTION_MINUSimputedrent_LONG_2023.csv') 
#itl3.sections.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_SECTION_MINUSimputedrent_LONG_2023.csv') 

#There's a single minus one value, set to zero
itl3.sections.cp[itl3.sections.cp == -1] <- 0

itl3.sections.cp <- itl3.sections.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()

itl3.sections.cp <- itl3.sections.cp %>% 
  filter(!qg("Activities of households|agri",SIC07_description)) %>% 
  mutate(
    SIC07_description = case_when(
      grepl('defence',SIC07_description,ignore.case = T) ~ 'public/defence',
      grepl('support',SIC07_description,ignore.case = T) ~ 'admin/support',
      grepl('financ',SIC07_description,ignore.case = T) ~ 'finance/insu',
      grepl('agri',SIC07_description,ignore.case = T) ~ 'Agri/mining',
      grepl('electr',SIC07_description,ignore.case = T) ~ 'power/water',
      grepl('information',SIC07_description,ignore.case = T) ~ 'ICT',
      grepl('manuf',SIC07_description,ignore.case = T) ~ 'Manuf',
      grepl('other',SIC07_description,ignore.case = T) ~ 'other',
      grepl('scientific',SIC07_description,ignore.case = T) ~ 'Scientific',
      grepl('real estate',SIC07_description,ignore.case = T) ~ 'Real est',
      grepl('transport',SIC07_description,ignore.case = T) ~ 'Transport',
      grepl('entertainment',SIC07_description,ignore.case = T) ~ 'Arts/Ent/Rec',
      grepl('human health',SIC07_description,ignore.case = T) ~ 'Health/soc',
      grepl('food service activities',SIC07_description,ignore.case = T) ~ 'Food/service',
      grepl('wholesale',SIC07_description,ignore.case = T) ~ 'Retail',
      .default = SIC07_description
    )
  )

smoothband = 3

#Get colours same for same SIC codes
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(itl3.sections.cp$SIC07_description))
set.seed(12)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[10:(10+(n-1))]


#Find the average within each place and each sector in each place
#Year is already in order, so this will find 3 year moving av between consecutive years
itl3.sections.cp <- itl3.sections.cp %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA),
    sector_regional_percent_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='center',fill=NA) * 100
  ) %>% 
  ungroup()


# plot these places
sections.plot = itl3.sections.cp %>% 
  filter(
    qg('arts',SIC07_description),
    Region_name %in% c(corecities,c('Barnsley','Doncaster','Rotherham')),
    !is.na(year)
    )

unique(sections.plot$Region_name)

placeorder = sections.plot %>% 
  filter(year == 2022) %>% #centre of 3 year smooth final point
  arrange(-sector_regional_percent_movingav) %>% 
  select(Region_name) %>% 
  distinct() %>% 
  pull

sections.plot = sections.plot %>% 
  mutate(
    Region_name = factor(Region_name, levels = placeorder)
  )

# PLOOOOT
ggplot(sections.plot,
       aes(x = year, 
           # y = sector_regional_percent_movingav,
           y = sector_regional_proportion,
           colour = Region_name
           )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank())




## GVA for 2 DIGITS 58,59,60,90 SUMMED----

# Let's just try and quickly get GVA for the appropriate SIC section as well for those...
# Taken from Leeds/Bradford linkage doc
itl3.2dig.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv') 

# Check what codes we have at this level
# 58 to 60, 90 to 91 grouped
# All good
unique(itl3.2dig.cp$SIC07_code)

#There's a single minus one value, set to zero
itl3.2dig.cp[itl3.2dig.cp == -1] <- 0

# Sum those sectors into a single CCI grouping and drop the others to get accurate total proportions
itl3.2dig.cp.ccisummed = itl3.2dig.cp %>% 
  filter(
    SIC07_code %in% c('58-60','90-91')
  ) %>%
  group_by(year,Region_name) %>% 
  summarise(
    value = sum(value),
    SIC07_code = '58-60,90-91',
    SIC07_description = 'CCI',
    ITL_code = max(ITL_code)#keep this to match cols so we can bind_rows
  )
  
# Drop those sectors from orig and add this new grouped one in
itl3.cci.cp = itl3.2dig.cp %>% 
  filter(
    !SIC07_code %in% c('58-60','90-91')
  ) %>% 
  bind_rows(itl3.2dig.cp.ccisummed)

# Should now be good for finding regional GVA proportions correctly for CCI grouping
itl3.cci.cp <- itl3.cci.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()

# Shorten sector names
itl3.cci.cp <- itl3.cci.cp %>% 
  mutate(
    SIC07_description_short = removecommonSICnameelements(SIC07_description, removemanuf = T, removeactivities = T)
  )


smoothband = 3



#Find the average within each place and each sector in each place
#Year is already in order, so this will find 3 year moving av between consecutive years
itl3.cci.cp <- itl3.cci.cp %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA),
    sector_regional_percent_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='center',fill=NA) * 100
  ) %>% 
  ungroup()


# plot these places
sections.plot = itl3.cci.cp %>% 
  filter(
    qg('cci',SIC07_description),
    Region_name %in% c(corecities,c('Barnsley','Doncaster','Rotherham','Cardiff and Vale of Glamorgan','Tyneside')),
    !is.na(year)
    )

unique(sections.plot$Region_name)

placeorder = sections.plot %>% 
  filter(year == 2023) %>% #centre of 3 year smooth final point
  arrange(-sector_regional_proportion) %>% 
  select(Region_name) %>% 
  distinct() %>% 
  pull

# placeorder = sections.plot %>% 
#   filter(year == 2022) %>% #centre of 3 year smooth final point
#   arrange(-sector_regional_percent_movingav) %>% 
#   select(Region_name) %>% 
#   distinct() %>% 
#   pull

sections.plot = sections.plot %>% 
  mutate(
    Region_name = factor(Region_name, levels = placeorder)
  )

# Save for qml
saveRDS(sections.plot,'local/cci_gva_plot.rds')

#Get colours same for same SIC codes
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(sections.plot$Region_name))
set.seed(12)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[12:(12+(n-1))]


# PLOOOOT
ggplot(sections.plot %>% filter(year >= 2015),
       aes(x = year, 
           # y = sector_regional_percent_movingav,
           y = sector_regional_proportion,
           colour = Region_name
           )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank())








# COMPANIES HOUSE INDIV FIRMS / PERCENT CHANGE----

# Taken from Bradford report
firm.change <- ch %>% 
  filter(localauthority_name %in% c('Sheffield','Barnsley','Doncaster','Rotherham'), 
         Employees_thisyear >= 1 & Employees_lastyear >= 1) %>%
  # filter(localauthority_name == 'Sheffield', Employees_thisyear >= 1 & Employees_lastyear >= 1) %>%
  mutate(
    employee_percentchange = percent_change(Employees_lastyear,Employees_thisyear)
  )


# unique(firm.change$localauthority_name)

# Add in list of CCI sectors (keeping top level if nested SICs)
# Get those from here:
lqs_for_indstrat = readRDS('local/lqs_for_indstrat.rds')
cci = lqs_for_indstrat %>% filter(qg('creative', sic_namefrom_indstrat_combo))
# Nested SICs to remove:
# 591 is covered by 59
# 592 is covered by 59
# 62011 is covered by 6201
unique(cci$sic)
# Filter accordingly
cci.sics = cci %>% filter(!sic %in% c('591','592','62011')) %>% 
  select(sic,sic_namefrom_indstrat_combo) %>% 
  distinct() %>% 
  mutate(
    sic_namefrom_indstrat_combo = gsub('CREATIVE: ','',sic_namefrom_indstrat_combo)
  )

# Add in - fuzzy join, given variable SIC code lengths
firm.change = firm.change %>%
  fuzzyjoin::regex_left_join(
    cci.sics %>% mutate(sicregex = paste0("^", sic)),
    by = c("SIC_5DIGIT_CODE" = "sicregex")
  ) %>% 
  filter(!is.na(sic_namefrom_indstrat_combo))# And keep only CCI sectors


firm.change <- firm.change %>% 
  mutate(
    sic_namefrom_indstrat_combo = fct_reorder(sic_namefrom_indstrat_combo, employee_percentchange),
    `Firm: ` = paste0(CompanyName,', ',Employees_lastyear,' >> ',Employees_thisyear),
    sizecategory = case_when(
      between(Employees_thisyear,1,1) ~ "1",
      between(Employees_thisyear,2,4) ~ "2-4",
      between(Employees_thisyear,5,9) ~ "5-9",
      between(Employees_thisyear,10,20) ~ "10-20",
      between(Employees_thisyear,21,99999) ~ "21+"
      # between(Employees_thisyear,51,999999) ~ "51+"
    ),
    sizecategory = factor(sizecategory, levels = rev(c('1','2-4','5-9','10-20','21+')))
  ) %>% 
  filter(!is.na(sic_namefrom_indstrat_combo))

table(firm.change$sizecategory)

#FACET doesn't work well with plotly - things overlap
#Plot them separately.
p <- ggplot(firm.change %>% filter(sizecategory == "5-9"),
            aes(y = sic_namefrom_indstrat_combo, x = employee_percentchange, group = `Firm: `)) +
  geom_jitter(height = 0.1, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.5, colour = 'green') +
  ylab("") +
  # theme(
  #   # panel.grid.major = element_line(linetype = "dotted"),
  #   # axis.title.x = element_text( size = 10, margin=margin(80,80,80,80)),
  #   axis.title.y = element_text( size = 10, margin=margin(30,30,30,30))
  # ) +
  ggtitle("Firms with 10-20 employees")

ggplotly(p, tooltip = 'Firm: ')


# A better way might be by sector then size band
p <- ggplot(firm.change %>% filter(qg('arts',sic_namefrom_indstrat_combo)),
            aes(y = sizecategory, x = employee_percentchange, group = `Firm: `)) +
  geom_jitter(height = 0.3, alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.5, colour = 'green') +
  ylab("") +
  # theme(
  #   # panel.grid.major = element_line(linetype = "dotted"),
  #   # axis.title.x = element_text( size = 10, margin=margin(80,80,80,80)),
  #   axis.title.y = element_text( size = 10, margin=margin(30,30,30,30))
  # ) +
  ggtitle("Firms with 10-20 employees")

ggplotly(p, tooltip = 'Firm: ')


# Try just counting for these
firmcount <- firm.change %>%
  st_set_geometry(NULL) %>% 
  group_by(sic_namefrom_indstrat_combo) %>% 
  mutate(
    sizecategory = case_when(
      Employees_thisyear == 1 ~ "1",
      between(Employees_thisyear,2,4) ~ "2-4",
      between(Employees_thisyear,5,9) ~ "5-9",
      between(Employees_thisyear,10,20) ~ "10-20",
      between(Employees_thisyear,21,50) ~ "21-50",
      between(Employees_thisyear,51,999999) ~ "51+"
    ),
    sizecategory = factor(sizecategory, levels = c('1','2-4','5-9','10-20','21-50','51+'))
  )

subsector = firmcount %>% filter(qg('arts',sic_namefrom_indstrat_combo))

#table from that to plot
firmtable <- tibble(
  `Firm size` = levels(subsector$sizecategory),
  `Count` = table(subsector$sizecategory),
  `Percent of firms` = paste0(round(table(subsector$sizecategory) %>% prop.table() * 100,2),"%")
)

knitr::kable(firmtable, escape = FALSE, caption = "Table: Bradford count/% of firms by employee band")





# CCI SIC90 AND OTHERS: DIGGING DEEPER----

# They'd like to know more. Right-ee-ho. Nabbing these sources from above:

# BRES based
saveRDS(lqs_for_indstrat,'local/lqs_for_indstrat.rds')
saveRDS(yeartoplots_for_indstrat,'local/yeartoplots_for_indstrat.rds')

# Companies house based

























