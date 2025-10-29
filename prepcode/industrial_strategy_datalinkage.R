# UK INDUSTRIAL STRATEGY DATA LINKAGES
# To BRES and Companies House data
# Used in e.g. SouthYorkshire_creativeindustries qml report
library(tidyverse)
library(nomisr)
library(stringdist)
library(sf)
library(zoo)
library(stringr)
source('functions/misc_functions.R')
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
    sic_namefrom_indstrat_short = removecommonSICnameelements(SIC_name, removemanuf = T),
    sic_namefrom_indstrat_short = ifelse(qg('Motion picture video/television programme productio',sic_namefrom_indstrat_short),
                                         "Film/TV programme production sound/music publishing activities",sic_namefrom_indstrat_short),
    sic_namefrom_indstrat_combo = paste0(toplevelindstrat,": ",sic_namefrom_indstrat_short, " (",sic_code,")")
  )

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
  rename(GEOGRAPHY_NAME = localauthority_name,)


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
lqs_for_indstrat %>% filter(sic == 60, DATE == 1, GEOGRAPHY_NAME == 'Sheffield') %>% View
lqs_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 1, GEOGRAPHY_NAME == 'Sheffield') %>% View

# Repeat for yeartoplots
yeartoplots_for_indstrat = yeartoplots %>% 
  inner_join(
    strategy_df %>% 
      mutate(sic_code = as.character(sic_code)) %>% 
      select(sic = sic_code,is_frontier,sic_namefrom_indstrat_combo),
    by = 'sic'
  )

# Check on our usual list of test sics
yeartoplots_for_indstrat %>% filter(sic == 60, DATE == 2, GEOGRAPHY_NAME == 'Sheffield') %>% View
yeartoplots_for_indstrat %>% filter(sic %in% c(58,5821,62011,6201), DATE == 2, GEOGRAPHY_NAME == 'Sheffield') %>% View


# Ticks all round
# In theory, we might be able to save those and use them. Let's see.
saveRDS(lqs_for_indstrat,'local/CH_lqs_for_indstrat.rds')
saveRDS(yeartoplots_for_indstrat,'local/CH_yeartoplots_for_indstrat.rds')

