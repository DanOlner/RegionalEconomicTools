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
    sic_namefrom_indstrat_short = removecommonSICnameelements(SIC_name, removemanuf = T, removeactivities = T),
    sic_namefrom_indstrat_short = ifelse(qg('Motion picture video/television programme productio',sic_namefrom_indstrat_short),
                                         "Film/TV prog production sound/music publishing",sic_namefrom_indstrat_short),
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
# 591 is covered by 59
# 62011 is covered by 6201
unique(cci$sic)

# Filter accordingly
cci = cci %>% filter(!sic %in% c('591','62011'))

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
       aes(x = DATE, y = percent_indstrat_cci_movingav, 
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
# ggplot(cciplot,
#        aes(x = DATE, y = percent_indstrat_cci, 
#            colour = GEOGRAPHY_NAME
#            # colour = fct_reorder(GEOGRAPHY_NAME,-percent_indstrat_cci_movingav) 
#        )) +
#   geom_line() +
#   geom_point(size = 1) +
#   scale_color_manual(values = randomcols) +
#   # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
#   theme(legend.title = element_blank())






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

#Get colours same for same SIC codes
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(itl3.cci.cp$SIC07_description))
set.seed(12)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[10:(10+(n-1))]


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

# Save for qml
saveRDS(sections.plot,'local/cci_gva_plot.rds')

# PLOOOOT
ggplot(sections.plot,
       aes(x = year, 
           y = sector_regional_percent_movingav,
           # y = sector_regional_proportion,
           colour = Region_name
           )) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_manual(values = randomcols) +
  # scale_x_continuous(breaks = c(2013,2017,2019,2021,2023)) +
  theme(legend.title = element_blank())





























