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
# bradford.2digit.ft = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_2DIGIT.csv") %>% filter(qg('bradford',GEOGRAPHY_NAME))

# bradford.section.ft = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_SECTION.csv")
# 

#For the sunburst code, we only need to keep the jobcounts for the highest hierarchical level - 5 digit
#For the rest, we'll need to add in a join field on 5 digit

#Actually, just realised, we don't need anything BUT the 5 digit.
#Can then just join the other SIC lookup and we're more or less done.
bradford.5digit.ft = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv") %>% 
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
bradford.nuts.5digit.ft = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE437_nuts2016level3_2_Fulltimeemployees_2015_2022_SIC_5DIGIT.csv") %>% 
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




# LQ PLOT BUT FOR GVA VS BRES JOBS VS CH JOBS OVERLAID FOR BRADFORD----

#I've just done the BRES / GVA linking process in misc_checks.R
#Here: https://github.com/DanOlner/RegionalEconomicTools/blob/b047d96d6513005c3947dcda2833881e3f7cd0eb/prepcode/misc_checks.R#L1155

#Sooo. Can we stick both LQs for jobs and GVA on same plot for a place?
#And possibly for CH in the last year too? (Maybe av jobs over that time)
#CH would need to be point not trajectory

#Let's get both and see
#Note, we'll also be able to check sector proportion similarity betw BRES and CH
#Though correlation of counts might be more useful?

gvabres <- readRDS('data/regionalGVA_plus_BRESjobcounts/regionalGVA_currentprices_BRES_FT_jobcount_bespoke2digitSIC_nONLY_MATCHING_GEOGs_2015_2023.rds') %>% 
  filter(!qg('households|agri', SIC07_description))

shortsectornames <- read_csv('data/shortsectornames_for_regionalGVA_2digitSICs.csv')

gvabres <- gvabres %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )

#Get two versions of LQ/proportions - job count and GVA
gva.props <- gvabres %>% 
  group_split(DATE) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description_shortened,
      valuevar = GVA) %>% 
  bind_rows()

job.props <- gvabres %>% 
  group_split(DATE) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description_shortened,
      valuevar = JOBCOUNT) %>% 
  bind_rows()


#Can I combine these into a single df to be able to process in one place?
#I can label each variable type and use that to split the plot

#SO - keep just bradford, drop region name, replace with variable name, combine
gva.props.bradford <- gva.props %>% 
  filter(qg('Bradford',Region_name)) %>% 
  mutate(varname = "GVA") %>% 
  select(DATE,varname,SIC07_description_shortened,sector_regional_proportion,value = GVA,LQ,LQ_log)

job.props.bradford <- job.props %>% 
  filter(qg('Bradford',Region_name)) %>% 
  mutate(varname = "JOBCOUNT") %>% 
  select(DATE,varname,SIC07_description_shortened,sector_regional_proportion,value = JOBCOUNT,LQ,LQ_log)

#Should have same col names now... tick
table(names(gva.props.bradford) == names(job.props.bradford))

props.combo <- bind_rows(gva.props.bradford,job.props.bradford)


#Make calcs for trajectory
LQ_slopes <- compute_slope_or_zero(
  data = props.combo, 
  varname, SIC07_description_shortened,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = DATE)


#Filter down to a single year... we may want to smooth years, let's see
yeartoplot <- props.combo %>% filter(DATE == max(DATE))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('varname', 'SIC07_description_shortened')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- props.combo %>% 
  group_by(varname, SIC07_description_shortened) %>% 
  summarise(
    min_LQ_all_time = min(LQ),
    max_LQ_all_time = max(LQ)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('varname', 'SIC07_description_shortened')
  )



sectorLQorder <- props.combo %>% filter(
  DATE == max(DATE)#use latest data
) %>% 
  select(varname, SIC07_description_shortened, LQ) %>% 
  pivot_wider(names_from = varname, values_from = LQ) %>% 
  mutate(LQdiff = abs(GVA-JOBCOUNT)) %>%
  arrange(-LQdiff) %>% 
  select(SIC07_description_shortened) %>% 
  pull()


#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description_shortened <- factor(yeartoplot$SIC07_description_shortened, levels = sectorLQorder, ordered = T)

#If I could plot both and space them out, that would be good (could get Bradford change showing too)
p <- LQ_baseplot(df = yeartoplot, alpha = 0, sector_name = SIC07_description_shortened, 
                 LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = 'GVA', shapenumber = 23,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = value, sector_regional_proportion = sector_regional_proportion,
                            region_name = varname,
                            sector_name = SIC07_description_shortened, change_over_time = slope, LQ_column = LQ,
                            nudgepos = -0.1, text = 7)



p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p,
                            placename = 'JOBCOUNT', shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            # value_column = value, sector_regional_proportion = sector_regional_proportion,#include numbers
                            region_name = varname,
                            sector_name = SIC07_description_shortened, change_over_time = slope, LQ_column = LQ,
                            nudgepos = 0.1, text = 7)
p <- p + 
  annotate(
    "text",
    label = "JOB COUNT in circles -->\nGVA in diamonds -->",
    x = 0.2, y = sectorLQorder[which(qg('furnit',sectorLQorder))]
  ) +
  coord_cartesian(xlim = c(0.1,7))


p 




# GVA V JOBS RECTANGLE PLOT----

# Example data
df <- tibble::tibble(
  sector = c("Manufacturing", "Services", "Construction", "Tech"),
  gva = c(100, 200, 50, 80),
  jobs = c(50, 300, 100, 40)
)

# Add x/y positions for rectangles
df <- df %>%
  mutate(xmin = cumsum(lag(gva, default = 0)),
         xmax = xmin + gva,
         ymin = 0,
         ymax = jobs)

ggplot(df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sector), color = "black") +
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = sector)) +
  labs(x = "GVA", y = "Jobs", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  theme_minimal() +
  guides(fill = F)




#OK, apply to actual sector data
#Get some consistent sector colours first
n <- length(unique(gvabres$SIC07_description_shortened))
set.seed(12)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[10:(10+(n-1))]


#Would also quite like to split manufacturing off
#Check in orig GVA file for the codes...
#This is the full list - some won't match but enough to label the correct ones at 2 digit for those that do
productionsectors <- c(
  'A-E',
  'AB (1-9)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CD-CG (19-23)',
  'CH (24-25)',
  'CI-CJ (26-27)',
  'CK-CL (28-30)',
  'CM (31-33)',
  '31-32',
  '33',
  'DE (35-39)',
  'F (41-43)',
  '41',
  '42',
  '43'
)

#Yep
table(productionsectors %in% gvabres$SIC_2DIGIT_CODE_GVA2023)
unique(gvabres$SIC_2DIGIT_CODE_GVA2023)[unique(gvabres$SIC_2DIGIT_CODE_GVA2023) %in% productionsectors]

#Label production sectors
gvabres <- gvabres %>% 
  mutate(
    productionsector = ifelse(
      SIC_2DIGIT_CODE_GVA2023 %in% productionsectors,
      'production','other'
    )
  )



place <- gvabres %>% 
  filter(qg('bradford',Region_name), DATE == max(DATE))

place <- gvabres %>% 
  filter(qg('bradford',Region_name), DATE == max(DATE))

#Sorts by actual order
plot.df <- place %>%
  arrange(GVA) %>% 
  mutate(xmin = cumsum(lag(GVA, default = 0)),
         xmax = xmin + GVA,
         ymin = 0,
         ymax = JOBCOUNT)

ggplot(plot.df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SIC07_description_shortened), color = "black") +
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = SIC07_description_shortened)) +
  labs(x = "GVA", y = "Job count", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  theme_minimal() +
  scale_fill_manual(values = setNames(randomcols,unique(gvabres$SIC07_description_shortened))) +
  guides(fill = F) +
  coord_flip()



#Multiple places?
place <- gvabres %>% 
  # filter(qg('sheffield|barnsley',Region_name), DATE == max(DATE))
  filter(qg('bradford|kirkees|calderdale|wakefield|leeds',Region_name), DATE == max(DATE))

#Sorts by actual order
plot.df <- place %>%
  # filter(productionsector == 'production') %>% 
  # group_by(Region_name,productionsector) %>% 
  group_by(Region_name) %>% 
  arrange(-GVA) %>% 
  mutate(xmin = cumsum(lag(GVA, default = 0)),
         xmax = xmin + GVA,
         ymin = 0,
         ymax = JOBCOUNT) %>% 
  ungroup()

ggplot(plot.df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SIC07_description_shortened), color = "black", size =0.25) +
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = SIC07_description_shortened)) +
  labs(x = "GVA", y = "Job count", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  # theme_minimal() +
  scale_fill_manual(values = setNames(randomcols,unique(gvabres$SIC07_description_shortened))) +
  guides(fill = F) +
  coord_flip() +
  # facet_wrap(~Region_name+productionsector, scales = 'free', ncol = 2)
  facet_wrap(~Region_name, scales = 'free', ncol = 2)
  # facet_wrap(~Region_name, ncol = 2)




























