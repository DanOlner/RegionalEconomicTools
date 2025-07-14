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
library(ggrepel)

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

#Already with suspicious looking employee counts removed etc
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
  filter(!qg('households|agri|membership', SIC07_description))

shortsectornames <- read_csv('data/shortsectornames_for_regionalGVA_2digitSICs.csv')

gvabres <- gvabres %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )

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





#couple of checks
# table(is.na(gvabres$JOBCOUNT))
# table(is.na(gvabres$GVA))
# range(gvabres$JOBCOUNT)
# range(gvabres$GVA)

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
  )  +
  coord_cartesian(xlim = c(0.1,7))


p 






# LET'S SEPARATE GVA AND JOBS OUT FOR BRADFORD AND COMPARE TO EVERYWHERE ELSE TOO
place = "Bradford"

#FUNCTION
#Split both gva.props and job.props by subsector list
makegvajoblqplots <- function(keepthesesectors){
  
  # gva.sub <- gva.props %>% filter(SIC07_description_shortened %in% keepthesesectors)

  LQ_slopes <- compute_slope_or_zero(
    data = gva.props, 
    Region_name, SIC07_description_shortened,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = DATE)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- gva.props %>% filter(DATE == max(DATE))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description_shortened')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- gva.props %>% 
    group_by(Region_name, SIC07_description_shortened) %>% 
    summarise(
      min_LQ_all_time = min(LQ),
      max_LQ_all_time = max(LQ)
    )
  
  #Join min and max
  yeartoplot <- yeartoplot %>% 
    left_join(
      minmaxes,
      by = c('Region_name', 'SIC07_description_shortened')
    )
  
  
  #Shorten here to subset of sectors
  yeartoplot <- yeartoplot %>% 
    filter(SIC07_description_shortened %in% keepthesesectors)
  
  
  sectorLQorder <- gva.props %>% filter(
    Region_name == place,
    DATE == max(DATE)#use latest data
  ) %>% 
    arrange(-LQ) %>% 
    select(SIC07_description_shortened) %>% 
    pull()
  
  
  #Turn the sector column into a factor and order by LCR's LQs
  yeartoplot$SIC07_description_shortened <- factor(yeartoplot$SIC07_description_shortened, levels = sectorLQorder, ordered = T)
  
  #If I could plot both and space them out, that would be good (could get Bradford change showing too)
  p <- LQ_baseplot(df = yeartoplot, alpha = 0.1, shape = 0, sector_name = SIC07_description_shortened, 
                   LQ_column = LQ, change_over_time = slope)
  
  p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                              placename = place, shapenumber = 16,
                              min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                              value_column = GVA, sector_regional_proportion = sector_regional_proportion,
                              region_name = Region_name,
                              sector_name = SIC07_description_shortened, change_over_time = slope, LQ_column = LQ,
                              text = 7)
  
  p <- p + 
    coord_cartesian(xlim = c(0.1,7)) +
    ggtitle("GVA")
  
  
  
  
  # job.sub <- job.props %>% filter(SIC07_description_shortened %in% keepthesesectors)
  
  # REPEAT FOR JOBS / BRADFORD (Using same factor order?)
  LQ_slopes <- compute_slope_or_zero(
    data = job.props, 
    Region_name, SIC07_description_shortened,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = DATE)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- job.props %>% filter(DATE == max(DATE))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description_shortened')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- job.props %>% 
    group_by(Region_name, SIC07_description_shortened) %>% 
    summarise(
      min_LQ_all_time = min(LQ),
      max_LQ_all_time = max(LQ)
    )
  
  #Join min and max
  yeartoplot <- yeartoplot %>% 
    left_join(
      minmaxes,
      by = c('Region_name', 'SIC07_description_shortened')
    )
  
  #Shorten here to subset of sectors
  yeartoplot <- yeartoplot %>% 
    filter(SIC07_description_shortened %in% keepthesesectors)
  
  
  
  #USE SECTOR ORDER FROM GVA LQS ABOVE
  yeartoplot$SIC07_description_shortened <- factor(yeartoplot$SIC07_description_shortened, levels = sectorLQorder, ordered = T)
  
  #If I could plot both and space them out, that would be good (could get Bradford change showing too)
  p2 <- LQ_baseplot(df = yeartoplot, alpha = 0.1, shape = 0, sector_name = SIC07_description_shortened, 
                   LQ_column = LQ, change_over_time = slope)
  
  p2 <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p2, 
                              placename = place, shapenumber = 16,
                              min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                              value_column = JOBCOUNT, sector_regional_proportion = sector_regional_proportion,
                              region_name = Region_name,
                              sector_name = SIC07_description_shortened, change_over_time = slope, LQ_column = LQ,
                              text = 7, value_col_ismoney = F)
  
  p2 <- p2 + 
    coord_cartesian(xlim = c(0.1,7)) +
    ggtitle("Jobs")
  
  p2 <- p2 + scale_y_discrete(position = "right")
  
  p + p2 

}



#production sector plot first
makegvajoblqplots(gva.props$SIC07_description_shortened[gva.props$productionsector == 'production'])
makegvajoblqplots(gva.props$SIC07_description_shortened[gva.props$productionsector == 'other'])




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
  # filter(qg('Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Tyne|Sheffield|Nottingham',Region_name) & !qg('greater|shire', Region_name), DATE == max(DATE))#core cities

#Sorts by actual order
plot.df <- place %>%
  # filter(productionsector == 'production') %>% 
  # group_by(Region_name,productionsector) %>% 
  group_by(Region_name) %>% 
  arrange(-GVA) %>% 
  # arrange(-JOBCOUNT) %>% 
  mutate(xmin = cumsum(lag(GVA, default = 0)),
         xmax = xmin + GVA,
         ymin = 0,
         ymax = JOBCOUNT) %>% 
  ungroup()

# plot.df <- place %>%
#   # filter(productionsector == 'production') %>% 
#   # group_by(Region_name,productionsector) %>% 
#   group_by(Region_name) %>% 
#   arrange(-GVA) %>% 
#   # arrange(-JOBCOUNT) %>% 
#   mutate(xmin = cumsum(lag(JOBCOUNT, default = 0)),
#          xmax = xmin + JOBCOUNT,
#          ymin = 0,
#          ymax = GVA) %>% 
#   ungroup()

ggplot(plot.df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SIC07_description_shortened), color = "black", size =0.25) +
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = SIC07_description_shortened)) +
  # labs(y = "GVA", x = "Job count", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  labs(x = "GVA", y = "Job count", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  # theme_minimal() +
  scale_fill_manual(values = setNames(randomcols,unique(gvabres$SIC07_description_shortened))) +
  guides(fill = F) +
  coord_flip() +
  # facet_wrap(~Region_name+productionsector, scales = 'free', ncol = 2)
  facet_wrap(~Region_name, scales = 'free', ncol = 5)
  # facet_wrap(~Region_name, ncol = 2)

#So close but not quite. Could just use for upper labels?
# p + geom_text_repel(
#   aes(x = (xmin + xmax)/2.05, y = (ymin + ymax)/2, label = SIC07_description_shortened),
#   alpha=1
#   # nudge_x = .05,
#   # box.padding = 1,
#   # nudge_y = 0.05,
#   # segment.curvature = -0.1,
#   # segment.ncp = 0.3,
#   # segment.angle = 20,
#   # max.overlaps = 20
# )



# GET AS MUCH RESOLUTION AS POSSIBLE FROM BRES 5 DIGIT JOBS IN LQS----

#Using the full resolution from the 2015-2023 linked BRES data
#And maybe smooth it all out too.

#5 digit is pretty noisy so we're going to smooth LQs after finding proportions
#Find props first cos jobs will correctly sum to 100%

#Nabbed from misc_checks.R
# bres = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv") 

#Not that one! The one made in misc_checks.R that links geogs
#Here: https://github.com/DanOlner/RegionalEconomicTools/blob/99b5acb3e11e2434e01f4776677e221d7dea86c1/prepcode/misc_checks.R#L1526
bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_NUTS3_n_ITL321_stacked_2_Fulltimeemployees_2015_2023_SIC_5DIGIT.csv")

#This comes with ALL NUTS and ITL3 data
#Which means 2022 is doubled up
#E.g. see here: 
#bres %>% filter(qg('bradford', GEOGRAPHY_NAME),qg('Museum activities',SIC_5DIGIT_NAME)) %>% View

#Remove one of them...
#Need to filter NOT year == 2022 NOT GEOGRAPHY_CODE_NUTS2018 is NA, keep the rest
#(2023 we need to keep!)
bres <- bres %>% 
  filter(!(DATE == 2022 & is.na(GEOGRAPHY_CODE_NUTS2018)))


#range(bres$DATE)#tick

SIClookup <- read_csv('data/SIClookup.csv')

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bres <- bres %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
  )

#Make shorter names, use those.
#Make lookup so can be merged in.
names.sections = unique(bres$SIC_SECTION_NAME)
shortnames.sections = reduceSICnames(unique(bres$SIC_SECTION_NAME),'section')

chk.sections = data.frame(names = names.sections, shortnames = shortnames.sections)

names.2digit = unique(bres$SIC_2DIGIT_NAME)
shortnames.2digit = reduceSICnames(unique(bres$SIC_SECTION_NAME),'2 digit')

chk.2digit = data.frame(names = names.2digit, shortnames = shortnames.2digit)


#And 729 categories in the 5 digit...
names.5digit = unique(bres$SIC_5DIGIT_NAME)
#NOTE: THE ORDER HERE IS BASED ON THE ORDER IN bres
#We should make an actual lookup to make sure we don't lose that order
shortnames.5digit = reduceSICnames(unique(bres$SIC_5DIGIT_NAME),'5 digit')

chk.5digit = data.frame(names = names.5digit, shortnames = shortnames.5digit)


#Merge into bres
bres <- bres %>% 
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>% 
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>% 
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names')) 


#Drop some sectors
bres <- bres %>% 
  filter(!qg('households|membership', SIC_2DIGIT_NAME))

#Check!
# bres %>% distinct(SIC_5DIGIT_NAME_SHORT,SIC_5DIGIT_NAME) %>% View


#Let's find LQ / proportions prior to smoothing those
#Rather than smoothing job count then doing LQ
#That way - jobs do sum in a particular year to the correct 100%

#Before doing that...
#We need to work out which 5 digits have all-zero job counts
#As those can't have LQs / aren't any use to us
#Though those will vary across years
#Might be easier to sort after calculating?

# checkonzeros <- bres %>% 
#   group_by(DATE,GEOGRAPHY_NAME,SIC_5DIGIT_NAME_SHORT) %>% 
#   summarise(
#     sectorpercent_zeroes = mean(JOBCOUNT == 0) * 100
#   )
# 
# unique(checkonzeros$sectorpercent_zeroes)
# 
# #OK, let's check if the zeroes persist over all the years in the data
# checkonzeros.allyears <- bres %>% 
#   group_by(GEOGRAPHY_NAME,SIC_5DIGIT_NAME_SHORT) %>% 
#   summarise(
#     sectorpercent_zeroes = mean(JOBCOUNT == 0) * 100
#   )

#And across both all years and all places?
checkonzeros.allyearsandplaces <- bres %>% 
  group_by(SIC_5DIGIT_NAME_SHORT) %>% 
  summarise(
    sectorpercent_zeroes = mean(JOBCOUNT == 0) * 100
  )

#Final - how does that vary per year?
# checkonzeros.peryear <- bres %>% 
#   group_by(DATE,SIC_5DIGIT_NAME_SHORT) %>% 
#   summarise(
#     sectorpercent_zeroes = mean(JOBCOUNT == 0) * 100
#   )


#OK - going to remove any sectors with 100% zeroes for all years
#Which are all agri
#And the one NA to (also agri!)
sectorstodrop <- checkonzeros.allyearsandplaces %>% 
  filter(sectorpercent_zeroes == 100 | is.na(sectorpercent_zeroes)) %>% 
  select(SIC_5DIGIT_NAME_SHORT) %>% 
  pull

bres <- bres %>% 
  filter(
    !SIC_5DIGIT_NAME_SHORT %in% sectorstodrop
  )



#NOW find props (will still have some issues but can deal with below)
bres <- bres %>% 
  arrange(DATE) %>% 
  group_split(DATE) %>%
  map(add_location_quotient_and_proportions,
      regionvar = GEOGRAPHY_NAME,
      lq_var = SIC_5DIGIT_NAME_SHORT,
      valuevar = JOBCOUNT) %>% 
  bind_rows()


#Then smooth the props and LQs...
bres.smooth <- bres %>% 
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME,SIC_5DIGIT_NAME_SHORT) %>%
  mutate(
    jobcount_movingav = rollapply(JOBCOUNT,3,mean,align='center',fill=NA),
    sector_regional_proportion_movingav = rollapply(sector_regional_proportion,3,mean,align='center',fill=NA),
    LQ_movingav = rollapply(LQ,3,mean,align='center',fill=NA)
  ) %>% 
  ungroup()


#Looking at one place / sector to check it looks sane
# bres.smooth %>% filter(qg('bradford', GEOGRAPHY_NAME),qg('prepared meals',SIC_5DIGIT_NAME_SHORT)) %>% View
# bres.smooth %>% 
#   # filter(qg('bradford', GEOGRAPHY_NAME)) %>% 
#   filter(qg('Liverpool', GEOGRAPHY_NAME)) %>% 
#   select(DATE,JOBCOUNT,SIC_5DIGIT_NAME_SHORT,sector_regional_proportion,LQ,jobcount_movingav,LQ_movingav) %>% 
#   arrange(SIC_5DIGIT_NAME_SHORT,DATE) %>% 
#   filter(!is.na(LQ_movingav)) %>% 
#   View




#Keep only smoothed years with data (the centre point)
#And re-find the log value after smoothing...
# x <- bres.smooth %>% 
bres.smooth <- bres.smooth %>%
  filter(!is.na(LQ_movingav)) %>% 
  mutate(LQ_movingav_log = log(LQ_movingav))

#RIGHT, NOW IT'S IN A FORM TO STICK IN THE LQ PLOTS
#Which I want to break down by section to begin with, though may make sector groupings bespoke


#So - run for each group of 5 digits
#But I want to put them in some LQ order for a place - 
#Average order for those sectors in each grouping would make sense
#Though could also do by section

#Get those values then we can use to order plot creation
#Average for the most recent (smoothed) year in our chosen place

place = 'Bradford'

#sections too many! Try smaller
avLQvalues_in_groupings <- bres.smooth %>%
  filter(DATE == max(DATE), GEOGRAPHY_NAME == place) %>% 
  group_by(SIC_2DIGIT_NAME_SHORT) %>% 
  summarise(mean_LQ = mean(LQ, na.rm = T)) %>% 
  arrange(desc(mean_LQ))


#Can use each of those to feed subsets into an LQ plot
#In this case, can calculate everything beforehand then feed in
#Order should still hold in subplots filtered by section
LQ_slopes <- compute_slope_or_zero(
  data = bres.smooth, 
  GEOGRAPHY_NAME, SIC_5DIGIT_NAME_SHORT,#slopes will be found within whatever grouping vars are added here
  y = LQ_movingav_log, x = DATE)

#Filter down to a single year... we may want to smooth years, let's see
yeartoplot <- bres.smooth %>% filter(DATE == max(DATE))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('GEOGRAPHY_NAME', 'SIC_5DIGIT_NAME_SHORT')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- bres.smooth %>% 
  group_by(GEOGRAPHY_NAME, SIC_5DIGIT_NAME_SHORT) %>% 
  summarise(
    min_LQ_all_time = min(LQ_movingav),
    max_LQ_all_time = max(LQ_movingav)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('GEOGRAPHY_NAME', 'SIC_5DIGIT_NAME_SHORT')
  )




sectorLQorder <- yeartoplot %>% filter(
  GEOGRAPHY_NAME == place,
  DATE == max(DATE)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(SIC_5DIGIT_NAME_SHORT) %>% 
  pull()


#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC_5DIGIT_NAME_SHORT <- factor(yeartoplot$SIC_5DIGIT_NAME_SHORT, levels = sectorLQorder, ordered = T)

#Examine to check what to filter out
yeartoplot %>% filter(GEOGRAPHY_NAME == 'Bradford') %>% View




#Test some alterations
#Use function in adhoc_functions
#And send in the actual df already filtered to what we want to plot
#Don't plot any 2 digit SICs with no values we want to show
#Shorten here to subset of sectors

#Shortern the list of 2 digit SICs to ones that actually have values we want to look at
#Keep only these 5 digit sectors for Bradford
keep.these.5digits <- yeartoplot %>% 
  filter(GEOGRAPHY_NAME == 'Bradford') %>% 
  filter(LQ_movingav > 0.001, jobcount_movingav >= 100) %>% 
  select(SIC_5DIGIT_NAME_SHORT) %>%
  mutate(SIC_5DIGIT_NAME_SHORT = as.character(SIC_5DIGIT_NAME_SHORT)) %>% 
  pull

#filter the yeartoplot data (which includes all other places)
#Based on that, to get a list of what 2 digits are left
yeartoplot.sub <- yeartoplot %>%
  filter(
    SIC_5DIGIT_NAME_SHORT %in% keep.these.5digits
  ) %>%
  mutate(jobcount_movingav = round(jobcount_movingav,0))

# length(unique(yeartoplot$SIC_2DIGIT_NAME_SHORT))
# length(unique(yeartoplot.sub$SIC_2DIGIT_NAME_SHORT))

#Plot each SIC 2 digit for the 5 digits in them
plotz <- map(yeartoplot.sub %>% group_split(SIC_2DIGIT_NAME_SHORT),LQplot_BRES_groupsof5digit)

#Tests
# debugonce(LQplot_BRES_groupsof5digit)
# LQplot_BRES_groupsof5digit(yeartoplot.sub %>% filter(qg('machinery manuf',SIC_2DIGIT_NAME_SHORT)))

#Get same order of 2 digits from that...
#Err which I think was just alphabetical!
twodigitnamesforsaves <- yeartoplot.sub %>% 
  group_split(SIC_2DIGIT_NAME_SHORT) %>% 
  map_chr(
    ~ .x %>% select(SIC_2DIGIT_NAME_SHORT) %>% 
        distinct() %>% 
        pull
        )

filenamez <- paste0(
  'local/outputs/bresLQplots_bradford/',
  gsub(' |/','',twodigitnamesforsaves),
  # gsub(' |/','',unique(yeartoplot.sub$SIC_2DIGIT_NAME_SHORT)),
  '.png')

# map2(filenamez[1:2],plotz[1:2],ggsave, height = 8, width = 9)
map2(filenamez,plotz,ggsave,height = 8, width = 9)

#Nearly but bit messy...
# wrap_plots(plotz,ncol = 8)






# COMPANIES HOUSE LQS (AND COMPARE TO ONS DATA)----

#ch already got and linked to shortened names above
#Note, we have local authorities here not ITL3 zones
length(unique(ch$localauthority_name))
length(unique(yeartoplot$GEOGRAPHY_NAME))#ITL3 2021

#But LQs are relative to national totals...
#Ah except of course CH is only GB
#Ah except except - BRES is *also* GB

#Bradford is the same shape as the ITL3

#Let's use average employment over the two points we've got for this LQ
#Rowwise seems slow compared to just adding column with base?
ch.avemp <- ch %>% 
  st_set_geometry(NULL) %>% 
  # filter(!is.na(mean(c(Employees_thisyear,Employees_lastyear)))) %>% #Keep only firms with employees in both of the years
  filter(!is.na(Employees_thisyear) | !is.na(Employees_lastyear)) %>% #Keep only firms with employees in one of the years
  select(CompanyName,CompanyNumber,accountcode,CompanyCategory,incorporationdate_formatted,age_of_firm_years,localauthority_code:ITL221NM,Employees_thisyear,Employees_lastyear,SIC_2DIGIT_CODE,SIC_5DIGIT_CODE,SIC_SECTION_NAME_SHORT:SIC_5DIGIT_NAME_SHORT) %>%
  rowwise() %>% 
  mutate(
    avemployeecount = mean(c(Employees_thisyear,Employees_lastyear), na.rm = T)#get mean if there's only one employee count in a year
  )

table(!is.na(ch.avemp$Employees_thisyear))
table(!is.na(ch.avemp$Employees_lastyear))

#Check on sample
#slice_sample(ch.avemp, n = 10) %>% View #for some reason not running
#ch.avemp[1:100,] %>% View

#LQs at 2 digit first - 
#For that, sum employee count by local authority and 2 digit sector
ch.avemp.2digitsums <- ch.avemp %>% 
  group_by(SIC_2DIGIT_NAME_SHORT,localauthority_name) %>% 
  summarise(avemployeecount = sum(avemployeecount))


#LQs from that!
ch.avemp.2digitsums <- add_location_quotient_and_proportions(
  df = ch.avemp.2digitsums,
  regionvar = localauthority_name,
  lq_var = SIC_2DIGIT_NAME_SHORT,
  valuevar = avemployeecount
)

#sanity check... tick, all sector total proportions sum to 1 for whole of GB
# ch.avemp.2digitsums %>% 
#   select(SIC_2DIGIT_NAME_SHORT,sector_total_proportion) %>% 
#   distinct() %>% 
#   summarise(sum(sector_total_proportion))


#Actuuuuaaaally
#We could use the two employee time points to get a rudimentary "where going in last year"
#datapoint to show LQ change. Though for that we'd need only firms with employee counts in both time points.
#Remind me of percentages for that.

#Firms with employees in one of the two years (and maybe both) 86.7%
table(!is.na(ch$Employees_thisyear) | !is.na(ch$Employees_lastyear)) %>% prop.table()

#Firms with employees in BOTH YEARS: 70%. Huh, not bad.
#That seems higher than last time I did this?
#Oh, it's ones with zero in
table(!is.na(ch$Employees_thisyear) & !is.na(ch$Employees_lastyear)) %>% prop.table()

# x <- ch %>% 
#   filter(!is.na(Employees_thisyear) & !is.na(Employees_lastyear))
# 
# g(x)

#Well that's OK for summing, don't need to filter out
ch.2digitsums <- ch %>% 
  st_set_geometry(NULL) %>% 
  filter(!is.na(Employees_thisyear) & !is.na(Employees_lastyear)) %>% #Keep only firms with employees in BOTH years even if it's zero
  select(CompanyName,CompanyNumber,accountcode,CompanyCategory,incorporationdate_formatted,age_of_firm_years,localauthority_code:ITL221NM,Employees_thisyear,Employees_lastyear,SIC_2DIGIT_CODE,SIC_2DIGIT_CODE_NUMERIC,SIC_5DIGIT_CODE,SIC_SECTION_NAME_SHORT:SIC_5DIGIT_NAME_SHORT) %>% 
  group_by(SIC_2DIGIT_NAME_SHORT,localauthority_name) %>% 
  summarise(
    employeecount_thisyear = sum(Employees_thisyear),
    employeecount_lastyear = sum(Employees_lastyear)
    ) %>% ungroup()

#Add 2 digit code numeric back in to label production easily
#dplyr distinct weirdly slow
#Do this instead, instant
twodigitlookup <- tibble(
  SIC_2DIGIT_CODE_NUMERIC = unique(ch$SIC_2DIGIT_CODE_NUMERIC),
  SIC_2DIGIT_NAME_SHORT = unique(ch$SIC_2DIGIT_NAME_SHORT)
)


ch.2digitsums <- ch.2digitsums %>% 
  left_join(
    twodigitlookup, by = 'SIC_2DIGIT_NAME_SHORT'
  )


#Make those into pseudo dates in an order we can get an LQ size change from
ch.2digitsums.long <- ch.2digitsums %>% 
  pivot_longer(employeecount_thisyear:employeecount_lastyear, names_to = 'timepoint', values_to = 'jobcount') %>% 
  mutate(
    timepoint_numeric = ifelse(timepoint == 'employeecount_lastyear', 1,2)
  )
 
 
#LQs again!
ch.2digitsums.long <- ch.2digitsums.long %>% 
  group_split(timepoint) %>%
  map(add_location_quotient_and_proportions,
        regionvar = localauthority_name,
        lq_var = SIC_2DIGIT_NAME_SHORT,
        valuevar = jobcount) %>% 
  bind_rows()

#Label production and other
#Production is 2 digits from 1 to 43
ch.2digitsums.long <- ch.2digitsums.long %>% 
  mutate(
    production = ifelse(SIC_2DIGIT_CODE_NUMERIC %in% c(1:43), 'production','other')
  )

# table(ch.2digitsums.long$production)

#sanity check again... tick, should sum to 1 here in each timepoint group
# ch.2digitsums.long %>%
#   select(timepoint,SIC_2DIGIT_NAME_SHORT,sector_total_proportion) %>%
#   distinct() %>%
#   group_by(timepoint) %>%
#   summarise(sum(sector_total_proportion))

#On to plotting
#Linear slope between those two timepoints...
#Could do with functioning this all up really

#Make calcs for trajectory

#Log vals here should give us rough % change between timepoints...
#TODO: have version to get accurate % change (or can just convert back)
LQ_slopes <- compute_slope_or_zero(
  data = ch.2digitsums.long, 
  localauthority_name, SIC_2DIGIT_NAME_SHORT,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = timepoint_numeric)


#Filter down to a single year...
#Might want the av of the two timepoints here maybe...
yeartoplot <- ch.2digitsums.long %>% filter(timepoint_numeric == max(timepoint_numeric))#use latest point

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('localauthority_name', 'SIC_2DIGIT_NAME_SHORT')
  )

place = 'Bradford'

sectorLQorder <- ch.2digitsums.long %>% filter(
  localauthority_name == place,
  timepoint_numeric == max(timepoint_numeric)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(SIC_2DIGIT_NAME_SHORT) %>% 
  pull()


#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC_2DIGIT_NAME_SHORT <- factor(yeartoplot$SIC_2DIGIT_NAME_SHORT, levels = sectorLQorder, ordered = T)

#Also keep only 2 digit sectors where Bradford has more than 100 workers recorded in that sector for CH
morethanx <- yeartoplot %>% 
  filter(
    localauthority_name == place,
    jobcount >= 100
    ) %>% 
  select(SIC_2DIGIT_NAME_SHORT) %>% 
  distinct() %>% 
  pull


yeartoplot <- yeartoplot %>% filter(
  !is.na(SIC_2DIGIT_NAME_SHORT),
  SIC_2DIGIT_NAME_SHORT %in% as.character(morethanx)
  )

#If I could plot both and space them out, that would be good (could get Bradford change showing too)
p <- LQ_baseplot(df = yeartoplot %>% filter(production == 'production'), alpha = 0.03, sector_name = SIC_2DIGIT_NAME_SHORT, 
                 LQ_column = LQ, change_over_time = slope)

# debugonce(addplacename_to_LQplot)
p <- addplacename_to_LQplot(df = yeartoplot %>% filter(production == 'production'), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            # min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount, sector_regional_proportion = sector_regional_proportion,
                            region_name = localauthority_name,
                            sector_name = SIC_2DIGIT_NAME_SHORT, change_over_time = slope, LQ_column = LQ,
                            value_col_ismoney = F, text = 7, maxLQvalmultiplier = 2)


# p <- p +
# annotate(
#   "text",
#   label = "JOB COUNT in circles -->\nGVA in diamonds -->",
#   x = 0.2, y = sectorLQorder[which(qg('furnit',sectorLQorder))]
# )  
  # coord_cartesian(xlim = c(0.1,50))

p <- p + ggtitle("production\n(2 digit sectors w/ 100+ employees in Bradford)")

p2 <- LQ_baseplot(df = yeartoplot %>% filter(production == 'other'), alpha = 0.03, sector_name = SIC_2DIGIT_NAME_SHORT, 
                 LQ_column = LQ, change_over_time = slope)

# debugonce(addplacename_to_LQplot)
p2 <- addplacename_to_LQplot(df = yeartoplot %>% filter(production == 'other'), plot_to_addto = p2, 
                            placename = place, shapenumber = 16,
                            # min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount, sector_regional_proportion = sector_regional_proportion,
                            region_name = localauthority_name,
                            sector_name = SIC_2DIGIT_NAME_SHORT, change_over_time = slope, LQ_column = LQ,
                            value_col_ismoney = F, text = 7, maxLQvalmultiplier = 2)

p2 <- p2 + ggtitle("other\n(2 digit sectors w/ 100+ employees in Bradford)")

p / p2




# WOULD ALSO THEN BE GOOD TO KNOW HOW CH COMPARES TO BRES FOR THE SAME SECTORS...
#Full BRES 2 digits, not the reduced version from BRES/GVA combo
#Have I already done that somewhere? Nope, don't seem to have used it...
bres15to23 <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_NUTS3_n_ITL321_stacked_2_Fulltimeemployees_2015_2023_SIC_5DIGIT.csv")

#Note that's prob got 2022 doubled up from the two different sources...
2











