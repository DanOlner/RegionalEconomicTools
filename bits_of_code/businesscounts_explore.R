#business count explore
library(tidyverse)
library(sf)#for geo stuff
library(tmap)#for mapping 
library(zoo)#for making smoothed moving averages
library(patchwork)#combine ggplots easily
library(plotly)#For interactive plots
library(RColorBrewer)
library(nomisr)

source('functions/misc_functions.R')
options(scipen = 999)



# NOMIS CHECKS----

#https://cran.r-project.org/web/packages/nomisr/vignettes/introduction.html
x <- nomis_data_info()

x %>% 
  filter(qg('business',name.value)) %>% 
  select(id,name.value) %>% 
  View

#"UK Business Counts - local units by industry and employment size band"
#NM_141_1
#Same for enterprises is NM_142_1

#Not superuseful!
q <- nomis_overview("NM_141_1")

#Does that have the geography and everything we need?
y <- nomis_get_metadata("NM_141_1")

#Check what's available
nomis_get_metadata(id = "NM_141_1", concept = "GEOGRAPHY", type = "type") %>% print(n=31)

#Check for the geogs we want
#In this case, aiming to get all greater manchester local authorities / boroughs
d <- nomis_get_metadata(id = "NM_141_1", concept = "geography", type = "TYPE424")

#Are all the boroughs in there?
#First check if we have a full name match
#Tick!
d %>% 
  filter(
    label.en %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")
  )

#OK, other vars... yep, still can't get industry breakdown for some reason, same as BRES
#NOMIS website has down to 5 digit, doubt that's available at all size bands
nomis_get_metadata(id = "NM_141_1", concept = "INDUSTRY")

nomis_get_metadata(id = "NM_141_1", concept = "EMPLOYMENT_SIZEBAND")
nomis_get_metadata(id = "NM_141_1", concept = "LEGAL_STATUS")
nomis_get_metadata(id = "NM_141_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_141_1", concept = "TIME")


#OK, what do we get?
placeids <- d %>% filter(label.en %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% select(id) %>% pull

gm <- nomis_get_data(id = "NM_141_1", time = "latest", MEASURES = 20100, LEGAL_STATUS = 0, geography = placeids)



#Also get for England, too add average... 2092957699
d <- nomis_get_metadata(id = "NM_141_1", concept = "geography", type = "TYPE499")

eng <- nomis_get_data(id = "NM_141_1", time = "latest", MEASURES = 20100, LEGAL_STATUS = 0, geography = "2092957699")





# LOCAL UNITS----

#Save
# saveRDS(gm,'local/data/greatermanchester_businesscounts_localunits_byfirmsizeandsector2024.rds')
gm <- readRDS('local/data/greatermanchester_businesscounts_localunits_byfirmsizeandsector2024.rds')

# saveRDS(eng,'local/data/england_businesscounts_localunits_byfirmsizeandsector2024.rds')
eng <- readRDS('local/data/england_businesscounts_localunits_byfirmsizeandsector2024.rds')



#Check on industry types...
# table(gm$INDUSTRY_TYPE)

# table(gm$EMPLOYMENT_SIZEBAND_NAME)

#What's in each / how many cats?
#2 digit is the broadest here, will have to bin for sections
gm %>% 
  filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') %>% 
  select(INDUSTRY_NAME) %>% 
  distinct()


#Keep just two digit
gm <- gm %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') 

#Merge in sections
SIClookup <- read_csv('data/SIClookup.csv')

#Check match... tick
table(unique(SIClookup$SIC_2DIGIT_CODE) %in% gm$INDUSTRY_CODE)

#Merge in sections...
gm <- gm %>% 
  rename(SIC_2DIGIT_CODE = INDUSTRY_CODE) %>% 
  left_join(
    SIClookup %>% select(SIC_2DIGIT_CODE,SIC_SECTION_CODE,SIC_SECTION_NAME) %>% distinct(),
    by = 'SIC_2DIGIT_CODE'
  )

#Shortemed!
gm <- gm %>% mutate(SIC_SECTION_NAME_SHORTERNED = reduceSICnames(SIC_SECTION_NAME,'section'))

unique(gm$SIC_SECTION_NAME_SHORTERNED)


#Remove a couple we don't want
gm <- gm %>% filter(!qg('extrat|households',SIC_SECTION_NAME_SHORTERNED))


#So plot wise...
#Prob want 1 section, 1 firm size, all 10 boroughs on one plot? Test...

#We want "count of firms of different sizes as a percent of total firm count" in each borough
#Note also at the moment, haven't summed counts per section

#Sums per section! Let's do that first
#We don't want every single employment size band (some overlap e.g. there's 0-4 and micro 0-9) 
#Drop the ones we don't want
gm.sectionsums <- gm %>% 
  filter(
    EMPLOYMENT_SIZEBAND_NAME %in% c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)')
    # EMPLOYMENT_SIZEBAND_NAME %in% c('0 to 4','10 to 19','20 to 49','')
  ) %>% 
  group_by(EMPLOYMENT_SIZEBAND_NAME,SIC_SECTION_NAME_SHORTERNED,GEOGRAPHY_NAME) %>% 
  summarise(firmcount = sum(OBS_VALUE)) %>% 
  ungroup()
  

#Add in total firm count per borough, so can then find percent of firm count for each sizeband/section combo
#(And mull if that's the best denominator... e.g. could examine a particular sector like ICT)
gm.sectionsums <- gm.sectionsums %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    totalfirmcount_per_LA = sum(firmcount),
    percent_firmcount = (firmcount/totalfirmcount_per_LA) * 100,
    EMPLOYMENT_SIZEBAND_NAME = factor(EMPLOYMENT_SIZEBAND_NAME, levels = c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)'))) %>% 
  ungroup() 



#REPEAT FOR ENGLAND
eng <- eng %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') 

#Merge in sections
#Check match... tick
table(unique(SIClookup$SIC_2DIGIT_CODE) %in% eng$INDUSTRY_CODE)

#Merge in sections...
eng <- eng %>% 
  rename(SIC_2DIGIT_CODE = INDUSTRY_CODE) %>% 
  left_join(
    SIClookup %>% select(SIC_2DIGIT_CODE,SIC_SECTION_CODE,SIC_SECTION_NAME) %>% distinct(),
    by = 'SIC_2DIGIT_CODE'
  )

#Shortemed!
eng <- eng %>% mutate(SIC_SECTION_NAME_SHORTERNED = reduceSICnames(SIC_SECTION_NAME,'section'))

unique(eng$SIC_SECTION_NAME_SHORTERNED)


#Remove a couple we don't want
eng <- eng %>% filter(!qg('extrat|households',SIC_SECTION_NAME_SHORTERNED))

#Sums per section! Let's do that first
#We don't want every single employment size band (some overlap e.g. there's 0-4 and micro 0-9) 
#Drop the ones we don't want
eng.sectionsums <- eng %>% 
  filter(
    EMPLOYMENT_SIZEBAND_NAME %in% c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)')
  ) %>% 
  group_by(EMPLOYMENT_SIZEBAND_NAME,SIC_SECTION_NAME_SHORTERNED) %>% 
  summarise(firmcount = sum(OBS_VALUE)) %>% 
  ungroup()


#Add in total firm count per borough, so can then find percent of firm count for each sizeband/section combo
#(And mull if that's the best denominator... e.g. could examine a particular sector like ICT)
eng.sectionsums <- eng.sectionsums %>% 
  mutate(
    GEOGRAPHY_NAME = "England",#add this back in to combine below
    totalfirmcount_per_LA = sum(firmcount),
    percent_firmcount = (firmcount/totalfirmcount_per_LA) * 100,
    EMPLOYMENT_SIZEBAND_NAME = factor(EMPLOYMENT_SIZEBAND_NAME, levels = c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)'))) %>% 
  ungroup() 

#Test with one bit!
# chk <- gm.sectionsums %>% filter(
#   SIC_SECTION_NAME_SHORTERNED == 'ICT'
#   # EMPLOYMENT_SIZEBAND_NAME == 'Micro (0 to 9)'
# )
# 
# 
# #Nabbed from businesscounts.Rmd in YPERN_dataexplore.R
# 
# ggplot(
#   chk, 
#   aes(x = EMPLOYMENT_SIZEBAND_NAME, y = percent_firmcount, fill = GEOGRAPHY_NAME)
# ) +
#   geom_bar(stat='identity', position = 'dodge') +
#   xlab("Business size band") +
#   ylab("Percent of region's total business count") +
#   scale_fill_brewer(palette = "Paired", direction = 1) +
#   theme(legend.title=element_blank())
# 
# 
# #This might be better
# chk <- gm.sectionsums %>% filter(
#   # SIC_SECTION_NAME_SHORTERNED == 'ICT'
#   EMPLOYMENT_SIZEBAND_NAME == 'Micro (0 to 9)'
# )
# 
# 
# ggplot(
#   chk, 
#   aes(x = SIC_SECTION_NAME_SHORTERNED, y = percent_firmcount, fill = GEOGRAPHY_NAME)
# ) +
#   geom_bar(stat='identity', position = 'dodge') +
#   xlab("Business size band") +
#   ylab("Percent of region's total firm count") +
#   scale_fill_brewer(palette = "Paired", direction = 1) +
#   theme(legend.title=element_blank()) +
#   coord_flip()


#EVERYTHING
#EVERYTHING PLUS ENGLAND
both <- bind_rows(gm.sectionsums,eng.sectionsums) %>% 
  mutate(
    GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME, levels = c(
      "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan","England"
    )),
    `%:` = paste0(round(percent_firmcount,1), ', count: ',totalfirmcount_per_LA)
  ) %>% rename(
    Borough = GEOGRAPHY_NAME
  )

p <- ggplot(
  both, 
  # gm.sectionsums, 
  aes(x = SIC_SECTION_NAME_SHORTERNED, y = percent_firmcount, fill = Borough, label = `%:`)
) +
  geom_bar(stat='identity', position = 'dodge') +
  xlab("SIC section") +
  ylab("% of borough's total local unit count (with England av)") +
  scale_fill_brewer(palette = "Paired", direction = 1) +
  theme(legend.title=element_blank()) +
  facet_wrap(~EMPLOYMENT_SIZEBAND_NAME, nrow = 1, scales = 'free_x') +
  coord_flip() +
  xlab("")
  # theme(legend.title=element_blank())#doesn't work in ggplotly
  # scale_color_discrete(name="")#Neither does this!

ggplotly(p, tooltip = '%:')




# REPEAT FOR ENTERPRISES----

# gm <- nomis_get_data(id = "NM_142_1", time = "latest", MEASURES = 20100, LEGAL_STATUS = 0, geography = placeids)

#Save
# saveRDS(gm,'local/data/greatermanchester_businesscounts_enterprises_byfirmsizeandsector2024.rds')
gm <- readRDS('local/data/greatermanchester_businesscounts_enterprises_byfirmsizeandsector2024.rds')


#Also get for England, too add average... 2092957699
# d <- nomis_get_metadata(id = "NM_142_1", concept = "geography", type = "TYPE499")

# eng <- nomis_get_data(id = "NM_142_1", time = "latest", MEASURES = 20100, LEGAL_STATUS = 0, geography = "2092957699")

# saveRDS(eng,'local/data/england_businesscounts_enterprises_byfirmsizeandsector2024.rds')
eng <- readRDS('local/data/england_businesscounts_enterprises_byfirmsizeandsector2024.rds')


#Check on industry types...
# table(gm$INDUSTRY_TYPE)

# table(gm$EMPLOYMENT_SIZEBAND_NAME)

#What's in each / how many cats?
#2 digit is the broadest here, will have to bin for sections
gm %>% 
  filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') %>% 
  select(INDUSTRY_NAME) %>% 
  distinct()


#Keep just two digit
gm <- gm %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') 

#Merge in sections
SIClookup <- read_csv('data/SIClookup.csv')

#Check match... tick
table(unique(SIClookup$SIC_2DIGIT_CODE) %in% gm$INDUSTRY_CODE)

#Merge in sections...
gm <- gm %>% 
  rename(SIC_2DIGIT_CODE = INDUSTRY_CODE) %>% 
  left_join(
    SIClookup %>% select(SIC_2DIGIT_CODE,SIC_SECTION_CODE,SIC_SECTION_NAME) %>% distinct(),
    by = 'SIC_2DIGIT_CODE'
  )

#Shortemed!
gm <- gm %>% mutate(SIC_SECTION_NAME_SHORTERNED = reduceSICnames(SIC_SECTION_NAME,'section'))

unique(gm$SIC_SECTION_NAME_SHORTERNED)


#Remove a couple we don't want
gm <- gm %>% filter(!qg('extrat|households',SIC_SECTION_NAME_SHORTERNED))


#So plot wise...
#Prob want 1 section, 1 firm size, all 10 boroughs on one plot? Test...

#We want "count of firms of different sizes as a percent of total firm count" in each borough
#Note also at the moment, haven't summed counts per section

#Sums per section! Let's do that first
#We don't want every single employment size band (some overlap e.g. there's 0-4 and micro 0-9) 
#Drop the ones we don't want
gm.sectionsums <- gm %>% 
  filter(
    EMPLOYMENT_SIZEBAND_NAME %in% c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)')
    # EMPLOYMENT_SIZEBAND_NAME %in% c('0 to 4','10 to 19','20 to 49','')
  ) %>% 
  group_by(EMPLOYMENT_SIZEBAND_NAME,SIC_SECTION_NAME_SHORTERNED,GEOGRAPHY_NAME) %>% 
  summarise(firmcount = sum(OBS_VALUE)) %>% 
  ungroup()


#Add in total firm count per borough, so can then find percent of firm count for each sizeband/section combo
#(And mull if that's the best denominator... e.g. could examine a particular sector like ICT)
gm.sectionsums <- gm.sectionsums %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    totalfirmcount_per_LA = sum(firmcount),
    percent_firmcount = (firmcount/totalfirmcount_per_LA) * 100,
    EMPLOYMENT_SIZEBAND_NAME = factor(EMPLOYMENT_SIZEBAND_NAME, levels = c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)'))) %>% 
  ungroup() 



#REPEAT FOR ENGLAND
eng <- eng %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)') 

#Merge in sections
#Check match... tick
table(unique(SIClookup$SIC_2DIGIT_CODE) %in% eng$INDUSTRY_CODE)

#Merge in sections...
eng <- eng %>% 
  rename(SIC_2DIGIT_CODE = INDUSTRY_CODE) %>% 
  left_join(
    SIClookup %>% select(SIC_2DIGIT_CODE,SIC_SECTION_CODE,SIC_SECTION_NAME) %>% distinct(),
    by = 'SIC_2DIGIT_CODE'
  )

#Shortemed!
eng <- eng %>% mutate(SIC_SECTION_NAME_SHORTERNED = reduceSICnames(SIC_SECTION_NAME,'section'))

unique(eng$SIC_SECTION_NAME_SHORTERNED)


#Remove a couple we don't want
eng <- eng %>% filter(!qg('extrat|households',SIC_SECTION_NAME_SHORTERNED))

#Sums per section! Let's do that first
#We don't want every single employment size band (some overlap e.g. there's 0-4 and micro 0-9) 
#Drop the ones we don't want
eng.sectionsums <- eng %>% 
  filter(
    EMPLOYMENT_SIZEBAND_NAME %in% c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)')
  ) %>% 
  group_by(EMPLOYMENT_SIZEBAND_NAME,SIC_SECTION_NAME_SHORTERNED) %>% 
  summarise(firmcount = sum(OBS_VALUE)) %>% 
  ungroup()


#Add in total firm count per borough, so can then find percent of firm count for each sizeband/section combo
#(And mull if that's the best denominator... e.g. could examine a particular sector like ICT)
eng.sectionsums <- eng.sectionsums %>% 
  mutate(
    GEOGRAPHY_NAME = "England",#add this back in to combine below
    totalfirmcount_per_LA = sum(firmcount),
    percent_firmcount = (firmcount/totalfirmcount_per_LA) * 100,
    EMPLOYMENT_SIZEBAND_NAME = factor(EMPLOYMENT_SIZEBAND_NAME, levels = c('Micro (0 to 9)','Small (10 to 49)','Medium-sized (50 to 249)','Large (250+)'))) %>% 
  ungroup() 


#EVERYTHING
#EVERYTHING PLUS ENGLAND
both <- bind_rows(gm.sectionsums,eng.sectionsums) %>% 
  mutate(
    GEOGRAPHY_NAME = factor(GEOGRAPHY_NAME, levels = c(
      "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan","England"
    )),
    `%:` = paste0(round(percent_firmcount,1), ', count: ',totalfirmcount_per_LA)
  ) %>% rename(
    Borough = GEOGRAPHY_NAME
  )

p <- ggplot(
  both, 
  # gm.sectionsums, 
  aes(x = SIC_SECTION_NAME_SHORTERNED, y = percent_firmcount, fill = Borough, label = `%:`)
) +
  geom_bar(stat='identity', position = 'dodge') +
  xlab("SIC section") +
  ylab("% of borough's total enterprise count (with England av)") +
  scale_fill_brewer(palette = "Paired", direction = 1) +
  theme(legend.title=element_blank()) +
  facet_wrap(~EMPLOYMENT_SIZEBAND_NAME, nrow = 1, scales = 'free_x') +
  coord_flip() +
  xlab("")
# theme(legend.title=element_blank())#doesn't work in ggplotly
# scale_color_discrete(name="")#Neither does this!

ggplotly(p, tooltip = '%:')












































