#Miscellaneous data checks
library(tidyverse)
library(nomisr)
library(stringdist)
library(sf)
source('functions/misc_functions.R')

options(scipen = 99)


# GENERAL NOMISR BRES CHECKS----

#BRES code, get "concepts" we can use to specify download 
a <- nomis_get_metadata(id = "NM_189_1")

#Pick on some of those (some of which don't seem to be working.)
#Note, MEASURE in the actual downloaded data is "MEASURE_NAME" column...
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")

#Point of confusion here - 
#Look at the full column range and how it's broken down:
#(for some sample data)
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

z <- nomis_get_data(id = "NM_189_1",  time = "2023", geography = placeid
                    # MEASURE = 1,
                    # MEASURES = 20100,
                    # EMPLOYMENT_STATUS = 1
)

#"MEASURE_MAME" has "count" and "industry percentage"
#"MEASURES_NAME" has "Value" and "percent"
#The latter - will have the "percent of the industry percentage" rows in
#So actually, BOTH will need selecting correctly

#Also - the 'concept ref' to access both of those isn't the same as the column names
#They don't have the extra "_NAME" part

#Also also - "MEASURE_TYPE" column, when downloaded, will say "percent", but it isn't.

#Are we clear? OK then. So, this works...?

#Taking the numbers from the ID column when using nomis_get_metadata on the concepts (see above)
z <- nomis_get_data(id = "NM_189_1",  time = "2023", 
                    geography = placeid,
                    MEASURE = 1,#1 is "Count", 2 is "Industry percent"
                    MEASURES = 20100#20100 is "value", 20301 is "percent"
)




#INSPECT FULL RANGE OF BRES AVAILABLE COLUMN NAMES----

#To select the right ones...
#Get random sample data
#ITL3
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

z <- nomis_get_data(id = "NM_189_1",  time = "2023", geography = placeid
                    # MEASURE = 1,
                    # MEASURES = 20100,
                    # EMPLOYMENT_STATUS = 1
)


#That data method is seeming different to how it was...
#Is it the same for others?
#ITL3
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

z <- nomis_get_data(id = "NM_189_1",  time = "2023",
                    geography = placeid, 
                    # measures = 20100, 
                    EMPLOYMENT_STATUS = 1
)





# CHECK BRES GEOGRAPHIES' MATCH TO OTHERS----

#We already know that NUTS2 2016 does match ITL2
#Check what most closely matches to ITL3

#Nab ITL3 codes and names from another source
ITL3 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SICsections_WIDE_2022.csv') %>% distinct(ITL_code, .keep_all = T) %>% select(1:2)

#Check what we can get from BRES
#TYPE437 is nuts 2016 level 3 - Bournemouth, Christchurch and Poole don't match ITL3, the rest does (see intersection check below)
#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2

#TYPE429 is ITL2 (2021)
#TYPE428 is ITL3 (2021)

#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 41)


#Ah - the list of geographies now actually contains ITLs:
z <- nomis_get_data(id = "NM_189_1", time = '2021', 
                    geography = "TYPE428", measures = 20100, 
                    # geography = "TYPE437", measures = 20100, 
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
                    )

#Check ITL code matches... tick
table(unique(z$GEOGRAPHY_CODE) %in% ITL3$ITL_code)

#Presumably not a perfect match the other way? BRES is GB only...
#So yes, just NI aren't matches
table(unique(ITL3$ITL_code) %in% z$GEOGRAPHY_CODE)
unique(ITL3$Region_name)[!unique(ITL3$ITL_code) %in% z$GEOGRAPHY_CODE]

#check if data present...
#2023 yes
#2022 yes
#2021 no - that then moves to BRES using NUTS3
table(!is.na(z$OBS_VALUE))
sample(z$OBS_VALUE,25)




#Just checking which years available for which geographies
#Can do with just one geography to save downloads


#Get code for a single random geog
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

#ITL3
#Get random geog to inspect which years there's data for
z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
#ITL3 ONLY GOES BACK TO 2015 BY DEFAULT
unique(z$DATE)
#AND ONLY HAS ACTUALY DATA FOR 2022/23
unique(z$DATE[!is.na(z$OBS_VALUE)])




#Same for ITL2 presumably... TICK, 2022-23 DATA ONLY
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE429") %>% filter(qg('south yorks',.$label.en)) %>% select(id) %>% pull

#ITL3
#Get random geog to inspect which years there's data for
z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
#ITL3 ONLY GOES BACK TO 2015 BY DEFAULT
unique(z$DATE)
#AND ONLY HAS ACTUALY DATA FOR 2022/23
unique(z$DATE[!is.na(z$OBS_VALUE)])






#NUTS 3 2018: data from 2015 to 2022, none for 2023
#Going to be different placeid for different geography type...
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE437") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#2015 to 2022 have data
unique(z$DATE[!is.na(z$OBS_VALUE)])




#NUTS 2 2018 (TYPE438) THE SAME as NUTS3 2018? 
#Tick - data for 2015 to 2022, none for 2023
#Going to be different placeid for different geography type...
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE438") %>% filter(qg('south yorks',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#2015 to 2022 have data
unique(z$DATE[!is.na(z$OBS_VALUE)])












#Earlier NUTS data? (Can check geog match after, but if harmonisable, would be useful to have)
#NUTS 3 2013 = TYPE449
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE449") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#... but every single year has data!!???
unique(z$DATE[!is.na(z$OBS_VALUE)])

#View... yep, these do have data in.
#So there's no reason why NUTS3 2018 doesn't?
z %>% filter(DATE == "2023") %>% v




#Check last available NUTS3 data, 2010
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE456") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#Again, data for all years...
unique(z$DATE[!is.na(z$OBS_VALUE)])

z %>% filter(DATE == "2023") %>% v

#At any rate, no older NUTS zones go back further than 2015 using the API




#CHECK ITL3(2021) AND NUTS3(2016/2018 DEPENDING WHO YOU ASK) FOR MISALIGNED GEOGRAPHIES----

#Issue: 
#BRES uses ITL3 from 2022.
#And NUTS3 2016 prior to that.
#They're *almost* exactly the same geographies
#The only obvious difference is:
#"Bournemouth + Poole / Dorset" vs "Bournemouth + Poole + Christchurch / Dorset"
#Where in the latter, Christchurch is part of the Dorset boundary
#So that little niggle will need fixing by combining into a single "Dorset" region.

#But that's from eyeballng.
#Let's check them with an intersect / area check
#Where large % area diffs will highlight larger differences to look at
itl3.geo <- st_read('../YPERN_dataexplore/data/geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp') %>% st_simplify(preserveTopology = T, dTolerance = 100)

nuts3.geo <- st_read("~/Dropbox/MapPolygons/UK/NUTS_Level_3_January_2018_GCB_in_the_United_Kingdom_2022_-2838064862322809072/NUTS_Level_3_January_2018_GCB_in_the_United_Kingdom.shp")

#Intersect, add areas...

#https://github.com/r-spatial/sf/issues/860
#Then general "make valid"... 
interz <- itl3.geo %>%
  select(ITL321NM) %>% 
  st_make_valid() %>%
  st_set_precision(1e5) %>%
  st_intersection(
    nuts3.geo %>%
      select(nuts318nm) %>% 
      st_set_precision(1e5) %>%
      st_make_valid()
    ) %>% 
  mutate(area = as.numeric(st_area(.)))


#Find % of entire area for arbitrary larger geography
interz <- interz %>% 
  group_by(ITL321NM) %>% 
  mutate(area_percent = (area / sum(area)) * 100) %>% 
  ungroup()

#Examined by ordering and looking in QGIS - 
#Confirmed, it's only Bournemouth. Christchurch and Poole that differ




