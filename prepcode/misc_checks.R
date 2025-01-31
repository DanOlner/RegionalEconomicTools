#Miscellaneous data checks
library(tidyverse)
library(nomisr)
library(stringdist)
library(sf)

# CHECK BRES GEOGRAPHIES' MATCH TO OTHERS----

#We already know that NUTS2 2016 does match ITL2
#Check what most closely matches to ITL3

#Nab ITL3 codes and names from another source
ITL3 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SICsections_WIDE_2022.csv') %>% distinct(ITL_code, .keep_all = T) %>% select(1:2)

#Check what we can get from BRES
#TYPE438 is nuts 2016 level 2, which matches ITL2 including SY
#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 41)

#NUTS 2016 LEVEL 3 would be the obvious ITL3 match...?

#2023 data actually not there yet at these geographies, it would appear...
#Use 2022

#Ah - the list of geographies now actually contains ITLs:
z <- nomis_get_data(id = "NM_189_1", time = '2023', 
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
  mutate(area = st_area(.))









