#Miscellaneous little jobs
library(tidyverse)
library(sf)
library(tmap)
source('functions/misc_functions.R')


# CREATE BESPOKE ITL3 GEOGRAPHY WHERE ITL2 FOR "DORSET/SOMERSET" replaces ITL3 for those and Bournemouth/Christchurch/Poole

#Use the ITL3 geography and union the appropriate zones, so we avoid any polygon mismatch issues
itl3.geo <- st_read('data/ITL_geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp')

plot(st_geometry(itl3.geo))

#check names
itl3.geo %>% 
  st_set_geometry(NULL) %>% 
  select(ITL321NM) %>% 
  filter(
    qg('bourne|dorset|somerset', ITL321NM)
  ) %>% pull

#Separate out the zones to union:
ds <- itl3.geo %>% 
  filter(
    ITL321NM!='Bath and North East Somerset, North Somerset and South Gloucestershire',#drop this one first so don't pick up in next line
    qg('bourne|dorset|somerset', ITL321NM)#keep these
  )

#check!
#Tick, 3 obs
ds %>% 
  st_set_geometry(NULL) %>% 
  select(ITL321NM) %>% 
  filter(
    qg('bourne|dorset|somerset', ITL321NM)
  ) %>% pull

# plot(st_geometry(ds))




#Get correct names for ITL2 from elsewhere
itl2.geo <- st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp')

itl2.geo %>% 
  st_set_geometry(NULL) %>% 
  select(ITL221NM,ITL221CD) %>% 
  filter(
    qg('dorset', ITL221NM)
  )


#Get BRES/GVA data we want this to link to, to match column and zone names there
#Some tweaking likely necessary...
bg <- read_csv('data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_currentprices_ITL3_SIC_3GROUPS_MINUSimputedrent_2022.csv')

names(bg)

#Dissolve into single zone
#And add in appropriate columns
#https://github.com/r-spatial/sf/issues/243
ds.combo <- st_union(ds) %>% 
  st_sf %>% 
  mutate(ITL_code = "TLK2", GEOGRAPHY_NAME = "Dorset and Somerset")



#Create bespoke geography from those
itl3.bespoke <- itl3.geo %>% 
  filter(
    !ITL321NM %in% ds$ITL321NM#keep only names NOT in the three we pulled out to replace
  ) %>% 
  rename(ITL_code = ITL321CD, GEOGRAPHY_NAME = ITL321NM) %>% 
  select(ITL_code, GEOGRAPHY_NAME) %>% 
  bind_rows(
    ds.combo#Add the ITL2 zone in...
  )

#Check... tick
itl3.bespoke %>% 
  st_set_geometry(NULL) %>% 
  select(GEOGRAPHY_NAME) %>% 
  filter(
    qg('bourne|dorset|somerset', GEOGRAPHY_NAME)
  ) %>% pull
  

#Names to alter to get to match (again, better system using codes and one canonical list plz!)
#List looks familiar, we have alternatives to swap out
itl3.bespoke <- itl3.bespoke %>%
  mutate(
    GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME),
    GEOGRAPHY_NAME = case_when(
      GEOGRAPHY_NAME == "Inverness and Nairn, Moray, and Badenoch and Strathspey" ~ "Inverness & Nairn and Moray, Badenoch & Strathspey",
      # GEOGRAPHY_NAME == "Inverness and Nairn, Moray, Badenoch and Strathspey" ~ "Inverness & Nairn and Moray, Badenoch & Strathspey",
      GEOGRAPHY_NAME == 'Caithness and Sutherland, and Ross and Cromarty' ~ 'Caithness & Sutherland and Ross & Cromarty',
      GEOGRAPHY_NAME == 'Lochaber, Skye and Lochalsh, Arran and Cumbrae, and Argyll and Bute' ~ 'Lochaber, Skye & Lochalsh, Arran & Cumbrae and Argyll & Bute',
      GEOGRAPHY_NAME == 'Na h-Eileanan Siar' ~ 'Na h-Eileanan Siar (Western Isles)',
      GEOGRAPHY_NAME == 'City of Edinburgh' ~ 'Edinburgh, City of',
      GEOGRAPHY_NAME == 'Perth and Kinross, and Stirling' ~ 'Perth & Kinross and Stirling',
      GEOGRAPHY_NAME == 'East Dunbartonshire, West Dunbartonshire, and Helensburgh and Lomond' ~ 'East Dunbartonshire, West Dunbartonshire and Helensburgh & Lomond',
      GEOGRAPHY_NAME == 'Inverclyde, East Renfrewshire, and Renfrewshire' ~ 'Inverclyde, East Renfrewshire and Renfrewshire',
      GEOGRAPHY_NAME == 'Dumfries and Galloway' ~ 'Dumfries & Galloway',
      .default = GEOGRAPHY_NAME
    ))

#All matches now good? Tick
table(unique(bg$GEOGRAPHY_NAME) %in% itl3.bespoke$GEOGRAPHY_NAME)
unique(bg$GEOGRAPHY_NAME)[!unique(bg$GEOGRAPHY_NAME) %in% itl3.bespoke$GEOGRAPHY_NAME]

plot(st_geometry(itl3.bespoke))

#Test some filesizes
#Geojson bigger than shapefile but small enough, and open
st_write(itl3.bespoke, 'data/ITL_geographies/ITL3_bespokeforGVA_BRESjoin.geojson')

#Check it works OK... tick
chk <- st_read( 'data/ITL_geographies/ITL3_bespokeforGVA_BRESjoin.geojson')
plot(st_geometry(chk))

nrow(chk)
length(unique(chk$GEOGRAPHY_NAME))
length(unique(chk$ITL_code))

#And match to joined data? Tick
#This way round as BRES join means no NI
table(unique(bg$ITL_code) %in% chk$ITL_code)
table(unique(bg$GEOGRAPHY_NAME) %in% chk$GEOGRAPHY_NAME)


#Check zone count in the joined data
length(unique(bg$ITL_code))
length(unique(bg$GEOGRAPHY_NAME))

#Err
# walk(2015:2022, ~ bg %>% filter(DATE == .x) %>% nrow %>% print)


#Join and plot to really really check
#Inner join to drop NI zones from ITL3 bespoke
chk2 <- chk %>% 
  inner_join(
    # bg %>% filter(DATE == '2019'), by = 'ITL_code'
    bg, by = 'ITL_code'
  ) %>% 
  mutate(gvaperFTjob = (gva/JOBCOUNT_FULLTIME)*1000000)


#plot manuf GVA per job
tm_shape(chk2 %>% filter(DATE == 2022, qg('services',SIC07_description))) +
  # tm_polygons('percentdiff', n = 6)
  tm_polygons('gvaperFTjob', n = 7, palette="PRGn")


tm_shape(chk2 %>% filter(DATE == 2022)) +
  tm_polygons('gvaperFTjob', n = 7, palette="PRGn") +
  tm_facets(by = "SIC07_description", nrow = 1, free.scales = T)










