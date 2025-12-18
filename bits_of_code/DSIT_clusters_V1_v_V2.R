# Checking difference between DIST innovation cluster data
# Version 1 vs version 2
library(tidyverse)
library(readODS)
library(sf)
library(tmap)
source('functions/misc_functions.R')

options(scipen = 999)

# VERSION 1----

# Version 1 data available in my dropbox here:
# https://www.dropbox.com/scl/fi/ijew6qf42tuq93pqaubzs/DSIT_innovationclusters_v1_FullFinalClusterExport_120224Update.xlsx?rlkey=b9vawp37cp6ugxqby6u1djfd5&dl=1
# Wayback machine downloadable here: https://web.archive.org/web/20240420222644/https://www.innovationclusters.dsit.gov.uk/app/uploads/2024/02/FullFinalClusterExport_120224Update.xlsx
# Original website here: https://web.archive.org/web/20240420222343/https://www.innovationclusters.dsit.gov.uk/
# Orig method report site here: https://web.archive.org/web/20240213191133/https://www.gov.uk/government/publications/identifying-and-describing-uk-innovation-clusters
# PDF report dropbox copy here: https://www.dropbox.com/scl/fi/rtcvchvn9tbpalvq8h2lk/VERSION1_uk-innovation-clusters-analytical-report.pdf?rlkey=fd4s15c2n62uaivh47vx48apm&dl=1

# From the first report data/methods:
# "Reported business counts, turnover and employee data for the firms in
# the RTIC clusters are based on Companies House. This data is limited
# in that only approximately 100,000 of the 5.3 million registered firms
# submit their full financials to Companies House, and only 75% of them
# submit employee data. Estimates are made based upon this limited
# data source. RTIC firm location data is restricted to (1) registered
# addresses; and (2) operating addresses, which The Data City have
# identified as listed on the firm’s website." p.48

url1 <- 'https://www.dropbox.com/scl/fi/ijew6qf42tuq93pqaubzs/DSIT_innovationclusters_v1_FullFinalClusterExport_120224Update.xlsx?rlkey=b9vawp37cp6ugxqby6u1djfd5&dl=1'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

# Each region is in its own sheet. We'll need to read each separately to get all the data for that
# Get sheet names first from the contents
# All ITL1 subregions...
# https://www.reddit.com/r/rstats/comments/nr3xhj/removing_rest_of_string_after_a_certain_character/
v1 = readxl::read_excel(path = p1f,range = "Sheet 1!A1:R3324") 

# Get rid of spaces in col names
names(v1) <- gsub(x = names(v1), pattern = ' ', replacement = '_')


# Checks
# We have the various different data sources...
unique(v1$DataSource)

# Do the 'pct share of cluster' values sum for all of those, or within each group?
v1 %>% 
  group_by(Sector_Name) %>% 
  summarise(sum(Pct_share_of_cluster)) %>% 
  print(n = 100)

# I think it's percent share for whole country but this dataset doesn't list every place that has is
# So it's less than 100%

# Also that's only using RTIC data i.e. Data City
# v1 %>% filter(DataSource == 'IDBR') %>% distinct(Sector_Name) %>% pull(Sector_Name)

# Yeah they're distinct (IDBR not quite SICs either?)
v1 %>% 
  group_by(DataSource) %>% 
  select(DataSource,Sector_Name) %>% 
  distinct() %>% 
  View


# So which value is being used for the percent of cluster size?
# It'll be the one with the perfect correlation for a particular RTIC...
pairs(
  v1 %>% filter(DataSource == 'RTIC', Sector_Name == 'CleanTech') %>% select(Estimated_Employees:Pct_share_of_cluster)
)

# Yep it's company location count (note, NOT distinct company count... 
# is the former meant to include branches of same company in different places?)

# Just to confirm for all RTICs
# ggplot(
#   v1 %>% filter(DataSource == 'RTIC'),
#   aes(x = Company_Location_Count, y = Pct_share_of_cluster)
# ) +
#   geom_point() +
#   facet_wrap(~Sector_Name, scales = 'free')

# Or rather easier... those are within rounding errors
v1 %>% 
  filter(DataSource == 'RTIC') %>% 
  group_by(Sector_Name) %>% 
  # summarise(r_squared = cor(Distinct_Company_Count, y = Pct_share_of_cluster)) %>% 
  summarise(r_squared = cor(Company_Location_Count, y = Pct_share_of_cluster)) %>%
  View

# So it was only ever count of firms in the first place
# Where does SY come in other measures?
v1 %>% filter(DataSource == 'RTIC', Sector_Name == 'CleanTech') %>% View

# Save an orderer version of that for others
write_csv(
  v1 %>% filter(DataSource == 'RTIC', Sector_Name == 'CleanTech') %>% arrange(desc(Pct_share_of_cluster)),
  'data/misc/RTIC_cleantechclusterdata_v1_ordered.csv'
  )

# South Yorkshire estimated employees 22K???
# Est turnover £19Billion... which is 5 billion more than the leading cluster in V2




# VERSION 2----

# Current live version: https://www.innovationclusters.dsit.gov.uk
# Doc and data page: https://www.gov.uk/government/collections/uk-innovation-clusters
# Summary/methods HTML page: https://www.gov.uk/government/publications/innovation-clusters-map-summary-and-methods/innovation-clusters-map-summary-and-methods

url1 <- 'https://assets.publishing.service.gov.uk/media/68dd21b9dadf7616351e4cc4/Innovation_clusters_data_f.ods'
p1f <- tempfile(fileext=".ods")
download.file(url1, p1f, mode="wb") 

v2 = readODS::read_ods(p1f, sheet = 'Data') 

# Get rid of spaces in col names
names(v2) <- gsub(x = names(v2), pattern = ' ', replacement = '_')

# NAs to get numerics set properly
v2[v2=='c'] = NA

# Convert to numeric
v2 = v2 %>% 
  mutate(
    across(Employment:Turnover, as.numeric)
  )


# Let's look just at RTIC again (noting the geography won't be the same as the boundaries have changed...)
v2.rtic = v2 %>% filter(Sector_Type == 'RTIC')
unique(v2.rtic$Sector)

# Let's look at the other types too, just to check
unique(v2$Sector_Type)

unique(v2$Sector[v2$Sector_Type == unique(v2$Sector_Type)[3]])

v2.rtic %>% filter(Sector == 'Cleantech') %>% View

# View all sectors in the different types
walk(unique(v2$Sector_Type), \(x) v2 %>% filter(Sector_Type == x) %>% select(Sector) %>% distinct %>% print(n = 100))

# Write that for use elsewhere
write_csv(
  v2.rtic %>% filter(Sector == 'Cleantech') %>% arrange(desc(Site_Count)),
  'data/misc/RTIC_cleantechclusterdata_v2_ordered.csv'
)



# Will need to actually look at map to work out relative scales, no names given
v2.rtic.geo = v2.rtic %>% st_as_sf(wkt = 'Geometry') %>%  st_set_crs("EPSG:4326")

# Tick
plot(st_geometry(v2.rtic.geo %>% filter(Sector == 'Cleantech')))

# Plot cluster numbers
# plot(v2.rtic.geo %>% filter(Sector == 'Cleantech') %>% select(Cluster) %>% mutate(Cluster = as.character(Cluster)))
qtm(v2.rtic.geo %>% filter(Sector == 'Cleantech') %>% select(Cluster), text = 'Cluster')

# The South Yorkshire cluster overlaps Sheffield, Rotherham and Chesterfield
# It's cluster 4.


# PULL OUT ALL CLUSTERS THAT OVERLAP WITH SOUTH YORKSHIRE----

# Make geo for all sector types
v2.geo = v2 %>% st_as_sf(wkt = 'Geometry') %>%  st_set_crs("EPSG:4326")

# Get SY ITL2 zone for overlap check
sy = st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp') %>% filter(ITL221NM == 'South Yorkshire') %>% select(ITL221NM)


# Get info on all overlaps
# Keep ones with at least 2% overlap
interset_w_sy = intersect_makelookup(sy, st_transform(v2.geo, 'EPSG:27700'), vartogroupby_fromsmallerzone = Sector, keepall = T) %>% 
  filter(area_percent >= 2)

# Plot all those...
plot(st_geometry(interset_w_sy))

# Let's pull those clusters out of the original data so we have the whole geometry
# Do via inner join to keep just matches
sy_v2 = v2 %>%
  inner_join(
    interset_w_sy %>% select(Sector,Cluster),
    by = c('Sector','Cluster')
  )

# Re-sf!
sy_v2 = sy_v2 %>% st_as_sf(wkt = 'Geometry') %>%  st_set_crs("EPSG:4326") %>% st_transform('EPSG:27700')

plot(sy)
# plot(sy, xlim = st_bbox(sy_v2)[c(1, 3)], ylim = st_bbox(sy_v2)[c(2, 4)])
plot(st_geometry(sy_v2), , add = T)
# plot(st_geometry(st_transform(sy_v2,'EPSG:27700')), , add = T)

sy_v2 %>% arrange(Sector_Type,Cluster) %>% relocate(Sector_Type, .before = Sector) %>% View

# Save that version as CSV including the geogs
write_csv(
  sy_v2 %>% arrange(Sector_Type,Cluster),
  'data/misc/RTIC_DSIT_clusters_v2_southyorkshire_2percentormoreoverlap.csv'
)










