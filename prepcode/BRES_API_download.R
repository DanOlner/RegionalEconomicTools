#BRES API download
library(tidyverse)
library(nomisr)
source('functions/datadownload_functions.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD LATEST BRES DATA FOR ITL2 ZONES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Find latest available year for all BRES data
#(Though note, below, some geographies at time of writing have no data in certain years)
time <- nomis_get_metadata(id = "NM_189_1", concept = "TIME")
latestyear <- as.numeric(time$id[length(time$id)])

#2015 is earliest in BRES data
years = 2015:latestyear

#TYPE437 is nuts 2016 level 3 - Bournemouth, Christchurch and Poole don't match ITL3, the rest does (see intersection checks in misc_checks.R )
#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2 2021
#TYPE429 is ITL2 (2021)
#TYPE428 is ITL3 (2021)

#Use this to view all geography types
#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 40)

#Looking for some other ways to reduce download size (industry never works...)
# nomis_get_metadata(id = "NM_189_1", concept = "INDUSTRY")
# nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
# nomis_get_metadata(id = "NM_189_1", concept = "TIME")


#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2 2021
#FULL TIME (run "EMPLOYMENT_STATUS" line above to see correct codes )
runBRESdownloader.fortheseyears(years = years, geography = "TYPE438", EMPLOYMENT_STATUS = 2)

#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2 2021
#PART TIME
# runBRESdownloader.fortheseyears(years = years, geography = "TYPE438", EMPLOYMENT_STATUS = 3)

#TYPE437 is nuts 2016 level 3 - geography only doesn't match ITL3 for Bournemouth/Poole/Christchurch, rest does
#FULL TIME
runBRESdownloader.fortheseyears(years = years, geography = "TYPE437", EMPLOYMENT_STATUS = 2)

#ITL2 and 3 (data only available for 2022 and 2023)
#Full time for both
#Run this version to prove data not available for anything but 2022 to 2023
# runBRESdownloader.fortheseyears(years = years, geography = "TYPE429", EMPLOYMENT_STATUS = 2)
runBRESdownloader.fortheseyears(years = 2022:2023, geography = "TYPE429", EMPLOYMENT_STATUS = 2)
runBRESdownloader.fortheseyears(years = 2022:2023, geography = "TYPE428", EMPLOYMENT_STATUS = 2)
