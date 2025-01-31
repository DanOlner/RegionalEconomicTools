#BRES API download
library(tidyverse)
library(nomisr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD LATEST BRES DATA FOR ITL2 ZONES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Find latest year
time <- nomis_get_metadata(id = "NM_189_1", concept = "TIME")
latestyear <- as.numeric(time$id[length(time$id)])

years = c(2015:latestyear)

#Reminder of geographies
#TYPE429 is ITL2 level
#TYPE428 is ITL3 level
#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 40)

#Looking for some other ways to reduce download size (industry never works...)
# nomis_get_metadata(id = "NM_189_1", concept = "INDUSTRY")
# nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
# nomis_get_metadata(id = "NM_189_1", concept = "TIME")

#Function to get data from NOMIS, save locally before combining for public repo
download_all_BRESopen <- function(year,employment_status,filename_append){
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), 
                      geography = "TYPE438", measures = 20100, 
                      EMPLOYMENT_STATUS = employment_status,
                      select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
                      )
  print('tick')
  saveRDS(z,paste0('local/data/BRES_NUTS2_',filename_append,'_',year,'.rds'))
}




#FULL TIME (run "EMPLOYMENT_STATUS" line above to see correct codes )
x <- Sys.time()
lapply(years, function(x) download_all_BRESopen(x,2,'FULLTIME'))
Sys.time() - x


#Combine into one DF (50mb)
itl2.bres <- list.files(path = "local/data/", pattern = "BRES_NUTS2_FULLTIME", full.names = T) %>% 
  map(readRDS) %>%
  bind_rows()

#Save for public repo
#Maybe not CSV, they're ~150mb
# write_csv(itl2.bres,paste0('data/BRES_FULLTIME_NUTS2_',years[1],'_',years[length(years)],'.csv'))
saveRDS(itl2.bres,paste0('data/BRES_FULLTIME_NUTS2_',years[1],'_',years[length(years)],'.rds'))




#PART TIME (run "EMPLOYMENT_STATUS" line above to see correct codes )
x <- Sys.time()
lapply(years, function(x) download_all_BRESopen(x,3,'PARTTIME'))
Sys.time() - x

#Combine into one DF
itl2.bres <- list.files(path = "local/data/", pattern = "BRES_NUTS2_PARTTIME", full.names = T) %>% 
  map(readRDS) %>%
  bind_rows()

#Save for public repo
#Maybe not CSV, they're ~150mb
# write_csv(itl2.bres,paste0('data/BRES_PARTTIME_NUTS2_',years[1],'_',years[length(years)],'.csv'))
saveRDS(itl2.bres,paste0('data/BRES_PARTTIME_NUTS2_',years[1],'_',years[length(years)],'.rds'))










#Next parts done in "BRES_process.R'