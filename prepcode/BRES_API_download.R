#BRES API download
library(tidyverse)
library(nomisr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD LATEST BRES DATA FOR ITL2 ZONES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Find latest year
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



#For adding employment status text to filename
employment_status_char <- nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS") %>% 
  mutate(
    employmentstatus_labelforfilename = str_replace_all(label.en, "[^[:alnum:]]", "")
    )

#Do same for geography, though include the code also
geog_char <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type") %>% 
  mutate(
    geog_labelforfilename = paste0(id,'_',str_replace_all(label.en, "[^[:alnum:]]", ""))
    )



#Function to get data from NOMIS, save locally before combining for public repo
#See misc_checks.R for more on the MEASURES selection... it's a tad confusing
download_BRES <- function(year,geog_type,employment_status){
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), 
                      geography = geog_type, 
                      MEASURE = 1,#1 is "Count", 2 is "Industry percent"
                      MEASURES = 20100,#20100 is "value", 20301 is "percent" (which is redundant as "value" of "industry percent" is percent)
                      EMPLOYMENT_STATUS = employment_status,
                      select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
                      )
  
  geog_text_forfilename <- geog_char$geog_labelforfilename[geog_char$id == geog_type]
  employmentstatus_forfilename <- employment_status_char$labelforfilename[employment_status_char$id == employment_status]
  
  filename_text = paste0('local/data/BRES/INDIV_YEARS/BRES_',geog_text_forfilename,'_', employmentstatus_forfilename  ,'_',year,'.rds')
  
  cat('Saving ', filename_text, '\n')
  
  saveRDS(z,filename_text)
  
}



#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2 2021
#FULL TIME (run "EMPLOYMENT_STATUS" line above to see correct codes )
geog_type = "TYPE438"
employment_status = 2

#USE FUNCTION ABOVE TO SAVE SEPARATE YEARS FROM BRES, TO BE COMBINED INTO SINGLE DF NEXT
x <- Sys.time()
lapply(years, function(x) download_BRES(x,geog_type,employment_status))
Sys.time() - x


#Text for getting filenames to combine
geog_text_forfilename <- geog_char$geog_labelforfilename[geog_char$id == geog_type]
employmentstatus_forfilename <- employment_status_char$labelforfilename[employment_status_char$id == employment_status]

filepattern = paste0('BRES_',geog_text_forfilename,'_', employmentstatus_forfilename)

#Combine into one DF (50mb)
itl2.bres <- list.files(path = "local/data/BRES/INDIV_YEARS/", pattern = filepattern, full.names = T) %>% 
  map(readRDS) %>%
  bind_rows()


#Remove years that have no data
#E.g. NUTS2 and 3 don't have data for 2023 (ITL 2 and 3 do, but don't have any prior to 2022)
#Some years have missing values e.g. due to no DEFRA data for Scotland
#We want ONLY to remove ENTIRE years with no data

#This number of NAs in a year means it's not got any data
obs_per_yr = nrow(itl2.bres) / length(unique(itl2.bres$DATE))

NAs = table(is.na(itl2.bres$OBS_VALUE), itl2.bres$DATE) %>% as.data.frame()

years_to_remove = NAs %>% filter(Var1 == 'TRUE', Freq == obs_per_yr) %>% select(Var2) %>% pull

itl2.bres <- itl2.bres %>% 
  filter(!DATE %in% years_to_remove)

#Save for public repo
# write_csv(itl2.bres,paste0('data/BRES_FULLTIME_NUTS2_',years[1],'_',years[length(years)],'.csv'))
saveRDS(itl2.bres,paste0(
  'data/BRES/BRES_ALLYEARSWITHDATA_',filepattern,'_',
  #years[1],'_',years[length(years)],
  min(itl2.bres$DATE),'_',max(itl2.bres$DATE),
  '.rds'
  )
  )







#PART TIME (run "EMPLOYMENT_STATUS" line above to see correct codes )
x <- Sys.time()
lapply(years, function(x) download_all_BRESopen(x,3,'PARTTIME'))
Sys.time() - x

#Combine into one DF
itl2.bres <- list.files(path = "local/data/BRES/", pattern = "BRES_NUTS2_PARTTIME", full.names = T) %>% 
  map(readRDS) %>%
  bind_rows()

#Save for public repo
#Maybe not CSV, they're ~150mb
# write_csv(itl2.bres,paste0('data/BRES_PARTTIME_NUTS2_',years[1],'_',years[length(years)],'.csv'))
saveRDS(itl2.bres,paste0('data/BRES_PARTTIME_NUTS2_',years[1],'_',years[length(years)],'.rds'))










#Next parts done in "BRES_process.R'