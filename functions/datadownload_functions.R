#DATA DOWNLOAD FUNCTIONS
#Misc functions
library(tidyverse)
library(nomisr)
#library(memoise)
source('functions/misc_functions.R')

# BRES DOWNLOAD FUNCTIONS----

## RUNS ONCE ON SOURCE - make DF for filename saves based on BRES var names----

#Set up if... just to stop repeatedly calling NOMIS API if this is made already
if(!exists("bres.var.labels")){

  employment_status <- nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS") %>%
    mutate(var = 'employment status')
  
  #Do same for geography, though include the code also
  geog <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type") %>%
    mutate(var = 'geography')
  
  bres.var.labels <- employment_status %>%
    bind_rows(geog) %>%
    mutate(
      labelforfilename = paste0(id,'_',str_replace_all(label.en, "[^[:alnum:]]", ""))
    )
  
  cat("Created BRES var labels via NOMIS API.\n")

}




## FUNCTION TO BE USED DIRECTLY: RUN THE DOWNLOADER FOR EACH YEAR, WHERE IT'S THEN SAVED, THEN RECOMBINE----

#Pass in the geography and EMPLOYMENT_STATUS argument that NOMIS wants for BRES
#Use those also for filenames
runBRESdownloader.fortheseyears <- function(years, ...){
  
  x <- Sys.time() 
  lapply(years, function(x) download_BRES(x, ...))
  print(paste0('BRES single year downloads: ',Sys.time() - x))
  
  recombineBRESsingleyears.intoONE.RDS(...)
  
}




## SUBFUNCTION: DOWNLOAD BRES DATA FOR DIFFERENT GEOGRAPHY TYPES AND EMPLOYMENT STATUS----

#Store as separate years, to be then combined into a single DF

#See "misc_checks.R" for digging into NOMIS use with BRES

#And also:
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

# download_BRES <- function(year,geog_type,employment_status){
download_BRES <- function(year, ...){ 
  
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), 
                      # geography = geog_type, 
                      ...,#geography and EMPLOYMENT_STATUS passed through to NOMIS directly
                      MEASURE = 1,#1 is "Count", 2 is "Industry percent"
                      MEASURES = 20100,#20100 is "value", 20301 is "percent" (which is redundant as "value" of "industry percent" is percent)
                      # EMPLOYMENT_STATUS = employment_status,
                      select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
  )
  
  # Extract text of passed args for getting filename labels
  dots <- list(...)
  
  geography <- dots[["geography"]]
  
  EMPLOYMENT_STATUS <- dots[["EMPLOYMENT_STATUS"]]
  
  #Text for filename
  #id will be unique, don't need to filter on var
  geog_text_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == geography]
  employmentstatus_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == EMPLOYMENT_STATUS]
  # geog_text_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == geog_type]
  # employmentstatus_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == employment_status]
  
  filename_text = paste0('local/data/BRES/INDIV_YEARS/BRES_',geog_text_forfilename,'_', employmentstatus_forfilename  ,'_',year,'.rds')
  
  cat('Saving ', filename_text, '\n')
  
  saveRDS(z,filename_text)
  
}




## SUBFUNCTION: RECOMBINE DOWNLOADED BRES DATA INTO SINGLE RDS FOR PUBLIC REPO----

recombineBRESsingleyears.intoONE.RDS <- function(...){
  
  cat("Starting to combine BRES separate year files into a single RDS...\n")
  
  # Extract text of passed args for getting filename labels
  dots <- list(...)
  
  geography <- dots[["geography"]]
  
  EMPLOYMENT_STATUS <- dots[["EMPLOYMENT_STATUS"]]
  

  #Text for getting filenames to combine
  #id will be unique, don't need to filter on var
  geog_text_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == geography]
  employmentstatus_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == EMPLOYMENT_STATUS]
  
  filepattern = paste0(geog_text_forfilename,'_', employmentstatus_forfilename)
  
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
  
  
  cat('Years in the dataframe: ', min(itl2.bres$DATE),'to',max(itl2.bres$DATE), '\n')
  
  itl2.bres <- itl2.bres %>% 
    filter(!DATE %in% years_to_remove)
  
  cat('Years with actual data (keeping only these): ', min(itl2.bres$DATE),' to ',max(itl2.bres$DATE), '\n')
  
  filename_text = paste0(
    'data/BRES/BRES_ALLYEARSWITHDATA_',filepattern,'_',
    #years[1],'_',years[length(years)],
    min(itl2.bres$DATE),'_',max(itl2.bres$DATE),
    '.rds'
  )
  
  #Save for public repo
  # write_csv(itl2.bres,paste0('data/BRES_FULLTIME_NUTS2_',years[1],'_',years[length(years)],'.csv'))
  saveRDS(itl2.bres, filename_text)
  
  cat('Saved ', filename_text, '\n')

}