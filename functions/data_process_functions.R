#DATA PROCESS FUNCTIONS
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
  
  # Extract text of passed args for getting filename labels
  # Do this first so we can check if the file has already been downloaded
  dots <- list(...)
  
  geography <- dots[["geography"]]
  
  EMPLOYMENT_STATUS <- dots[["EMPLOYMENT_STATUS"]]
  
  #Text for filename
  #id will be unique, don't need to filter on var
  geog_text_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == geography]
  employmentstatus_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == EMPLOYMENT_STATUS]
  # geog_text_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == geog_type]
  # employmentstatus_forfilename <- bres.var.labels$labelforfilename[bres.var.labels$id == employment_status]
  
  filename_text = paste0('local/data/BRES/INDIV_YEARS/BRES_',
                         geog_text_forfilename,'_', employmentstatus_forfilename  ,'_',year,'.rds')
  
  if(!file.exists(filename_text)){
  
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), 
                      # geography = geog_type, 
                      ...,#geography and EMPLOYMENT_STATUS passed through to NOMIS directly
                      MEASURE = 1,#1 is "Count", 2 is "Industry percent"
                      MEASURES = 20100,#20100 is "value", 20301 is "percent" (which is redundant as "value" of "industry percent" is percent)
                      # EMPLOYMENT_STATUS = employment_status,
                      select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
                      
  )
  
  cat('Saving ', filename_text, '\n')
  saveRDS(z,filename_text)
  
  } else {
    cat(year, 'already downloaded, moving on. (Delete contents of folder if you want to replace.)\n')
  }
  
  
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
    'local/data/BRES/BRES_ALLYEARSWITHDATA_',filepattern,'_',
    #years[1],'_',years[length(years)],
    min(itl2.bres$DATE),'_',max(itl2.bres$DATE),
    '.rds'
  )
  
  #Save for public repo
  # write_csv(itl2.bres,paste0('data/BRES_FULLTIME_NUTS2_',years[1],'_',years[length(years)],'.csv'))
  saveRDS(itl2.bres, filename_text)
  
  cat('Saved ', filename_text, '\n')

}




# REGIONAL GVA / BRES LINK FUNCTIONS----

## MAKE LONG LIST OF SIC SECTORS FROM THE REGIONAL GVA'S ITL2 AND ITL3 BESPOKE SIC CODES

#What is this doing?
#ONS regional GVA data has ITL2 and ITL3 data, with SIC codes at 2 digit level
#But they're bespoke - some SIC codes are grouped together e.g. 
#Mining and quarrying: 5 to 9 grouped, each separate in the original 2 digit SICs

#This makes for entertainment when trying to link other SIC coded datasets to it - 
#like BRES, which uses all the separate codes.

#One approach to fixing that:
#Take the ONS regional SIC list with its bespoke SICs
#And break each grouped SIC so that it's spread over several rows
#E.g. again Mining and quarrying: 5 to 9 is currently on one line
#This code will spread that out onto 5 separate rows, 5,6,7,8,9

#So that the BRES job count data can then be joined by SIC 2 digit code
#And summed by the bespoke SIC sectors in the GVA data
#To give job counts for those bespoke sectors

#This faff is only necessary for the 2 digit codes - SIC *sections* do match across both

#We also want the function to work for BOTH ITL2 & 3
#Which have *different* bespoke SIC bins (fewer at the smaller geography)
#72 categories for ITL2 and 48 for ITL3

#NOTE ALSO: spreading out all the numbers, some won't be actual 2 digit SICs
#E.g. there's no 4.

#Pass in ITL GVA dataframe as is, function will extract what it needs
make.GVA.SICs.long <- function(ITL.df){
  
  distinct.list <- ITL.df %>% select(SIC07_description,SIC07_code) %>%
    distinct(SIC07_description, .keep_all = T) %>% 
    filter(!qg('imp',SIC07_code))
  
  #Keep copy to merge in later
  df <- distinct.list
  
  #Pull out numbers in brackets, don't need the rest
  #Can then split into separate SIC codes next...
  #https://stackoverflow.com/a/8613332
  repl <- regmatches(df$SIC07_code, gregexpr("(?<=\\().*?(?=\\))", df$SIC07_code, perl=T))
  repl[lengths(repl) == 0] <- NA
  repl <- unlist(repl)
  
  df$SIC07_code <- ifelse(is.na(repl), df$SIC07_code, repl)
  
  
  # Split the rows with hyphenated codes
  split_rows <- function(row) {
    
    start_code <- as.numeric(strsplit(row$SIC07_code, "-")[[1]][1])
    end_code <- as.numeric(strsplit(row$SIC07_code, "-")[[1]][2])
    
    codes <- as.character(start_code:end_code)
    
    data.frame(
      SIC07_description = rep(row$SIC07_description, length(codes)),
      SIC07_code = codes,
      stringsAsFactors = FALSE
    )
    
  }
  
  # Identify the rows to be split
  rows_to_split <- grepl("-", df$SIC07_code)
  
  # Use lapply to preserve dataframe structure and then rbind to combine the rows
  expanded_rows <- do.call(rbind, lapply(1:nrow(df), function(i) {
    if (rows_to_split[i]) {
      split_rows(df[i, , drop = FALSE])
    }
  }))
  
  # Remove the hyphenated rows from the original dataframe
  df <- df[!rows_to_split, ]
  
  # Combine the two dataframes
  df <- rbind(df, expanded_rows)
  
  df <- df %>% mutate(SIC07_code_numeric = as.numeric(SIC07_code))
  
  #Add original ITL grouped SIC codes 
  #Same SIC names, so just joining back in
  df <- df %>% 
    left_join(
      distinct.list %>% rename(SIC07_code_fromGVAdata = SIC07_code),
      by = 'SIC07_description'
    ) %>% 
    relocate(SIC07_code_fromGVAdata, .after = SIC07_description) %>% 
    arrange(SIC07_code_numeric)
  
  #remove any non-existent SIC rows e.g. there's no 4 in 2 digit SICs.
  df %>% 
    filter(
      SIC07_code_numeric %in% unique(
        as.numeric(read_csv('data/SIClookup.csv',show_col_types = FALSE)$SIC_2DIGIT_CODE)
        ))
  
}






#Take in filenames, load the RDS and - assuming it's a dataframe/tibble - 
#Resave as zipped CSV (so can be accessed by non-R-users)
#Will need to save temp CSV in same folder then delete
Convert.RDS.to.zipped.CSV <- function(filenames){
  
  filenames %>% walk(~ {
    
  }
  )
  
  
  #Use the zip library for system-agnostic zipping
  
  
}






