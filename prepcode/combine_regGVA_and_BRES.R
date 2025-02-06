#combine GVA and BRES data, having processed it all elsewhere
#Tests for this all is misc_checks.R
library(tidyverse)
source('functions/data_process_functions.R')

#Aim: add BRES job counts (full and part time) to all the regional GVA files created

#There are distinct job count files, all summed appropriately
#that each match their own GVA file
#For:
#ITL2 v ITL3
#And within each of those:
#3 separate SIC groupings:
#Production, construction and services
#Sections
#2 digit (the bespoke groupings that the regional GVA data uses, different for ITL2 & 3)




#ITL2 AND 3 A SLIGHTLY DIFFERENT PROCESS
#plus zone-specific region name tweaks and geography fettling

#SIC 2 DIGIT also needs a pre-step
#Of summing the BRES data to the ONS bespoke SIC categories

#ITL2 first
for(SICgrouping in c('3GROUPS','SIC_SECTION','SIC_2DIGIT')){
# for(SICgrouping in c('3GROUPS','SIC_SECTION')){
  
  #Can run same code on both current price and CV files
  gva.filenames <- list.files(
        path = 'data/regionalGVA',
        pattern = SICgrouping,
        full.names = T
        )
  
  #We only ever want the long data versions
  gva.filenames <- gva.filenames[qg('long',gva.filenames)]
  
  #And we only ever want excluding imputed rent
  #If we're going to be dividing by job numbers
  
  #Unless SIC 2 digit because the join will take care of it!
  if(!SICgrouping == 'SIC_2DIGIT'){
    gva.filenames <- gva.filenames[qg('imputed',gva.filenames)]
  }
  
  #And only ITL2
  gva.filenames <- gva.filenames[qg('ITL2',gva.filenames)]
  

  
  #Load into list
  #Slight geog tweak to get full matches on geog name
  #This is for BRES NUTS2 to GVA ITL2... (so will prob need updating for ITL2-ITL2 matches, huzzah)
  all.gva <- gva.filenames %>% map(read_csv, show_col_types = F) %>% 
    map(
      ~ .x %>% mutate(
        Region_name = gsub('Bristol Area','Bristol area',Region_name),
        Region_name = gsub('Northumberland, and Tyne and Wear','Northumberland and Tyne and Wear',Region_name),
        Region_name = gsub('West Wales and The Valleys','West Wales',Region_name)
      )
    )
  
  
  
  
  #We'll join just the TWO BRES files for ITL2 + the correct SIC group to each of these
  #One for full time, one for part time
  BRES.filenames <- list.files(
    path = 'data/BRES/separate_SIC_types_summedfrom5digitSIC',
    pattern = SICgrouping,
    full.names = T
  )
  
  #And we'll actually use NUTS2 at this point, to get 2015 to most recent year in that data
  #(Will have to update for later ITL2 years...)
  BRES.filenames <- BRES.filenames[qg('TYPE438',BRES.filenames)]
  
  #Read those in
  #And also delete unnecessary column (that it's good to leave in if used directly)
  #and rename so join can happen smoothly
  all.bres <- BRES.filenames %>% map(read_csv, show_col_types = F) %>% 
    map(
      ~ .x %>% rename(
        SIC_CODE = contains('_CODE'),
        geog = GEOGRAPHY_NAME#temp name change to drop other col, faff!
        ) %>%
        select(-contains('_NAME')) %>% 
        rename(GEOGRAPHY_NAME = geog)
    )
  
  #confirm geog columns all match
  #Lead on BRES match, as there's no NI in the BRES data, it'll be dropped from GVA with inner join
  # table(unique(all.bres[[1]]$GEOGRAPHY_NAME) %in% all.gva[[1]]$Region_name)
  
  
  
  #If 2 digit, have to sum BRES counts to match ONS bespoke 2 digit categories
  if(SICgrouping == 'SIC_2DIGIT'){
    
    #Nice tidy function to create the correct lookup! 
    sics.forBRESjoin.itl2 <- make.GVA.SICs.long(gva.itl2)
    
    all.bres.wlookup <- all.bres %>% 
      map(~ .x %>% 
            left_join(sics.forBRESjoin.itl2, by = c('SIC_CODE2' = 'SIC07_code_numeric'))
      )
    
    #Sum! Get jobcount totals summed to the GVA bespoke SIC 2 digits 
    #overwrite, keep...
    all.bres <- all.bres.wlookup %>% 
      map(~ .x %>% 
            group_by(DATE,GEOGRAPHY_NAME,SIC07_code_fromGVAdata) %>% 
            summarise(
              JOBCOUNT = sum(JOBCOUNT)#Why we need this field to remain same for both for now
            ) %>% 
            ungroup() %>% 
            filter(!is.na(SIC07_code_fromGVAdata))
      ) %>% 
      map(~.x %>% rename(SIC_CODE = SIC07_code_fromGVAdata))
    
  }
  
  #Then can carry on...
  
  #COMBINE!
  #Map iterates over each gva file
  #reduce iterates over the BRES files (full and part time)
  #And joins both to each gva file
  #Returns a list of each GVA/bres FT/bres PT combo
  #Is also, as a bonus, horribly unreadable
  gva.n.bres <- map(all.gva, function(gva_df) {
    all.bres %>%
    reduce(~ .x %>% inner_join(
      .y,
      by = c('GEOGRAPHY_NAME', 'DATE', 'SIC_CODE')),
      .init = gva_df %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
                             SIC_CODE = SIC07_code ,gva = value)) %>% 
    relocate(DATE, .before = ITL_code) %>% 
    rename(JOBCOUNT_FULLTIME = JOBCOUNT.x, JOBCOUNT_PARTTIME = JOBCOUNT.y)
  })
  
  #Tweak filenames
  newnames <- basename(gva.filenames) %>%
    gsub('_LONG','',.) %>%
    gsub('regionalGVA_','data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_',.)
  
  #saaaave
  gva.n.bres %>% walk2(newnames, ~ {
    .x %>% write_csv(file = .y)
  })
    
  
  
}#end SICgrouping for
