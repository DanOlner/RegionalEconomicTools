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



#ITL2 first----

#To make the Dorset ITL2 geog available to drop into ITL3
#to sort the Christchurch issue

for(SICgrouping in c('3GROUPS','SIC_SECTION','SIC_2DIGIT')){
  
  cat(SICgrouping,'\n')

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
    sics.forBRESjoin <- make.GVA.SICs.long(all.gva[[1]])
    
    all.bres.wlookup <- all.bres %>% 
      map(~ .x %>% 
            left_join(sics.forBRESjoin, by = c('SIC_CODE2' = 'SIC07_code_numeric'))
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
  #And joins both to each gva file (passed in as the reduce .init argument)
  #Returns a list of each GVA/bres FT/bres PT combo
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
    
  
  
}#end ITL2 SICgrouping for







# ITL3 SECOND----

#Different geog name matches to change
#And bespoke ITL2 replacement to drop in to deal with christchurch issue
#So need to run second, so ITL2 is done and ready


for(SICgrouping in c('3GROUPS','SIC_SECTION','SIC_2DIGIT')){
  
  cat(SICgrouping,'\n')

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
  gva.filenames <- gva.filenames[qg('ITL3',gva.filenames)]
  

  
  #Load into list
  #Slight geog tweak to get full matches on geog name
  #This is for BRES NUTS2 to GVA ITL2... (so will prob need updating for ITL2-ITL2 matches, huzzah)
  all.gva <- gva.filenames %>% map(read_csv, show_col_types = F) %>% 
    map(~ .x %>%
        mutate(
          Region_name = gsub(' CC','',Region_name),
          Region_name = case_when(
            Region_name == "Inverness and Nairn, Moray, Badenoch and Strathspey" ~ "Inverness & Nairn and Moray, Badenoch & Strathspey",
            Region_name == 'Caithness and Sutherland, and Ross and Cromarty' ~ 'Caithness & Sutherland and Ross & Cromarty',
            Region_name == 'Lochaber, Skye and Lochalsh, Arran and Cumbrae, and Argyll and Bute' ~ 'Lochaber, Skye & Lochalsh, Arran & Cumbrae and Argyll & Bute',
            Region_name == 'Na h-Eileanan Siar' ~ 'Na h-Eileanan Siar (Western Isles)',
            Region_name == 'City of Edinburgh' ~ 'Edinburgh, City of',
            Region_name == 'Perth and Kinross, and Stirling' ~ 'Perth & Kinross and Stirling',
            Region_name == 'East Dunbartonshire, West Dunbartonshire, and Helensburgh and Lomond' ~ 'East Dunbartonshire, West Dunbartonshire and Helensburgh & Lomond',
            Region_name == 'Inverclyde, East Renfrewshire, and Renfrewshire' ~ 'Inverclyde, East Renfrewshire and Renfrewshire',
            Region_name == 'Dumfries and Galloway' ~ 'Dumfries & Galloway',
            .default = Region_name
          ))
    )
  
  
  
  
  #We'll join just the TWO BRES files for ITL2 + the correct SIC group to each of these
  #One for full time, one for part time
  BRES.filenames <- list.files(
    path = 'data/BRES/separate_SIC_types_summedfrom5digitSIC',
    pattern = SICgrouping,
    full.names = T
  )
  
  #And we'll actually use NUTS2 at this point, to get 2015 to most recent year in that data
  #(Will have to update for later ITL3 years...)
  BRES.filenames <- BRES.filenames[qg('TYPE437',BRES.filenames)]
  
  #Read those in
  #And also delete unnecessary column (that it's good to leave in if used directly)
  #and rename so join can happen smoothly
  
  #Renaming gsubs to match ITL3
  #Good place here to look for better way to match geogs!
  all.bres <- BRES.filenames %>% map(read_csv, show_col_types = F) %>% 
    map(
      ~ .x %>% rename(
        SIC_CODE = contains('_CODE'),
        geog = GEOGRAPHY_NAME#temp name change to drop other col, faff!
        ) %>%
        select(-contains('_NAME')) %>% 
        rename(GEOGRAPHY_NAME = geog) %>% 
        mutate(GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME))
    )  
  
  #confirm geog columns all match
  #Lead on BRES match, as there's no NI in the BRES data, it'll be dropped from GVA with inner join
  
  #For ITL3, Bournemouth and Poole won't match - will be replacing whole area with ITL2 below
  
  # table(unique(all.bres[[1]]$GEOGRAPHY_NAME) %in% all.gva[[1]]$Region_name)
  # unique(all.bres[[1]]$GEOGRAPHY_NAME)[!unique(all.bres[[1]]$GEOGRAPHY_NAME) %in% all.gva[[1]]$Region_name]
  
  
  
  
  
  #If 2 digit, have to sum BRES counts to match ONS bespoke 2 digit categories
  if(SICgrouping == 'SIC_2DIGIT'){
    
    #Nice tidy function to create the correct lookup! 
    #USE ITL3 VERSION
    sics.forBRESjoin <- make.GVA.SICs.long(all.gva[[1]])
    
    all.bres.wlookup <- all.bres %>% 
      map(~ .x %>% 
            left_join(sics.forBRESjoin, by = c('SIC_CODE2' = 'SIC07_code_numeric'))
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
  
  
  #LAST ITL3 TASK:
  #1. Substitute in the Dorset ITL2 for ITL3s Dorset + Somerset + B/C/Poole
  #Because: they don't match between ITL3 and NUTS3 - 
  #ITL3 has "Bournemouth, Christchurch and Poole"
  #NUTS3 is "Bournemouth and Poole"
  #Christchurch moved from Dorset into the B+P zone for ITL3
  
  #Am replacing with the overlapping ITL2 because:
  #We could sum up BC+P and Dorset for non-chained volume zones
  #But chained volume would *need* to have the entire ITL2 replaced
  #As it's the only directly available data (reminder, CV can't be summed across places)
  #So to keep consistent geography so can compare like with like, do for all
  
  #Confirm which zones to remove to be replaced with the ITL2 values
  #(Looking at the zones in QGIS)
  #Bath and north east somerset is a different zone, can leave that one...
  # unique(gva.n.bres[[1]]$GEOGRAPHY_NAME)[qg('bourne|dorset|somerset',unique(gva.n.bres[[1]]$GEOGRAPHY_NAME))]
  
  #Version with those areas removed (BC+P already missing of course)
  gva.n.bres.geogedit <- gva.n.bres %>% 
    map(~.x %>% filter(
      !GEOGRAPHY_NAME %in% c('Somerset','Dorset')
      )
    )
  
  #tick
  # unique(gva.n.bres.geogedit[[1]]$GEOGRAPHY_NAME)[qg('bourne|dorset|somerset',unique(gva.n.bres.geogedit[[1]]$GEOGRAPHY_NAME))]
  
  
  #Get the correct matching ITL2 files to drop in the matching ITL2 data
  #Easiet way is just to repeat the ITL2 version of the filename finder
  match.filenames <- list.files(
    path = 'data/regionalGVA',
    pattern = SICgrouping,
    full.names = T
  )
  
  #We only ever want the long data versions
  match.filenames <- match.filenames[qg('long',match.filenames)]
  
  #And we only ever want excluding imputed rent
  #If we're going to be dividing by job numbers
  
  #Unless SIC 2 digit because the join will take care of it!
  if(!SICgrouping == 'SIC_2DIGIT'){
    match.filenames <- match.filenames[qg('imputed',match.filenames)]
  }
  
  #And only ITL2
  match.filenames <- match.filenames[qg('ITL2',match.filenames)]
  
  match.filenames <- basename(match.filenames) %>%
    gsub('_LONG','',.) %>%
    gsub('regionalGVA_','data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_',.)
    
  #Get matching ITL2 files
  #(Which will of course only work if the ITL2 loop above is run first)
  match.gva <- match.filenames %>% map(read_csv, show_col_types = F)
    
    
  
  #col names all good? Tick.
  # walk2(match.gva, gva.n.bres.geogedit, ~{
  #     print(table(names(.x) %in% names(.y)))
  #   })
  
  #Nab the appropriate ITL2 from the match data and bind rows
  gva.n.bres.tweakedITL3 <- gva.n.bres.geogedit %>% 
    map2(match.gva, 
      ~ .x %>% 
      bind_rows(
        .y %>% filter(GEOGRAPHY_NAME == 'Dorset and Somerset')
      )
    )
  
  #Confirm correct data joined... tick
  # match.gva[[1]] %>% filter(qg('dorset',GEOGRAPHY_NAME))
  # gva.n.bres.tweakedITL3[[1]] %>% filter(qg('dorset',GEOGRAPHY_NAME))
  # 
  # gva.n.bres[[1]] %>% filter(qg('sheff',GEOGRAPHY_NAME))
  # gva.n.bres.tweakedITL3[[1]] %>% filter(qg('sheff',GEOGRAPHY_NAME))
  
  
  
  #Tweak filenames
  newnames <- basename(gva.filenames) %>%
    gsub('_LONG','',.) %>%
    gsub('regionalGVA_','data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_',.)
  
  #saaaave
  gva.n.bres %>% walk2(newnames, ~ {
    .x %>% write_csv(file = .y)
  })
    
  
  
}#end ITL3 SICgrouping for
