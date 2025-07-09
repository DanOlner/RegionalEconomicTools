#Using data saved in BRES_API_download.R
#Take every combined BRES file in the data/BRES folder
#And sum up job counts from 5 digit counts into SIC 2 digit, SIC section and SIC 'production/construction/services'

#For an illustration of why summing from 5 digits gives likely better values than using the ones already present, see section "CHECK HOW THE "SECTION AND 2 DIGIT JOBCOUNTS SUMMED FROM 5 DIGIT" #compare tothe BRES original versions of 2 DIG and Sections" in "misc_checks.R" that shows even spread around the original values.
#Compare to the rounding method used to see why we can get more accurate values by summing:
#https://www.nomisweb.co.uk/articles/1103.aspx
library(tidyverse)
source('functions/misc_functions.R')


#1. LOAD AND PREP EACH ALL-YEARS BRES RDS----

#Get all relevant filenames from the public-facing BRES data folder
#(Keep filenames for later to resave; code is agnostic about each file's source, job counts will get summed the same)
bresfilenames <- list.files(path = "data/BRES/", pattern = 'ALLYEARSWITHDATA', full.names = T) 

#Put into a list to carry out the same summing task on each
BRESdata <- bresfilenames %>% map(readRDS)

#Keep only the 5 digit SICs, which we'll use to sum job counts to the others
#Apply to each df in list
BRESdata <- BRESdata %>%
  map(~ .x %>% filter(INDUSTRY_TYPE == 'SIC 2007 subclass (5 digit)'))



#Join SIC lookup to them, to then sum by the correct SIC group
SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match between lookup and BRES... tick!
table(unique(BRESdata[[1]]$INDUSTRY_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes - will sum job counts in both of these, taken from 5 digit
BRESdata <- BRESdata %>%
  map(~ .x %>%
        left_join(
          SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_2DIGIT_CODE,SIC_SECTION_NAME,SIC_SECTION_CODE),
          by = c('INDUSTRY_NAME' = 'SIC_5DIGIT_NAME')
          )
      )


#We also need to make our own 'production/construction/services' labels
#Examining any of the relevant GVA files in the regional GVA folder
#Gives the following letter breakdowns:
#A-E is production sector
#F = construction
#G-T is services sector

#Use a letter grepl check to mark those categories
BRESdata <- BRESdata %>%
  map(~ .x %>%
        mutate(
          SIC_3GROUP_CODE = case_when(
            qg("[A-E]",SIC_SECTION_CODE) ~ "A-E",
            qg("F",SIC_SECTION_CODE) ~ "F (41-43)",
            qg("[G-T]",SIC_SECTION_CODE) ~ "G-T"
        ),
          SIC_3GROUP_NAME = case_when(
            qg("[A-E]",SIC_SECTION_CODE) ~ "Production sector",
            qg("F",SIC_SECTION_CODE) ~ "Construction",
            qg("[G-T]",SIC_SECTION_CODE) ~ "Services sector"
            )
        )
  )

#Check all were populated... TICK
# table(BRESdata[[1]]$SIC_3GROUP_CODE)
# table(BRESdata[[1]]$SIC_3GROUP_NAME)

#Check all match correct section... TICK
#NA for extraterritorial orgs, but zero jobs in that - will get dropped on match with GVA, which doesn't have it
# BRESdata[[1]] %>% select(SIC_SECTION_NAME,SIC_SECTION_CODE,SIC_3GROUP_NAME,SIC_3GROUP_CODE) %>% distinct %>% v


#2. GET SUMMED JOB COUNTS FOR SIC 2 DIGIT, SECTIONS AND 3-GROUP FROM 5-DIGIT----

#New lists of dataframes summarising based on each:

#SIC 2 digit
BRESdata.2digitsums <- BRESdata %>%
  map(~ .x %>%
        group_by(DATE,GEOGRAPHY_NAME,SIC_2DIGIT_CODE) %>% 
        summarise(
          JOBCOUNT = sum(OBS_VALUE, na.rm = T),#Some agri points have no values via DEFRA
          SIC_2DIGIT_NAME = max(SIC_2DIGIT_NAME),#will keep matching name
          GEOGRAPHY_CODE = max(GEOGRAPHY_CODE)#will keep matching name
          ) %>% 
        ungroup() %>% 
        mutate(
          SIC_2DIGIT_CODE_NUMERIC = as.numeric(SIC_2DIGIT_CODE)#For matching later
        ) %>% 
        relocate(SIC_2DIGIT_CODE, .after = SIC_2DIGIT_NAME) %>% 
        relocate(GEOGRAPHY_CODE, .before = GEOGRAPHY_NAME) %>% 
        relocate(JOBCOUNT,.after = SIC_2DIGIT_CODE_NUMERIC) 
  )

#SIC sections
BRESdata.sectionsums <- BRESdata %>%
  map(~ .x %>%
        group_by(DATE,GEOGRAPHY_NAME,SIC_SECTION_CODE) %>% 
        summarise(
          JOBCOUNT = sum(OBS_VALUE, na.rm = T),
          SIC_SECTION_NAME = max(SIC_SECTION_NAME),#will keep matching name
          GEOGRAPHY_CODE = max(GEOGRAPHY_CODE)#will keep matching name
          ) %>% 
        ungroup() %>% 
        relocate(SIC_SECTION_CODE, .after = SIC_SECTION_NAME) %>% 
        relocate(GEOGRAPHY_CODE, .before = GEOGRAPHY_NAME) %>% 
        relocate(JOBCOUNT,.after = SIC_SECTION_CODE)
  )

#SIC 3-group
BRESdata.3groupsums <- BRESdata %>%
  map(~ .x %>%
        group_by(DATE,GEOGRAPHY_NAME,SIC_3GROUP_CODE) %>% 
        summarise(
          JOBCOUNT = sum(OBS_VALUE, na.rm = T),
          SIC_3GROUP_NAME = max(SIC_3GROUP_NAME),#will keep matching name
          GEOGRAPHY_CODE = max(GEOGRAPHY_CODE)#will keep matching name
          ) %>% 
        ungroup() %>% 
        relocate(SIC_3GROUP_CODE, .after = SIC_3GROUP_NAME) %>% 
        relocate(JOBCOUNT,.after = SIC_3GROUP_CODE) %>% 
        relocate(GEOGRAPHY_CODE, .before = GEOGRAPHY_NAME) %>% 
        filter(!is.na(SIC_3GROUP_CODE))#drop sum columns we don't need (is extraterr again)
  )



#While here, also make a matching version of the 5-digit SIC codes that we can use if we want to
BRESdata.5digit <- BRESdata %>%
  map(~ .x %>% 
        select(DATE,GEOGRAPHY_CODE,GEOGRAPHY_NAME,SIC_5DIGIT_NAME = INDUSTRY_NAME,JOBCOUNT = OBS_VALUE) %>% 
        mutate(SIC_5DIGIT_CODE = str_sub(SIC_5DIGIT_NAME,1,5)) %>% 
        relocate(SIC_5DIGIT_CODE, .after = SIC_5DIGIT_NAME) %>% 
        relocate(JOBCOUNT,.after = SIC_5DIGIT_CODE) %>% 
        relocate(GEOGRAPHY_CODE, .before = GEOGRAPHY_NAME) %>% 
        filter(!is.na(SIC_5DIGIT_CODE))#drop sum columns we don't need (is extraterr again)
        )


#3. SAVE ALL SEPARATELY----

#Test size of largest if we use CSV... 2.5mb, fine
# write_csv(BRESdata.2digitsums[[1]],'local/data/BRES/testsize.csv')

#Place all in public repo

# BRESdata.2digitsums %>%
#   map(~ .x %>%
#         write_csv(paste0)


# Apply a side-effect function to each dataframe-filename pair
#Use each time
updatefolder <- gsub("/BRES/","/BRES/separate_SIC_types_summedfrom5digitSIC",bresfilenames)

#Then each save separately
finalfilenames <- gsub('.rds','_SIC_2DIGIT.csv',updatefolder)


walk2(BRESdata.2digitsums, finalfilenames, ~ {
  .x %>% write_csv(file = .y)
})


finalfilenames <- gsub('.rds','_SIC_SECTION.csv',updatefolder)

walk2(BRESdata.sectionsums, finalfilenames, ~ {
  .x %>% write_csv(file = .y)
})


finalfilenames <- gsub('.rds','_SIC_3GROUPS.csv',updatefolder)

walk2(BRESdata.3groupsums, finalfilenames, ~ {
  .x %>% write_csv(file = .y)
})


finalfilenames <- gsub('.rds','_SIC_5DIGIT.csv',updatefolder)

walk2(BRESdata.5digit, finalfilenames, ~ {
  .x %>% write_csv(file = .y)
})











        