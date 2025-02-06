#Miscellaneous data checks
library(tidyverse)
library(nomisr)
library(stringdist)
library(sf)
source('functions/misc_functions.R')

options(scipen = 99)


# GENERAL NOMISR BRES CHECKS----

#BRES code, get "concepts" we can use to specify download 
a <- nomis_get_metadata(id = "NM_189_1")

#Pick on some of those (some of which don't seem to be working.)
#Note, MEASURE in the actual downloaded data is "MEASURE_NAME" column...
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")

#Point of confusion here - 
#Look at the full column range and how it's broken down:
#(for some sample data)
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

z <- nomis_get_data(id = "NM_189_1",  time = "2023", geography = placeid
                    # MEASURE = 1,
                    # MEASURES = 20100,
                    # EMPLOYMENT_STATUS = 1
)

#"MEASURE_MAME" has "count" and "industry percentage"
#"MEASURES_NAME" has "Value" and "percent"
#The latter - will have the "percent of the industry percentage" rows in
#So actually, BOTH will need selecting correctly

#Also - the 'concept ref' to access both of those isn't the same as the column names
#They don't have the extra "_NAME" part

#Also also - "MEASURE_TYPE" column, when downloaded, will say "percent", but it isn't.

#Are we clear? OK then. So, this works...?

#Taking the numbers from the ID column when using nomis_get_metadata on the concepts (see above)
z <- nomis_get_data(id = "NM_189_1",  time = "2023", 
                    geography = placeid,
                    MEASURE = 1,#1 is "Count", 2 is "Industry percent"
                    MEASURES = 20100#20100 is "value", 20301 is "percent"
)




#INSPECT FULL RANGE OF BRES AVAILABLE COLUMN NAMES----

#To select the right ones...
#Get random sample data
#ITL3
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

z <- nomis_get_data(id = "NM_189_1",  time = "2023", geography = placeid
                    # MEASURE = 1,
                    # MEASURES = 20100,
                    # EMPLOYMENT_STATUS = 1
)


#That data method is seeming different to how it was...
#Is it the same for others?
#ITL3
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

z <- nomis_get_data(id = "NM_189_1",  time = "2023",
                    geography = placeid, 
                    # measures = 20100, 
                    EMPLOYMENT_STATUS = 1
)





# CHECK BRES GEOGRAPHIES' MATCH TO OTHERS----

#We already know that NUTS2 2016 does match ITL2
#Check what most closely matches to ITL3

#Nab ITL3 codes and names from another source
ITL3 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SICsections_WIDE_2022.csv') %>% distinct(ITL_code, .keep_all = T) %>% select(1:2)

#Check what we can get from BRES
#TYPE437 is nuts 2016 level 3 - Bournemouth, Christchurch and Poole don't match ITL3, the rest does (see intersection check below)
#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2

#TYPE429 is ITL2 (2021)
#TYPE428 is ITL3 (2021)

#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 41)


#Ah - the list of geographies now actually contains ITLs:
z <- nomis_get_data(id = "NM_189_1", time = '2021', 
                    geography = "TYPE428", measures = 20100, 
                    # geography = "TYPE437", measures = 20100, 
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
                    )

#Check ITL code matches... tick
table(unique(z$GEOGRAPHY_CODE) %in% ITL3$ITL_code)

#Presumably not a perfect match the other way? BRES is GB only...
#So yes, just NI aren't matches
table(unique(ITL3$ITL_code) %in% z$GEOGRAPHY_CODE)
unique(ITL3$Region_name)[!unique(ITL3$ITL_code) %in% z$GEOGRAPHY_CODE]

#check if data present...
#2023 yes
#2022 yes
#2021 no - that then moves to BRES using NUTS3
table(!is.na(z$OBS_VALUE))
sample(z$OBS_VALUE,25)




#Just checking which years available for which geographies
#Can do with just one geography to save downloads


#Get code for a single random geog
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

#ITL3
#Get random geog to inspect which years there's data for
z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
#ITL3 ONLY GOES BACK TO 2015 BY DEFAULT
unique(z$DATE)
#AND ONLY HAS ACTUALY DATA FOR 2022/23
unique(z$DATE[!is.na(z$OBS_VALUE)])




#Same for ITL2 presumably... TICK, 2022-23 DATA ONLY
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE429") %>% filter(qg('south yorks',.$label.en)) %>% select(id) %>% pull

#ITL3
#Get random geog to inspect which years there's data for
z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
#ITL3 ONLY GOES BACK TO 2015 BY DEFAULT
unique(z$DATE)
#AND ONLY HAS ACTUALY DATA FOR 2022/23
unique(z$DATE[!is.na(z$OBS_VALUE)])






#NUTS 3 2018: data from 2015 to 2022, none for 2023
#Going to be different placeid for different geography type...
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE437") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#2015 to 2022 have data
unique(z$DATE[!is.na(z$OBS_VALUE)])




#NUTS 2 2018 (TYPE438) THE SAME as NUTS3 2018? 
#Tick - data for 2015 to 2022, none for 2023
#Going to be different placeid for different geography type...
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE438") %>% filter(qg('south yorks',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#2015 to 2022 have data
unique(z$DATE[!is.na(z$OBS_VALUE)])












#Earlier NUTS data? (Can check geog match after, but if harmonisable, would be useful to have)
#NUTS 3 2013 = TYPE449
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE449") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#... but every single year has data!!???
unique(z$DATE[!is.na(z$OBS_VALUE)])

#View... yep, these do have data in.
#So there's no reason why NUTS3 2018 doesn't?
z %>% filter(DATE == "2023") %>% v




#Check last available NUTS3 data, 2010
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE456") %>% filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull


z <- nomis_get_data(id = "NM_189_1",  
                    geography = placeid, measures = 20100, 
                    # geography = "TYPE437",
                    EMPLOYMENT_STATUS = 1,
                    select = c('DATE','GEOGRAPHY_CODE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

#2015 to 2023 date values
unique(z$DATE)
#Again, data for all years...
unique(z$DATE[!is.na(z$OBS_VALUE)])

z %>% filter(DATE == "2023") %>% v

#At any rate, no older NUTS zones go back further than 2015 using the API




#CHECK ITL3(2021) AND NUTS3(2016/2018 DEPENDING WHO YOU ASK) FOR MISALIGNED GEOGRAPHIES----

#Issue: 
#BRES uses ITL3 from 2022.
#And NUTS3 2016 prior to that.
#They're *almost* exactly the same geographies
#The only obvious difference is:
#"Bournemouth + Poole / Dorset" vs "Bournemouth + Poole + Christchurch / Dorset"
#Where in the latter, Christchurch is part of the Dorset boundary
#So that little niggle will need fixing by combining into a single "Dorset" region.

#But that's from eyeballng.
#Let's check them with an intersect / area check
#Where large % area diffs will highlight larger differences to look at
itl3.geo <- st_read('../YPERN_dataexplore/data/geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp') %>% st_simplify(preserveTopology = T, dTolerance = 100)

nuts3.geo <- st_read("~/Dropbox/MapPolygons/UK/NUTS_Level_3_January_2018_GCB_in_the_United_Kingdom_2022_-2838064862322809072/NUTS_Level_3_January_2018_GCB_in_the_United_Kingdom.shp")

#Intersect, add areas...

#https://github.com/r-spatial/sf/issues/860
#Then general "make valid"... 
interz <- itl3.geo %>%
  select(ITL321NM) %>% 
  st_make_valid() %>%
  st_set_precision(1e5) %>%
  st_intersection(
    nuts3.geo %>%
      select(nuts318nm) %>% 
      st_set_precision(1e5) %>%
      st_make_valid()
    ) %>% 
  mutate(area = as.numeric(st_area(.)))


#Find % of entire area for arbitrary larger geography
interz <- interz %>% 
  group_by(ITL321NM) %>% 
  mutate(area_percent = (area / sum(area)) * 100) %>% 
  ungroup()

#Examined by ordering and looking in QGIS - 
#Confirmed, it's only Bournemouth. Christchurch and Poole that differ






#COMPARE BRES AND REGIONAL GVA DOWNLOADS, WORKING TOWARDS SIC CODE MATCH/MERGE----  

#Probably also some geography fettling but let's do SIC first
#SIC sections should be nice and easy

#The tricky bit is GVA summed categories at different levels
#Which I did wrangle in previous code, but let's see again

#GVA data is a bespoke version of 2-digit SICs, where some are combined e.g. "CB (13-15)"

#Just need one year to check, let's use ITL2
bres.itl2 <- readRDS('data/BRES/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_2_Fulltimeemployees_2022_2023.rds')

#The industry codes are also in the name, just need to separate off the first part of the string...
#(The INDUSTRY_CODE field is just this, no point downloading)
#https://www.r-bloggers.com/2024/07/extracting-strings-before-a-space-in-r/
bres.itl2 <- bres.itl2 %>% 
  mutate(
    INDUSTRY_CODE = stringr::str_extract(INDUSTRY_NAME, "^[^ ]+")
  )

#ALso - take 5 digit job counts and sum those to the 2 digit categories
#Because they're likely more accurate / granular
#Due to odd choice to apply rounding in the same way at all levels:
#See - https://www.nomisweb.co.uk/articles/1103.aspx


#Run though of the plan here, to deconfuse!
#1. Keep only 5 digit BRES data, because the counts are more granular
#2. Merge in the 2 digit SIC code (and elsewhere the sections etc)
#3. Sum job counts to the 2 digit SIC codes

#And somewhere else, do some sanity checks to see if this is returning better numbers
#Than just using the BRES own 2 digit counts
#Given all this effort!


#Only need to keep the BRES 5 digit to sum...
# bres.itl2.5digit <- bres.itl2 %>% filter(INDUSTRY_TYPE == 'SIC 2007 division (2 digit)')
bres.itl2.5digit <- bres.itl2 %>% filter(INDUSTRY_TYPE == 'SIC 2007 subclass (5 digit)')

#Get SIC lookup
SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match... tick!
table(unique(bres.itl2.5digit$INDUSTRY_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))
# table(unique(bres.itl2.2digit$INDUSTRY_NAME) %in% unique(SIClookup$SIC_2DIGIT_NAME))

#Join on 5 digit name
#Keep 2 digit and section codes - will sum job counts in both of these, taken from 5 digit
bres.itl2.5digit <- bres.itl2.5digit %>% 
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_2DIGIT_CODE,SIC_SECTION_NAME,SIC_SECTION_CODE),
    by = c('INDUSTRY_NAME' = 'SIC_5DIGIT_NAME')
  )


#Sum 5 digit full time time job counts to 2 digit SICs 
bres.itl2.2digit.summed <- bres.itl2.5digit %>% 
  group_by(DATE,GEOGRAPHY_NAME,SIC_2DIGIT_CODE) %>% 
  summarise(
    JOBCOUNT = sum(OBS_VALUE),
    SIC_2DIGIT_NAME = max(SIC_2DIGIT_NAME)#will keep matching name
    ) %>% 
  ungroup() %>% 
  mutate(
    SIC_2DIGIT_CODE_NUMERIC = as.numeric(SIC_2DIGIT_CODE)#For matching later
  )

#Same for SIC sections
bres.itl2.sections.summed <- bres.itl2.5digit %>% 
  group_by(DATE,GEOGRAPHY_NAME,SIC_SECTION_CODE) %>% 
  summarise(COUNT = sum(OBS_VALUE)) %>% 
  ungroup() 




#CHECK HOW THE "SECTION AND 2 DIGIT JOBCOUNTS SUMMED FROM 5 DIGIT" #compare tothe BRES original versions of 2 DIG and Sections----

chk.2digit <- bres.itl2.2digit.summed %>% 
  left_join(
    bres.itl2 %>% filter(qg('2 dig',INDUSTRY_TYPE)) %>% rename(ORIG_COUNT = OBS_VALUE),
    by = c('DATE','GEOGRAPHY_NAME','SIC_2DIGIT_CODE' = 'INDUSTRY_CODE')
  )

#Find count % of orig count
#And look at spread around that
chk.2digit <- chk.2digit %>% 
  mutate(percent_of_orig = (ORIG_COUNT / COUNT) * 100 )

#Generally great - evenly spread around 100% - 
#though there are a few outliers...
#Eyeballing, those all make sense. Mainly very low numbers
ggplot(chk.2digit, aes(x = percent_of_orig)) +
  geom_density()


#CAN'T CHECK SECTIONS AS THE ORIG BRES INDUSTRY TYPES DOESN'T INCLUDE IT AS A CATEGORY
#2 Digit is as close as we get, and just checked that...



#Then! -->

#Regional GVA
gva.itl2 <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL2_allavailableSICs_LONG_2022.csv')
gva.itl3 <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_allavailableSICs_LONG_2022.csv')

#So yes, previous code for this is in ukcompare/explore_code/GVA_region_by_sector_explore.R
#Section: Linking BRES EMPLOYMENT TO GVA----
#Currently here: https://github.com/DanOlner/ukcompare/blob/08ce5c1c75b27755ceb56cb23e9ff353e5d16031/explore_code/GVA_region_by_sector_explore.R#L1991 

#Copying rationale:
#PLAN:
#if we mark the bres categories with group names
#Such that ones that need collating are in the same group
#Can then just do easily with dplyr

#Create a copy of GV where the codes are expanded to 1 each per row
#But the names will be duplicates
#Can then merge on the codes, and then group by these names to create the groups
#Is the theory

#So:
#In the GVA data, some SIC two digit codes are combined:
#unique(gva.itl2$SIC07_code)
#unique(gva.itl3$SIC07_code)

#ITL2 & 3 have different numbers of SIC catagories, with different combos
#Fewer at more granular geography
length(unique(gva.itl2$SIC07_code))#72
length(unique(gva.itl3$SIC07_code))#48


#Expand those GVA SIC combos onto their own rows
#We'll only need a smaller number of columns
#To then merge in the BRES data, to sum by SIC grouping once there

#Key point: we will NOT be summing GVA values here at all
#We just sum up the jobcounts for these SIC combos
#Then can link to the combo codes in the GVA file

#Why go on about that? 
#Cos it means BRES job totals can be connected to chained volume measures
#(Which wouldn't be true if we were summing GVA vals, that's only valid for current prices)



#In theory, this should work for ITL2 and 3...
#Get distinct list of the GVA codes

#Drop imputed rent also - 
#BRES job count data doesn't have any jobs for that sector, of course
df <- gva.itl2 %>% select(SIC07_description,SIC07_code) %>% 
  distinct(SIC07_description, .keep_all = T) %>% 
  filter(!qg('imp',SIC07_code))

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


#Check matches... tick
#Other way round, we lose 99 - see below (no job count, doesn't matter)
table(unique(df$SIC07_code_numeric) %in% unique(bres.itl2.2digit.summed$SIC_2DIGIT_CODE_NUMERIC))





#Join them
#Inner join loses one BRES category that has no job count
#"99 : Activities of extraterritorial organisations and bodies"
both <- bres.itl2.2digit.summed %>% 
  inner_join(
    df,
    by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric')
  )


#Can now group by GVA industry name and sum the counts
bres_w_gvacodes <- both %>% 
  group_by(SIC07_description, GEOGRAPHY_NAME, DATE) %>%
  summarise(
    COUNT = sum(COUNT, na.rm=T)
  ) %>% ungroup()

#All that needs now is the original number code merged back in, so we have those labelled


#... but we need the correct code to do that
#Which we removed earlier
#get back
df_w <- gva.itl2 %>% select(SIC07_description,SIC07_code) %>% 
  distinct(SIC07_description, .keep_all = T) %>% 
  filter(!qg('imp',SIC07_code))

table(unique(bres_w_gvacodes$SIC07_description) %in% df_w$SIC07_description)

bres_w_gvacodes <- bres_w_gvacodes %>% 
  left_join(
    df_w,
    by = 'SIC07_description'
  ) 
# %>% 
#   select(-INDUSTRY_CODE) %>% 
#   rename(INDUSTRY_CODE = GVA_INDUSTRY_CODE, INDUSTRY_NAME = GVA_INDUSTRY_NAME)





# TEST FUNCTIONAL VERSION OF THE ABOVE BRES/GVA LINK CODE----

#All good for getting BRES join file!
# debugonce(make.GVA.SICs.long)
sics.forBRESjoin.itl2 <- make.GVA.SICs.long(gva.itl2)
sics.forBRESjoin.itl3 <- make.GVA.SICs.long(gva.itl3)



#TEST COMBINING THE PRODUCED GVA AND BRES DATA----

## FOR ITL2 / NUTS2----

#Each now created separately, BRES aggd from 5 digit into each SIC band we want 
#Add in both a full time and part time job count column
#So many files - how to efficiently combine?
#Possibly may require matching filenames over both, which currently I have not done

#But let's look at a sample.
#Start with the easy? SIC sections.
#And ITL2 so no worries about the non-matching single geography for now
#But we do need to change that one upper/lower case letter mismatch!
#(Which will need to be done as ITL and NUTS codes don't match, need to use names)
gva <- read_csv("data/regionalGVA/regionalGVA_currentprices_ITL2_SICsections_LONG_2022.csv") %>%
  mutate(
    Region_name = gsub('Bristol Area','Bristol area',Region_name)#see below, upper vs lower case non match
  )

#Only has the one matching year... ooo catchy name!
bres.ft <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_2_Fulltimeemployees_2022_2023_SIC_SECTION.csv") %>% rename(JOBCOUNT_FULLTIME = JOBCOUNT)

bres.pt <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_3_Parttimeemployees_2022_2023_SIC_SECTION.csv") %>% rename(JOBCOUNT_PARTTIME = JOBCOUNT)

#SIC section match? Tick.
table(unique(gva$SIC07_code) %in% bres.ft$SIC_SECTION_CODE)

#Geography name match?
table(unique(gva$Region_name) %in% bres.ft$GEOGRAPHY_NAME)

#Date matches... ah yes, just the one year here
table(unique(gva$year) %in% bres.ft$DATE)

#OK...
#Northern Ireland isn't in this BRES data, so that will go when joining anyway

#The other non match is a lower vs upper case "A"...
#Let's just fix that on import (done above)
# unique(gva$Region_name)[!unique(gva$Region_name) %in% bres.ft$GEOGRAPHY_NAME]
# unique(bres.ft$GEOGRAPHY_NAME)[!unique(bres.ft$GEOGRAPHY_NAME) %in% gva$Region_name]

#So now... join!
#Inner join so we drop NI
#FULL TIME
# gva.n.bres <- bres.ft %>% 
#   inner_join(
#     gva %>% rename(gva = value),
#     by = c('GEOGRAPHY_NAME' = 'Region_name', 'DATE' = 'year', 'SIC_SECTION_CODE' = 'SIC07_code')
#     )
# 
# #PART TIME
# gva.n.bres <- gva.n.bres %>% 
#   inner_join(
#     bres.pt %>% select(-SIC_SECTION_NAME),
#     by = c('GEOGRAPHY_NAME', 'DATE', 'SIC_SECTION_CODE')
#   ) %>% 
#   relocate(JOBCOUNT_FULLTIME, .before = JOBCOUNT_PARTTIME)


#Using reduce to do in one... which is not terribly readable but works
#.init is the first df to join (gva)
#The list of BRES data then joins each in turn to that
gva.n.bres <- list(bres.ft %>% select(-SIC_SECTION_NAME),bres.pt %>% select(-SIC_SECTION_NAME)) %>%
  reduce(~ .x %>% inner_join(
                       .y,
                       by = c('GEOGRAPHY_NAME', 'DATE', 'SIC_SECTION_CODE')),
                   .init = gva %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
                                          SIC_SECTION_CODE = SIC07_code ,gva = value)) %>% 
  relocate(DATE, .before = ITL_code)
  
  




#So that's an example of an easy one
#Three sector groupings should be the same

#Let's see how we get on with the tricky one
#That needs a pre-stage of summing BRES job counts by the GVA bespoke 2-digit SICs

#We have a nice function to get the correct lookup! 
#Hiding the horrible code underneath, phew
sics.forBRESjoin.itl2 <- make.GVA.SICs.long(gva.itl2)

#Get one of the relevant BRES's...
bres.2digit.ft <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_2_Fulltimeemployees_2022_2023_SIC_2DIGIT.csv") %>% mutate(type = 'FULL TIME')

bres.2digit.pt <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_3_Parttimeemployees_2022_2023_SIC_2DIGIT.csv") %>% mutate(type = 'PART TIME')

#Merge in the bespoke SIC lookup for summing up job counts by them
#This time we *do* want map
#Returns a list with both those dfs in
bres.2digit.wlookup <- list(bres.2digit.ft,bres.2digit.pt) %>% 
  map(~ .x %>% 
        left_join(sics.forBRESjoin.itl2, by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric'))
  )

#Sum! Get jobcount totals summed to the GVA bespoke SIC 2 digits 
bres.2digit.wlookup.summed <- bres.2digit.wlookup %>% 
  map(~ .x %>% 
        group_by(DATE,GEOGRAPHY_NAME,SIC07_code_fromGVAdata) %>% 
        summarise(
          JOBCOUNT = sum(JOBCOUNT),#Why we need this field to remain same for both for now
          type = max(type)#keep job FT/PT type for later
          ) %>% 
        ungroup() %>% 
        filter(!is.na(SIC07_code_fromGVAdata))
  )
  

#... Which can then be joined with the GVA data by the SIC code it's been summe into
gva <- read_csv("data/regionalGVA/regionalGVA_currentprices_ITL2_allavailableSICs_LONG_2022.csv") %>%
  mutate(
    Region_name = gsub('Bristol Area','Bristol area',Region_name)#see below, upper vs lower case non match
  )

#Check match with one of them... tick
table(unique(bres.2digit.wlookup.summed[[1]]$SIC07_code_fromGVAdata) %in% gva$SIC07_code)


#Join
#Note imputed rent gets dropped as it's not in the BRES data
gva.n.bres.2digit <- bres.2digit.wlookup.summed %>%
  reduce(~ .x %>% inner_join(
    .y,
    by = c('GEOGRAPHY_NAME', 'DATE', 'SIC07_code_fromGVAdata')),
    .init = gva %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
                           SIC07_code_fromGVAdata = SIC07_code ,gva = value)) %>% #temp gva code rename for ease of join
  relocate(DATE, .before = ITL_code) %>% 
  rename(
    JOBCOUNT_FT = JOBCOUNT.x, JOBCOUNT_PT = JOBCOUNT.y
  ) %>% 
  select(-c(type.x,type.y))

#Check dropped sectors when that join is done... yep, just that one
unique(gva$SIC07_description)[!unique(gva$SIC07_code) %in% gva.n.bres.2digit$SIC07_code_fromGVAdata]

#There will be some others with zero job counts...
#Activities of households, no jobs there








#Check the bespoke SICs also work for the NUTS geogs over more years
#The above is just the one year...
#Looking at geog level 2 again

#Should be same code?
bres.2digit.ft <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE438_nuts2016level2_2_Fulltimeemployees_2015_2022_SIC_2DIGIT.csv") %>% mutate(type = 'FULL TIME')

bres.2digit.pt <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE438_nuts2016level2_3_Parttimeemployees_2015_2022_SIC_2DIGIT.csv") %>% mutate(type = 'PART TIME')

#Merge in the bespoke SIC lookup for summing up job counts by them
#This time we *do* want map
#Returns a list with both those dfs in
bres.2digit.wlookup <- list(bres.2digit.ft,bres.2digit.pt) %>% 
  map(~ .x %>% 
        left_join(sics.forBRESjoin.itl2, by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric'))
  )

#Sum! Get jobcount totals summed to the GVA bespoke SIC 2 digits 
bres.2digit.wlookup.summed <- bres.2digit.wlookup %>% 
  map(~ .x %>% 
        group_by(DATE,GEOGRAPHY_NAME,SIC07_code_fromGVAdata) %>% 
        summarise(
          JOBCOUNT = sum(JOBCOUNT),#Why we need this field to remain same for both for now
          type = max(type)#keep job FT/PT type for later
        ) %>% 
        ungroup() %>% 
        filter(!is.na(SIC07_code_fromGVAdata))
  )


#Already have correct gva from above
#Yep, this joins on all years, 2015 to 2022 currently
gva.n.bres.2digit.itl2 <- bres.2digit.wlookup.summed %>%
  reduce(~ .x %>% inner_join(
    .y,
    by = c('GEOGRAPHY_NAME', 'DATE', 'SIC07_code_fromGVAdata')),
    .init = gva %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
                           SIC07_code_fromGVAdata = SIC07_code ,gva = value)) %>% #temp gva code rename for ease of join
  relocate(DATE, .before = ITL_code) %>% 
  rename(
    JOBCOUNT_FT = JOBCOUNT.x, JOBCOUNT_PT = JOBCOUNT.y
  ) %>% 
  select(-c(type.x,type.y))

#Check dropped sectors when that join is done... yep, just that one
unique(gva$SIC07_description)[!unique(gva$SIC07_code) %in% gva.n.bres.2digit.itl2$SIC07_code_fromGVAdata]



## FOR ITL3 / NUTS3----

#Where the plan is:
#Replace entire of Somerset, Dorset, Bournemouth/Poole/Christchurch
#With "Dorset / Somerset" ITL2 zone

#This deals with that one tiny changing geography in the south
#(If ONS changed the GVA vals accordingly into the past, which I wonder about...)

#There's an option to aggregate Dorset + Bournemouth/Poole/Christchurch
#(Christchurch is the only bit changing between those two zones)
#But that won't work for chained volume cos that can't be summed
#So for consistency, prob going to stick to the
#"use ITL2 zone that overlaps them all" plan

#Which we now have from above, so can test what that looks like.
#Issue is the need for a bespoke geography to be able to map as well, but will mull that

#Another option is just to caveat with 
#"Bournemouth etc will be wrong, watch those numbers"

#Remove "CC" from end of names
#And fix that one "and"
#Remaining non matches will be Northern Ireland, as BRES data via NOMIS doesn't have it
gva.itl3 <- read_csv("data/regionalGVA/regionalGVA_currentprices_ITL3_allavailableSICs_LONG_2022.csv") %>% 
  mutate(
    Region_name = gsub(' CC','',Region_name),
    Region_name = ifelse(Region_name == "Inverness and Nairn, Moray, Badenoch and Strathspey",
                         "Inverness and Nairn, Moray, and Badenoch and Strathspey",#spot the difference!
                         Region_name
                         )
    )
  

bres.2digit.ft <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_2DIGIT.csv") %>% 
  mutate(
    type = 'FULL TIME',
    GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME)
    )

bres.2digit.pt <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_3_Parttimeemployees_2022_2023_SIC_2DIGIT.csv") %>% 
  mutate(
    type = 'PART TIME',
    GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME)
  )

#Check geography matches
table(unique(gva.itl3$Region_name) %in% bres.2digit.ft$GEOGRAPHY_NAME)

#After fixes, just NI doesn't match
#Those will get dropped during join
unique(gva.itl3$Region_name)[!unique(gva.itl3$Region_name) %in% bres.2digit.ft$GEOGRAPHY_NAME]
unique(bres.2digit.ft$GEOGRAPHY_NAME)[!unique(bres.2digit.ft$GEOGRAPHY_NAME) %in% gva.itl3$Region_name]



#Get itl3 bespoke SIC lookup
#Reminder, the SIC list differs in the GVA data for iTL2 and 3
#Just for extra fun
sics.forBRESjoin.itl3 <- make.GVA.SICs.long(gva.itl3)


#All same code works nicely

#Merge in the bespoke SIC lookup for summing up job counts by them
#This time we *do* want map
#Returns a list with both those dfs in
bres.2digit.wlookup <- list(bres.2digit.ft,bres.2digit.pt) %>% 
  map(~ .x %>% 
        left_join(sics.forBRESjoin.itl3, by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric'))
  )

#Sum! Get jobcount totals summed to the GVA bespoke SIC 2 digits 
bres.2digit.wlookup.summed <- bres.2digit.wlookup %>% 
  map(~ .x %>% 
        group_by(DATE,GEOGRAPHY_NAME,SIC07_code_fromGVAdata) %>% 
        summarise(
          JOBCOUNT = sum(JOBCOUNT),#Why we need this field to remain same for both for now
          type = max(type)#keep job FT/PT type for later
        ) %>% 
        ungroup() %>% 
        filter(!is.na(SIC07_code_fromGVAdata))
  )


#Yep, this joins on all years, 2015 to 2022 currently
gva.n.bres.2digit <- bres.2digit.wlookup.summed %>%
  reduce(~ .x %>% inner_join(
    .y,
    by = c('GEOGRAPHY_NAME', 'DATE', 'SIC07_code_fromGVAdata')),
    .init = gva.itl3 %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
                           SIC07_code_fromGVAdata = SIC07_code ,gva = value)) %>% #temp gva code rename for ease of join
  relocate(DATE, .before = ITL_code) %>% 
  rename(
    JOBCOUNT_FT = JOBCOUNT.x, JOBCOUNT_PT = JOBCOUNT.y
  ) %>% 
  select(-c(type.x,type.y))#prob not necessary; FT and PT is in the correct order, would be fine to not do that

#Check dropped sectors when that join is done... yep, just that one
unique(gva.itl3$SIC07_description)[!unique(gva.itl3$SIC07_code) %in% gva.n.bres.2digit$SIC07_code_fromGVAdata]

#Check no missing vals... tick
table(is.na(gva.n.bres.2digit$gva))
table(is.na(gva.n.bres.2digit$JOBCOUNT_FT))
table(is.na(gva.n.bres.2digit$JOBCOUNT_PT))




## NUTS3 INCLUDING GEOG FIDDLY BITS----

#Then onto NUTS3 check for same GVA file
#Where we have to do something about the altered Dorset geog
#i.e. replace with ITL2 larger zone
#Can definitely come up with a better way to do this! Code lookup e.g.
gva.itl3.forNUTS3match <- read_csv("data/regionalGVA/regionalGVA_currentprices_ITL3_allavailableSICs_LONG_2022.csv") %>%
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
    ),
    
  )


bres.2digit.ft <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE437_nuts2016level3_2_Fulltimeemployees_2015_2022_SIC_2DIGIT.csv") %>% 
  mutate(
    type = 'FULL TIME',
    GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME)
  )

bres.2digit.pt <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE437_nuts2016level3_3_Parttimeemployees_2015_2023_SIC_2DIGIT.csv") %>% 
  mutate(
    type = 'PART TIME',
    GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME)
  )

#Check geography matches
table(unique(gva.itl3.forNUTS3match$Region_name) %in% bres.2digit.ft$GEOGRAPHY_NAME)

#After fixes, just NI doesn't match
#Those will get dropped during join

#A few other differences here, including the Bournemouth/Poole issue
#But let's fix just the various "same place different text" issues
unique(gva.itl3.forNUTS3match$Region_name)[!unique(gva.itl3.forNUTS3match$Region_name) %in% bres.2digit.ft$GEOGRAPHY_NAME]
unique(bres.2digit.ft$GEOGRAPHY_NAME)[!unique(bres.2digit.ft$GEOGRAPHY_NAME) %in% gva.itl3.forNUTS3match$Region_name]


#ALL the same code works...
#Merge in the bespoke SIC lookup for summing up job counts by them
#This time we *do* want map
#Returns a list with both those dfs in
bres.2digit.wlookup <- list(bres.2digit.ft,bres.2digit.pt) %>% 
  map(~ .x %>% 
        left_join(sics.forBRESjoin.itl3, by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric'))
  )

#Sum! Get jobcount totals summed to the GVA bespoke SIC 2 digits 
bres.2digit.wlookup.summed <- bres.2digit.wlookup %>% 
  map(~ .x %>% 
        group_by(DATE,GEOGRAPHY_NAME,SIC07_code_fromGVAdata) %>% 
        summarise(
          JOBCOUNT = sum(JOBCOUNT),#Why we need this field to remain same for both for now
          type = max(type)#keep job FT/PT type for later
        ) %>% 
        ungroup() %>% 
        filter(!is.na(SIC07_code_fromGVAdata))
  )


#Yep, this joins on all years, 2015 to 2022 currently
gva.n.bres.2digit <- bres.2digit.wlookup.summed %>%
  reduce(~ .x %>% inner_join(
    .y,
    by = c('GEOGRAPHY_NAME', 'DATE', 'SIC07_code_fromGVAdata')),
    .init = gva.itl3.forNUTS3match %>% rename(GEOGRAPHY_NAME = Region_name, DATE = year,
                                SIC07_code_fromGVAdata = SIC07_code ,gva = value)) %>% #temp gva code rename for ease of join
  relocate(DATE, .before = ITL_code) %>% 
  rename(
    JOBCOUNT_FT = JOBCOUNT.x, JOBCOUNT_PT = JOBCOUNT.y
  ) %>% 
  select(-c(type.x,type.y))#prob not necessary; FT and PT is in the correct order, would be fine to not do that

#Check dropped sectors when that join is done... yep, just that one
unique(gva.itl3$SIC07_description)[!unique(gva.itl3$SIC07_code) %in% gva.n.bres.2digit$SIC07_code_fromGVAdata]

#Check no missing vals... tick
table(is.na(gva.n.bres.2digit$gva))
table(is.na(gva.n.bres.2digit$JOBCOUNT_FT))
table(is.na(gva.n.bres.2digit$JOBCOUNT_PT))

#With that join, we should only have Bournemouth...etc missing (plus NI zones)
#As no inner join match on the name
#TICK
unique(gva.itl3.forNUTS3match$Region_name)[!unique(gva.itl3.forNUTS3match$Region_name) %in% gva.n.bres.2digit$GEOGRAPHY_NAME]


#So then one job here, one somewhere else:
#1. Substitute in the Dorset ITL2 for Dorset + Somerset + B/C/Poole
#2. Make a bespoke ITL3 geography that does the same, for mapping

#So for the dorset sub - the above leaves BCR out, just have to remove the others 
#Then add in that ITL2

#Confirm which zones to remove to be replaced with the ITL2 values
#(Looking at the zones in QGIS)
#Bath and north east somerset is a different zone, can leave that one...
unique(gva.n.bres.2digit$GEOGRAPHY_NAME)[qg('bourne|dorset|somerset',unique(gva.n.bres.2digit$GEOGRAPHY_NAME))]

#So, doing carefully first to check...
#Version with those areas removed:
gva.n.bres.2digit.geogedit <- gva.n.bres.2digit %>% 
  filter(
    !GEOGRAPHY_NAME %in% c('Somerset','Dorset')
  )

#tick
unique(gva.n.bres.2digit.geogedit$GEOGRAPHY_NAME)[qg('bourne|dorset|somerset',unique(gva.n.bres.2digit.geogedit$GEOGRAPHY_NAME))]


#This has the dorset/somerset ITL2 zone we want to drop in (from above)
gva.n.bres.2digit.itl2

#col names all good? Tick.
# table(names(gva.n.bres.2digit.geogedit) %in% names(gva.n.bres.2digit.itl2))

gva.n.bres.2digit.tweakedITL3 <- gva.n.bres.2digit.geogedit %>% 
  bind_rows(
    gva.n.bres.2digit.itl2 %>% filter(GEOGRAPHY_NAME == 'Dorset and Somerset')
  )

#Tick. And that'll work for current prices and chained volume





