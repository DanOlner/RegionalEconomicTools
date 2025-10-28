#Miscellaneous data checks
library(tidyverse)
library(nomisr)
library(stringdist)
library(sf)
library(stringr)
source('functions/misc_functions.R')
source('functions/data_process_functions.R')

options(scipen = 99)


# GENERAL NOMISR BRES CHECKS----

#BRES code, get "concepts" we can use to specify download 
a <- nomis_get_metadata(id = "NM_189_1")

#Pick on some of those (some of which don't seem to be working.)
#Note, MEASURE in the actual downloaded data is "MEASURE_NAME" column...
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
geogz = nomis_get_metadata(id = "NM_189_1", concept = "GEOGRAPHY", type = "type")

#Point of confusion here - 
#Look at the full column range and how it's broken down:
#(for some sample data)
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE428") %>% 
  filter(qg('sheffield',.$label.en)) %>% select(id) %>% pull

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






#While here, compare the employment number type (while keeping just the measures we want, straight count)
#Reminder of employee cats in the BRES data
## Employees: An employee is anyone aged 16 years or over that an organisation directly pays from its payroll(s), in return for carrying out a full-time or part-time job or being on a training scheme. It excludes voluntary workers, self-employed, working owners who are not paid via PAYE. 
# Full-time employees: those working more than 30 hours per week.
# Part-time employees: those working 30 hours or less per week.
# Employment includes employees plus the number of working owners. BRES therefore includes self-employed workers as long as they are registered for VAT or Pay-As-You-Earn (PAYE) schemes. Self employed people not registered for these, along with HM Forces and Government Supported trainees are excluded.
#And note, no gig economy jobs: 
#https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/workersinthegigeconomy

z <- nomis_get_data(id = "NM_189_1",  time = "2022", 
                    geography = 'TYPE429',
                    MEASURE = 1,
                    MEASURES = 20100,
                    select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','EMPLOYMENT_STATUS_NAME','OBS_VALUE')
                    # EMPLOYMENT_STATUS = 1
)

unique(z$EMPLOYMENT_STATUS_NAME)
unique(z$INDUSTRY_TYPE)

#Compare side by side
chk <- z %>% 
  filter(INDUSTRY_NAME != 'Total', qg('2 digit',INDUSTRY_TYPE)) %>% 
  pivot_wider(
    names_from = EMPLOYMENT_STATUS_NAME, values_from = OBS_VALUE
  ) %>% 
  mutate(
    employment_as_percent_of_employees = (Employment / Employees) * 100
  )
  

ggplot(chk, aes(x = employment_as_percent_of_employees)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(90,300))

#Difference by sector?
ggplot(chk, aes(y = employment_as_percent_of_employees, x = INDUSTRY_NAME)) +
  geom_boxplot() +
  coord_flip(ylim = c(90,300)) 


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




#CHECK HOW THE "SECTION AND 2 DIGIT JOBCOUNTS SUMMED FROM 5 DIGIT" compare to the BRES original versions of 2 DIG and Sections----

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



## ALSO NEED TO CHECK FOR SIC SECTIONS, DIFF FOR ITL3 AND NUTS3----

#And there's also a missing var that I didn't think there should be.








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




# DO GEOG NAME CHECKS ON BRES NUTS AND GVA ITL----

#A few are wonky
#Get one of the relevant BRES's...
bres.chk <- read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE438_nuts2016level2_2_Fulltimeemployees_2015_2022_SIC_2DIGIT.csv")

gva.chk <- read_csv("data/regionalGVA/regionalGVA_currentprices_ITL2_SIC_SECTION_LONG_2022.csv")

table(unique(bres.chk$GEOGRAPHY_NAME) %in% gva.chk$Region_name)
unique(bres.chk$GEOGRAPHY_NAME)[!unique(bres.chk$GEOGRAPHY_NAME) %in% gva.chk$Region_name]
unique(gva.chk$Region_name)[!unique(gva.chk$Region_name) %in% bres.chk$GEOGRAPHY_NAME]



# CHECK RESAVING BRES DATA AS ZIPPED CSV ACTUALLY IS SMALL ENOUGH FILE----

#Output from BRES_API_DOWNLOAD
#Picking on the biggest one in the folder
chk <- readRDS("data/BRES/BRES_ALLYEARSWITHDATA_TYPE437_nuts2016level3_3_Parttimeemployees_2015_2023.rds")

#need to save the CSV first...
#300mb vs 42mb for RDS compressed object
write_csv(chk,"local/cuttings/test.csv")

#Yep, goes down to same compressed size as RDS
zip::zip("local/cuttings/test.zip", "local/cuttings/test.csv")



# HOW MANY SIC CATEGORIES IN SIC 2 DIGIT BESPOKES IN THE GVA DATA?----

# ONS combined some of them - more at ITL3 level.

itl2 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_2DIGIT_LONG_2022.csv')
itl3 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_2DIGIT_LONG_2022.csv')

#72 for ITL2
length(unique(itl2$SIC07_description))
#48 for ITL3
length(unique(itl3$SIC07_description))



#How many in the BRES 5 digit summed data?
chk <- read_csv('data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_2DIGIT.csv')

#88
length(unique(chk$SIC_2DIGIT_NAME))

#Which is close to the full list, yes?
SIClookup <- read_csv('data/SIClookup.csv')

#Is in fact full list... though SIC lookup doesn't have imputed rent, which it should
length(unique(SIClookup$SIC_2DIGIT_NAME))




# CHECK SECTION DIFFERENCES BETWEEN ITL2 AND 3 GVA DATA----

#Then need to sum job count data appropriately in the combined section groups

#itl2 - just need for names
itl2 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_SECTION_WIDE_2022.csv')

itl3 <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_SECTION_WIDE_2022.csv')

unique(itl2$SIC07_code)
unique(itl3$SIC07_code)

#Only two combos in ITL3...
unique(itl3$SIC07_code)[!unique(itl3$SIC07_code) %in% unique(itl2$SIC07_code)]




# RECHECK HARMONISING BRES DATA FOR 2023: 1. MAKING A BESPOKE SIC LOOKUP----

#Goals: given new SIC and ITL 2025 categories in the latest GVA data
#How best to link with the latest BRES (which still uses ITL 2021 as latest
#and also NUTS prior to that, which doesn't match either).

#I may have been overcomplicating it / think I can reduce the stages
#Am doing this for July 2025 Bradford project, so will focus on ITL3 level first

#So - a simple approach (which I already used to get higher accuracy job counts from 5 digit values)
#Is just to use the BRES 5 digit files and add a lookup to it.
#Already easy for the basic SIC lookup. Let's see about adding a lookup for the newer GVA SIC categories.

#Thus (also used in "BradfordExplore.R" for making treemaps etc.)
# bres = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv") 

SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match between lookup and BRES... tick!
# table(unique(bradford.5digit.ft$SIC_5DIGIT_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
 # bres <- bres %>%
 #  left_join(
 #    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
 #    by = 'SIC_5DIGIT_NAME'
 #  )

#Now we just need 2023-GVA-specific sections and 2-digit
#Will this function still work, I wonder?

#Let's get ITL3 current price file
gva <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv')

#47 categories (88 in the full list)
unique(gva$SIC07_description)

#87 here - what's missing?
chk <- make.GVA.SICs.long(gva)
#Activities of extraterritorial... is the missing one
#That's not in the GVA data, which is that's missing...
#However, it IS in the SIC lookup (as it should be) so make this one GVA and BRES specific
SIClookup$SIC_2DIGIT_NAME[!SIClookup$SIC_2DIGIT_CODE_NUMERIC %in% chk$SIC07_code_numeric]

#sanity check... tick
# table(gva$SIC07_code %in% chk$SIC07_code_fromGVAdata)

#There's code combine_regGVA... that can map across all appropriate BRES files
#But let's test first
#Reminder of the goal: JUST MAKE A LOOKUP that can be merged into the BRES files
#To then count up jobs by the GVA categories

#So...
#Err can I just confirm that all 1st 2 digits of 5 digit values match the two digit ones?
#Tick. So actually we only need that for matching / making GVA-specific lookup
table(SIClookup$SIC_2DIGIT_CODE == str_sub(SIClookup$SIC_5DIGIT_CODE,1,2))
table(SIClookup$SIC_3DIGIT_CODE == str_sub(SIClookup$SIC_5DIGIT_CODE,1,3))


#We don't need to involve that 5 digit BRES file here at all.
#We can match to the SIClookup and add in some more details to that instead
#Maybe including the shortened names, if needed

#They all match
table(chk$SIC07_code_numeric %in% SIClookup$SIC_2DIGIT_CODE_NUMERIC)


#So....
SIClookup <- SIClookup %>% 
  left_join(
    chk %>% select(
      SIC_2DIGIT_NAME_GVA2023 = SIC07_description, 
      SIC_2DIGIT_CODE_GVA2023 = SIC07_code_fromGVAdata,
      SIC_2DIGIT_CODE_NUMERIC = SIC07_code_numeric
      ), by = 'SIC_2DIGIT_CODE_NUMERIC'
  )

#Mostly OK
#Lack of an 'imputed rent' category to match against in that full list
#That shouldn't matter for this job but keep an eye on
#TODO: check on missing imputed rent in SIC lookup (possibly just add in manually, it's just one row)




#Can we just repeat the same process for sections (minus imputed rent)?
gva <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_SECTION_MINUSimputedrent_LONG_2023.csv')

#18 categories (21 in the full list including extrat)
unique(gva$SIC07_description)

#87 again - right number to match against
chk <- make.GVA.SICs.long(gva)

SIClookup <- SIClookup %>% 
  left_join(
    chk %>% select(
      SIC_SECTION_NAME_GVA2023 = SIC07_description, 
      SIC_SECTION_CODE_GVA2023 = SIC07_code_fromGVAdata,
      SIC_2DIGIT_CODE_NUMERIC = SIC07_code_numeric
    ), by = 'SIC_2DIGIT_CODE_NUMERIC'
  )

#tick
# unique(SIClookup$SIC_SECTION_NAME_GVA2023)

#OK - can now join this to BRES 2023 to count up jobs by sector groupings in GVA 2023
#Just then need to do the geographies
#Keep
write_csv(SIClookup,'data/SIClookup_forBRES_REGIONALGVA_2023_JOIN.csv')




# AND MAKING NUTS3/ITL3 2021/2025 HARMONISER----

#Or something appromimating that
#Start by just looking at what categories we have in our 3 data sources
#2 are BRES:
#NUTS3 from 2015 to 2022
#ITL3 2021 from 2022 to 2023
#Then the 2023 GVA data, which uses ITL3 2025

bres.nuts <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE437_nuts2016level3_2_Fulltimeemployees_2015_2022_SIC_5DIGIT.csv")

#Why is that missing some geog codes?
#Hmm - missing for some years?
#UPDATE: NOT THE CASE IN FULL TIME EMPLOYEES, IS TRUE FOR PART TIME. WHY?
bres.nuts.unique <- bres.nuts %>% 
  select(GEOGRAPHY_CODE,GEOGRAPHY_NAME) %>% 
  distinct()


#Then ITL3 2021
bres.itl3 <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv")

bres.itl3.unique <- bres.itl3 %>% 
  select(GEOGRAPHY_CODE,GEOGRAPHY_NAME) %>% 
  distinct()

#So: nuts2016 and ITL3 2021 have the SAME NUMBER CODE for ones that do match

#Ah yes, reminder: the only difference here is:
#ITL3 has "Bournemouth, Christchurch and Poole"
#NUTS3 is "Bournemouth and Poole"
#Christchurch moved from Dorset into the B+P zone for ITL3

#Previous solution was to replace all those with the higher ITL2 zone
#Since I'm getting this done a little quickly right now for Bradford comparison, might just drop them


#ITL3 2025 is going to be the more exciting issue!
#lLready loaded in previous section
gva.itl32025.unique <- gva %>% 
  select(ITL_code,Region_name) %>% 
  distinct()

#Unlike the Christchurch issue, I think every change is nested
#Which means summing job counts should be possible
#There is a lookup...
#Note there's also a shapefile, which we may need
#https://geoportal.statistics.gov.uk/datasets/itl-level-3-2021-to-itl-level-3-2025-lookup-in-the-uk/explore
itl.lookup <- read_csv('data/ITL_Level_3_(2021)_to_ITL_Level_3_(2025)_Lookup_in_the_UK.csv')

length(unique(itl.lookup$ITL321CD))
length(unique(itl.lookup$ITL325CD))

#Which differ?
itl.lookup$ITL325NM[!itl.lookup$ITL325NM %in% itl.lookup$ITL321NM]

#Ah OK - it goes in both directions
#E.g. highlands/islands in 2025 combines 7 2021 zones into 1
#So would involve some summing in both BRES and GVA to get match

#Separate out ones with more in 2021 (2025 combined them in some way)
#And more in 2025 (2025 separated them out e.g. BDR went into separate places)

separatedintosmaller.2025 <- itl.lookup %>% 
  group_by(ITL321NM) %>% 
  summarise(count = n(), ITL321CD = max(ITL321CD)) %>%
  filter(count > 1)

combinedintolarger.2025 <- itl.lookup %>% 
  group_by(ITL325NM) %>% 
  summarise(count = n(), ITL325CD = max(ITL325CD)) %>%
  filter(count > 1)

#East Dunbartonshire, West Dunbartonshire, and Helensburgh and Lomond
#Is I think the only one that gets split into two - 
#Helensburgh and Lomond end up in "highlands and islands"
#The rest tesselate.

#Though let's double check that...
itl.lookup %>% filter(ITL321NM %in% separatedintosmaller.2025$ITL321NM) %>% View

#Ah no, also split:
#Camden and City of London - now Westminster and City of London
#Westminster was separate in 2021





#And reminder: for chained volume - CANNOT SUM GVA FIGURES FROM SMALLER GEOGS
#So would have to revert to picking the nearest shared ITL2 and replacing
#Or bigger if overlap with more than one ITL2...

#Lack of consistency acros time is...!!!!

#Let's just pick out scots changes
alldiffs <- itl.lookup$ITL325NM[!itl.lookup$ITL325NM %in% itl.lookup$ITL321NM]

scotsdiffs <- alldiffs[qg('cumber|westmo|highlands|dunbar|ayr',alldiffs)]

#Ah hang on, we do also have a 2021 ITL that got split up across two others as well
#So the changes don't perfectly nest
itl.lookup %>% filter(ITL325NM %in% scotsdiffs) %>% select(ITL325NM,ITL321NM) %>% View



#Check ITL3 2021 and NUTS matches on the number code
#How many have same number code / does it just leave Bournemouth etc?
bres.itl3.unique <- bres.itl3.unique %>% 
  mutate(code_numbers = str_sub(GEOGRAPHY_CODE,-2,-1))

bres.nuts.unique <- bres.nuts.unique %>% 
  mutate(code_numbers = str_sub(GEOGRAPHY_CODE,-2,-1))

bres.comparison <- bres.itl3.unique %>% 
  rename(GEOGRAPHY_CODE_ITL32021 = GEOGRAPHY_CODE, GEOGRAPHY_NAME_ITL32021 = GEOGRAPHY_NAME) %>% 
  left_join(
    bres.nuts.unique %>% 
      rename(GEOGRAPHY_CODE_NUTS = GEOGRAPHY_CODE, GEOGRAPHY_NAME_NUTS = GEOGRAPHY_NAME)
  )

#Nope that doesn't work!
#This site says there's a lookup, I've not managed to find it
#https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/europeancitystatistics

#can't find existing NUTS/ITL2021 lookup
#I did already do this by manually tweaking non matching names...


#But let's just remind myself what we're aiming for here:
#Partial/most places matches to have GVA and jobs counts values
#for ITL3 2025 zones, so we can get GVA per FT job

#BRES NUTS and BRES ITL3 2021 I can do full match, will just be missing Dorset
#Check name match as is
table(bres.nuts.unique$GEOGRAPHY_NAME %in% bres.itl3.unique$GEOGRAPHY_NAME)

#Non match names...
bres.nuts.unique$GEOGRAPHY_NAME[!bres.nuts.unique$GEOGRAPHY_NAME %in% bres.itl3.unique$GEOGRAPHY_NAME]
bres.itl3.unique$GEOGRAPHY_NAME[!bres.itl3.unique$GEOGRAPHY_NAME %in% bres.nuts.unique$GEOGRAPHY_NAME]

#Yeah I think I already manually fixed that... (tho possibly in the wrong direction but shouldn't matter)
#Here: https://github.com/DanOlner/RegionalEconomicTools/blob/3188a0d1716e1d3498454c3f00e0c11e36bfae08/prepcode/combine_regGVA_and_BRES.R#L220 

#Update the ITL3 zone names
bres.itl3.unique.tweaknames <- bres.itl3.unique %>% 
  mutate(
    GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME),
    GEOGRAPHY_NAME = case_when(
      GEOGRAPHY_NAME == "Inverness and Nairn, Moray, and Badenoch and Strathspey" ~ "Inverness & Nairn and Moray, Badenoch & Strathspey",
      GEOGRAPHY_NAME == 'Caithness and Sutherland, and Ross and Cromarty' ~ 'Caithness & Sutherland and Ross & Cromarty',
      GEOGRAPHY_NAME == 'Lochaber, Skye and Lochalsh, Arran and Cumbrae, and Argyll and Bute' ~ 'Lochaber, Skye & Lochalsh, Arran & Cumbrae and Argyll & Bute',
      GEOGRAPHY_NAME == 'Na h-Eileanan Siar' ~ 'Na h-Eileanan Siar (Western Isles)',
      GEOGRAPHY_NAME == 'City of Edinburgh' ~ 'Edinburgh, City of',
      GEOGRAPHY_NAME == 'Perth and Kinross, and Stirling' ~ 'Perth & Kinross and Stirling',
      GEOGRAPHY_NAME == 'East Dunbartonshire, West Dunbartonshire, and Helensburgh and Lomond' ~ 'East Dunbartonshire, West Dunbartonshire and Helensburgh & Lomond',
      GEOGRAPHY_NAME == 'Inverclyde, East Renfrewshire, and Renfrewshire' ~ 'Inverclyde, East Renfrewshire and Renfrewshire',
      GEOGRAPHY_NAME == 'Dumfries and Galloway' ~ 'Dumfries & Galloway',
      .default = GEOGRAPHY_NAME
    ))

bres.nuts.unique$GEOGRAPHY_NAME[!bres.nuts.unique$GEOGRAPHY_NAME %in% bres.itl3.unique.tweaknames$GEOGRAPHY_NAME]
bres.itl3.unique.tweaknames$GEOGRAPHY_NAME[!bres.itl3.unique.tweaknames$GEOGRAPHY_NAME %in% bres.nuts.unique$GEOGRAPHY_NAME]

#Also need CCs removing from the nuts ones...
bres.nuts.unique.tweaknames <- bres.nuts.unique %>% 
  mutate(GEOGRAPHY_NAME = gsub(' CC','',GEOGRAPHY_NAME))

#THERE, FULL MATCH APART FROM ALTERED BOURNEMOUTH GEOG
bres.nuts.unique.tweaknames$GEOGRAPHY_NAME[!bres.nuts.unique.tweaknames$GEOGRAPHY_NAME %in% bres.itl3.unique.tweaknames$GEOGRAPHY_NAME]
bres.itl3.unique.tweaknames$GEOGRAPHY_NAME[!bres.itl3.unique.tweaknames$GEOGRAPHY_NAME %in% bres.nuts.unique.tweaknames$GEOGRAPHY_NAME]



#So we can make a consistent NUTS3/ITL3 across those two BRES files now
#Before messing with ITL3 2025 at all

#Make lookup first, then can keep from both
#Do full join so we can see the bournemouth difference
nuts3.itl321.lookup <- bres.nuts.unique.tweaknames %>% 
  select(GEOGRAPHY_NAME,GEOGRAPHY_CODE_NUTS2018 = GEOGRAPHY_CODE) %>% 
  full_join(
    bres.itl3.unique.tweaknames %>% select(GEOGRAPHY_NAME,GEOGRAPHY_CODE_ITL321 = GEOGRAPHY_CODE),
    by = 'GEOGRAPHY_NAME'
  )

#Save! 
write_csv(nuts3.itl321.lookup, 'data/NUTS3_2018_v_ITL3_2021_lookup.csv')




#So let's make a 2015-2023 ITL321 focused BRES df from that
#Should really matter which way we do this but...

#Merge in NUTS3 code lookup to connect
# bres.itl3 <- bres.itl3 %>% 
#   left_join(
#     nuts3.itl321.lookup %>% select(GEOGRAPHY_CODE_NUTS2018,GEOGRAPHY_CODE = GEOGRAPHY_CODE_ITL321),
#     by = 'GEOGRAPHY_CODE'
#   )
# 
# table(!is.na(bres.itl3$GEOGRAPHY_CODE_NUTS2018))  
# 
# #Non-matches should just be bournemouth... tick
# unique(bres.itl3$GEOGRAPHY_NAME[is.na(bres.itl3$GEOGRAPHY_CODE_NUTS2018)])

#OH YES, ORDER WILL MATTER
#We want to keep ITL3 2021 codes in both
#So we can then use the 21-25 lookup after for GVA

#Then we can just row bind them once all columns match...
bres.nuts <- bres.nuts %>%
  left_join(
    nuts3.itl321.lookup %>% select(GEOGRAPHY_CODE_ITL321,GEOGRAPHY_CODE = GEOGRAPHY_CODE_NUTS2018),
    by = 'GEOGRAPHY_CODE'
  ) %>% 
  rename(GEOGRAPHY_CODE_NUTS2018 = GEOGRAPHY_CODE) %>% 
  relocate(GEOGRAPHY_CODE_ITL321, .before = GEOGRAPHY_CODE_NUTS2018)

table(!is.na(bres.nuts$GEOGRAPHY_CODE_ITL321))

#Non-matches should just be bournemouth... tick
unique(bres.nuts$GEOGRAPHY_NAME[is.na(bres.nuts$GEOGRAPHY_CODE_ITL321)])


#Keep the two different code names in, to keep code differences clear
#Esp useful when then introducing third round of different geographies!
bres.itl3 <- bres.itl3 %>% 
  rename(GEOGRAPHY_CODE_ITL321 = GEOGRAPHY_CODE) %>% 
  mutate(GEOGRAPHY_CODE_NUTS2018 = NA)

#Then check names now match... tick
table(names(bres.nuts) %in% names(bres.itl3))

#stack on top of each other!
bres15to23 <- bind_rows(bres.itl3,bres.nuts) 


#Names aren't all entirely consistent - small hack, replace names from one source
bres15to23 <- bres15to23 %>% 
  select(-GEOGRAPHY_NAME) %>% 
  left_join(
    bres.itl3 %>% select(GEOGRAPHY_CODE_ITL321,GEOGRAPHY_NAME) %>% distinct(),
    by = 'GEOGRAPHY_CODE_ITL321'
  )


#Confirm itl3 codes all there (minus bournemouth for the earlier years...)
#Tick
table(!is.na(bres15to23$GEOGRAPHY_CODE_ITL321))
unique(bres15to23$GEOGRAPHY_NAME[is.na(bres15to23$GEOGRAPHY_CODE_ITL321)])


#Save in same place with similar name
write_csv(bres15to23,"local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_NUTS3_n_ITL321_stacked_2_Fulltimeemployees_2015_2023_SIC_5DIGIT.csv")




# NEXT: create a version that uses as much of the gva ITL3 2025 as we can link to
#Which might include some summing of GVA - if we're using current prices
#Or some substituting for larger geographies, if chained volume e.g. SY for Shef + BDR

#Not worrying about the SIC summations at the mo, let's just check on geographies...

#Also - see above for separatedintosmaller.2025
#Two places get shifted around in ways that don't tesselate - best drop those for now
#For the rest - if using current prices - we can use this list to sum GVA
#To make a slightly reduced ITL list

#So - get gva for that bespoke 2 digit SIC list
gva.2digit <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv')

#Remove places that combined in 2025
gva.2digit <- gva.2digit %>% 
  filter(!ITL_code %in% combinedintolarger.2025$ITL325CD)

#This will mean we get no "multiple matches" warning here
#One place per code
#But will need to remove a couple that split across places...
#Merge in 2025 ITL codes
gva.2digit <- gva.2digit %>% 
  left_join(
    itl.lookup %>% select(ITL321CD,ITL321NM,ITL_code = ITL325CD),#Will need names to keep
    by = 'ITL_code'
  )

#Remove the two overlap ones (see above)
gva.2digit <- gva.2digit %>% 
  filter(!qg('city of lon|camden|East Dunbartonshire and West Dunbartonshire',Region_name))



#AT THIS POINT, WE COULD REMOVE THOSE OTHERS, OR REPLACE LARGER AREAS FROM THE ITL2 DATA FOR CV
#But for current prices - we can total up the GVA
gva.2digit.sumplaces <- gva.2digit %>% 
  group_by(ITL321CD,year,SIC07_code) %>% 
  summarise(
    value = sum(value),
    Region_name = max(ITL321NM),
    SIC07_description = max(SIC07_description)
    )

#OK, that's our best-for-now GVA 2 digit file that can match against BRES
#Save
write_csv(gva.2digit.sumplaces,'data/regionalGVA/regionalGVA_currentprices_COMBINED_ITL21_N_25_TO_MATCH_BRES2023_SIC_2DIGIT_LONG_2023.csv')



# CHECKS ON BRES15TO23 AND BESPOKE GVA CURRENT PRICES BEFORE COMBINING IN NEXT STAGE

#Check we now have geography match
table(unique(bres15to23$GEOGRAPHY_CODE_ITL321) %in% gva.2digit.sumplaces$ITL321CD)
table(unique(gva.2digit.sumplaces$ITL321CD) %in% bres15to23$GEOGRAPHY_CODE_ITL321)

#Nom matches still christchurch, yes?
unique(bres15to23$GEOGRAPHY_NAME)[!unique(bres15to23$GEOGRAPHY_CODE_ITL321) %in% gva.2digit.sumplaces$ITL321CD]
unique(gva.2digit.sumplaces$Region_name)[!unique(gva.2digit.sumplaces$ITL321CD) %in% bres15to23$GEOGRAPHY_CODE_ITL321]


#We now need to apply the BRES job summing to the bespoke GVA 2 digit categories
#Use lookup made earlier!

#Link to BRES, count jobs by GVA 2 digit SIC categories
#May want to repeat for sections 
# x <- bres15to23 %>% 
bres15to23 <- bres15to23 %>% 
  left_join(
    SIClookup %>% select(SIC_5DIGIT_CODE,SIC_2DIGIT_CODE_GVA2023),
    by = 'SIC_5DIGIT_CODE'
  )

#check - is just extrat, we don't want anyway
table(!is.na(bres15to23$SIC_2DIGIT_CODE_GVA2023))
bres15to23 %>% filter(is.na(SIC_2DIGIT_CODE_GVA2023)) %>% View

bres15to23 <- bres15to23 %>% 
  filter(!is.na(SIC_2DIGIT_CODE_GVA2023))

#confirm correct GVA 2 digit count...
length(unique(bres15to23$SIC_2DIGIT_CODE_GVA2023))
length(unique(gva.2digit.sumplaces$SIC07_code))

#No imputed rent match - again, fine, is meaningless for job count
#Has already been removed via joins
unique(gva.2digit.sumplaces$SIC07_code)[!unique(gva.2digit.sumplaces$SIC07_code) %in% unique(bres15to23$SIC_2DIGIT_CODE_GVA2023)]



#Right - can now sum job counts by the bespoke 2 digit SICs
bres15to23.gva2digitSICs <- bres15to23 %>% 
  group_by(DATE,GEOGRAPHY_CODE_ITL321,SIC_2DIGIT_CODE_GVA2023) %>% 
  summarise(
    JOBCOUNT = sum(JOBCOUNT)
    ) %>% 
  ungroup()


#I think we might finally be ready to join to the 2 digit 2023 GVA...
bres.gva.2digit.2023 <- bres15to23.gva2digitSICs %>% 
  inner_join(
    gva.2digit.sumplaces,
    by = c('DATE' = 'year','GEOGRAPHY_CODE_ITL321' = 'ITL321CD','SIC_2DIGIT_CODE_GVA2023' = 'SIC07_code')
  ) %>% 
  rename(GVA = value)


#Why some NA job counts?
#I think the issue with the missing values is just early years with no agri data...
bres.gva.2digit.2023 %>% filter(is.na(JOBCOUNT)) %>% View


#OK... SAAAAVE
write_csv(bres.gva.2digit.2023,'data/regionalGVA_plus_BRESjobcounts/regionalGVA_currentprices_BRES_FT_jobcount_bespoke2digitSIC_nONLY_MATCHING_GEOGs_2015_2023.csv')

saveRDS(bres.gva.2digit.2023,'data/regionalGVA_plus_BRESjobcounts/regionalGVA_currentprices_BRES_FT_jobcount_bespoke2digitSIC_nONLY_MATCHING_GEOGs_2015_2023.rds')




#Version for chained volume at ITL3 as well, save as different file
#Tho arguably easier just to stick these in one CSV, will leave separate...
bres.gva.2digit.2023 <- readRDS('data/regionalGVA_plus_BRESjobcounts/regionalGVA_currentprices_BRES_FT_jobcount_bespoke2digitSIC_nONLY_MATCHING_GEOGs_2015_2023.rds')

#checking... yeah, 2022-23, too many years
#I think the diff is probably Christchurch and Poole in last two years
table(bres.gva.2digit.2023$DATE)
table(bres.gva.2digit.2023$DATE, bres.gva.2digit.2023$Region_name)





itl3.cv <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_2DIGIT_LONG_2023.csv')

#Check daft values... newp
table(itl3.cv$value < 0)

#Reminder: this 2023 BRES/GVA data *only* has matching ITL3 zones - 
#i.e. BRES was ITL 2021 and not all of them now match, have kept only those that do
#Except for a few where they could be combined e.g. Barnsley Doncaster Rotherham

#Ah, so - this means for CV it's going to be EVEN LOWER
#Because we can't sum, and can only match on direct one to one matches

#Also, BRES is GB only. So check again what doesn't match...
table(unique(bres.gva.2digit.2023$GEOGRAPHY_CODE_ITL321) %in% itl3.cv$ITL_code)

unique(bres.gva.2digit.2023$Region_name)[!unique(bres.gva.2digit.2023$GEOGRAPHY_CODE_ITL321) %in% itl3.cv$ITL_code]

#Hmm. ITL codes not matching... oh of course it's not, "ITL321" means ITL3 2021!
#Check if names better... yep!
table(unique(bres.gva.2digit.2023$GEOGRAPHY_CODE_ITL321) %in% itl3.cv$ITL_code)
table(unique(bres.gva.2digit.2023$Region_name) %in% itl3.cv$Region_name)

#Missings... yep, correct, just ones that got shifted about 21 -> 25
unique(bres.gva.2digit.2023$Region_name)[!unique(bres.gva.2digit.2023$Region_name) %in% itl3.cv$Region_name]

#And check SIC code match... tick
table(unique(bres.gva.2digit.2023$SIC_2DIGIT_CODE_GVA2023) %in% itl3.cv$SIC07_code)


#OK, we join on name! 


#Right, join CV GVA value by place name, see what region's are left.
#Should be 10 fewer, 145 total
bres.gva.chainedvolume.2digit.2023 <- bres.gva.2digit.2023 %>% 
  select(-GVA) %>% 
  inner_join(
    itl3.cv %>% select(DATE = year, GVA = value, GEOGRAPHY_CODE_ITL325 = ITL_code,Region_name,SIC_2DIGIT_CODE_GVA2023 = SIC07_code),
    by = c('DATE','Region_name','SIC_2DIGIT_CODE_GVA2023')
  )

#Checks...
unique(bres.gva.chainedvolume.2digit.2023$Region_name)

#Should probably drop any places that don't appear in every year
#TODO: keep only places with data in all years in BRES/GVA linked for current prices
table(bres.gva.chainedvolume.2digit.2023$DATE, bres.gva.chainedvolume.2digit.2023$Region_name)

bres.gva.chainedvolume.2digit.2023 <- bres.gva.chainedvolume.2digit.2023 %>% filter(!qg('bournemouth', Region_name))


#SaaaHAhaaaave
write_csv(bres.gva.2digit.2023,'data/regionalGVA_plus_BRESjobcounts/regionalGVA_chainedvolume_BRES_FT_jobcount_bespoke2digitSIC_nONLY_MATCHING_GEOGs_2015_2023.csv')

saveRDS(bres.gva.2digit.2023,'data/regionalGVA_plus_BRESjobcounts/regionalGVA_chainedvolume_BRES_FT_jobcount_bespoke2digitSIC_nONLY_MATCHING_GEOGs_2015_2023.rds')





# CHECK SIC MATCHES PULLED FROM IND STRATEGY DOC----

#Excluded codes: Frontier areas without clear SIC mappings (e.g., Clean Tech, Hydrogen, Heat Pumps, Nuclear) not included
indsic <- read_csv('data/industrialstrategy2025sectordefs.csv')
# indsic <- read_csv('data/industrialstrategy2025sectordefs_test.csv')


#LOAD BRES 5 DIGIT
bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_NUTS3_n_ITL321_stacked_2_Fulltimeemployees_2015_2023_SIC_5DIGIT.csv")

#This comes with ALL NUTS and ITL3 data
#Which means 2022 is doubled up
#E.g. see here:
#bres %>% filter(qg('bradford', GEOGRAPHY_NAME),qg('Museum activities',SIC_5DIGIT_NAME)) %>% View

#Remove one of them...
#Need to filter NOT year == 2022 NOT GEOGRAPHY_CODE_NUTS2018 is NA, keep the rest
#(2023 we need to keep!)
bres <- bres %>%
  filter(!(DATE == 2022 & is.na(GEOGRAPHY_CODE_NUTS2018)))


#range(bres$DATE)#tick

SIClookup <- read_csv('data/SIClookup.csv')

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bres <- bres %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
  )

#Make shorter names, use those.
#Make lookup so can be merged in.
names.sections = unique(bres$SIC_SECTION_NAME)
shortnames.sections = reduceSICnames(unique(bres$SIC_SECTION_NAME),'section')

chk.sections = data.frame(names = names.sections, shortnames = shortnames.sections)

names.2digit = unique(bres$SIC_2DIGIT_NAME)
shortnames.2digit = reduceSICnames(unique(bres$SIC_SECTION_NAME),'2 digit')

chk.2digit = data.frame(names = names.2digit, shortnames = shortnames.2digit)


#And 729 categories in the 5 digit...
names.5digit = unique(bres$SIC_5DIGIT_NAME)
#NOTE: THE ORDER HERE IS BASED ON THE ORDER IN bres
#We should make an actual lookup to make sure we don't lose that order
shortnames.5digit = reduceSICnames(unique(bres$SIC_5DIGIT_NAME),'5 digit')

chk.5digit = data.frame(names = names.5digit, shortnames = shortnames.5digit)


#Merge into bres
bres <- bres %>%
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>%
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>%
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names'))


#Drop some sectors
bres <- bres %>%
  filter(!qg('households|membership', SIC_2DIGIT_NAME))

#Check!
# bres %>% distinct(SIC_5DIGIT_NAME_SHORT,SIC_5DIGIT_NAME) %>% View

#Save sample of BRES just for one place and one year
# write_csv(bres %>% filter(GEOGRAPHY_NAME == 'Bradford', DATE == 2023) %>% slice_sample(n = 30),'local/data/sample5digitBRES.csv')



#THEN: need to find way to match against differing levels of code in the indstrat list
#And summing if necessary
#One way would be to join against the indstrat and use to group

#But a few tests/checks... 
#I think all four digit are same as five digit but with a zero missing
#Check
fourz <- indsic$sic_code[nchar(indsic$sic_code) == 4]
fourz <- fourz[!is.na(fourz)]

fourz <- paste0(fourz,"0")

#Not all
table(fourz %in% SIClookup$SIC_5DIGIT_CODE)

fourz[!fourz %in% SIClookup$SIC_5DIGIT_CODE]



#OK try this
# bres_df <- read.csv("local/data/sample5digitBRES.csv", colClasses = c(SIC_5DIGIT_CODE = "character"))
# strategy_df <- read.csv("data/industrialstrategy2025sectordefs.csv", colClasses = c(sic_code = "character"))
# 
# # Function to match and sum jobs for SICs at varying digit lengths
# sum_jobs_for_sic <- function(sic_prefix, bres_data) {
#   # Pad strategy SIC to the left if needed
#   pattern <- paste0("^", sic_prefix)
#   matched_rows <- bres_data %>%
#     filter(str_detect(SIC_5DIGIT_CODE, pattern))
#   sum(matched_rows$JOBCOUNT, na.rm = TRUE)
# }
# 
# # Apply row-wise to strategy SIC definitions
# strategy_with_jobs <- strategy_df %>%
#   rowwise() %>%
#   mutate(total_jobs = sum_jobs_for_sic(sic_code, bres_df)) %>%
#   ungroup()
# 
# # View result
# print(strategy_with_jobs)




#Test on larger amount (without grouping by year or place yet)
#Tick
# strategy_with_jobs <- indsic %>%
#   rowwise() %>%
#   mutate(total_jobs = sum_jobs_for_sic(sic_code, bres %>% filter(GEOGRAPHY_NAME == 'Bradford', DATE == 2023))) %>%
#   ungroup()
# 
# #Check it works for grouping by place and year
# strategy_with_jobs <- indsic %>%
#   rowwise() %>%
#   mutate(total_jobs = sum_jobs_for_sic(sic_code, bres %>% filter(GEOGRAPHY_NAME %in% c('Bradford','Leeds'), DATE %in% c(2022,2023)))) %>%
#   ungroup()





#BETTER APPROACH: JOIN SICS TO BRES THEN SUM BY THAT JOIN
#Use fuzzy join to get easy match
#^ = match on just these first characters

#Ind sic codes, a few filters:
#Don't keep any NAs in SIC_name - those are ind strat categories with no SIC code
strategy_df <- indsic %>%
  filter(!is.na(SIC_name)) %>% 
  mutate(regex = paste0("^", sic_code))

#Only use smoothed data values
bres_annotated <- fuzzyjoin::regex_left_join(
  bres,
  # bres %>% filter(!is.na(jobcount_movingav)),
  strategy_df %>% distinct(sic_code, .keep_all = T),#Some like 20 are doubled up in e.g. foundationals, keep only one
  by = c("SIC_5DIGIT_CODE" = "regex")
)

#Then just summarise
#Don't need to keep indstrat sectors we have no match for
bres_summary <- bres_annotated %>%
  filter(!is.na(sic_code)) %>% 
  group_by(DATE,GEOGRAPHY_NAME,SIC_name) %>%
  summarise(
    JOBCOUNT = sum(JOBCOUNT, na.rm = TRUE), .groups = "drop",
    sic_codefromindstrat = max(sic_code),
    topsector_indstrat = max(sector_name),
    # SIC_5DIGIT_CODE = max(SIC_5DIGIT_CODE),
    # SIC_5DIGIT_NAME_SHORT = max(SIC_5DIGIT_NAME_SHORT),#some of these won't be right
    SIC_2DIGIT_NAME_SHORT = max(SIC_2DIGIT_NAME_SHORT),
    is_frontier = max(is_frontier)
    ) %>% 
  rename(sic_namefrom_indstrat = SIC_name)
  

#Sanity checks... same number of cats, OK
length(unique(bres_summary$sic_namefrom_indstrat))
length(unique(indsic$sic_code))
unique(bres_summary$DATE)

table(is.na(bres_summary$sic_code))
# bres_summary %>% filter(is.na(sic_code)) %>% View


#Some more sanity checks. Did things sum correctly? Pick a couple of test sectors
chk <- bres_summary %>% 
  filter(sic_namefrom_indstrat == 'Manufacture of chemicals and chemical products', DATE == 2023)




#Find moving av AFTER summing
#Should be moving av of those totals
#Get moving av for jobs in BRES for 5 digit, use that latest year...
smoothband = 3

bres_summary <- bres_summary %>% 
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME,sic_namefrom_indstrat) %>%
  mutate(
    jobcount_movingav = rollapply(JOBCOUNT,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(jobcount_movingav))

#check... tick
table(is.na(bres_summary$jobcount_movingav),bres_summary$DATE)



#MAKE SHORTER NAMES THAT COMBINE WITH THE INDSTRAT TOP CAT
#Top level cats first, that we will combine with the shortened sector name
unique(bres_summary$topsector_indstrat)

bres_summary <- bres_summary %>% 
  mutate(
    toplevelindstrat = case_when(
      qg('manufactur', topsector_indstrat) ~ "ADV MANUF",
      qg('business', topsector_indstrat) ~ "PBS",
      qg('financ', topsector_indstrat) ~ "FIN",
      qg('creative', topsector_indstrat) ~ "CREATIVE",
      qg('foundation', topsector_indstrat) ~ "FOUNDATION",
      qg('digital', topsector_indstrat) ~ "DIGITAL",
      qg('life sci', topsector_indstrat) ~ "LIFESCI",
      qg('defence', topsector_indstrat) ~ "DEFENCE"
    )
  )

#And shortern other names
bres_summary <- bres_summary %>% 
  mutate(
    sic_namefrom_indstrat_short = removecommonSICnameelements(bres_summary$sic_namefrom_indstrat, removemanuf = T)
  )

# table(bres_summary$toplevelindstrat)
# table(bres_summary$sic_namefrom_indstrat_short)

#Make combo
bres_summary <- bres_summary %>% 
  mutate(
    sic_namefrom_indstrat_combo = paste0(toplevelindstrat,": ",sic_namefrom_indstrat_short)
  )

table(bres_summary$sic_namefrom_indstrat_combo)


#Have manually added in those specific ind strat codes into the CSV
#Let's see if we can get LQs for those...
islq <- bres_summary %>% 
  group_split(DATE) %>% 
  map(
    add_location_quotient_and_proportions,
    regionvar = GEOGRAPHY_NAME,
    lq_var = sic_namefrom_indstrat_combo,
    valuevar = jobcount_movingav
) %>% 
  bind_rows() %>% 
  mutate(jobcount_movingav = round(jobcount_movingav,0))


LQ_slopes <- compute_slope_or_zero(
  data = islq, 
  GEOGRAPHY_NAME, sic_namefrom_indstrat_combo,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = DATE)


#Filter down to a single year... we may want to smooth years, let's see
yeartoplot <- islq %>% filter(DATE == max(DATE))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('GEOGRAPHY_NAME', 'sic_namefrom_indstrat_combo')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- islq %>% 
  group_by(GEOGRAPHY_NAME, sic_namefrom_indstrat_combo) %>% 
  summarise(
    min_LQ_all_time = min(LQ, na.rm = T),
    max_LQ_all_time = max(LQ, na.rm = T)
  ) %>% 
  mutate(
    min_LQ_all_time = ifelse(is.infinite(min_LQ_all_time),NA,min_LQ_all_time),
    max_LQ_all_time = ifelse(is.infinite(max_LQ_all_time),NA,max_LQ_all_time)
  )

# table(is.infinite(minmaxes$min_LQ_all_time))
# table(is.infinite(minmaxes$max_LQ_all_time))

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('GEOGRAPHY_NAME', 'sic_namefrom_indstrat_combo')
  )

#Save all the bits! So we can repeat for different places easily
# saveRDS(islq,'local/islq.rds')
# saveRDS(yeartoplot,'local/is_bres_yeartoplot.rds')



#Right ee ho...
#place = "Sheffield"
place = "Bradford"

sectorLQorder <- islq %>% filter(
  DATE == max(DATE),#use latest data
  GEOGRAPHY_NAME == place) %>% 
  arrange(-LQ) %>% 
  select(sic_namefrom_indstrat_combo) %>% 
  pull()


#Split by frontier or not

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$sic_namefrom_indstrat_combo <- factor(yeartoplot$sic_namefrom_indstrat_combo, levels = sectorLQorder, ordered = T)

#Remove some LQs
yeartoplot.lqfiltered <- yeartoplot %>% filter(LQ > 0 & LQ < 100, jobcount_movingav > 99)

#Drop any sectors that Bradford now doesn't have after that filter
sectorstokeep <- yeartoplot.lqfiltered %>% filter(GEOGRAPHY_NAME == place) %>% select(sic_namefrom_indstrat_combo) %>% pull

yeartoplot.lqfiltered <- yeartoplot.lqfiltered %>% filter(sic_namefrom_indstrat_combo %in% sectorstokeep)

#save a copy of that to save processing time in quarto
saveRDS(yeartoplot.lqfiltered,'local/data/bres_indstrat_LQs.rds')

p <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), alpha = 0.1, shape = 0, sector_name = sic_namefrom_indstrat_combo, LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount_movingav, sector_regional_proportion = sector_regional_proportion,
                            region_name = GEOGRAPHY_NAME,
                            sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = F)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat frontier sector")

p

p <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 0), alpha = 0.1, shape = 0, sector_name = sic_namefrom_indstrat_combo, LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 0), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount_movingav, sector_regional_proportion = sector_regional_proportion,
                            region_name = GEOGRAPHY_NAME,
                            sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = F)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat other sector")

p


#While we're here... compare the job totals for Bradford in these
#To full job totals in BRES as a whole 
#And compare to UK
#What proportion of jobs are ind strat ready? (Even without looking deeper into other sectors)

#THERE'S SOME DOUBLE COUNTING TO REMOVE HERE I THINK
#Where subsectors also have their parent sector included...
#Not many of those but still needs fixing

#For which we need 3 year smoothed BRES job counts
brestots <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_NUTS3_n_ITL321_stacked_2_Fulltimeemployees_2015_2023_SIC_5DIGIT.csv")

#Remove one of them...
#Need to filter NOT year == 2022 NOT GEOGRAPHY_CODE_NUTS2018 is NA, keep the rest
#(2023 we need to keep!)
brestots <- brestots %>%
  filter(!(DATE == 2022 & is.na(GEOGRAPHY_CODE_NUTS2018)))

#Get totals first then smooth
totjobsperITL3 <- brestots %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  summarise(
    total_jobs = sum(JOBCOUNT, na.rm = T)
  )

#Then smooth...
totjobsperITL3 <- totjobsperITL3 %>% 
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    totaljobs_movingav = rollapply(total_jobs,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()
  

#Same vals for ind-strat-only sectors
indstrat_totjobsperITL3 <- fuzzyjoin::regex_left_join(
  bres,
  strategy_df %>% distinct(sic_code, .keep_all = T),#Some like 20 are doubled up in e.g. foundationals, keep only one
  by = c("SIC_5DIGIT_CODE" = "regex")
) %>% 
  filter(!is.na(sector_name))

#Then just summarise totals in places for each year
indstrat_totjobsperITL3 <- indstrat_totjobsperITL3 %>%
  group_by(DATE,GEOGRAPHY_NAME) %>%
  summarise(
    total_jobs = sum(JOBCOUNT, na.rm = TRUE), .groups = "drop",
    sic_namefrom_indstrat = max(SIC_name),
    is_frontier = max(is_frontier)
    ) 

#Smooth years...
indstrat_totjobsperITL3 <- indstrat_totjobsperITL3 %>%
  arrange(DATE) %>% 
    group_by(GEOGRAPHY_NAME) %>% 
    mutate(
      totaljobs_movingav = rollapply(total_jobs,smoothband,mean,align='center',fill=NA)
    ) %>% 
    ungroup()


#Now can just join on geography name for the appropriate smoothed year to find ratios
both <- totjobsperITL3 %>% 
  filter(DATE == max(DATE)-1) %>% 
  select(-total_jobs) %>% 
  inner_join(
    indstrat_totjobsperITL3 %>% filter(DATE == max(DATE)-1) %>% select(-total_jobs) %>% rename(totINDSTRATjobs_movingav = totaljobs_movingav),
    by = 'GEOGRAPHY_NAME'
  ) %>% 
  mutate(
    percent_indstratjobs = (totINDSTRATjobs_movingav/totaljobs_movingav) * 100
  )

#Err... two places with more than 100%. Right you are.
#OK, Bradford very average!
ggplot(both,aes(x = percent_indstratjobs)) +
  geom_histogram(binwidth = 4) +
  geom_vline(
    xintercept = both %>% filter(GEOGRAPHY_NAME == 'Bradford') %>% select(percent_indstratjobs) %>% pull
      ) +
  coord_cartesian(xlim = c(0,90))


#Oh maybe not average
ecdf(both$percent_indstratjobs)(both %>% filter(GEOGRAPHY_NAME == 'Bradford') %>% select(percent_indstratjobs) %>% pull)






# REPEAT FOR BRES 2024 LOCAL AUTHORITY LEVEL----

#Excluded codes: Frontier areas without clear SIC mappings (e.g., Clean Tech, Hydrogen, Heat Pumps, Nuclear) not included
indsic <- read_csv('data/industrialstrategy2025sectordefs.csv')
# indsic <- read_csv('data/industrialstrategy2025sectordefs_test.csv')


#LOAD BRES 5 DIGIT
bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE423_localauthoritiescountyunitaryasofApril2023_2_Fulltimeemployees_2015_2024_SIC_5DIGIT.csv")

SIClookup <- read_csv('data/SIClookup.csv')

#Join SIC lookup on 5 digit name
#Keep 2 digit and section codes
#May shorten names in a mo...
bres <- bres %>%
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME),
    by = 'SIC_5DIGIT_NAME'
  )

#Make shorter names, use those.
#Make lookup so can be merged in.
names.sections = unique(bres$SIC_SECTION_NAME)
shortnames.sections = reduceSICnames(unique(bres$SIC_SECTION_NAME),'section')

chk.sections = data.frame(names = names.sections, shortnames = shortnames.sections)

names.2digit = unique(bres$SIC_2DIGIT_NAME)
shortnames.2digit = reduceSICnames(unique(bres$SIC_SECTION_NAME),'2 digit')

chk.2digit = data.frame(names = names.2digit, shortnames = shortnames.2digit)


#And 729 categories in the 5 digit...
names.5digit = unique(bres$SIC_5DIGIT_NAME)
#NOTE: THE ORDER HERE IS BASED ON THE ORDER IN bres
#We should make an actual lookup to make sure we don't lose that order
shortnames.5digit = reduceSICnames(unique(bres$SIC_5DIGIT_NAME),'5 digit')

chk.5digit = data.frame(names = names.5digit, shortnames = shortnames.5digit)


#Merge into bres
bres <- bres %>%
  left_join(chk.sections %>% rename(SIC_SECTION_NAME_SHORT = shortnames), by = c('SIC_SECTION_NAME' = 'names')) %>%
  left_join(chk.2digit %>% rename(SIC_2DIGIT_NAME_SHORT = shortnames), by = c('SIC_2DIGIT_NAME' = 'names')) %>%
  left_join(chk.5digit %>% rename(SIC_5DIGIT_NAME_SHORT = shortnames), by = c('SIC_5DIGIT_NAME' = 'names'))


#Drop some sectors
bres <- bres %>%
  filter(!qg('households|membership', SIC_2DIGIT_NAME))

#Check!
# bres %>% distinct(SIC_5DIGIT_NAME_SHORT,SIC_5DIGIT_NAME) %>% View

#Save sample of BRES just for one place and one year
# write_csv(bres %>% filter(GEOGRAPHY_NAME == 'Bradford', DATE == 2023) %>% slice_sample(n = 30),'local/data/sample5digitBRES.csv')



#THEN: need to find way to match against differing levels of code in the indstrat list
#And summing if necessary
#One way would be to join against the indstrat and use to group


#BETTER APPROACH: JOIN SICS TO BRES THEN SUM BY THAT JOIN
#Use fuzzy join to get easy match
#^ = match on just these first characters

#Ind sic codes, a few filters:
#Don't keep any NAs in SIC_name - those are ind strat categories with no SIC code
strategy_df <- indsic %>%
  filter(!is.na(SIC_name)) %>% 
  mutate(regex = paste0("^", sic_code))

#Only use smoothed data values
bres_annotated <- fuzzyjoin::regex_left_join(
  bres,
  # bres %>% filter(!is.na(jobcount_movingav)),
  strategy_df %>% distinct(sic_code, .keep_all = T),#Some like 20 are doubled up in e.g. foundationals, keep only one
  by = c("SIC_5DIGIT_CODE" = "regex")
)

#Then just summarise
#Don't need to keep indstrat sectors we have no match for
bres_summary <- bres_annotated %>%
  filter(!is.na(sic_code)) %>% 
  group_by(DATE,GEOGRAPHY_NAME,SIC_name) %>%
  summarise(
    JOBCOUNT = sum(JOBCOUNT, na.rm = TRUE), .groups = "drop",
    sic_codefromindstrat = max(sic_code),
    topsector_indstrat = max(sector_name),
    # SIC_5DIGIT_CODE = max(SIC_5DIGIT_CODE),
    # SIC_5DIGIT_NAME_SHORT = max(SIC_5DIGIT_NAME_SHORT),#some of these won't be right
    SIC_2DIGIT_NAME_SHORT = max(SIC_2DIGIT_NAME_SHORT),
    is_frontier = max(is_frontier)
  ) %>% 
  rename(sic_namefrom_indstrat = SIC_name)


#Sanity checks... same number of cats, OK
length(unique(bres_summary$sic_namefrom_indstrat))
length(unique(indsic$sic_code))
unique(bres_summary$DATE)

# table(is.na(bres_summary$sic_code))
# bres_summary %>% filter(is.na(sic_code)) %>% View


#Some more sanity checks. Did things sum correctly? Pick a couple of test sectors
chk <- bres_summary %>% 
  filter(sic_namefrom_indstrat == 'Manufacture of chemicals and chemical products', DATE == 2023)




#Find moving av AFTER summing
#Should be moving av of those totals
#Get moving av for jobs in BRES for 5 digit, use that latest year...
smoothband = 3

bres_summary <- bres_summary %>% 
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME,sic_namefrom_indstrat) %>%
  mutate(
    jobcount_movingav = rollapply(JOBCOUNT,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(jobcount_movingav))

#check... tick
table(is.na(bres_summary$jobcount_movingav),bres_summary$DATE)



#MAKE SHORTER NAMES THAT COMBINE WITH THE INDSTRAT TOP CAT
#Top level cats first, that we will combine with the shortened sector name
unique(bres_summary$topsector_indstrat)

bres_summary <- bres_summary %>% 
  mutate(
    toplevelindstrat = case_when(
      qg('manufactur', topsector_indstrat) ~ "ADV MANUF",
      qg('business', topsector_indstrat) ~ "PBS",
      qg('financ', topsector_indstrat) ~ "FIN",
      qg('creative', topsector_indstrat) ~ "CREATIVE",
      qg('foundation', topsector_indstrat) ~ "FOUNDATION",
      qg('digital', topsector_indstrat) ~ "DIGITAL",
      qg('life sci', topsector_indstrat) ~ "LIFESCI",
      qg('defence', topsector_indstrat) ~ "DEFENCE"
    )
  )

#And shortern other names
bres_summary <- bres_summary %>% 
  mutate(
    sic_namefrom_indstrat_short = removecommonSICnameelements(bres_summary$sic_namefrom_indstrat, removemanuf = T)
  )

# table(bres_summary$toplevelindstrat)
# table(bres_summary$sic_namefrom_indstrat_short)

#Make combo
bres_summary <- bres_summary %>% 
  mutate(
    sic_namefrom_indstrat_combo = paste0(toplevelindstrat,": ",sic_namefrom_indstrat_short)
  )

table(bres_summary$sic_namefrom_indstrat_combo)


#Have manually added in those specific ind strat codes into the CSV
#Let's see if we can get LQs for those...
islq <- bres_summary %>% 
  group_split(DATE) %>% 
  map(
    add_location_quotient_and_proportions,
    regionvar = GEOGRAPHY_NAME,
    lq_var = sic_namefrom_indstrat_combo,
    valuevar = jobcount_movingav
  ) %>% 
  bind_rows() %>% 
  mutate(jobcount_movingav = round(jobcount_movingav,0))


LQ_slopes <- compute_slope_or_zero(
  data = islq, 
  GEOGRAPHY_NAME, sic_namefrom_indstrat_combo,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = DATE)


#Filter down to a single year... we may want to smooth years, let's see
yeartoplot <- islq %>% filter(DATE == max(DATE))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('GEOGRAPHY_NAME', 'sic_namefrom_indstrat_combo')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- islq %>% 
  group_by(GEOGRAPHY_NAME, sic_namefrom_indstrat_combo) %>% 
  summarise(
    min_LQ_all_time = min(LQ, na.rm = T),
    max_LQ_all_time = max(LQ, na.rm = T)
  ) %>% 
  mutate(
    min_LQ_all_time = ifelse(is.infinite(min_LQ_all_time),NA,min_LQ_all_time),
    max_LQ_all_time = ifelse(is.infinite(max_LQ_all_time),NA,max_LQ_all_time)
  )

# table(is.infinite(minmaxes$min_LQ_all_time))
# table(is.infinite(minmaxes$max_LQ_all_time))

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('GEOGRAPHY_NAME', 'sic_namefrom_indstrat_combo')
  )

#Save all the bits! So we can repeat for different places easily
# saveRDS(islq,'local/islq.rds')
# saveRDS(yeartoplot,'local/is_bres_yeartoplot.rds')



#Right ee ho...
#place = "Sheffield"
place = "Bradford"

sectorLQorder <- islq %>% filter(
  DATE == max(DATE),#use latest data
  GEOGRAPHY_NAME == place) %>% 
  arrange(-LQ) %>% 
  select(sic_namefrom_indstrat_combo) %>% 
  pull()


#Split by frontier or not

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$sic_namefrom_indstrat_combo <- factor(yeartoplot$sic_namefrom_indstrat_combo, levels = sectorLQorder, ordered = T)

#Remove some LQs
yeartoplot.lqfiltered <- yeartoplot %>% filter(LQ > 0 & LQ < 100, jobcount_movingav > 99)

#Drop any sectors that Bradford now doesn't have after that filter
sectorstokeep <- yeartoplot.lqfiltered %>% filter(GEOGRAPHY_NAME == place) %>% select(sic_namefrom_indstrat_combo) %>% pull

yeartoplot.lqfiltered <- yeartoplot.lqfiltered %>% filter(sic_namefrom_indstrat_combo %in% sectorstokeep)

#save a copy of that to save processing time in quarto
saveRDS(yeartoplot.lqfiltered,'local/data/bres_indstrat_LQs.rds')

p <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), alpha = 0.1, shape = 0, sector_name = sic_namefrom_indstrat_combo, LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount_movingav, sector_regional_proportion = sector_regional_proportion,
                            region_name = GEOGRAPHY_NAME,
                            sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = F)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat frontier sector")

p

p <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 0), alpha = 0.1, shape = 0, sector_name = sic_namefrom_indstrat_combo, LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 0), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount_movingav, sector_regional_proportion = sector_regional_proportion,
                            region_name = GEOGRAPHY_NAME,
                            sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = F)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat other sector")

p


#While we're here... compare the job totals for Bradford in these
#To full job totals in BRES as a whole 
#And compare to UK
#What proportion of jobs are ind strat ready? (Even without looking deeper into other sectors)

#THERE'S SOME DOUBLE COUNTING TO REMOVE HERE I THINK
#Where subsectors also have their parent sector included...
#Not many of those but still needs fixing

#For which we need 3 year smoothed BRES job counts
brestots <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_NUTS3_n_ITL321_stacked_2_Fulltimeemployees_2015_2023_SIC_5DIGIT.csv")

#Remove one of them...
#Need to filter NOT year == 2022 NOT GEOGRAPHY_CODE_NUTS2018 is NA, keep the rest
#(2023 we need to keep!)
brestots <- brestots %>%
  filter(!(DATE == 2022 & is.na(GEOGRAPHY_CODE_NUTS2018)))

#Get totals first then smooth
totjobsperITL3 <- brestots %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  summarise(
    total_jobs = sum(JOBCOUNT, na.rm = T)
  )

#Then smooth...
totjobsperITL3 <- totjobsperITL3 %>% 
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    totaljobs_movingav = rollapply(total_jobs,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()


#Same vals for ind-strat-only sectors
indstrat_totjobsperITL3 <- fuzzyjoin::regex_left_join(
  bres,
  strategy_df %>% distinct(sic_code, .keep_all = T),#Some like 20 are doubled up in e.g. foundationals, keep only one
  by = c("SIC_5DIGIT_CODE" = "regex")
) %>% 
  filter(!is.na(sector_name))

#Then just summarise totals in places for each year
indstrat_totjobsperITL3 <- indstrat_totjobsperITL3 %>%
  group_by(DATE,GEOGRAPHY_NAME) %>%
  summarise(
    total_jobs = sum(JOBCOUNT, na.rm = TRUE), .groups = "drop",
    sic_namefrom_indstrat = max(SIC_name),
    is_frontier = max(is_frontier)
  ) 

#Smooth years...
indstrat_totjobsperITL3 <- indstrat_totjobsperITL3 %>%
  arrange(DATE) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(
    totaljobs_movingav = rollapply(total_jobs,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()


#Now can just join on geography name for the appropriate smoothed year to find ratios
both <- totjobsperITL3 %>% 
  filter(DATE == max(DATE)-1) %>% 
  select(-total_jobs) %>% 
  inner_join(
    indstrat_totjobsperITL3 %>% filter(DATE == max(DATE)-1) %>% select(-total_jobs) %>% rename(totINDSTRATjobs_movingav = totaljobs_movingav),
    by = 'GEOGRAPHY_NAME'
  ) %>% 
  mutate(
    percent_indstratjobs = (totINDSTRATjobs_movingav/totaljobs_movingav) * 100
  )

#Err... two places with more than 100%. Right you are.
#OK, Bradford very average!
ggplot(both,aes(x = percent_indstratjobs)) +
  geom_histogram(binwidth = 4) +
  geom_vline(
    xintercept = both %>% filter(GEOGRAPHY_NAME == 'Bradford') %>% select(percent_indstratjobs) %>% pull
  ) +
  coord_cartesian(xlim = c(0,90))


#Oh maybe not average
ecdf(both$percent_indstratjobs)(both %>% filter(GEOGRAPHY_NAME == 'Bradford') %>% select(percent_indstratjobs) %>% pull)







# REPEAT FOR CH----

ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds')
# ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_July2025.rds')
# Sys.time() - x

#get short SIC names
shorts <- readRDS('local/data/bradford_SICs_withshortnames.rds')
#shorts <- readRDS('local/data/bradford_SICs_withshortnames.rds')

ch = ch %>%
  left_join(
    shorts %>% select(SIC_5DIGIT_CODE,SIC_SECTION_NAME_SHORT:SIC_5DIGIT_NAME_SHORT),
    by = 'SIC_5DIGIT_CODE'
  )



#Somewhere I've got timepoint change between last two already done
#Taking from BradfordExplore.R here
#https://github.com/DanOlner/RegionalEconomicTools/blob/a517bdd5b37d3696de080160c68d772921afe4fa/bits_of_code/BradfordExplore.R#L1139
ch.5digitsums <- ch %>% 
  st_set_geometry(NULL) %>% #don't forget this!!
  filter(!is.na(Employees_thisyear) & !is.na(Employees_lastyear)) %>% #Keep only firms with employees in BOTH years even if it's zero
  select(CompanyName,CompanyNumber,accountcode,CompanyCategory,
         incorporationdate_formatted,age_of_firm_years,localauthority_code:ITL221NM,
         Employees_thisyear,Employees_lastyear,SIC_2DIGIT_CODE,SIC_2DIGIT_CODE_NUMERIC,SIC_5DIGIT_CODE,
         SIC_SECTION_NAME_SHORT:SIC_5DIGIT_NAME_SHORT) %>% 
  group_by(SIC_5DIGIT_CODE,localauthority_name) %>% 
  summarise(
    employeecount_thisyear = sum(Employees_thisyear),
    employeecount_lastyear = sum(Employees_lastyear)
  ) %>% ungroup()



#Make those into pseudo dates in an order we can get an LQ size change from
ch.5digitsums.long <- ch.5digitsums %>% 
  pivot_longer(employeecount_thisyear:employeecount_lastyear, names_to = 'timepoint', values_to = 'jobcount') %>% 
  mutate(
    timepoint_numeric = ifelse(timepoint == 'employeecount_lastyear', 1,2)
  )


#Use fuzzy join to add ind strat in again
ch_annotated <- fuzzyjoin::regex_left_join(
  ch.5digitsums.long,
  strategy_df %>% distinct(sic_code, .keep_all = T),#Some like 20 are doubled up in e.g. foundationals, keep only one
  by = c("SIC_5DIGIT_CODE" = "regex")
)



#Then just summarise
ch_summary <- ch_annotated %>%
  filter(!is.na(sic_code)) %>% #remove any non ind strat firms
  group_by(timepoint_numeric,localauthority_name, sic_code) %>%
  summarise(
    total_jobs = sum(jobcount, na.rm = TRUE), .groups = "drop",
    sic_namefrom_indstrat = max(SIC_name),
    topsector_indstrat = max(sector_name),
    # SIC_5DIGIT_CODE = max(SIC_5DIGIT_CODE),
    # SIC_5DIGIT_NAME_SHORT = max(SIC_5DIGIT_NAME_SHORT),#some of these won't be right
    # SIC_2DIGIT_NAME_SHORT = max(SIC_2DIGIT_NAME_SHORT),
    is_frontier = max(is_frontier)
  ) 





ch_summary <- ch_summary %>% 
  mutate(
    toplevelindstrat = case_when(
      qg('manufactur', topsector_indstrat) ~ "ADV MANUF",
      qg('business', topsector_indstrat) ~ "PBS",
      qg('financ', topsector_indstrat) ~ "FIN",
      qg('creative', topsector_indstrat) ~ "CREATIVE",
      qg('foundation', topsector_indstrat) ~ "FOUNDATION",
      qg('digital', topsector_indstrat) ~ "DIGITAL",
      qg('life sci', topsector_indstrat) ~ "LIFESCI",
      qg('defence', topsector_indstrat) ~ "DEFENCE"
    )
  )

#And shortern other names
ch_summary <- ch_summary %>% 
  mutate(
    sic_namefrom_indstrat_short = removecommonSICnameelements(ch_summary$sic_namefrom_indstrat, removemanuf = T)
  )

# table(bres_summary$toplevelindstrat)
# table(bres_summary$sic_namefrom_indstrat_short)

#Make combo
ch_summary <- ch_summary %>% 
  mutate(
    sic_namefrom_indstrat_combo = paste0(toplevelindstrat,": ",sic_namefrom_indstrat_short)
  )

table(ch_summary$sic_namefrom_indstrat_combo)





#Then LQs again!
ch_summary <- ch_summary %>% 
  group_split(timepoint_numeric) %>%
  map(add_location_quotient_and_proportions,
      regionvar = localauthority_name,
      lq_var = sic_namefrom_indstrat_combo,
      valuevar = total_jobs) %>% 
  bind_rows()


#Log vals here should give us rough % change between timepoints...
#TODO: have version to get accurate % change (or can just convert back)

#Get rid of household own use
ch_summary <- ch_summary %>% 
  filter(!qg('household own|membership',sic_namefrom_indstrat_combo))

LQ_slopes <- compute_slope_or_zero(
  data = ch_summary, 
  localauthority_name, sic_namefrom_indstrat_combo,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = timepoint_numeric)


#Filter down to a single year...
#Might want the av of the two timepoints here maybe...
yeartoplot <- ch_summary %>% filter(timepoint_numeric == max(timepoint_numeric))#use latest point

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('localauthority_name', 'sic_namefrom_indstrat_combo')
  )



#Save relevant bits!
saveRDS(ch_summary,'local/ch_summary.rds')
saveRDS(yeartoplot,'local/is_ch_yeartoplot.rds')


place = 'Sheffield'

sectorLQorder <- ch_summary %>% filter(
  localauthority_name == place,
  timepoint_numeric == max(timepoint_numeric)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(sic_namefrom_indstrat_combo) %>% 
  pull()


#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$sic_namefrom_indstrat_combo <- factor(yeartoplot$sic_namefrom_indstrat_combo, levels = sectorLQorder, ordered = T)

#Also keep only 2 digit sectors where Bradford has more than 100 workers recorded in that sector for CH
# morethanx <- yeartoplot %>% 
#   filter(
#     localauthority_name == place,
#     jobcount >= 100
#     ) %>% 
#   select(SIC_2DIGIT_NAME_SHORT) %>% 
#   distinct() %>% 
#   pull
# 
# 
# yeartoplot <- yeartoplot %>% filter(
#   !is.na(SIC_2DIGIT_NAME_SHORT),
#   SIC_2DIGIT_NAME_SHORT %in% as.character(morethanx)
#   )

#Remove NA sector
# yeartoplot <- yeartoplot %>% 
#   filter(!is.na(SIC_2DIGIT_NAME_SHORT))

#Remove some LQs
yeartoplot.lqfiltered <- yeartoplot %>% filter(LQ > 0 & LQ < 100, total_jobs > 99)

#Drop any sectors that Bradford now doesn't have after that filter
sectorstokeep <- yeartoplot.lqfiltered %>% filter(localauthority_name == place) %>% select(sic_namefrom_indstrat_combo) %>% pull
yeartoplot.lqfiltered <- yeartoplot.lqfiltered %>% filter(sic_namefrom_indstrat_combo %in% sectorstokeep)

#save a copy of that to save processing time in quarto
# saveRDS(yeartoplot.lqfiltered,'local/data/companieshouse_indstrat_LQs.rds')




p1 <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), alpha = 0.03, sector_name = sic_namefrom_indstrat_combo, 
                  LQ_column = LQ, change_over_time = slope)

# debugonce(addplacename_to_LQplot)
p1 <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), plot_to_addto = p1, 
                             placename = place, shapenumber = 16,
                             # min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                             value_column = total_jobs, sector_regional_proportion = sector_regional_proportion,
                             region_name = localauthority_name,
                             sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                             value_col_ismoney = F, text = 7)
                             # value_col_ismoney = F, text = 7, maxLQvalmultiplier = 2,useplacenameforminmaxdisplay = T, overridetextpos = 14)

p1 <- p1 + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat frontier sector")

p1


p2 <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 0), alpha = 0.03, sector_name = sic_namefrom_indstrat_combo, 
                  LQ_column = LQ, change_over_time = slope)

# debugonce(addplacename_to_LQplot)
p2 <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 0), plot_to_addto = p2, 
                             placename = place, shapenumber = 16,
                             # min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                             value_column = total_jobs, sector_regional_proportion = sector_regional_proportion,
                             region_name = localauthority_name,
                             sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                             value_col_ismoney = F, text = 7)
                             # value_col_ismoney = F, text = 7, maxLQvalmultiplier = 2,useplacenameforminmaxdisplay = T, overridetextpos = 14)

p2 <- p2 + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat other sector")

p2








# REPEAT INDSTRAT LINK FOR SHEFFIELD AND SOUTH YORKSHIRE----


place = "Sheffield"

#Blue peter this up
#Done here: https://github.com/DanOlner/RegionalEconomicTools/blob/1d5df72210586d0570cd687534bcfb3c98827ece/prepcode/misc_checks.R#L1991
#save a copy of that to save processing time in quarto
yeartoplot.lqfiltered <- readRDS('local/data/bres_indstrat_LQs.rds')



#If I could plot both and space them out, that would be good (could get Bradford change showing too)
p <- LQ_baseplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), alpha = 0.1, shape = 0, sector_name = sic_namefrom_indstrat_combo, LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(is_frontier == 1), plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = jobcount_movingav, sector_regional_proportion = sector_regional_proportion,
                            region_name = GEOGRAPHY_NAME,
                            sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = F)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat frontier sector")

p

#Blue peter this up
#Done here: https://github.com/DanOlner/RegionalEconomicTools/blob/1d5df72210586d0570cd687534bcfb3c98827ece/prepcode/misc_checks.R#L1991
#save a copy of that to save processing time in quarto
yeartoplot.lqfiltered.ch <- readRDS('local/data/companieshouse_indstrat_LQs.rds')

p1 <- LQ_baseplot(df = yeartoplot.lqfiltered.ch %>% filter(is_frontier == 1), alpha = 0.03, sector_name = sic_namefrom_indstrat_combo, 
                  LQ_column = LQ, change_over_time = slope)

# debugonce(addplacename_to_LQplot)
p1 <- addplacename_to_LQplot(df = yeartoplot.lqfiltered.ch %>% filter(is_frontier == 1), plot_to_addto = p1, 
                             placename = place, shapenumber = 16,
                             # min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                             value_column = total_jobs, sector_regional_proportion = sector_regional_proportion,
                             region_name = localauthority_name,
                             sector_name = sic_namefrom_indstrat_combo, change_over_time = slope, LQ_column = LQ,
                             value_col_ismoney = F, text = 7)
# value_col_ismoney = F, text = 7, maxLQvalmultiplier = 2,useplacenameforminmaxdisplay = T, overridetextpos = 14)

p1 <- p1 + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("IndStrat frontier sector")

p1





