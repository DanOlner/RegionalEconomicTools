#productivity analysis based on ONS per hour worked
library(tidyverse)
library(zoo)

#Nope that's by region
# url1 <- 'https://www.ons.gov.uk/file?uri=/economy/economicoutputandproductivity/productivitymeasures/datasets/annualregionallabourproductivity/1998to2023/prodbyregaccessiblefinal.xlsx'
# p1f <- tempfile(fileext=".xlsx")
# download.file(url1, p1f, mode="wb") 

#By ITL2 and 3
url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")



# DO ITL3----

#"Table A5: Chained Volume (unsmoothed) GVA (B) per hour worked indices; ITL2 and ITL3 subregions, 2004 - 2023"
prodITL3 <- readxl::read_excel(path = p1f,range = "A5!A5:W247")#Excluding some ITLs with no data in latest year

names(prodITL3) <- gsub(x = names(prodITL3), pattern = ' ', replacement = '_')

#Tidy
prodITL3 = prodITL3 %>% 
  filter(ITL_level == 'ITL3') %>% 
  pivot_longer(Index_2004:Index_2023, names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
    ) %>% 
  select(-ITL_level)



#Get all slopes for sectors in that place
slopes = get_slope_and_se_safely(
  prodITL3, 
  Region_name,
  y = log(value), 
  x = year, 
  neweywest = T)


#Add 95% confidence intervals around the slope
slopes = slopes %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )


#Mark 4 SY LAs and plot
slopes = slopes %>% 
  ungroup() %>% 
  mutate(
    SY_LAs = ifelse(qg('rotherh|doncaste|sheffie|barnsley', Region_name),'SY LA','Other')
  )

#Adjust to get percentage change per year from log slopes
slopes = slopes %>% 
  mutate(
    across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'),
    slopegroups = as.numeric(cut_number(slope_percentperyear,4))
    )

#Plot the slope with those confidence intervals
ggplot(slopes %>% filter(slopegroups %in% c(1,3,4)), aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope), colour = SY_LAs, size = SY_LAs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1, alpha = 0.5) +
  scale_size_manual(values = c(1,2)) +
  scale_colour_brewer(palette = 'Set1', direction = -1, name = "ITL3") +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('Av. % change per year') +
  ylab("") +
  facet_wrap(~slopegroups, scales = 'free') +
  ggtitle(paste0('ITL3 av. chained volume GVA per hour\n% change per year\n',min(prodITL3$year),'-',max(prodITL3$year),', 95% conf intervals'))
  

# #Adjust to get percentage change per year from log slopes
# slopes = slopes %>% 
#   mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))
# 
# #Plot the slope with those confidence intervals
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   xlab('Av. % change per year') +
#   ylab("")



# DO ITL2----



prodITL2 <- readxl::read_excel(path = p1f,range = "A5!A5:W247")#Excluding some ITLs with no data in latest year

names(prodITL2) <- gsub(x = names(prodITL2), pattern = ' ', replacement = '_')

#Tidy
prodITL2 = prodITL2 %>% 
  filter(ITL_level == 'ITL2') %>% 
  pivot_longer(Index_2004:Index_2023, names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)


#Look at some actual slopes
ggplot(
  prodITL2 %>% filter(
    # year %in% startyear:endyear,
    # Region_name == place,
    qg('south york|manchester', Region_name)
  ),
  aes(x = year, y = value, colour = Region_name)
) +
  geom_line() +
  geom_point() +
  geom_smooth(method = 'lm')




#Get all slopes for sectors in that place
slopes = get_slope_and_se_safely(
  prodITL2, 
  Region_name,
  y = log(value), 
  x = year, 
  neweywest = T)


#Add 95% confidence intervals around the slope
slopes = slopes %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )


#Mark 4 SY LAs and plot
slopes = slopes %>% 
  ungroup() %>% 
  mutate(
    N_MCAs = ifelse(qg('Manch|York|Mersey', Region_name),'N MCA','a-other')
  )

table(slopes$N_MCAs)

#Adjust to get percentage change per year from log slopes
slopes = slopes %>% 
  mutate(
    across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'),
    slopegroups = as.numeric(cut_number(slope_percentperyear,4))
  )

#Plot the slope with those confidence intervals
ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope), colour = N_MCAs, size = N_MCAs)) +
# ggplot(slopes %>% filter(slopegroups %in% c(1,3,4)), aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope), colour = N_MCAs, size = N_MCAs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1, alpha = 0.5) +
  scale_size_manual(values = c(1,2)) +
  scale_colour_brewer(palette = 'Set1', direction = -1, name = "ITL3") +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('Av. % change per year') +
  ylab("") +
  facet_wrap(~slopegroups, scales = 'free') +
  ggtitle(paste0('ITL2 av. chained volume GVA per hour\n% change per year\n',min(prodITL3$year),'-',max(prodITL3$year),', 95% conf intervals'))







# GAPS! ----

#GVA per HOUR WORKED: Separating Sheffield from Barnsley / Doncaster / Rotherham
#Nabbed from https://danolner.github.io/RegionalEconomicTools/gdp_gaps.html

#Get per hour worked for labels
#ONS link to the Excel sheets: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions
#Data folder name of downloaded Excel sheets (from which the CSVs below have been exported)
#itlproductivity.xlsx
perhourworked <- read_csv('data/misc/Table A4 Current Price unsmoothed GVA B per hour worked £ ITL2 and ITL3 subregions 2004 to 2021.csv') %>% 
  rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `2004`:`2021`, names_to = 'year', values_to = 'gva') %>% 
  mutate(year = as.numeric(year))

#Rank to see which ITL2 changed position the most
perhourworked <- perhourworked %>% 
  group_by(year) %>% 
  mutate(rank = rank(gva))

#3 year smoothing
perhourworked <- perhourworked %>% 
  arrange(year) %>% 
  group_by(region) %>%
  mutate(
    movingav = rollapply(gva,3,mean,align='center',fill=NA),
    rank_movingav = rollapply(rank,3,mean,align='center',fill=NA),
    rank_movingav_7yr = rollapply(rank,7,mean,align='center',fill=NA)
  )

#Picking out England and North etc...
#Via https://github.com/DanOlner/regionalGVAbyindustry

#Northern England
north <- perhourworked$region[grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y', perhourworked$region, ignore.case = T)] %>% unique

#South England
south <- perhourworked$region[!grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y|Scot|Highl|Wales|Ireland', perhourworked$region, ignore.case = T)] %>% unique

#South minus London
south.minus.london <- south[!grepl('london',south,ignore.case = T)]

#England!
england <- c(north,south)

#England minus London
england.minus.london <- england[!grepl('london',england,ignore.case = T)]

#UK minus London
uk.minus.london <- perhourworked$region[!grepl('london',perhourworked$region,ignore.case = T)] %>% unique

#Add those regions into the per hour worked data
perhourworked <- perhourworked %>% 
  mutate(ns_england_restofUK = case_when(
    region %in% north ~ 'North England',
    region %in% south ~ 'South Eng (inc. London)',
    .default = 'Scot/Wales/NI'
  ))

perhourworked <- perhourworked %>% 
  mutate(ns_england_restofUK_londonseparate = case_when(
    region %in% north ~ 'North England',
    region %in% south.minus.london ~ 'South Eng (exc. London)',
    grepl('london',region,ignore.case = T) ~ 'London',
    .default = 'Scot/Wales/NI'
  ))

#And a category for 'UK minus London'
perhourworked <- perhourworked %>% 
  mutate(UK_minus_london = case_when(
    grepl('london',region,ignore.case = T) ~ 'London',
    .default = 'UK minus London'
  ))



















#Just use existing format in this code, so redo...

#"Table A4: Current Price (unsmoothed) GVA (B) per hour worked (£); ITL2 and ITL3 subregions, 2004 - 2023"
perhourworked.itl3 <- readxl::read_excel(path = p1f,range = "A4!A5:W247") %>% 
  rename(ITL = ITL_level, ITLcode = ITL_code, region = Region_name) %>% 
  filter(ITL == 'ITL3') %>% 
  pivot_longer(cols = Pounds_2004:Pounds_2023, names_to = 'year', values_to = 'gva') %>% 
  mutate(
    year = substr(year,8,11),
    year = as.numeric(year)
    )



#Add the ITL2 code lookup to ITL3 data
perhourworked.itl3 <- perhourworked.itl3 %>% 
  mutate(
    ITL2code = str_sub(perhourworked.itl3$ITLcode,1,-2)
  )


#Merge in region labels from the ITL2 data
perhourworked.itl3 <- perhourworked.itl3 %>% 
  left_join(
    perhourworked %>% ungroup() %>% distinct(ITLcode, ns_england_restofUK, ns_england_restofUK_londonseparate,UK_minus_london) %>% select(ITL2code = ITLcode, ns_england_restofUK, ns_england_restofUK_londonseparate,UK_minus_london),
    by = 'ITL2code'
  )


corecities <- perhourworked.itl3$region[grepl(x = perhourworked.itl3$region, pattern = 'sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Tyne|Nottingham', ignore.case = T)] %>% unique

#Yes, all in there, need to remove a few...
corecities <- corecities[!grepl(x = corecities, pattern = 'Greater|shire', ignore.case = T)]
corecities <- corecities[order(corecities)]

perhourworked.itl3 <- perhourworked.itl3 %>% 
  mutate(
    core_cities = ifelse(region %in% corecities, 'Core city', 'Other'),
    core_cities_minus_sheffield = ifelse(region %in% corecities[corecities!='Sheffield'],'Core city (exc. Sheffield)','Other')#without sheffield, so not including in weighted average
  )

#Check region match... tick
# table(perhourworked.itl3$region %in% totalhoursperweek.itl3$region)

#Join 
perhourworked.withtotalhours.itl3 <- perhourworked.itl3 %>% 
  ungroup() %>% 
  # select(-c(movingav,rank_movingav,rank_movingav_7yr)) %>% #drop moving averages, redo after
  left_join(
    totalhoursperweek.itl3 %>% select(region,hours_per_week,year),
    by = c('region','year')
  )



#For other groupings...
weightedaverages.perhourworked.itl3.regions <- perhourworked.withtotalhours.itl3 %>% 
  rename(region_grouping = ns_england_restofUK_londonseparate) %>% 
  group_by(region_grouping,year) %>%
  summarise(
    weighted_mean_gva = weighted.mean(gva, hours_per_week, na.rm=T)#get weighted average by each ITL2 grouping
  ) %>% #merge in SY values
  left_join(perhourworked.itl3 %>% filter(region == 'Sheffield') %>% ungroup() %>% select(year, Sheffield_gva = gva), by = c('year')) %>% 
  left_join(perhourworked.itl3 %>% filter(grepl(x = region, pattern = 'Barnsley', ignore.case = T)) %>% ungroup() %>% select(year, BDR_gva = gva), by = c('year')) 
#Repeat for UK minus London
weightedaverages.perhourworked.itl3.UKminusLondon <- perhourworked.withtotalhours.itl3 %>% 
  rename(region_grouping = UK_minus_london) %>% 
  group_by(region_grouping,year) %>%
  summarise(
    weighted_mean_gva = weighted.mean(gva, hours_per_week, na.rm=T)#get weighted average by each ITL2 grouping
  ) %>% #merge in SY values
  left_join(perhourworked.itl3 %>% filter(region == 'Sheffield') %>% ungroup() %>% select(year, Sheffield_gva = gva), by = c('year')) %>% 
  left_join(perhourworked.itl3 %>% filter(grepl(x = region, pattern = 'Barnsley', ignore.case = T)) %>% ungroup() %>% select(year, BDR_gva = gva), by = c('year')) 

#Repeat for core cities exc Sheffield
weightedaverages.perhourworked.itl3.cores <- perhourworked.withtotalhours.itl3 %>% 
  rename(region_grouping = core_cities_minus_sheffield) %>% 
  group_by(region_grouping,year) %>%
  summarise(
    weighted_mean_gva = weighted.mean(gva, hours_per_week, na.rm=T)#get weighted average by each ITL2 grouping
  ) %>% #merge in SY values
  left_join(perhourworked.itl3 %>% filter(region == 'Sheffield') %>% ungroup() %>% select(year, Sheffield_gva = gva), by = c('year')) %>% 
  left_join(perhourworked.itl3 %>% filter(grepl(x = region, pattern = 'Barnsley', ignore.case = T)) %>% ungroup() %>% select(year, BDR_gva = gva), by = c('year')) 


#Append those two
weightedavs.itl3 <- bind_rows(
  weightedaverages.perhourworked.itl3.regions,
  weightedaverages.perhourworked.itl3.UKminusLondon %>% filter(region_grouping!='London'),#London already in .regions
  weightedaverages.perhourworked.itl3.cores %>% filter(region_grouping!='Other')
)

#Make Sheffield and BDR gva long, and find proportion diffs
weightedavs.itl3 <- weightedavs.itl3 %>% 
  pivot_longer(cols = Sheffield_gva:BDR_gva, names_to = 'place', values_to = 'gva') %>% 
  mutate(
    prop_diff = (weighted_mean_gva - gva)/gva
  ) %>% 
  arrange(year) %>% 
  group_by(region_grouping,place) %>% 
  mutate(
    prop_diff_3yrsmooth = rollapply(prop_diff,3,mean,align='center',fill=NA),
    prop_diff_5yrsmooth = rollapply(prop_diff,5,mean,align='center',fill=NA),
  )


#Keep factor order for next plot
weightedavs.itl3$region_grouping <- fct_reorder(weightedavs.itl3$region_grouping, weightedavs.itl3$prop_diff_3yrsmooth, .desc=T)

ggplot(weightedavs.itl3 %>% filter(region_grouping!='London'), aes(x = year, y = prop_diff_3yrsmooth * 100, colour = region_grouping)) +
  # ggplot(weightedavs.itl3 %>% filter(region_grouping!='London'), aes(x = year, y = prop_diff_3yrsmooth * 100, colour = fct_reorder(region_grouping, prop_diff_3yrsmooth, .desc=T))) +
  geom_line() +
  geom_point(size = 2) +
  coord_cartesian(ylim = c(-5,82)) +
  geom_hline(yintercept = 0, alpha = 0.2, size = 2) +
  facet_wrap(~place) +
  scale_color_brewer(palette = 'Paired', direction = -1) +
  ylab("Percent difference of... (3 year average)") +
  theme(legend.title=element_blank()) +
  ggtitle("Sheffield vs Barnsley/Doncaster/Rotherham\nGVA per hour worked: % difference of other regions.")






# ITL3 Hours % change v GVA % change on average----

#So need the number of hours and chained volume GVA
#Hours count is in the per hour productivity sheet
#GVA in the region by industry sheet

#hours worked first
url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

#"Productivity Hours Worked per Week; ITL2 and ITL3 subregions (constrained to ITL1), 2004 - 2023"
hoursworked <- readxl::read_excel(path = p1f,range = "Productivity Hours!A5:W247")

names(hoursworked) <- gsub(x = names(hoursworked), pattern = ' ', replacement = '_')

hoursworked = hoursworked %>%
  filter(ITL_level == 'ITL3') %>% 
  pivot_longer(Hours_2004:Hours_2023, names_to = 'year', values_to = 'hoursworkedperweek') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)



#Then ITL3 CV for all industries (so has to include imputed rent as we can't sum CV)
url2 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p2f <- tempfile(fileext=".xlsx")
download.file(url2, p2f, mode="wb") 

#"Table 3b: ITL3, chained volume measures in 2019 money value, pounds million [note 2]"
itl3.all <- readxl::read_excel(path = p2f, range = "Table 3b!A2:AD11468")

names(itl3.all) <- gsub(x = names(itl3.all), pattern = ' ', replacement = '_')

itl3.all = itl3.all %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl3.all)[length(names(itl3.all))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year)) %>% 
  rename(gva_cv = value)



#Check link between two is valid... tick
table(unique(hoursworked$ITL_code) %in% itl3.all$ITL_code)

#2004:2023
unique(hoursworked$year)[unique(hoursworked$year) %in% unique(itl3.all$year)]

#Get list of corecities
corecities <- getdistinct('sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Tyne|Nottingham', itl3.all$Region_name)

#Yes, all in there, need to remove a few...
corecities <- corecities[!grepl(x = corecities, pattern = 'Greater|shire', ignore.case = T)]
corecities <- corecities[order(corecities)]

#Check match also works in hours worked... tick
table(corecities %in% hoursworked$Region_name)



#Get London ITL3s
itl_lookup = read_csv("data/LAD_(December_2024)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv")

#Get all London ITL2 zones then use to get unique ITL3 zones
itl3.lon = itl_lookup %>%
  filter(qg('london',ITL225NM)) %>% 
  select(ITL325CD,ITL325NM) %>% 
  distinct()

#Check itl3 code match... tick
table(itl3.lon$ITL325CD %in% hoursworked$ITL_code)
table(itl3.lon$ITL325CD %in% itl3.all$ITL_code)
#tick
table(itl3.lon$ITL325NM %in% hoursworked$Region_name)
table(itl3.lon$ITL325NM %in% itl3.all$Region_name)


#Find average slopes and CIs for both of those separately
#For these years
startyear = 2014
endyear = 2023

startyear = 2004
endyear = 2013

#Hours per week first
slopes.hours = get_slope_and_se_safely(
  hoursworked %>% filter(year %in% c(startyear:endyear)), 
  Region_name,
  y = log(hoursworkedperweek), 
  x = year, 
  neweywest = F)

#Add 95% confidence intervals around the slope
slopes.hours = slopes.hours %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )


#Mark 4 SY LAs and plot
slopes.hours = slopes.hours %>% 
  ungroup() %>% 
  # mutate(
  #   SY_LAs = ifelse(qg('rotherh|doncaste|sheffie|barnsley', Region_name),'SY LA','Other'),
  #   corecities = ifelse(Region_name %in% corecities,'core city','Other'),
  #   london = ifelse(qg('london',Region_name), 'London','Other')
  # )
  mutate(
    itl3label = case_when(
      # qg('york', Region_name) ~ 'SY LA',#will overwrite sheffield as core city
      qg('rotherh|doncaste|sheffie|barnsley', Region_name) ~ 'SY LA',#will overwrite sheffield as core city
      Region_name %in% corecities ~ 'Core city',
      Region_name %in% itl3.lon$ITL325NM ~ 'London',
      .default = 'Other'
      ),
    itl3label = factor(itl3label, ordered = T, levels = c('Other','London','Core city','SY LA'))
  )

#Adjust to get percentage change per year from log slopes
slopes.hours = slopes.hours %>% 
  mutate(
    across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'),
    slopegroups = as.numeric(cut_number(slope_percentperyear,4))
  )




#Then GVA CV
slopes.gva = get_slope_and_se_safely(
  itl3.all %>% filter(year %in% c(startyear:endyear)), 
  Region_name,
  y = log(gva_cv), 
  x = year, 
  neweywest = F)

#Add 95% confidence intervals around the slope
slopes.gva = slopes.gva %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )



#Mark 4 SY LAs and plot
slopes.gva = slopes.gva %>% 
  ungroup() %>% 
  mutate(
    itl3label = case_when(
      # qg('york', Region_name) ~ 'SY LA',#will overwrite sheffield as core city
      qg('rotherh|doncaste|sheffie|barnsley', Region_name) ~ 'SY LA',#will overwrite sheffield as core city
      Region_name %in% corecities ~ 'Core city',
      Region_name %in% itl3.lon$ITL325NM ~ 'London',
      .default = 'Other'
    ),
    itl3label = factor(itl3label, ordered = T, levels = c('Other','London','Core city','SY LA'))
  )

#Adjust to get percentage change per year from log slopes
slopes.gva = slopes.gva %>% 
  mutate(
    across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'),
    slopegroups = as.numeric(cut_number(slope_percentperyear,4))
  )


table(slopes.hours$itl3label)
table(slopes.gva$itl3label)


#Keep the bits we want and combine into single df
both <- slopes.gva %>% 
  select(Region_name,itl3label,
         slope_percentperyear_GVA = slope_percentperyear,
         ci95min_percentperyear_GVA = ci95min_percentperyear,
         ci95max_percentperyear_GVA = ci95max_percentperyear) %>% 
  inner_join(
    slopes.hours %>% 
      select(Region_name,
             slope_percentperyear_HOURS = slope_percentperyear,
             ci95min_percentperyear_HOURS = ci95min_percentperyear,
             ci95max_percentperyear_HOURS = ci95max_percentperyear),
    by = 'Region_name'
  )


#PLOT
# ggplot(both, aes(x = slope_percentperyear_HOURS, y = slope_percentperyear_GVA, colour = itl3label, size = itl3label)) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min_percentperyear_HOURS, xmax = ci95max_percentperyear_HOURS),width = 0.1, alpha = 0.3, size = 1) +
#   geom_errorbar(aes(ymin = ci95min_percentperyear_GVA, ymax = ci95max_percentperyear_GVA),width = 0.1, alpha = 0.3, size = 1) +
#   geom_abline(slope = 1, intercept = 0, colour = 'black', alpha = 0.5, size = 2) +
#   geom_hline(yintercept = 0, colour = 'black', alpha = 0.3, size = 1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.3, size = 1) +
#   scale_color_brewer(palette = 'Set1', direction = 1) +
#   scale_size_manual(values = c(2,1,4)) +
#   coord_fixed()
  # xlab('Av. % change per year') +
  # ylab("") +
  # ggtitle('All industries in ITL2 zones, chained volume GVA\nAv. % change per year\n2014-2023, 95% conf intervals')


#Poor control over visibility. Layer instead. Base plot then function.
addtogva_hours_plot = function(p,plotdata, showerrors = T, sizeval = 1, colourval = 'black', alphaval = 1){
  
  p = p + geom_point(data = plotdata, aes(x = slope_percentperyear_HOURS, y = slope_percentperyear_GVA, group = Region_name), colour = colourval, size = sizeval, alpha = alphaval) 
  
  if(showerrors){
    
    p = p + geom_errorbar(data = plotdata, aes(x = slope_percentperyear_HOURS, y = slope_percentperyear_GVA, xmin = ci95min_percentperyear_HOURS, xmax = ci95max_percentperyear_HOURS),width = 0.1, size = 1, colour = colourval, alpha = alphaval) +
    geom_errorbar(data = plotdata, aes(x = slope_percentperyear_HOURS, y = slope_percentperyear_GVA, ymin = ci95min_percentperyear_GVA, ymax = ci95max_percentperyear_GVA),width = 0.1, size = 1, colour = colourval, alpha = alphaval) 
    
  }
  
    p  + geom_abline(slope = 1, intercept = 0, colour = 'black', alpha = 0.5, size = 2) +
      geom_hline(yintercept = 0, colour = 'black', alpha = 0.3, size = 1) +
      geom_vline(xintercept = 0, colour = 'black', alpha = 0.3, size = 1)
  
}




p = ggplot()

p = addtogva_hours_plot(p, both %>% filter(itl3label == 'Other'), F, 1, 'black', 0.4)
p = addtogva_hours_plot(p, both %>% filter(itl3label == 'London'), F, 2, 'red', 0.4)
p = addtogva_hours_plot(p, both %>% filter(itl3label == 'Core city'), T, 3, RColorBrewer::brewer.pal(3, "Set1")[2], 0.5)
p = addtogva_hours_plot(p, both %>% filter(itl3label == 'SY LA'), T, 7, RColorBrewer::brewer.pal(3, "Set1")[3], 1)

p = p + coord_fixed()

ggplotly(p, tooltip = 'Region_name')

