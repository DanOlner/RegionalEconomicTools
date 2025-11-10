# YORKSHIRE AND HUMBER SECTORS----
# Data prep / checks / explore for Nov 2025 Y&H sector/cluster analysis
library(tidyverse)
library(plotly)
library(nomisr)
library(sf)
library(zoo)
library(stringr)
library(RColorBrewer)
source('functions/misc_functions.R')
source('functions/adhoc_functions.R')
source('functions/data_process_functions.R')

options(scipen = 99)


# STRUCTURAL CHANGE VIA GVA----

## 2 digit GVA prep at ITL1 level----

# Using ITL1 level might allow maximising the sector category list. Let's look.
# We may want to drop this into the easy squeezy versions, though those might be machine readable soon anyway
#Latest release here:
#https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry

#Workaround for lack of URL download native in readxl package
#Via https://stackoverflow.com/a/79311678/5023561
#2024: url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx'
#2025:
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

#Table 1c is current prices with ITL1 zones
gva.2digit <- readxl::read_excel(path = p1f,range = "Table 1c!A2:AD1730") 

#More process-able names with no spaces
names(gva.2digit) <- gsub(x = names(gva.2digit), pattern = ' ', replacement = '_')

#WARNING: ONLY CORRECT LIST TO REMOVE FOR ITL1 (only one change from ITL2 though, E (36-39))
#SICs to remove to leave just unique SIC values
#Still works for 2025 as well as 2024 to leave correct highest res SICs
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'B (5-9)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'E (36-39)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

gva.2digit <- gva.2digit %>% 
  filter(
    !SIC07_code %in% SICremoves,
    !qg('united kingdom|england|extra',Region_name)
    ) 

# Looking OK
unique(gva.2digit$SIC07_code)
unique(gva.2digit$Region_name)

# Enlongen
gva.2digit <- gva.2digit %>%  
  pivot_longer(`1998`:names(gva.2digit)[length(names(gva.2digit))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# Tick
# gva.2digit %>% filter(qg('yorkshire', Region_name)) %>% View

# Add smoothed vals
smoothband = 3

gva.2digit = gva.2digit %>% 
  arrange(year) %>% 
    group_by(Region_name,SIC07_description) %>% 
    mutate(
      gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
    ) %>% 
    ungroup()




## 2 digit proportion plots----

# Oh it would appear the function does it for me
# Well done past me!
place = 'Yorkshire and The Humber'

p <- twod_proportionplot(
  df = gva.2digit,
  x_regionnames = place, 
  y_regionnames = unique(gva.2digit$Region_name[gva.2digit$Region_name != place]),
  regionvar = Region_name,
  category_var = SIC07_description, 
  valuevar = gva_movingav, 
  timevar = year, 
  start_time = 2007, end_time = 2022,
  # start_time = 1999, end_time = 2006,
  # compasspoints_to_display = c('SW','NW')
  compasspoints_to_display = c('SE','NE')
)

#add some extras
p <- p + 
  xlab(paste0(place, ' GVA proportion')) +
  ylab(paste0('UK GVA proportion (MINUS ',place,')')) +
  coord_fixed(xlim = c(0.1,12), ylim = c(0.1,12)) +  # good for log scale
  scale_y_log10() +
  scale_x_log10()

p



# Repeat for SIC sections----

# I think 2 digit may be a little messy, though the patterns intriguing
#Table 1c is current prices with ITL1 zones
gva.sections <- readxl::read_excel(path = p1f,range = "Table 1c!A2:AD1730") 

#More process-able names with no spaces
names(gva.sections) <- gsub(x = names(gva.sections), pattern = ' ', replacement = '_')

#Keep SIC sections
#This gets all the letters, saves having to manually filter
SIC_sections <- gva.sections$SIC07_code[substr(gva.sections$SIC07_code,2,2) == ' '] %>% unique


#TWO VERSIONS - ONE THAT KEEPS IMPUTED RENT, ONE THAT REMOVES
#To do the latter for SIC sections, just need to replace the SIC section that includes it with the lower level SIC that does not
#This is also then the quickest way to remove imputed rent from the national total and regional totals
#(Reminder - which we can only do using 'current prices' because only those can be re-summed, unlike chained volume measures)

#REPLACE "L (68)" REAL ESTATE ACTIVITIES (WHICH INCLUDES IMPUTED RENT) WITH JUST 68 "Real estate activities, excluding imputed rental"  
#NOTE: "68" WILL NEED REPLACING AGAIN WITH L (68) AFTER FOR LATER MATCHES, BUT PUT EXC IMPUTED RENT NOTE IN
SIC_sections_minusImputedRent = SIC_sections
SIC_sections_minusImputedRent[SIC_sections_minusImputedRent == 'L (68)'] <- '68'

gva.sections <- gva.sections %>% 
  filter(
    SIC07_code %in% SIC_sections,
    !qg('united kingdom|england|extra',Region_name)
  ) 

# Looking OK
unique(gva.sections$SIC07_code)
unique(gva.sections$Region_name)


# Enshorten names
shortsectionnames = reduceSICnames(unique(gva.sections$SIC07_description),'section')
lookup = data.frame(SIC07_description = unique(gva.sections$SIC07_description), shortnames = shortsectionnames)

gva.sections = gva.sections %>% 
  left_join(
    lookup, by = 'SIC07_description'
  ) %>% 
  select(-SIC07_description) %>% 
  rename(SIC07_description = shortnames) %>% 
  relocate(SIC07_description, .after = SIC07_code)

# Enlongen whole thing
gva.sections <- gva.sections %>%  
  pivot_longer(`1998`:names(gva.sections)[length(names(gva.sections))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

# Tick
# gva.sections %>% filter(qg('yorkshire', Region_name)) %>% View

# Add smoothed vals
smoothband = 3

gva.sections = gva.sections %>% 
  arrange(year) %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()


## Some checks----

# What proportion of whole economy is London?
london.chk = gva.sections %>% 
  mutate(is_london = ifelse(qg('London',Region_name), 'London','the_rest')) %>% 
  # filter(!qg('real est|finance',SIC07_description)) %>% #Test with these two removed...
  group_by(year,is_london) %>% 
  summarise(totalgva= sum(value)) %>% 
  pivot_wider(names_from = is_london, values_from = totalgva) %>% 
  summarise(london_percent = (London/(London + the_rest)) * 100)

# From about a fifth to about a quarter of whole economy
# Wonder which sectors responsible? A rabbit hole for another time
ggplot(london.chk, aes(x =year, y = london_percent)) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0,100))







## SIC section proportion plots----

# Oh it would appear the function does it for me
# Well done past me!
place = 'Yorkshire and The Humber'

p <- twod_proportionplot(
  # df = gva.sections %>% filter(!qg('london',Region_name)),# Version with London removed
  df = gva.sections,
  x_regionnames = place, 
  y_regionnames = unique(gva.sections$Region_name[gva.sections$Region_name != place]),
  regionvar = Region_name,
  category_var = SIC07_description, 
  valuevar = gva_movingav, 
  timevar = year, 
  # start_time = 2007, end_time = 2022,
  # start_time = 1999, end_time = 2007,
  start_time = 1999, end_time = 2022
  # compasspoints_to_display = c('SW','NW')
  # compasspoints_to_display = c('SE','NE')
)

#add some extras
p <- p + 
  xlab(paste0(place, ' GVA proportion')) +
  ylab(paste0('UK GVA proportion (MINUS ',place,')')) 
  # coord_fixed(xlim = c(0.1,12), ylim = c(0.1,12)) +  # good for log scale
  # scale_y_log10() +
  # scale_x_log10()

p



## Wiggly prop plot----

# We need region values pre-calculated...
# debugonce(get_tworegions_proportions)
regionvals = get_tworegions_proportions(
  df = gva.sections %>% filter(!is.na(gva_movingav)),
  # df = gva.sections %>% filter(!is.na(gva_movingav), !qg('london',Region_name)),#Version with London removed
  regionvar = Region_name,
  category_var = SIC07_description,
  valuevar = gva_movingav,
  timevar = year,
  x_regionnames = place,
  y_regionnames = unique(gva.sections$Region_name[gva.sections$Region_name != place])
) %>% 
  mutate(
    x_sector_total_percent = x_sector_total_proportion * 100,
    y_sector_total_percent = y_sector_total_proportion * 100,
    `Y&H%` = x_sector_total_percent
    )


# Find sectors with average regional percent below a certain value
# And remove those sectors entirely from the df so they don't display
dropthesesectors = regionvals %>% 
  group_by(SIC07_description) %>% 
  summarise(meanpercent = mean(x_sector_total_percent)) %>% 
  filter(meanpercent < 3) %>% 
  pull(SIC07_description)


# debugonce(twod_generictimeplot_multipletimepoints)
p <- twod_generictimeplot_multipletimepoints(
  # df = regionvals %>% filter(SIC07_description == 'ICT'),
  df = regionvals %>% filter(!SIC07_description %in% dropthesesectors),
  category_var = SIC07_description,
  x_var = x_sector_total_percent,
  y_var = y_sector_total_percent,
  label_var = `Y&H%`,
  timevar = year,
  # compasspoints_to_display = c('NE','SE'),
  # compasspoints_to_display = c('NW','SW'),
  times = c(1999:2022) #3 yr moving av endpoints
  # times = c(seq(1999,2022,3),2022)
  # times = c(1999,2003,2008,2012,2016,2020,2022) #3 yr moving av endpoints
  # times = c(2000:2021) #5 yr moving av endpoints
)

p + coord_fixed() +
# p + coord_fixed(xlim = c(3,21), ylim = c(3,17)) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, size = 3) + 
  # scale_x_log10() +
  # scale_y_log10() +
  xlab(paste0('Yorkshire & Humber')) +
  ylab(paste0('UK minus Y&H'))






## Then do LQ plots for Y&H verses other ITL1s for 2 digit sectors----

# For a bit more GVA detail of the Y&H comparison

# LQ plot prep... I should really function this stuff up
islq <- gva.2digit %>% 
  filter(
    !is.na(gva_movingav),
    !qg('London',Region_name)#Remove London
    # year < 2008 #examine some subsets of time
    ) %>% 
  group_split(year) %>% 
  map(
    add_location_quotient_and_proportions,
    regionvar = Region_name,
    lq_var = SIC07_description,
    valuevar = gva_movingav
  ) %>% 
  bind_rows() %>% 
  mutate(gva_movingav = round(gva_movingav,0))

# Some checks
# islq %>% filter(qg('creative',SIC07_description),year == 2022) %>% View


LQ_slopes <- compute_slope_or_zero(
  data = islq, 
  Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year)


#Filter down to a single year... we may want to smooth years, let's see
yeartoplot <- islq %>% filter(year == max(year))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('Region_name', 'SIC07_description')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- islq %>% 
  group_by(Region_name, SIC07_description) %>% 
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
    by = c('Region_name', 'SIC07_description')
  )

# Join other SIC levels to it so we can break down into production / other
# I think I have a lookup for that, though that will need a tweak due to one extra sector in ITL1

# Have just updated regionalGVA_siccode_lookupmaker.R for ITL1 as well...
# (Forgot I had that!)
gvalookup_itl1 = read_csv('data/siclookup_forregionalGVAcategories_ITL1.csv')

# Confirm... tick
# table(unique(gvalookup_itl1$SIC07_code) %in% unique(islq$SIC07_code))
yeartoplot <- yeartoplot %>%
  left_join(
    gvalookup_itl1 %>% select(-SIC07_description),
    by = 'SIC07_code'
  )




place = "Yorkshire and The Humber"

sectorLQorder <- islq %>% filter(
  year == max(year),#use latest data
  Region_name == place) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)

#Remove some LQs
yeartoplot.lqfiltered <- yeartoplot %>% filter(LQ > 0 & LQ < 100)

#Drop any sectors that Bradford now doesn't have after that filter
sectorstokeep <- yeartoplot.lqfiltered %>% filter(Region_name == place) %>% select(SIC07_description) %>% pull

yeartoplot.lqfiltered <- yeartoplot.lqfiltered %>% filter(SIC07_description %in% sectorstokeep)

# Make base layer just of labelled core cities
# Remove sheffield also, don't want that labelled
itl1_baselayer = yeartoplot.lqfiltered %>% filter(Region_name != place)

# Add in placename abbreviations
itl1_baselayer = itl1_baselayer %>% 
  mutate(
    placename_short = case_when(
      qg('north east',Region_name) ~ 'NE',
      qg('north west',Region_name) ~ 'NW',
      qg('east mid',Region_name) ~ 'EM',
      qg('west mid',Region_name) ~ 'WM',
      qg('south east',Region_name) ~ 'SE',
      qg('south west',Region_name) ~ 'SW',
      qg('northern ire',Region_name) ~ 'NI',
      .default = str_sub(Region_name,1,2)#Only London, Scotland, Wales 
    )
    # placename_short = str_sub(Region_name,1,2),
    # placename_short = ifelse(placename_short == 'Ci','Ed',placename_short)#Edinburgh
  )

# Check
# itl1_baselayer %>% select(Region_name,placename_short) %>% distinct() %>% View

# grouptodisplay = 'Services sector'
grouptodisplay = c('Production sector','Construction')

# debugonce(LQ_baseplot)
p <- LQ_baseplot(df = itl1_baselayer %>% filter(SIC07_description_three %in% grouptodisplay), alpha = 0.8, shape = 0, sector_name = SIC07_description, LQ_column = LQ, change_over_time = slope, labelcolumn = placename_short)

p <- addplacename_to_LQplot(df = yeartoplot.lqfiltered %>% filter(SIC07_description_three %in% grouptodisplay), plot_to_addto = p, maxLQvalmultiplier = 1,
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = gva_movingav, sector_regional_proportion = sector_regional_proportion,
                            region_name = Region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ,
                            text = 7, value_col_ismoney = T)

p <- p + 
  # coord_cartesian(xlim = c(0.1,7)) +
  ggtitle("Y&H vs other ITL1s for 2 digit GVA")

p



## SERVICES V PRODUCTION / GOODS----

# Working with code from https://danolner.github.io/RegionalEconomicTools/intro_gvajobsdata_in_R.html

gvalookup_itl1 = read_csv('data/siclookup_forregionalGVAcategories_ITL1.csv')

# Check match to sections... tick
table(unique(gva.sections$SIC07_code) %in% unique(gva.gs$SIC07_code))

# Put into goods/services bins and getting moving average
# gva.gs.smoothed = gva.sections %>% 
#   # filter(qg('financ|imputed'))
#   left_join(
#     gvalookup_itl1 %>% select(-c(SIC07_code,SIC07_description)) %>% distinct() %>% rename(SIC07_code = SIC07_code_sections),
#     by = 'SIC07_code'
#   ) %>% 
#   mutate(
#     goodsservices = ifelse(SIC07_description_three == 'Services sector', 'Services','Prod/Constr')
#   ) %>% 
#   group_by(
#     year,Region_name,goodsservices
#   ) %>% 
#   summarise(
#     gva = sum(value)
#   ) %>% 
#   arrange(year) %>% 
#   group_by(Region_name,goodsservices) %>% 
#   mutate(
#     gva_movingav = rollapply(gva,1,mean,align='center',fill=NA)
#     # gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA)
#   ) %>% 
#   ungroup() %>% 
#   filter(!is.na(gva_movingav)) 


# Use 2 digit so we can filter out finance and imputed rent to see what difference those make
# Actually, it does make sense to remove imputed rent, think I can argue for that...
# Also, it's better not smoothed.
gva.gs.smoothed = gva.2digit %>% 
  filter(!qg("Owner-occupiers' imputed rental", SIC07_description)) %>% 
  # filter(!qg("financ|Owner-occupiers' imputed rental", SIC07_description)) %>% 
  left_join(
    gvalookup_itl1 %>% select(-SIC07_description),
    by = 'SIC07_code'
  ) %>% 
  mutate(
    goodsservices = ifelse(SIC07_description_three == 'Services sector', 'Services','Prod/Constr')
  ) %>% 
  group_by(
    year,Region_name,goodsservices
  ) %>% 
  summarise(
    gva = sum(value)
  ) %>% 
  arrange(year) %>% 
  group_by(Region_name,goodsservices) %>% 
  mutate(
    gva_movingav = rollapply(gva,1,mean,align='center',fill=NA)
    # gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(gva_movingav)) 


# Get proportion and LQ
gva.gs.smoothed = gva.gs.smoothed %>% 
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = goodsservices,
      valuevar = gva_movingav) %>% 
  bind_rows() %>% 
  mutate(sector_regional_percent = sector_regional_proportion * 100)


# Order by most recent year's data
place_order <- gva.gs.smoothed %>% filter(
  qg('services',goodsservices),
  year == max(year)#use latest data
) %>% 
  arrange(-sector_regional_percent) %>% 
  pull(Region_name)

gva.gs.smoothed$Region_name <- factor(gva.gs.smoothed$Region_name, levels = place_order, ordered = T)


# Plot change over time - 
# Pick one of the two
ggplot(
  gva.gs.smoothed %>% 
    filter(qg('services',goodsservices)) %>% 
    mutate(is_ynh = Region_name == 'Yorkshire and The Humber'), 
  aes(x = year, y = sector_regional_percent, colour = Region_name, size = is_ynh)) +
  scale_size_manual(values = c(0.5,2)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  theme(legend.title = element_blank()) +
  guides(size = F) +
  ylab('Percent service sectors') 



# In theory, goods v services LQ should help unpick that story...
ggplot(
  gva.gs.smoothed %>% 
    # filter(qg('services',goodsservices)) %>% 
    mutate(is_ynh = Region_name == 'Yorkshire and The Humber'), 
  aes(x = year, y = LQ, colour = Region_name, size = is_ynh)) +
  scale_size_manual(values = c(0.5,2)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  theme(legend.title = element_blank()) +
  guides(size = F) +
  ylab('Percent service sectors') +
  scale_y_log10() +
  geom_hline(yintercept = 1) +
  facet_wrap(~goodsservices, scales = 'free_y')

# This is intriguing
# It looks like:
# (a) services are becoming more evenly spread across the country
# (you should see services LQ SD lower over time as it converges to the mean)
# (b) production has continued to relatively concentrate everywhere else except London
# Suggesting what?

# Check SDs
gva.gs.smoothed %>% 
  group_by(year,goodsservices) %>% 
  summarise(
    lqsd = sd(LQ)
  ) %>% 
  ggplot(
    aes(x = year, y = lqsd, colour = goodsservices)
  ) + geom_line()


# Let's wigglyplot this up
# debugonce(twod_generictimeplot_multipletimepoints)
# p <- twod_generictimeplot_multipletimepoints(
#   # df = regionvals %>% filter(SIC07_description == 'ICT'),
#   df = gva.gs.smoothed %>% 
#     select(year,Region_name,sector_regional_percent,goodsservices) %>% 
#     pivot_wider(names_from = goodsservices, values_from = sector_regional_percent) %>% 
#     rename(Prod = `Prod/Constr`) %>% 
#     mutate(prodforlabels = Prod),#Needs to be different col or it has a hissy fit
#   category_var = Region_name,
#   x_var = Prod,
#   y_var = Services,
#   label_var = prodforlabels,
#   timevar = year,
#   # compasspoints_to_display = c('NE','SE'),
#   # compasspoints_to_display = c('NW','SW'),
#   times = c(1998:2023) #3 yr moving av endpoints
#   # times = c(seq(1999,2022,3),2022)
#   # times = c(1999,2003,2008,2012,2016,2020,2022) #3 yr moving av endpoints
#   # times = c(2000:2021) #5 yr moving av endpoints
# )
# 
# p + coord_fixed() +
#   # p + coord_fixed(xlim = c(3,21), ylim = c(3,17)) +
#   geom_abline(slope = 1, intercept = 0, alpha = 0.5, size = 3) 



# LINKING BRES JOBS + GVA----

# Can we use 2024 BRES at local authority level?
# Do some of those not match with ITL3 borders?
# For those that don't, can we tweak?

# Get latest BRES for LAs
# bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE423_localauthoritiescountyunitaryasofApril2023_2_Fulltimeemployees_2015_2024_SIC_5DIGIT.csv")


# district/unitary - should hopefully match ITL3 and LA borders below better
bres <- read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE432_localauthoritiesdistrictunitaryasofApril2021_2_Fulltimeemployees_2015_2024_SIC_2DIGIT.csv")

# 2024 local authority geographies
# la.2024 = st_read("~/Library/CloudStorage/Dropbox/MapPolygons/UK/2024/Local_Authority_Districts_May_2024_Boundaries_UK_BFC/LAD_MAY_2024_UK_BFC.shp")

# 2021 local authority geographies
la.2021 = st_read("~/Library/CloudStorage/Dropbox/MapPolygons/UK/2021/Local_Authority_Districts_December_2021_UK_BGC_2022_-6651079422179559093.geojson")

# All but one doesn't match... and that's a spelling error
# Rest are none GB
# "Rhondda Cynon Taff" in BRES should be "Rhondda Cynon Taf"
# Once that's fixed...
bres = bres %>% 
  mutate(
    GEOGRAPHY_NAME = if_else(GEOGRAPHY_NAME == "Rhondda Cynon Taff", "Rhondda Cynon Taf", GEOGRAPHY_NAME)
  )

table(unique(bres$GEOGRAPHY_NAME) %in% la.2021$LAD21NM)
unique(bres$GEOGRAPHY_NAME)[!unique(bres$GEOGRAPHY_NAME) %in% la.2021$LAD21NM]
unique(la.2021$LAD21NM)[!unique(la.2021$LAD21NM) %in% bres$GEOGRAPHY_NAME]



# The issue: ITL3 from 2021 – which BRES uses – doesn’t have e.g. SY 4 places. ITL3 2025 does, for GVA data. 
# So to try and match ITL3 2025 GVA to BRES…? Let us try BRES LA data! (District / unitary, not county / unitary… Jesus.)

# Get ITL3 2025 to check match against LAs from BRES
itl3.2025 = st_read("~/Library/CloudStorage/Dropbox/MapPolygons/UK/2025/International_Territorial_Level_3_(January_2025)_Boundaries_UK_BGC_V2.geojson")

# Names may not match but geography might still be same...
table(unique(itl3.2025$ITL325NM) %in% unique(bres$GEOGRAPHY_NAME))


# From a quick QGIS stare at ITL3 2025 and LA 2021, looks like the latter nests into the former OK 
# But let's check fully
overlap = intersect_makelookup(larger_zone = itl3.2025, smaller_zone = la.2021, vartogroupby_fromsmallerzone = LAD21NM)

# Ah there's a single one that isn't clearly nested - 
overlap %>% filter(area_percent < 99) %>% pull(LAD21NM,area_percent)

# Err where?
plot(st_geometry(la.2021))
plot(st_geometry(la.2021 %>% filter(qg('north ayr',LAD21NM))), colour = 'red', add = T)
plot(st_geometry(la.2021 %>% filter(qg('north ayr',LAD21NM))))

# Looking again in QGIS, it's Arran that's at issue:
# Assigned to different geographies in ITL32025 and LA 2021

# The rest of LA2021 can be nested into ITL3 2025 (and so summed up correctly)
# Quick solution: drop the offending bits of Scotland.
# Tricky to lose some rural comparisons for bits of YNY but...

# It may well be there are other geographies we can piece it togethe from
# But can come back to that

# Which ones to drop from each?
# Think it has to be the one LA 2021 zone and the two ITL3s it overlaps
# LA zone: North Ayrshire S12000021
# IT3 2025 two zones: 
# TLM20 Highlands and Islands
# TLM93 North Ayrshire and East Ayrshire

# Then can aggregate job counts to match to rest of ITL3 2025 values



## LINK BRES 2024 AND ITL3 2023----

# Given the above. Two point five stages:
# 1. Drop those troublesome zones, keep the ones we can work with
# 2. Aggregrate BRES values to ITL3 2025 minus those zones
# 3. Get ITL3 2025 SIC code lookup to aggregrate by SIC

# JUST summarise BRES to the correct geogs and SICs
# Can then separately link to either current prices or chained volume

# All SIC 2 digit values

# Work with these

# Fix spelling error to match LAs (which we need to match LA / ITL3 lookup, see above)
bres = read_csv("local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE432_localauthoritiesdistrictunitaryasofApril2021_2_Fulltimeemployees_2015_2024_SIC_2DIGIT.csv") %>% mutate(
  GEOGRAPHY_NAME = if_else(GEOGRAPHY_NAME == "Rhondda Cynon Taff", "Rhondda Cynon Taf", GEOGRAPHY_NAME)
)

# Just want geogs and SICs, CV or CP is irrelevant at this point
itl3 = read_csv("data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv")

# Drop one geog from BRES, 2 from ITL3
bres = bres %>% filter(!qg('north ayr', GEOGRAPHY_NAME))

itl3 = itl3 %>% 
  filter(
    !qg('TLM20|TLM93', ITL_code)
  )


# Attach lookup via intersect_makelookup above
bres = bres %>% 
  left_join(
    overlap %>%
      st_set_geometry(NULL) %>% 
      select(ITL325CD,ITL325NM,GEOGRAPHY_CODE = LAD21CD),
    by = 'GEOGRAPHY_CODE'
  )

# That should give us the ITL3 groups to count by...
length(unique(bres$GEOGRAPHY_NAME))
length(unique(bres$ITL325NM))
length(unique(itl3$Region_name))#This is UK, BRES is GB. Confirming...
unique(itl3$Region_name)[!unique(itl3$Region_name) %in% unique(bres$ITL325NM)]# Tick


# Count up by ITL3 (and sector and date, to keep the same)
bres = bres %>% 
  group_by(DATE,SIC_2DIGIT_CODE,ITL325NM) %>% 
  summarise(
    JOBCOUNT = sum(JOBCOUNT),
    ITL325CD = max(ITL325CD)#Will be unique code per group
    )


# Now to summarise by GVA-bespoke-SIC for ITL3 level
# gvalookup_itl3 = read_csv('data/siclookup_forregionalGVAcategories_ITL3.csv')

# Ah no not that. Code nabbed from combine_regGVA_and_BRES.R
#Nice tidy function to create the correct lookup! 
sics.forBRESjoin <- make.GVA.SICs.long(itl3)

bres <- bres %>% 
  mutate(
    SIC_2DIGIT_CODE_numeric = as.numeric(SIC_2DIGIT_CODE)
  ) %>% 
  left_join(sics.forBRESjoin, by = c('SIC_2DIGIT_CODE_numeric' = 'SIC07_code_numeric')
            )

# Take a look at the link result... looking OK
bres %>% 
  ungroup() %>% 
  select(SIC_2DIGIT_CODE,SIC_2DIGIT_CODE_numeric,SIC07_code_fromGVAdata,SIC07_description) %>% 
  distinct() %>% 
  View

# Non matching code 99 is extraterr - not in BRES, no jobs
table(unique(bres$SIC07_code_fromGVAdata) %in% unique(itl3$SIC07_code))
unique(bres$SIC07_code_fromGVAdata)[!unique(bres$SIC07_code_fromGVAdata) %in% unique(itl3$SIC07_code)]


# OK - we can now sum BRES by GVA 2025 SIC code for ITL3 level
bres.at.itl3 = bres %>% 
  group_by(DATE,ITL325NM,SIC07_code_fromGVAdata) %>%
  summarise(
    JOBCOUNT = sum(JOBCOUNT),
    SIC07_description = max(SIC07_description),#unique value, keep
    SIC_numeric = max(SIC_2DIGIT_CODE_numeric)#Keep to order by
  ) %>% 
  relocate(SIC07_description, .after = SIC07_code_fromGVAdata) %>% 
  ungroup() %>% 
  arrange(SIC_numeric) %>% 
  select(-c(SIC_numeric)) %>% 
  filter(!is.na(SIC07_code_fromGVAdata))
  
# Phew
unique(bres.at.itl3$SIC07_code_fromGVAdata)

# Saaave
saveRDS(bres.at.itl3, 'local/data/BRES2024_linkedtoITL3_2025_GVAsiccodes.rds')



## Add in GVA data, get GVA per FT and plot----

# Use chained volume to get actual values - each 2 digit is separate, so is fine
itl3 = read_csv("data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_2DIGIT_LONG_2023.csv")

# Bespoke for-GVA-SICs-ITL3 bres data from above
bres = readRDS('local/data/BRES2024_linkedtoITL3_2025_GVAsiccodes.rds')

# Check match... tick (not other way round, BRES is GB only)
table(unique(bres$ITL325NM) %in% itl3$Region_name)
# Double tick
table(unique(bres$SIC07_code_fromGVAdata) %in% itl3$SIC07_code)


# Link
# Inner join so we only keep common years (2015 to 2023)
bres.gva = bres %>% 
  rename(SIC07_code = SIC07_code_fromGVAdata, year = DATE, Region_name = ITL325NM) %>% 
  inner_join(
    itl3 %>% select(-SIC07_description) %>% rename(gva = value),
    by = c('year','Region_name','SIC07_code')
  )

# Check...
# table(!is.na(bres.gva$JOBCOUNT))
# table(!is.na(bres.gva$gva))

# bres.gva %>% filter(is.na(gva)) %>% View

# Huh. In the original sheet, agri in Southampton has [u], meaning 'too low for disclosure' I think
# 'Too low', I can safely set to zero. Kind of odd, though, that's it's zeros elsewhere...
bres.gva = bres.gva %>% mutate(gva = ifelse(is.na(gva), 0, gva))

saveRDS(bres.gva,'local/data/BRES2024_linkedtoITL3_2025_GVAsiccodes_withgvaattached.rds')

# Nabbing code from Bradford output
# This should be same list...?
shortsectornames = read_csv('data/shortsectornames_for_regionalGVA_2digitSICs.csv')

bres.gva <- bres.gva %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )%>% 
  filter(!qg('households|membership', SIC07_description_shortened))

# Add in values we need
smoothband = 3

bres.gva = bres.gva %>% 
  mutate(
    gvaperjob = gva/JOBCOUNT
  ) %>% 
  arrange(year) %>% 
  group_by(Region_name,SIC07_description_shortened) %>%
  mutate(
    gvaperjob_movingav = rollapply(gvaperjob * 1000,smoothband,mean,align='center',fill=NA)
  ) %>% ungroup()





#CHAINED VOL
#Break into n groups, using factor order created in previous line
prod.data <- bres.gva %>% 
  mutate(
    year = year - 2000,#Make dates 2 digit, more readable on axis
    SIC07_description_shortened = fct_reorder(SIC07_description_shortened, gvaperjob_movingav, .desc = T),
    # highlow = ifelse(SIC07_description_shortened %in% levels(SIC07_description_shortened)[1:23],'high','low'),#Break into two plots to stack, split by total av productivity
    sectorgrouping = cut_number(as.integer(SIC07_description_shortened), 8) %>% as.integer
  ) %>% 
  filter(!qg('hous',SIC07_description), !is.na(gvaperjob_movingav))

#Test
# debugonce(plotprod)
# plotprod(prod.data %>% filter(sectorgrouping==6),job_PPM_ofUKtotal_movingav)#current prices
# plotprod(prod.data %>% filter(sectorgrouping==6),gvaperjob_movingav)#chained vol

# Some bits for retconning the function to work for Bradford and for this
# placestoadd = unique(bres.gva$Region_name[qg('Leeds|Bradford|kirklees|wakef',bres.gva$Region_name)])

# Via geog portal. We just want (at the moment) ITL1 <> 3
itl2025lookup = read_csv("local/LAD_(April_2025)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv") %>% 
  select(ITL125NM,ITL325CD,ITL325NM) %>% 
  distinct()

ynh_itl3s = itl2025lookup %>% filter(qg('yorkshire and', ITL125NM)) %>% 
  mutate(ITL325NM = ifelse(qg('Kingston upon Hull', ITL325NM), 'Hull',ITL325NM))#shorten Hull

# Hmmph, rename for ease of getting to work!
plotz <- map(
  prod.data %>% 
    rename(DATE = year) %>%
    mutate(Region_name = ifelse(qg('Kingston upon Hull', Region_name), 'Hull',Region_name)) %>% #shorten Hull
    group_split(sectorgrouping),
  plotprod_generic,
  gvaperjob_movingav,
  # placestoadd = unique(ynh_itl3s$ITL325NM)[1:3]
  # placestoadd = unique(ynh_itl3s$ITL325NM)[c(1,4,6,11)]#Cities
  # placestoadd = unique(ynh_itl3s$ITL325NM)[c(2,3,5)]#Rural
  placestoadd = unique(ynh_itl3s$ITL325NM)[c(7,8,9)]#BDR
  )

patchwork::wrap_plots(plotz,ncol = 1)
































