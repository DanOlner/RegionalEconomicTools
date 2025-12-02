# YORKSHIRE AND HUMBER SECTORS----
# Data prep / checks / explore for Nov 2025 Y&H sector/cluster analysis
library(tidyverse)
library(plotly)
library(nomisr)
library(sf)
library(tmap)
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

# Save
saveRDS(gva.sections,'local/gvasections.rds')

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



# plotz = ynh_autoplots("Manufacture of furniture; other manufacturing")



## SIC section proportion plots----

# Oh it would appear the function does it for me
# Well done past me!
place = 'Yorkshire and The Humber'

# Run wiggly plot (below) data collection code here to get sector percents for filtering
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
  filter(meanpercent >= 6) %>% 
  pull(SIC07_description)

p <- twod_proportionplot(
  # df = gva.sections %>% filter(!qg('london',Region_name)),# Version with London removed
  df = gva.sections %>% filter(!SIC07_description %in% dropthesesectors),
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
  filter(meanpercent < 6) %>% 
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
# islq %>% filter(qg('basic metal',SIC07_description),year == 2022) %>% View


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
# Test version without London
gva.gs.smoothed = gva.gs.smoothed %>% 
  # filter(!qg('london', Region_name)) %>% 
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

saveRDS(gva.gs.smoothed,'local/gvagssmoothed.rds')

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
  ylab('Location quotient') +
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

# That's interesting... would like to see that for sections and 2 digit too at some point
# A measure of changing specialisation / concentration over time
# Which SD has gone up / dropped the most?

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




## 2D GVA V JOB PLOTS FOR Y&H ITL3s----

# Sticking them all on together, let's see if that works.
# Percent changes done in the function
  
bres.gva.2d = bres.gva %>% 
  arrange(year) %>% 
  group_by(Region_name,SIC07_description) %>%
  mutate(
    gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
    jobcount_movingav = rollapply(JOBCOUNT,smoothband,mean,align='center',fill=NA),
    `gva/job` = (gva_movingav / jobcount_movingav) * 1000,
    placename_shorter = case_when(
      qg('east rid',Region_name) ~ 'East Riding',
      qg('north and north',Region_name) ~ "N/NE L'shire",
      qg('north york',Region_name) ~ 'N Yorks',
      qg('Calderdale and K',Region_name) ~ "C'dale/K'lees",
      .default = Region_name),# e.g. york stays the same
    placename_shortest = case_when(
      Region_name == 'York' ~ 'Yk',
      qg('east rid',Region_name) ~ 'ER',
      qg('north and north',Region_name) ~ 'NL',
      qg('north york',Region_name) ~ 'NY',
      qg('Sheffield',Region_name) ~ 'Sh',
      qg('Barnsley',Region_name) ~ 'Ba',
      qg('Rother',Region_name) ~ 'Ro',
      qg('Doncast',Region_name) ~ 'Do',
      qg('Bradford',Region_name) ~ 'Br',
      qg('Leeds',Region_name) ~ 'Le',
      qg('Calderdale and K',Region_name) ~ 'CK',
      qg('Wakefield',Region_name) ~ 'Wa',
      .default = Region_name
    )
  ) %>% ungroup() %>% 
  filter(!is.na(jobcount_movingav))#keep only smoothed years
  
# Get list of shorter place names for Y&H
ynhshortnames = bres.gva.2d %>%
  filter(Region_name %in% ynh_itl3s$ITL325NM) %>% 
  pull(placename_shortest) %>% 
  unique

saveRDS(ynhshortnames, 'local/data/ynhshortestnames.rds')
# saveRDS(ynhshortnames, 'local/data/ynhshortnames.rds')

# Add in sector regional prop of jobs
# No, don't do this, gva here is chained volume, can't sum!
# bres.gva.2d = bres.gva.2d %>% 
#   group_split(year) %>% 
#   map(add_location_quotient_and_proportions, 
#       regionvar = Region_name,
#       lq_var = SIC07_description,
#       valuevar = gva_movingav) %>% 
#       # valuevar = jobcount_movingav) %>% 
#   bind_rows()

# Use GVA proportion from current prices instead to get a value to filter on
itl3.for2d = read_csv("data/regionalGVA/regionalGVA_currentprices_itl3_SIC_2DIGIT_LONG_2023.csv")
# 
itl3.for2d = itl3.for2d %>%
  mutate(value = ifelse(value == -1, 0, value)) %>%
  arrange(year) %>%
  group_by(Region_name,SIC07_description) %>%
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
  ) %>%
  ungroup() %>%
  filter(!is.na(gva_movingav)) #Keep only smoothed data years
# mutate(gva_movingav = round(gva_movingav,0))
 
itl3.for2d = itl3.for2d %>%
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = gva_movingav) %>%
  bind_rows() %>% 
  filter(!is.na(gva_movingav)) #Keep only smoothed data years


 #Add in the sector proportions to filter by
bres.gva.2d = bres.gva.2d %>% 
  left_join(
    itl3.for2d %>% select(ITL_code,year,SIC07_code,sector_regional_propfrom_CP = sector_regional_proportion),
    by = c('ITL_code','year','SIC07_code')
  )

# Tick
# table(!is.na(chk$sector_regional_propfrom_CP))

# Save for retroactivity sticking in multiplot above
saveRDS(bres.gva.2d, 'local/data/bresgva2d.rds')


sector <- bres.gva %>%
  # filter(qg('information',SIC07_description)) %>%
  filter(qg('fabricated',SIC07_description)) %>%
  pull(SIC07_description) %>%
  unique()

for(sector in unique(bres.gva$SIC07_description)){

  placestokeep <- bres.gva.2d %>% 
    filter(year == max(year), SIC07_description == sector) %>% 
    filter(sector_regional_propfrom_CP * 100 > 1.5) %>%#Keep only places where this sector makes up 1%+ of reg econ
    select(placename_shortest) %>% 
    distinct() %>% 
    pull
  
  # Check we've got some places and some ynh places
  if(length(placestokeep) > 0 & mean(ynhshortnames %in% placestokeep) > 0){
  
    p <- twod_percentplot(
      df = bres.gva.2d %>% filter(SIC07_description == sector, placename_shortest %in% placestokeep),
      category_var = placename_shortest,
      x_var = gva_movingav,
      y_var = jobcount_movingav,#
      # y_var = JOBS_sector_regional_percent_movingav,#this shows structural change better - jobs have grown nominally in most sectors (but breaks GVA/job diagonal)
      timevar = year,
      label_var = `gva/job`,
      category_var_value_to_highlight = ynhshortnames,
      label_only_highlightedplaces = T,
      start_time = 2016,
      end_time = 2022,
      returndata = T,
      backgroundvectoralpha = 0.2,
      # useboxoverlayforlabels = T,
      overlay_arrowsize = 1
    )
    
    # Get range if percents just for any ynh itl3s present
    ynh_data = p[[2]] %>% filter(placename_shortest %in% ynhshortnames)
    
    xminmax = range(ynh_data$x_pct_change)
    yminmax = range(ynh_data$y_pct_change)
    
    # If no neg values, adjust
    # # if(xminmax[1] > 0) xminmax[1] = -20
    # if(yminmax[1] > 0) yminmax[1] = -20
    
    # Test values for within padding range
    # xminmax = c(-10,10)
    
    # Make sure each axis has at least a bit of padding
    padding = 20
    # xminmax = ifelse(abs(xminmax) < 20, 20 * (xminmax / abs(xminmax)), xminmax)
  
    xminmax <- pmax(pmin(xminmax, c(-padding, Inf)), c(-Inf, padding))
    yminmax <- pmax(pmin(yminmax, c(-padding, Inf)), c(-Inf, padding))
    
    # Zoom in on ynh places
    # p[[1]] = p[[1]] + coord_fixed()
    p[[1]] = p[[1]] + coord_fixed(xlim = xminmax * 1.2, ylim = yminmax * 1.2)
    
    p[[1]] <- p[[1]] + 
      # ggtitle(paste0(sector,': ', round(bradjobs/1000,1), 'K jobs in Bradford, ',bradpercentjobs,'% of tot')) +
      xlab("GVA % change 2015/17 av to 2021/23 av") +
      ylab("JOB COUNT % change 2015/17 av to 2021/23 av") +
      ggtitle(sector)
    # coord_cartesian(xlim = c(-50,130), ylim = c(-30,100))
    
    # Remove punct and spaces from sector name for filename
    ggsave(paste0('local/outputs/ynh_2djobgvapercentchangeplots/',gsub('[[:punct:]]| ','',sector),'.png'), plot = p[[1]], width = 10, height = 10)
    
  }#End if length placestokeep

}




# Y&H AS A WHOLE VERSUS VARIETY WITHIN----

# Idea 1: LQ for Y&H and ITL1s sitting above LQ for ITL3s, for each SIC.
# Which will need some SIC addition, but that’s OK.

# Noting the argument for removing London to get a clearer picture...

# Define year range we want to look at for all
# This will be the SMOOTHED YEAR VALUES
year_range = c(2016,2022)

# Job 1: check SIC mismatch between ITL1 and ITL3
# ITL1 processed in 1st section
itl1 = gva.2digit

itl3 = read_csv("data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv")

# ITL1 has a few more categories...
table(unique(itl1$SIC07_code) %in% itl3$SIC07_code)

# In ITL1 not in ITL3
unique(itl1$SIC07_code)[!unique(itl1$SIC07_code) %in% itl3$SIC07_code]
# In ITL3 not in ITL1
unique(itl3$SIC07_code)[!unique(itl3$SIC07_code) %in% itl1$SIC07_code]

# Full list in each
unique(itl1$SIC07_code)
unique(itl3$SIC07_code)

# Prob easiest at this point just to label manually and group/add
# So - label ITL1 SICs with their ITL3 heading
# Only have to do the ones that differ
# Leaving both 68 IMP and 68 in for now, will remove in a moment
itl1 = itl1 %>% 
  mutate(
    SIC3_group = case_when(
      SIC07_code %in% unique(itl1$SIC07_code)[1:5] ~ unique(itl3$SIC07_code)[1],  
      SIC07_code %in% unique(itl1$SIC07_code)[6:7] ~ unique(itl3$SIC07_code)[2],  
      SIC07_code %in% unique(itl1$SIC07_code)[8:10] ~ unique(itl3$SIC07_code)[3],  
      SIC07_code %in% unique(itl1$SIC07_code)[11:13] ~ unique(itl3$SIC07_code)[4],  
      SIC07_code %in% unique(itl1$SIC07_code)[14:17] ~ unique(itl3$SIC07_code)[5],  
      SIC07_code %in% unique(itl1$SIC07_code)[18:19] ~ unique(itl3$SIC07_code)[6],  
      SIC07_code %in% unique(itl1$SIC07_code)[20:21] ~ unique(itl3$SIC07_code)[7],  
      SIC07_code %in% unique(itl1$SIC07_code)[22:24] ~ unique(itl3$SIC07_code)[8],  
      SIC07_code %in% unique(itl1$SIC07_code)[25:26] ~ unique(itl3$SIC07_code)[9],  
      SIC07_code %in% unique(itl1$SIC07_code)[28:32] ~ unique(itl3$SIC07_code)[11],  
      SIC07_code %in% unique(itl1$SIC07_code)[39:41] ~ unique(itl3$SIC07_code)[18],  
      SIC07_code %in% unique(itl1$SIC07_code)[46:48] ~ unique(itl3$SIC07_code)[23],  
      SIC07_code %in% unique(itl1$SIC07_code)[49:51] ~ unique(itl3$SIC07_code)[24],  
      SIC07_code %in% unique(itl1$SIC07_code)[52:54] ~ unique(itl3$SIC07_code)[25],  
      SIC07_code %in% unique(itl1$SIC07_code)[46:48] ~ unique(itl3$SIC07_code)[23],  
      SIC07_code %in% unique(itl1$SIC07_code)[60:61] ~ unique(itl3$SIC07_code)[31],  
      SIC07_code %in% unique(itl1$SIC07_code)[62:63] ~ unique(itl3$SIC07_code)[32],  
      SIC07_code %in% unique(itl1$SIC07_code)[65:67] ~ unique(itl3$SIC07_code)[34],  
      SIC07_code %in% unique(itl1$SIC07_code)[75:76] ~ unique(itl3$SIC07_code)[42],  
      SIC07_code %in% unique(itl1$SIC07_code)[77:78] ~ unique(itl3$SIC07_code)[43],  
      .default = SIC07_code
    )
  )

# Tick
# itl1 %>% select(SIC07_code,SIC3_group) %>% distinct() %>% View

# Sum ITL1 to ITL3 SIC categories and add in the SIC description from ITL3
itl1.summedtoitl3SICs = itl1 %>% 
  group_by(SIC3_group,year,Region_name) %>% 
  summarise(
    value = sum(value),
    ITL_code = max(ITL_code)
    ) %>% 
  ungroup() %>% 
  rename(SIC07_code = SIC3_group) %>% 
  left_join(
    itl3 %>% select(SIC07_code,SIC07_description) %>% distinct(),
    by = 'SIC07_code'
  ) %>% 
  select(year,ITL_code,Region_name,SIC07_code,SIC07_description,value) %>% 
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
    ))


# Filter out London prior to LQs being found
itl1.summedtoitl3SICs = itl1.summedtoitl3SICs %>% 
  filter(Region_name != 'London')

# Tick
unique(itl1.summedtoitl3SICs$SIC07_code) %in% unique(itl3$SIC07_code)
unique(itl1.summedtoitl3SICs$SIC07_description) %in% unique(itl3$SIC07_description)



# OK, now we find LQs for ITL1 and ITL3 and then combine, sorting by ITL1 overall
# Probably removing London, but let's run for all for now...

# Moving av first, to use for LQs
smoothband = 3

itl1.summedtoitl3SICs = itl1.summedtoitl3SICs %>% 
  arrange(year) %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(gva_movingav)) #Keep only smoothed data years
  # mutate(gva_movingav = round(gva_movingav,0))


itl1.summedtoitl3SICs = itl1.summedtoitl3SICs %>% 
  group_split(year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = gva_movingav) %>% 
  bind_rows()

# Keep only these smoothed years to display
itl1.summedtoitl3SICs = itl1.summedtoitl3SICs %>%
  filter(year %in% year_range)

# Save for use elsewhere
saveRDS(itl1.summedtoitl3SICs, 'local/data/itl1summedtoitl3SICs.rds')



# Repeat for ITL3... 

# Add in moving av first, so can use that for LQ
# get rid of the pesky -1
itl3 = itl3 %>% 
  mutate(value = ifelse(value == -1, 0, value)) %>% 
  arrange(year) %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(gva_movingav)) #Keep only smoothed data years
  # mutate(gva_movingav = round(gva_movingav,0))


# Filter out London prior to LQs being found
londonitl3s = itl2025lookup %>% 
  filter(qg('London', ITL125NM)) %>% 
  pull(ITL325NM)

itl3 = itl3 %>% filter(!Region_name %in% londonitl3s)


itl3 = itl3 %>% 
  group_split(year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = gva_movingav) %>% 
  bind_rows()

# Keep only these smoothed years to display
itl3 = itl3 %>% filter(year %in% year_range)



# Keep only Y&H
itl3.ynh = itl3 %>% filter(Region_name %in% ynh_itl3s$ITL325NM)
unique(itl3.ynh$Region_name)

# Now shortern those names
itl3.ynh = itl3.ynh %>% 
  mutate(
    placename_shorter = case_when(
      qg('east rid',Region_name) ~ 'East Riding',
      qg('north and north',Region_name) ~ "N/NE L'shire",
      qg('north york',Region_name) ~ 'N Yorks',
      qg('Sheffield',Region_name) ~ 'Sheffield',
      qg('Calderdale and K',Region_name) ~ "C'dale/K'lees",
      .default = Region_name# e.g. york stays the same
  ),
    placename_shortest = case_when(
      Region_name == 'York' ~ 'Yk',
      qg('east rid',Region_name) ~ 'ER',
      qg('north and north',Region_name) ~ 'NL',
      qg('north york',Region_name) ~ 'NY',
      qg('Sheffield',Region_name) ~ 'Sh',
      qg('Barnsley',Region_name) ~ 'Ba',
      qg('Rother',Region_name) ~ 'Ro',
      qg('Doncast',Region_name) ~ 'Do',
      qg('Bradford',Region_name) ~ 'Br',
      qg('Leeds',Region_name) ~ 'Le',
      qg('Calderdale and K',Region_name) ~ 'CK',
      qg('Wakefield',Region_name) ~ 'Wa' 
  )
  )

unique(itl3.ynh$placename_shorter)  
unique(itl3.ynh$placename_shortest)  

# Save for use elsewhere
saveRDS(itl3.ynh, 'local/data/itl3ynh.rds')



# Actually, just realising what I'm after is a bit different, probably:
# We want, for Sector A:
# ITL1 sector A LQ, single line (with other ITLs in background)
# ITL3 sector A LQs, so with each Y&H place ordered in one plot

# Which is ONE sector per ITL3 plot
# Now, can we jimmy the existing code to this just by wrapping variables...?
# Yep, tick


# Get ITL1 LQ for that sector - just going to be a single row 

# Test sector
# sector = unique(itl3$SIC07_description)[qg('petrol',unique(itl3$SIC07_description))]
sectorlist = unique(itl3$SIC07_description)[!qg('households|personal service|membership',unique(itl3$SIC07_description))]

bres.gva.2d = readRDS('local/data/bresgva2d.rds')

for(sector in sectorlist){

  # Get ITL1 for Y&H with other ITL1s in background
  # Will just be a single row, don't need to order (yet)
  # Will order for final output by LQ in Y&H...
  LQ_slopes <- compute_slope_or_zero(
    data = itl1.summedtoitl3SICs, 
    Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = year)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- itl1.summedtoitl3SICs %>% filter(year == max(year))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- itl1.summedtoitl3SICs %>% 
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
  
  # Then just plot for that single sector
  place = 'Yorkshire and The Humber'
  
  # Get LQ range to display
  # Here, range will be LQs for places for this sector 
  # not the range bars, most likely, though we may need to get whichever is highest/lowest
  # lqmin = yeartoplot %>% filter(SIC07_description == sector) %>% 
  #   filter(LQ == min(LQ)) %>% 
  #   pull(LQ)
  lq_range = range(yeartoplot$LQ[yeartoplot$SIC07_description == sector])
  
  # Display money value and regional prop on x axis
  yaxisdisplay = paste0(
    "£",
    round(yeartoplot %>% filter(SIC07_description == sector, Region_name == place) %>% pull(gva_movingav),0),
    "M, ",
    round(yeartoplot %>% filter(SIC07_description == sector, Region_name == place) %>% select(sector_regional_proportion) * 100,2),
    "%"
  )
    
  
  p <- LQ_baseplot(df = yeartoplot %>% filter(SIC07_description == sector), alpha = 0.8, shape = 0, sector_name = SIC07_description, LQ_column = LQ, change_over_time = slope, labelcolumn = placename_short)
  
  p <- addplacename_to_LQplot(df = yeartoplot %>% filter(SIC07_description == sector), plot_to_addto = p, maxLQvalmultiplier = 0.2,
                              placename = place, shapenumber = 16,
                              min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                              value_column = gva_movingav, sector_regional_proportion = sector_regional_proportion,
                              region_name = Region_name,
                              sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ,
                              text = 7, value_col_ismoney = T)
  
  p <- p + 
    # coord_cartesian(xlim = c(0.1,7)) +
    theme(
      # axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title = element_text(hjust = 0.5,face = "bold", size = 14)
      ) +
    ylab(yaxisdisplay) +
    ggtitle(sector) +
    coord_cartesian(xlim = lq_range)
  
  
  
  
  # Get ITL3 plot made for the places in Y&H
  LQ_slopes <- compute_slope_or_zero(
    data = itl3.ynh, 
    Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = year)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- itl3.ynh %>% filter(year == max(year))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- itl3.ynh %>% 
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
  
  # Have just upyeard regionalGVA_siccode_lookupmaker.R for ITL1 as well...
  # (Forgot I had that!)
  # gvalookup_itl3 = read_csv('data/siclookup_forregionalGVAcategories_ITL3.csv')
  
  # Confirm... tick
  # table(unique(gvalookup_itl1$SIC07_code) %in% unique(islq$SIC07_code))
  # yeartoplot <- yeartoplot %>%
  #   left_join(
  #     gvalookup_itl3 %>% select(-SIC07_description),
  #     by = 'SIC07_code'
  #   )
  
  
  # Make a column with the amount and % in to display as part of labels
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = paste0(
        placename_shorter,
        ": £",
        round(gva_movingav,0),
        "M, ",
        round(sector_regional_proportion * 100,2),
        "%"
        )
    )
  
  
  # Filter down just to the sector we're displaying
  # so we can get order correct
  yeartoplot = yeartoplot %>% filter(SIC07_description == sector)
  
  # placeLQorder <- yeartoplot %>% 
  #   arrange(-LQ) %>% 
  #   pull(displayregions) 
  
  #Turn the sector column into a factor and order by LQs
  # yeartoplot$displayregions <- factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = fct_reorder(displayregions,LQ_log,.desc = T)
    )
  
  # factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  
  
  # Get range to display on x axis
  # Use min and max of minmaxes here
  plot_range = minmaxes %>% filter(SIC07_description == sector) %>% 
    select(min_LQ_all_time:max_LQ_all_time) %>% 
    pivot_longer(min_LQ_all_time:max_LQ_all_time, names_to = 'cols', values_to = 'vals') %>% 
    ungroup() %>% 
    reframe(range = range(vals)) %>% 
    pull(range) 
  
  if(plot_range[1]==0) plot_range[1] = 0.1# Avoid log infs
  
  # debugonce(LQ_baseplot)
  p2 <- LQ_baseplot(df = yeartoplot, alpha = 1, sector_name = displayregions, 
                   LQ_column = LQ, change_over_time = slope)
  
  # debugonce(addplacename_to_LQplot)
  p2 <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p2, maxLQvalmultiplier = 20,#Hide it!
                              placename = sector, shapenumber = 16,
                              min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                              value_column = gva_movingav, sector_regional_proportion = sector_regional_proportion,
                              region_name = SIC07_description,
                              sector_name = displayregions, change_over_time = slope, LQ_column = LQ,
                              text = 7, value_col_ismoney = T)
  
  p2 <- p2 +
    coord_cartesian(xlim = plot_range) +
    ggtitle("")
  
  
  # ALSO GET JOBS/GVA 2D PLOT (worked out below)
  p3 = persector_jobsgva_percentchangeplot_ynh(sector)
  
  # If we have a 2D plot (i.e. at least one ynh place has 2%+ jobs in this sector)
  if(!is.null(p3)){
    
    plottosave = p / p2 / p3 + patchwork::plot_layout(heights = c(2, 10, 10))
    
    # Remove punctuation and spaces from filename
    ggsave(paste0('local/outputs/ynh_sectorLQplots/',gsub('[[:punct:]]| ','',sector),'.png'), plot = plottosave, width = 9, height = 17)
    
  } else {
    
    plottosave = p / p2 + patchwork::plot_layout(heights = c(2, 10))
    
    # Remove punctuation and spaces from filename
    ggsave(paste0('local/outputs/ynh_sectorLQplots/',gsub('[[:punct:]]| ','',sector),'.png'), plot = plottosave, width = 9, height = 11)
    
  }
  

}#end for sector


# Some checks
# itl3.ynh and bres.gva.2d should have the same 21-23 sector regional props
# But they're not showing up like that
# How come?

# Hah - 2022 is the reference year for CV isn't it? 
# GVA values won't match in other years...? Tick
# So yes - BRES + GVA is using chained volume, ITL3 is using current prices...
# itl3.ynh %>% filter(year == 2016, placename_shorter %in% ynhshortnames, qg('petrol', SIC07_description)) %>% View('itl3')
# bres.gva.2d %>% filter(year == 2016, placename_shorter %in% ynhshortnames,qg('petrol', SIC07_description)) %>% View('bres2d')




## Check moving average discrepancy

# Oooooh. Scratch that. Realised before looking - 
# itl3 is current prices, BRES + GVA is using chained volume values
# I can filter, but it'll need to be by the CP source - DO NOT find LQs from CV!


# Between bres.gva and itl3 above
# LQ will be different if London not excluded BUT
# That shouldn't affect GVA moving avs or per-region sector proportions
# And they're different. Why?

# ITL1s here have been binned into the ITL3 less-granular SICs
# But again that shouldn't it as will share same SICs

# So, let's just redo each from source

# ITL3 raw first
# itl3 = read_csv("data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv")
# 
# itl3 = itl3 %>% 
#   mutate(value = ifelse(value == -1, 0, value)) %>% 
#   arrange(year) %>% 
#   group_by(Region_name,SIC07_description) %>% 
#   mutate(
#     gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
#   ) %>% 
#   ungroup() %>% 
#   filter(!is.na(gva_movingav)) #Keep only smoothed data years
# # mutate(gva_movingav = round(gva_movingav,0))
# 
# 
# # Then BRES + GVA
# bres.gva.2d = bres.gva %>% 
#   arrange(year) %>% 
#   group_by(Region_name,SIC07_description) %>%
#   mutate(
#     gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
#     jobcount_movingav = rollapply(JOBCOUNT,smoothband,mean,align='center',fill=NA),
#     `gva/job` = (gva_movingav / jobcount_movingav) * 1000,
#     placename_shorter = case_when(
#       qg('east rid',Region_name) ~ 'East Riding',
#       qg('north and north',Region_name) ~ "N/NE L'shire",
#       qg('north york',Region_name) ~ 'N Yorks',
#       qg('Calderdale and K',Region_name) ~ "C'dale/K'lees",
#       .default = Region_name)# e.g. york stays the same
#   ) %>% ungroup() %>% 
#   filter(!is.na(jobcount_movingav))#keep only smoothed years
# 
# 
# 
# 
# # Filter out London prior to LQs being found
# londonitl3s = itl2025lookup %>% 
#   filter(qg('London', ITL125NM)) %>% 
#   pull(ITL325NM)
# 
# itl3 = itl3 %>% filter(!Region_name %in% londonitl3s)
# 
# 
# itl3 = itl3 %>% 
#   group_split(year) %>% 
#   map(add_location_quotient_and_proportions, 
#       regionvar = Region_name,
#       lq_var = SIC07_description,
#       valuevar = gva_movingav) %>% 
#   bind_rows()
# 
# # Keep only these smoothed years to display
# itl3 = itl3 %>% filter(year %in% year_range)
# 
# 
# # Then BRES + GVA
# bres.gva.2d = bres.gva %>% 
#   arrange(year) %>% 
#   group_by(Region_name,SIC07_description) %>%
#   mutate(
#     gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
#     jobcount_movingav = rollapply(JOBCOUNT,smoothband,mean,align='center',fill=NA),
#     `gva/job` = (gva_movingav / jobcount_movingav) * 1000,
#     placename_shorter = case_when(
#       qg('east rid',Region_name) ~ 'East Riding',
#       qg('north and north',Region_name) ~ "N/NE L'shire",
#       qg('north york',Region_name) ~ 'N Yorks',
#       qg('Calderdale and K',Region_name) ~ "C'dale/K'lees",
#       .default = Region_name)# e.g. york stays the same
#   ) %>% ungroup() %>% 
#   filter(!is.na(jobcount_movingav))#keep only smoothed years
# 
# # Get list of shorter place names for Y&H
# ynhshortnames = bres.gva.2d %>%
#   filter(Region_name %in% ynh_itl3s$ITL325NM) %>% 
#   pull(placename_shorter) %>% 
#   unique
# 
# # Add in sector regional prop of jobs
# bres.gva.2d = bres.gva.2d %>% 
#   group_split(year) %>% 
#   map(add_location_quotient_and_proportions, 
#       regionvar = Region_name,
#       lq_var = SIC07_description,
#       valuevar = gva_movingav) %>% 
#   # valuevar = jobcount_movingav) %>% 
#   bind_rows()
# 




# QUICK INDSTRAT V Y&H PLACES LQS?-----

# Can we use the code above that puts places on y axis and do this relatively quickly?
indstrat_sums = readRDS('local/indstrat_sums.rds')

# These already are smoothed...
# Have not filtered London out here, probably should?
indstrat_sums = indstrat_sums %>% 
  filter(!is.na(totaljobs_movingav)) %>% 
  rename(year = DATE, Region_name = GEOGRAPHY_NAME) %>% 
  group_split(year) %>% 
    map(
      add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = indstrat_code,
      valuevar = totaljobs_movingav
    ) %>% 
    bind_rows()

# Hmm I think we've got unitary and county in the indstrat data
# lad_lookup = read_csv("local/LAD_(April_2025)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv")
# lad_lookup = read_csv("local/Local_Authority_District_(April_2021)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2021)_Lookup_in_United_Kingdom.csv")
# 
# table(unique(lad_lookup$LAD21NM) %in% indstrat_sums$Region_name)

# Pick manually to get just Y&H places
# ynh itl3 names gets us most of the way
itl3.ynh = readRDS('local/data/itl3ynh.rds')
ynhnames = unique(itl3.ynh$Region_name)

ynhnames = c(ynhnames[-c(2,11)], 'Calderdale','Kirklees','Lincolnshire','North Lincolnshire')
# ynhnames = c(ynhnames[c(2,13,14,15,16)], 'Calderdale','Kirklees','Lincolnshire','North Lincolnshire')

table(ynhnames %in% indstrat_sums$Region_name)

# Keep only those
indstrat_sums = indstrat_sums %>% 
  filter(
    Region_name %in% ynhnames
  )


# Enshortenemateifyadoodoo
# ynhshortnames - not that short? And also won't match LAs here
unique(indstrat_sums$Region_name)

indstrat_sums = indstrat_sums %>%
  mutate(
    Region_name = case_when(
      Region_name == 'Lincolnshire' ~ 'L\'shire',
      qg('North linc',Region_name) ~ 'N L\'shire',
      qg('North york',Region_name) ~ 'N Yorks',
      qg('East riding',Region_name) ~ 'East Riding',
      .default = Region_name
    )
  )


  


# Get ITL3 plot made for the places in Y&H
LQ_slopes <- compute_slope_or_zero(
  data = indstrat_sums, 
  Region_name, indstrat_code,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year)


#Filter down to a single year... we may want to smooth years, let's see
yeartoplot <- indstrat_sums %>% filter(year == max(year))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('Region_name', 'indstrat_code')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- indstrat_sums %>% 
  group_by(Region_name, indstrat_code) %>% 
  summarise(
    min_LQ_all_time = min(LQ, na.rm = T),
    max_LQ_all_time = max(LQ, na.rm = T)
  ) %>% 
  mutate(
    min_LQ_all_time = ifelse(is.infinite(min_LQ_all_time),NA,min_LQ_all_time),
    max_LQ_all_time = ifelse(is.infinite(max_LQ_all_time),NA,max_LQ_all_time)
  )

saveRDS(minmaxes,'local/data/ynh_indstrat_minmaxes.rds')


# table(is.infinite(minmaxes$min_LQ_all_time))
# table(is.infinite(minmaxes$max_LQ_all_time))

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('Region_name', 'indstrat_code')
  )

# Join other SIC levels to it so we can break down into production / other
# I think I have a lookup for that, though that will need a tweak due to one extra sector in ITL1

# Have just upyeard regionalGVA_siccode_lookupmaker.R for ITL1 as well...
# (Forgot I had that!)
# gvalookup_itl3 = read_csv('data/siclookup_forregionalGVAcategories_ITL3.csv')

# Confirm... tick
# table(unique(gvalookup_itl1$SIC07_code) %in% unique(islq$SIC07_code))
# yeartoplot <- yeartoplot %>%
#   left_join(
#     gvalookup_itl3 %>% select(-indstrat_code),
#     by = 'SIC07_code'
#   )


# Make a column with the amount and % in to display as part of labels
yeartoplot = yeartoplot %>% 
  mutate(
    displayregions = paste0(
      Region_name,
      ": ",
      round(totaljobs_movingav/1000,1),
      "K, ",
      round(sector_regional_proportion * 100,2),
      "%"
    )
  )

saveRDS(yeartoplot,'local/data/ynh_indstrat_lqplotdata.rds')


# Function up to get repeat plots
make_indstrat_ynhlqplots = function(indstrat_name){

  # Filter down just to the sector we're displaying
  # so we can get order correct
  yeartoplot.sub = yeartoplot %>% filter(indstrat_code == indstrat_name)
  
  # placeLQorder <- yeartoplot %>% 
  #   arrange(-LQ) %>% 
  #   pull(displayregions) 
  
  #Turn the sector column into a factor and order by LQs
  # yeartoplot$displayregions <- factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  yeartoplot.sub = yeartoplot.sub %>% 
    mutate(
      displayregions = fct_reorder(displayregions,LQ_log,.desc = T)
    )
  
  # factor(yeartoplot.sub$displayregions, levels = placeLQorder, ordered = T)
  
  
  # Get range to display on x axis
  # Use min and max of minmaxes here
  plot_range = minmaxes %>% filter(indstrat_code == indstrat_name) %>% 
    select(min_LQ_all_time:max_LQ_all_time) %>% 
    pivot_longer(min_LQ_all_time:max_LQ_all_time, names_to = 'cols', values_to = 'vals') %>% 
    ungroup() %>% 
    reframe(range = range(vals)) %>% 
    pull(range) 
  
  if(plot_range[1]==0) plot_range[1] = 0.1# Avoid log infs
  
  # debugonce(LQ_baseplot)
  p2 <- LQ_baseplot(df = yeartoplot.sub, alpha = 1, sector_name = displayregions, 
                    LQ_column = LQ, change_over_time = slope)
  
  # debugonce(addplacename_to_LQplot)
  p2 <- addplacename_to_LQplot(df = yeartoplot.sub, plot_to_addto = p2, maxLQvalmultiplier = 20,#Hide it!
                               placename = unique(yeartoplot.sub$indstrat_code), shapenumber = 16,
                               min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                               value_column = totaljobs_movingav, sector_regional_proportion = sector_regional_proportion,
                               region_name = indstrat_code,
                               sector_name = displayregions, change_over_time = slope, LQ_column = LQ,
                               text = 7, value_col_ismoney = T)
  
  p2 +
    coord_cartesian(xlim = plot_range) +
    ggtitle(unique(yeartoplot.sub$indstrat_code))

}


# make_indstrat_ynhlqplots(unique(indstrat_sums$indstrat_code)[2])
plotz_gg = map(unique(indstrat_sums$indstrat_code), make_indstrat_ynhlqplots)

plotz = patchwork::wrap_plots(plotz_gg, ncol = 2)

# Save those plots as is for QMD
saveRDS(plotz,'local/data/indstrat_lqplots_ynh.rds')


# TEST MAKING 2-DIGIT SECTOR MAP FROM CH DATA----

# The idea here: add a hex map of CH data for the ITL3-level SIC sectors used above.
# To show (a) where it's actually densest and 
# (b) maybe with bivariate, where it's growing

# For which, we don't need too much CH data, only Y&H itself. 
# Let's get that, subset, then drop the orig
ch = readRDS('../companieshouseopen/local/PROCESSED_accountextracts_n_livelist_geocoded_combined_Oct2025.rds')
# 
# # Get ITL2 to 1 lookup for 2021 (the ones we've got here)
itl2to1lookup = read_csv("local/Local_Authority_District_(April_2021)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2021)_Lookup_in_United_Kingdom.csv") %>%
  select(ITL121NM,ITL221CD,ITL221NM) %>%
  distinct()
 
# Same as 2025, turns out...
ch.ynh = ch %>%
  filter(
    ITL221CD %in% (itl2to1lookup %>% filter(qg('humber',ITL121NM)) %>% pull(ITL221CD))
  )
# 
saveRDS(ch.ynh, 'local/data/ch_yorkshire_n_humber_October2025.rds')

# While we have the GB data:
# Going to try a micro-hex LQ measure
# Rather than try and use the function
# (Which would be findind it for all hexes)
# Let's do the calc manually.

# Again, I'm going to exclude London
# There are five London ITL2s...
# Blimey, dropping London is such a big drop in firm number!
ch = ch %>% 
  filter(
    !ITL221CD %in% (itl2to1lookup %>% filter(qg('london',ITL121NM)) %>% pull(ITL221CD))
  )

# Test version where we're finding the LQs relative JUST
# To Y&H i.e. is this bit more or less conc than Y&H average?
# To see where clusters are regionally?
ch = ch %>% 
  filter(
    ITL221CD %in% (itl2to1lookup %>% filter(qg('humber',ITL121NM)) %>% pull(ITL221CD))
  )


# OK, can now find LQs...
# Actually, if we just use the existing function and ITL2s
# We'll get the national numbers and can use those
# Let's also use average employee number across both available years
ch.gb.nationalprops = ch %>% 
  st_set_geometry(NULL) %>% 
  filter(Employees_thisyear > 0, Employees_lastyear > 0) 


# Just remembering rowwise means is massively faster when vectored outside dplyr
ch.gb.nationalprops$mean_employeecount = 
  rowMeans(cbind(
    ch.gb.nationalprops$Employees_thisyear,
    ch.gb.nationalprops$Employees_lastyear
    ))

# Add in the SIC desription level from ITL3 for grouping to match rest
# We don't want any firms with no SICs at this point either
ch.gb.nationalprops = ch.gb.nationalprops %>% 
  left_join(sics.forBRESjoin %>% select(-SIC07_code) %>% rename(SIC07_description_from_ITL3=SIC07_description,SIC07code_from_ITL3 =SIC07_code_fromGVAdata), by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric')
  ) %>% filter(
    !is.na(SIC07_description_from_ITL3)
  )


# Group by ITl2, just for arbitrary subgrouping at this point
ch.gb.nationalprops.sums = ch.gb.nationalprops %>% 
  group_by(ITL221NM,SIC07_description_from_ITL3) %>% 
  summarise(total_meanemployees = sum(mean_employeecount))

# Now can find LQs but we're only after the GB-minus-London sector proportions
# Only doing once, don't need to map separate years
ch.gb.nationalprops.sums = ch.gb.nationalprops.sums %>% 
  add_location_quotient_and_proportions(
    regionvar = ITL221NM,
    lq_var = SIC07_description_from_ITL3,
    valuevar = total_meanemployees
  ) 




# Y&H level only - can get per-hex LQs from here
# Overwrites GB ch above but we don't need now
ch = readRDS('local/data/ch_yorkshire_n_humber_October2025.rds')

# Add in ITL3 level 2 digit bespoke SICs
# Code nabbed from above...
# We already have this, which should work...
# sics.forBRESjoin <- make.GVA.SICs.long(itl3)
ch = ch %>% 
  left_join(sics.forBRESjoin %>% select(-SIC07_code) %>% rename(SIC07_description_from_ITL3=SIC07_description,SIC07code_from_ITL3 =SIC07_code_fromGVAdata), by = c('SIC_2DIGIT_CODE_NUMERIC' = 'SIC07_code_numeric')
  )

# Very nearly the same, good enough
# table(is.na(ch$SIC07_code_fromGVAdata))
# table(is.na(ch$SIC_2DIGIT_CODE_NUMERIC))



# Now let's find a useful hexmap size and summarise...

# From AI economy work / sector_linkages.R
sq = st_make_grid(ch, cellsize = 4000, square = F)

#Turn into sf object so gridsquares can have IDs to group by
sq <- sq %>% st_sf() %>% mutate(id = 1:nrow(.))

# Save for automated maps
# saveRDS(sq,'local/data/sq.rds')

overlay <- st_intersection(ch,sq)

#Save for output
# saveRDS(overlay,'local/data/AIE4measures_hexoverlay.rds')
# saveRDS(sq,'local/data/sq_forhexoverlay.rds')

#This no longer needs to be geo, which will speed up
#Can link back to grids once done

#Let's find an average AIIE weighted by employee number in each grid square
hexsummary <- overlay %>% 
  st_set_geometry(NULL) %>% 
  filter(Employees_thisyear > 0, Employees_lastyear > 0, !is.na(SIC07_description_from_ITL3)) %>% #Only firms with employees recorded in latest year
  # filter(between(Employees_thisyear,1,3)) %>% #Microfirms
  # filter(between(Employees_thisyear,4,9)) %>% #Microfirms
  # filter(Employees_thisyear > 9) %>%
  group_by(id,SIC07_description_from_ITL3) %>% 
  summarise(
    totalemployees_thisyear = sum(Employees_thisyear),
    totalemployees_lastyear = sum(Employees_lastyear),
    totalfirms = n()
  ) %>%
  ungroup()
  # group_by(id) %>%
  # filter(sum(totalemployees) >= 10) %>% #keep only gridsquares where total employee count is more than / equal to 100

# Save that for use elsewhere
# saveRDS(hexsummary,'local/hexsummary_for_dictionary.rds')

# Add in average emp value over both timepoints
hexsummary$total_meanemployees = 
  rowMeans(cbind(
    hexsummary$totalemployees_thisyear,
    hexsummary$totalemployees_lastyear
  ))


# Get hex-level proportions
hexsummary = hexsummary %>% 
  add_location_quotient_and_proportions(
    regionvar = id,
    lq_var = SIC07_description_from_ITL3,
    valuevar = total_meanemployees
  ) 

# Keep only relevant bits, merge in GB-minus-London props
# Make final LQ
hexsummary = hexsummary %>% 
  select(
    -c(totalemployees_thisyear,totalemployees_lastyear,region_totalsize,total_sectorsize:LQ_log)
    ) %>% 
  left_join(
    ch.gb.nationalprops.sums %>% 
      select(SIC07_description_from_ITL3,sector_total_proportion) %>% 
      distinct(),
    by = 'SIC07_description_from_ITL3'
  ) %>% 
  mutate(
    LQ = sector_regional_proportion / sector_total_proportion,
    LQ_log = log(LQ)#Option for symmetric either side of LQ = 1
    )
  


# Do the total employee filter when we're down to sectors...
# Test one now!
# sector = unique(itl3$SIC07_description)[qg('informat',unique(itl3$SIC07_description))]
# sector = unique(itl3$SIC07_description)[qg('fabricated',unique(itl3$SIC07_description))]
# sector = unique(itl3$SIC07_description)[qg('textiles',unique(itl3$SIC07_description))]
sector = unique(itl3$SIC07_description)[qg('furniture',unique(itl3$SIC07_description))]
# sector = unique(itl3$SIC07_description)[qg('agri',unique(itl3$SIC07_description))]

hexsummary.sector = hexsummary %>% filter(SIC07_description_from_ITL3 == sector)


#Link that back into the grid squares...
#Use right join to drop empties
sq.ch <- sq %>% 
  right_join(
    hexsummary.sector,
    by = 'id'
  ) %>% 
  filter(total_meanemployees >= 25) 
# %>% 
  # filter(totalemployees_thisyear >= 25) %>% 
  # mutate(
  #   emp_percentchange = percent_change(totalemployees_thisyear,totalemployees_lastyear)
  # )

sq.ch <- sq.ch %>% 
  mutate(hovertext = paste0("Firm count: ",totalfirms, ", id: ",id))

# tmap_mode('view')
tmap_mode('plot')

# tm_shape(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber'])) +
#   tm_polygons(fill = '#a6baa8') +
# tm_shape(sq.ch %>% filter(emp_percentchange < 100)) +
#   tm_polygons(
#     fill = tm_vars(c("totalemployees_thisyear", "emp_percentchange"),
#                    multivariate = TRUE), id="hovertext",
#     fill.scale = 
#       tm_scale_bivariate(
#         scale1 = tm_scale_intervals(style = "kmeans", n = 3, labels = c("L", "M", "H")),
#         scale2 = tm_scale_intervals(style = "kmeans", n = 3, labels = c("L", "M", "H")),
#         # values = "stevens.bluered")) +
#         values = "bu_br_bivs")) 
  # tm_view(set.view = c(7, 51, 4)) +
  # tm_shape(itl3.2025) +
  # tm_polygons( = 'black', lwd = 1, fill_alpha = 0.3) 
  # tm_shape(itl2) +
  # tm_borders(col = 'black', lwd = 4) 
  # tm_view(set_view = c(-2.2,53.49326048352635,11))#centred on GM
# tm_view(set_view = c(-1.598452,52.740283,8))
# tm_view(bbox = "England")


# Make a masking layer
mask = st_bbox(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber'])) %>% st_as_sfc()

mask = st_buffer(mask, dist = 10000, endCapStyle = "SQUARE")

ynhshp = st_union(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber']))

# Leave a hole for Y&H to show through
mask = st_difference(mask, ynhshp)

# Save mask for use elsewhere
saveRDS(mask,'local/ynhmask.rds')

# Pre-filter Y&H ITL3s, again for ease of re-use
itl3.ynh = itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber'])

saveRDS(itl3.ynh,'local/itl3ynh.rds')

# OK, maybe not bivariate!
tm_basemap("OpenStreetMap") +
  tm_shape(mask) +#add in masking layer for rest of basemap
  tm_polygons(col = 'white', fill = 'white') +
tm_shape(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber']), is.main = TRUE) +
  # tm_polygons(fill = '#a6baa8') +
  tm_polygons(fill = 'white', fill_alpha = 0.6) +
  tm_shape(sq.ch) +
  # tm_shape(sq.ch %>% mutate(emp_percentchange = ifelse(emp_percentchange > 100, 100, emp_percentchange))) +#Cap at 100 for display purposes; those can be "100%+ increase"
  # tm_shape(sq.ch %>% filter(totalemployees_thisyear > 50)) +
  # tm_shape(sq.ch) +
  tm_polygons(
    fill = "LQ_log", id="hovertext",
    # fill = "totalemployees_thisyear", id="hovertext",
    # fill.scale = tm_scale_continuous_log(midpoint = 1, values = "-matplotlib.rd_bu"),
    # fill.scale = tm_scale_continuous_log(midpoint = 1, values = "matplotlib.rd_yl_bu"),
    fill.scale = tm_scale_intervals(style = "pretty", n = 5, values = "-matplotlib.rd_bu"),
    fill.legend = tm_legend(position = tm_pos_out("right", "center")),
    # fill.scale = tm_scale_intervals(style = "kmeans", n = 4, values = "matplotlib.rd_yl_bu"),
    col_alpha = 0.5
    ) +
  tm_shape(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber'])) +
  tm_borders(col_alpha = 0.3)



## SAVE MAPS FOR RE-USE----

# Openstreetmap doesn't work in quarto, it seems.
# Save as images and use those instead.
for(sector in unique(itl3$SIC07_description)[!qg("households|personal service|membership|occupiers' imputed",unique(itl3$SIC07_description))]){

  hexsummary.sector = hexsummary %>% filter(SIC07_description_from_ITL3 == sector)
  
  #Link that back into the grid squares...
  #Use right join to drop empties
  sq.ch <- sq %>% 
    right_join(
      hexsummary.sector,
      by = 'id'
    ) %>% 
    filter(total_meanemployees >= 10) 
  
  sq.ch <- sq.ch %>% 
    mutate(hovertext = paste0("Firm count: ",totalfirms, ", id: ",id))
  
  p = tm_basemap("OpenStreetMap") +
    tm_shape(mask) +#add in masking layer for rest of basemap
    tm_polygons(col = 'white', fill = 'white') +
    tm_shape(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber']), is.main = TRUE) +
    tm_polygons(fill = 'white', fill_alpha = 0.6) +
    tm_shape(sq.ch) +
    tm_polygons(
      fill = "LQ_log", id="hovertext",
      # fill.scale = tm_scale_continuous_log(midpoint = 1, values = "-matplotlib.rd_bu"),
      fill.scale = tm_scale_intervals(style = "pretty", n = 5, values = "-matplotlib.rd_bu"),
      fill.legend = tm_legend(position = tm_pos_out("right", "center")),
      col_alpha = 0.5
    ) +
    tm_shape(itl3.2025 %>% filter(ITL325CD %in% itl2025lookup$ITL325CD[itl2025lookup$ITL125NM == 'Yorkshire and The Humber'])) +
    tm_borders(col_alpha = 0.3)
  
  # tmap_save(p, paste0('local/outputs/ch_hexmaps_fordictionary/',gsub('[[:punct:]]| ','',sector),'.jpeg'), width = 3, height = 2)  
  tmap_save(p, paste0('quarto_docs/images/ynh_lq_maps/',gsub('[[:punct:]]| ','',sector),'.jpeg'), width = 5, height = 4)

}


## SOME MORE CHECKS----

# What's the split of GVA in 'furniture/other' that's combined at ITL3
# But separate at ITL1?
itl1 %>% 
  filter(
    year == max(year),
    SIC07_code %in% c('31','32')
    ) %>% 
  group_by(Region_name) %>% 
  mutate(
    percent = (value / sum(value)) * 100
  ) %>% View
  










