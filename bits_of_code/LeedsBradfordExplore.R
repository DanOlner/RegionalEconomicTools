#Explore Leeds Bradford data
library(tidyverse)
library(spdep)
library(sf)#for geo stuff
library(tmap)#for mapping 
library(zoo)#for making smoothed moving averages
library(patchwork)#combine ggplots easily
library(plotly)#For interactive plots
library(ggdist)
library(tidyr)
library(distributional)

source('functions/misc_functions.R')
options(scipen = 999)

#Set ggplot theme
theme_set(theme_light())


# OLDER 2024 GVA BRES DATA----

## Productivity over time

#Where final data year is 2022 so now quite out of date
#But doing BRES / ONS link could take a while to get same output...
#(Though we could bodge that for now, using whichever zones do match currently across all years)
gva.jobs.ITL2.sections.cv <- read_csv('https://raw.githubusercontent.com/DanOlner/RegionalEconomicTools/refs/heads/gh-pages/data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_chainedvolume_ITL2_SIC_SECTION_MINUSimputedrent_2022.csv') %>% 
  mutate(
    gvaperjob = (gva/JOBCOUNT_FULLTIME) * 1000
  )


gva.jobs.ITL2.sections.cv <- gva.jobs.ITL2.sections.cv %>% 
  mutate(gvaperjob = gva/JOBCOUNT_FULLTIME) %>%
  group_by(GEOGRAPHY_NAME,SIC07_description) %>%
  mutate(
    jobcount_movingav = rollapply(JOBCOUNT_FULLTIME,smoothband,mean,align='center',fill=NA),
    gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
    `gva/job moving av` = rollapply(gvaperjob * 1000,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()



gva.jobs.ITL2.sections.cv <- gva.jobs.ITL2.sections.cv %>% 
  mutate(
    SIC_SECTION_REDUCED = case_when(
      # grepl('admin',SIC07_description,ignore.case = T) ~ 'Admin',
      grepl('agri',SIC07_description,ignore.case = T) ~ 'Agri',
      grepl('electr',SIC07_description,ignore.case = T) ~ 'power',
      grepl('information',SIC07_description,ignore.case = T) ~ 'ICT',
      grepl('manuf',SIC07_description,ignore.case = T) ~ 'Manuf',
      grepl('mining',SIC07_description,ignore.case = T) ~ 'Mining',
      grepl('other',SIC07_description,ignore.case = T) ~ 'other',
      grepl('scientific',SIC07_description,ignore.case = T) ~ 'Scientific',
      grepl('real estate',SIC07_description,ignore.case = T) ~ 'Real est',
      grepl('transport',SIC07_description,ignore.case = T) ~ 'Transport',
      grepl('water',SIC07_description,ignore.case = T) ~ 'Water',
      grepl('entertainment',SIC07_description,ignore.case = T) ~ 'Entertainment',
      grepl('human health',SIC07_description,ignore.case = T) ~ 'Health/soc',
      grepl('food service activities',SIC07_description,ignore.case = T) ~ 'Food/service',
      grepl('wholesale',SIC07_description,ignore.case = T) ~ 'Retail',
      .default = SIC07_description
    )
  )

#PLOT: GVA PER FT JOB FOR SIC SECTIONS OVER TIME
ggplot(
  gva.jobs.ITL2.sections.cv %>% 
    # filter(qg('constr|manuf|scientific|information|transport|health|food|entertainment',SIC07_description)) %>%
    mutate(
      DATE = DATE - 2000,#Make dates 2 digit, more readable on axis
      placetoshow = qg('west y',GEOGRAPHY_NAME),
      SIC_SECTION_REDUCED = fct_reorder(SIC_SECTION_REDUCED, gvaperjob, .desc = T)
    ),
  aes(x = DATE, y = `gva/job moving av`, group = GEOGRAPHY_NAME, size = placetoshow, colour = placetoshow)) +
  coord_cartesian(xlim = c(16,21)) +
  geom_jitter(width = 0.1) +
  scale_size_manual(values = c(1,5)) +
  scale_colour_brewer(palette = 'Set1', direction = -1, name = "Sector") +
  facet_wrap(~SIC_SECTION_REDUCED, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10))) +
  guides(colour = F, size = F) +
  xlab('year') +
  ylab('GVA per FT (3 year moving average)')



#Latest 2025 / 2023 GVA DATA----


## Chained vol real term change vals over time----

itl2.sections.cv <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_SECTION_LONG_2023.csv') 

#Add moving average
#Set range of average - 3 years
smoothband = 3

#Find the average within each place and each sector in each place
#Year is already in order, so this will find 3 year moving av between consecutive years
itl2.sections.cv <- itl2.sections.cv %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()

place = "West Yorkshire"

#top 12 sectors in latest year
top12sectors <- itl2.sections.cv %>% 
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>% 
  arrange(-gva_movingav) %>% 
  slice_head(n= 12) %>% 
  select(SIC07_description) %>% 
  pull()

#Plot! 
ggplot(itl2.sections.cv %>% 
         filter(
           Region_name == place, 
           SIC07_description %in% top12sectors,
           !is.na(gva_movingav)#Remove NAs created by 3 year moving average
         ), 
       aes(x = year, y = gva_movingav, colour = fct_reorder(SIC07_description,-gva_movingav) )) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('SIC GVA (chained volume) ') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by GVA in latest year\n(', smoothband,' year moving average)'))


## SAME FOR ITL3----

itl3.sections.cv <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_SECTION_LONG_2023.csv') 

#Add moving average
#Set range of average - 3 years
smoothband = 3

#Find the average within each place and each sector in each place
#Year is already in order, so this will find 3 year moving av between consecutive years
itl3.sections.cv <- itl3.sections.cv %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()

place = itl3.sections.cv$Region_name[qg('Leeds',itl3.sections.cv$Region_name)] %>% unique

#top 12 sectors in latest year
top12sectors <- itl3.sections.cv %>% 
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>% 
  arrange(-gva_movingav) %>% 
  slice_head(n= 12) %>% 
  select(SIC07_description) %>% 
  pull()



#Get colours same for same SIC codes
#https://stackoverflow.com/a/33144808/5023561
#Make different pastel-ish colours
n <- length(unique(itl3.sections.cv$SIC07_description))
set.seed(12)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[10:(10+(n-1))]


#Plot! 
leedsplot <- ggplot(itl3.sections.cv %>% 
                      filter(
                        Region_name == place, 
                        SIC07_description %in% top12sectors,
                        !is.na(gva_movingav)#Remove NAs created by 3 year moving average
                      ), 
                    aes(x = year, y = gva_movingav, colour = fct_reorder(SIC07_description,-gva_movingav) )) +
  geom_point() +
  geom_line() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  scale_color_manual(values = setNames(randomcols,unique(itl3.sections.cv$SIC07_description))) +#set manual pastel colours matching sector name
  ylab('SIC GVA (chained volume) ') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by GVA in latest year\n(', smoothband,' year moving average)'))

# leedsplot


place = itl3.sections.cv$Region_name[qg('Bradford',itl3.sections.cv$Region_name)] %>% unique

#top 12 sectors in latest year
top12sectors <- itl3.sections.cv %>% 
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>% 
  arrange(-gva_movingav) %>% 
  slice_head(n= 12) %>% 
  select(SIC07_description) %>% 
  pull()

#Plot! 
bradfordplot <- ggplot(itl3.sections.cv %>% 
                         filter(
                           Region_name == place, 
                           SIC07_description %in% top12sectors,
                           !is.na(gva_movingav)#Remove NAs created by 3 year moving average
                         ), 
                       aes(x = year, y = gva_movingav, colour = fct_reorder(SIC07_description,-gva_movingav) )) +
  geom_point() +
  geom_line() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  scale_color_manual(values = setNames(randomcols,unique(itl3.sections.cv$SIC07_description))) +#set manual pastel colours matching sector name
  ylab('SIC GVA (chained volume) ') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by GVA in latest year\n(', smoothband,' year moving average)'))

leedsplot + bradfordplot


## REPEAT LEEDS/BRADFORD BUT DO SECTION PROPORTIONS OVER TIME (FOR BETTER COMPARISON)----

itl3.sections.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_SECTION_MINUSimputedrent_LONG_2023.csv') 

#There's a single minus one value, set to zero
itl3.sections.cp[itl3.sections.cp == -1] <- 0

itl3.sections.cp <- itl3.sections.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()



#Sector proportion plots for ones with largest...
#Add moving average
#Set range of average - 3 years
smoothband = 3

#Find the average within each place and each sector in each place
#Year is already in order, so this will find 3 year moving av between consecutive years
itl3.sections.cp <- itl3.sections.cp %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA),
    sector_regional_prop_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()

place = itl3.sections.cp$Region_name[qg('Leeds',itl3.sections.cp$Region_name)] %>% unique

#top 12 sectors in latest year
top12sectors <- itl3.sections.cp %>% 
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>% 
  arrange(-sector_regional_prop_movingav) %>% 
  slice_head(n= 12) %>% 
  select(SIC07_description) %>% 
  pull()

#Plot! 
leedsplot <- ggplot(itl3.sections.cp %>% 
                      filter(
                        Region_name == place, 
                        SIC07_description %in% top12sectors,
                        !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
                      ), 
                    aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
  geom_point() +
  geom_line() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  scale_color_manual(values = setNames(randomcols,unique(itl3.sections.cv$SIC07_description))) +#set manual pastel colours matching sector name
  ylab('Sector: percent of regional economy') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by sector proportion size in latest year\n(', smoothband,' year moving average)'))


place = itl3.sections.cp$Region_name[qg('Bradford',itl3.sections.cp$Region_name)] %>% unique

#top 12 sectors in latest year
# top12sectors <- itl3.sections.cp %>% 
#   filter(
#     Region_name == place,
#     year == max(year) - 1#to get 3 year moving average value
#   ) %>% 
#   arrange(-sector_regional_prop_movingav) %>% 
#   slice_head(n= 12) %>% 
#   select(SIC07_description) %>% 
#   pull()

#Plot! USE LEEDS TOP SECTORS
bradsplot <- ggplot(itl3.sections.cp %>% 
                      filter(
                        Region_name == place, 
                        SIC07_description %in% top12sectors,
                        !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
                      ), 
                    aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
  geom_point() +
  geom_line() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  scale_color_manual(values = setNames(randomcols,unique(itl3.sections.cv$SIC07_description))) +#set manual pastel colours matching sector name
  ylab('Sector: percent of regional economy') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by sector proportion size in latest year\n(', smoothband,' year moving average)'))


bradsplot + leedsplot






## SIC 2 DIGIT - REPEAT LEEDS/BRADFORD BUT DO PROPORTIONS OVER TIME (FOR BETTER COMPARISON)----

itl3.2digit.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv') %>% 
  filter(!qg('imputed',SIC07_description))

#There's a single minus one value, set to zero
itl3.2digit.cp[itl3.2digit.cp == -1] <- 0

itl3.2digit.cp <- itl3.2digit.cp %>%
  split(.$year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>%
  bind_rows()

n <- length(unique(itl3.2digit.cp$SIC07_description))
randomcols <- col_vector[10:(10+(n-1))]


#Sector proportion plots for ones with largest...
#Add moving average
#Set range of average - 3 years
smoothband = 3

#Find the average within each place and each sector in each place
#Year is already in order, so this will find 3 year moving av between consecutive years
itl3.2digit.cp <- itl3.2digit.cp %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA),
    sector_regional_prop_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()

# place = itl3.2digit.cp$Region_name[qg('Leeds',itl3.2digit.cp$Region_name)] %>% unique
place = itl3.2digit.cp$Region_name[qg('Kirklees',itl3.2digit.cp$Region_name)] %>% unique

#top 15 sectors in latest year
top12sectors <- itl3.2digit.cp %>% 
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>% 
  arrange(-sector_regional_prop_movingav) %>% 
  slice_head(n= 15) %>% 
  select(SIC07_description) %>% 
  pull()




#Plot! 
leedsplot <- ggplot(itl3.2digit.cp %>% 
                      filter(
                        Region_name == place, 
                        SIC07_description %in% top12sectors,
                        !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
                      ), 
                    aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
  geom_point() +
  geom_line() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  scale_color_manual(values = setNames(randomcols,unique(itl3.2digit.cp$SIC07_description))) +#set manual pastel colours matching sector name
  ylab('Sector: percent of regional economy') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by sector proportion size in latest year\n(', smoothband,' year moving average)'))


place = itl3.2digit.cp$Region_name[qg('Bradford',itl3.2digit.cp$Region_name)] %>% unique

#top 15 sectors in latest year
top12sectors <- itl3.2digit.cp %>%
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>%
  arrange(-sector_regional_prop_movingav) %>%
  slice_head(n= 15) %>%
  select(SIC07_description) %>%
  pull()

#Plot! USE LEEDS TOP SECTORS
bradsplot <- ggplot(itl3.2digit.cp %>% 
                      filter(
                        Region_name == place, 
                        SIC07_description %in% top12sectors,
                        !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
                      ), 
                    aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
  geom_point() +
  geom_line() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  scale_color_manual(values = setNames(randomcols,unique(itl3.2digit.cp$SIC07_description))) +#set manual pastel colours matching sector name
  ylab('Sector: percent of regional economy') +
  theme(plot.title = element_text(face = 'bold')) +
  labs(colour = 'SIC section') +
  ggtitle(paste0(place, ' GVA over time\nTop 12 sectors by sector proportion size in latest year\n(', smoothband,' year moving average)'))


bradsplot + leedsplot





## LEEDS V BRADFORD LQ PLOT FOR SECTIONS----

#Use
#LQ_slopes %>% filter(slope==0)
#To see which didn't get slopes (only 8 rows in the current data)
LQ_slopes <- compute_slope_or_zero(
  data = itl3.sections.cp, 
  Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year)

#Filter down to a single year
yeartoplot <- itl3.sections.cp %>% filter(year == max(year))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('Region_name','SIC07_description')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- itl3.sections.cp %>% 
  group_by(SIC07_description,Region_name) %>% 
  summarise(
    min_LQ_all_time = min(LQ),
    max_LQ_all_time = max(LQ)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('Region_name','SIC07_description')
  )

place = 'Leeds'

#Get a vector with sectors ordered by the place's LQs, descending order
#Use this next to factor-order the SIC sectors
sectorLQorder <- itl3.sections.cp %>% filter(
  Region_name == place,
  year == max(year)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)


p <- LQ_baseplot(df = yeartoplot, alpha = 0.1, sector_name = SIC07_description, 
                 LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot, placename = place,
                            plot_to_addto = p, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time, max_LQ_all_time = max_LQ_all_time,#Range bars won't appear if either of these not included
                            value_column = value, sector_regional_proportion = sector_regional_proportion,#Sector size numbers won't appear if either of these not included
                            region_name = Region_name,#The next four, the function needs them all 
                            sector_name = SIC07_description,
                            change_over_time = slope, 
                            LQ_column = LQ 
)

p





## LEEDS V BRADFORD LQ PLOT FOR 2 DIGIT----

#Remove imputed rent
itl3.2digit.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_2DIGIT_LONG_2023.csv') %>% 
  filter(!qg('imputed',SIC07_description))

#There's a single minus one value, set to zero
itl3.2digit.cp[itl3.2digit.cp == -1] <- 0

itl3.2digit.cp <- itl3.2digit.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()


#Use
#LQ_slopes %>% filter(slope==0)
#To see which didn't get slopes (only 8 rows in the current data)
LQ_slopes <- compute_slope_or_zero(
  data = itl3.2digit.cp, 
  Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year)

#Filter down to a single year
yeartoplot <- itl3.2digit.cp %>% filter(year == max(year))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('Region_name','SIC07_description')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- itl3.2digit.cp %>% 
  group_by(SIC07_description,Region_name) %>% 
  summarise(
    min_LQ_all_time = min(LQ),
    max_LQ_all_time = max(LQ)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('Region_name','SIC07_description')
  )

place = 'Leeds'

#Get a vector with sectors ordered by the place's LQs, descending order
#Use this next to factor-order the SIC sectors
sectorLQorder <- itl3.2digit.cp %>% filter(
  Region_name == place,
  year == max(year)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)


p <- LQ_baseplot(df = yeartoplot, alpha = 0.1, sector_name = SIC07_description, 
                 LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot, placename = place,
                            plot_to_addto = p, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time, max_LQ_all_time = max_LQ_all_time,#Range bars won't appear if either of these not included
                            value_column = value, sector_regional_proportion = sector_regional_proportion,#Sector size numbers won't appear if either of these not included
                            region_name = Region_name,#The next four, the function needs them all 
                            sector_name = SIC07_description,
                            change_over_time = slope, 
                            LQ_column = LQ 
)

p


#Comparing to Bradford
#Repeat but overlay other places
p <- LQ_baseplot(df = yeartoplot, alpha = 0, sector_name = SIC07_description, 
                 LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = 'Bradford', shapenumber = 23,
                            region_name = Region_name,#The next four, the function needs them all 
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
#                             placename = 'South Yorkshire', shapenumber = 22,
#                             region_name = Region_name,
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = place, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = value, sector_regional_proportion = sector_regional_proportion,#include numbers
                            region_name = Region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
p <- p + 
  annotate(
    "text",
    label = "Bradford: diamonds",
    x = 0.05, y = 'Education',
    
  )

p


#Bradford first compared to Leeds
sectorLQorder <- itl3.2digit.cp %>% filter(
  Region_name == 'Bradford',
  year == max(year)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)


p <- LQ_baseplot(df = yeartoplot, alpha = 0, sector_name = SIC07_description, 
                 LQ_column = LQ, change_over_time = slope)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = 'Leeds', shapenumber = 23,
                            region_name = Region_name,#The next four, the function needs them all 
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
#                             placename = 'South Yorkshire', shapenumber = 22,
#                             region_name = Region_name,
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)

p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p, 
                            placename = 'Bradford', shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                            value_column = value, sector_regional_proportion = sector_regional_proportion,#include numbers
                            region_name = Region_name,
                            sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
p <- p + 
  annotate(
    "text",
    label = "Bradford: diamonds",
    x = 0.05, y = 'Education',
    
  )

p


## Spearman correlations over time: Similarity of those economies LQ-wise? And how that changes?

#Basics, let's just check correlation over time...

lb <- itl3.2digit.cp %>% 
  filter(Region_name %in% c('Leeds','Bradford')) %>% 
  select(Region_name,SIC07_description,year,LQ_log) %>% 
  pivot_wider(names_from = Region_name, values_from = LQ_log)

#Check correlation change over time there...
corz <- lb %>% 
  group_by(year) %>% 
  summarise(correlation = cor(Bradford, Leeds, method = 'spearman')) 

#Looks like possible economic movement towards each other after the crash?
#Except... that's probably just a function of Leeds finance dropping in proportion over that time
#Raising all other proportions, isn't it?
plot(corz$year,corz$correlation)

#Can test that theory by removing finance...
#No, doesn't seem to be that
corz <- lb %>% 
  filter(!qg('financ',SIC07_description)) %>% 
  group_by(year) %>% 
  summarise(correlation = cor(Bradford, Leeds, method = 'spearman')) 

plot(corz$year,corz$correlation)

#So note, if that holds up, the relationship swaps from neg to pos - moving together?
#Would need to compare to other places
#And I think using ranks, we can repeat this for chained volume - avoiding proportion change dependencies
#Let's do that...



## Spearman again on chained volume ranks (avoid proportion change dependencies)

itl3.2digit.cv <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_2DIGIT_LONG_2023.csv') %>% 
  filter(!qg('imputed',SIC07_description))

#There's a single minus one value, set to zero
itl3.2digit.cp[itl3.2digit.cp == -1] <- 0

lb <- itl3.2digit.cv %>% 
  filter(Region_name %in% c('Leeds','Bradford')) %>% 
  select(Region_name,SIC07_description,year,value) %>% 
  pivot_wider(names_from = Region_name, values_from = value)

#Check rank correlation change over time there...
corz <- lb %>% 
  group_by(year) %>% 
  summarise(correlation = cor(Bradford, Leeds, method = 'spearman')) 

plot(corz$correlation ~ corz$year)
lm(corz$correlation ~ corz$year) %>% summary


#So that's a better isolated test - 
#LQ won't only be affected by internal prop changes
#But structural changes in the UK that might be equally affecting both

#I'd like to just look at those separate rankings by eye
lb <- lb %>% 
  group_by(year) %>% 
  mutate(across(Bradford:Leeds, rank, .names = "{.col}_rank"))

#So - likely to be a bunch of sectors that are the same rank order 
#e.g. agri will always be at or near lowest point
#So - need to know how compares to other pair comparisons
#While we're here, would also be good to know if ranks are likely to be higher for neighbours
#So let's do some spatial stuff too



## Spatial dependency / similarity of neighbouring economies over time----

#Noting that there's a tidy wrapper...
#https://sfdep.josiahparry.com/
#sticking to what I know for now!

#Via https://github.com/DanOlner/spatialanalysistastersession/blob/master/spatialanalysis_taster_code.R

#Get ITL3 polygons (this is 2025 data, so it's the latest ones...)
itl3 <- st_read("~/Dropbox/MapPolygons/UK/2025/International_Territorial_Level_3_(January_2025)_Boundaries_UK_BGC_V2.geojson")

#Check match... tick
table(itl3$ITL325CD %in% itl3.2digit.cv$ITL_code)
table(unique(itl3.2digit.cv$ITL_code) %in% itl3$ITL325CD)

#To spdep
itl3.sp <- as_Spatial(itl3)
neighbours <- poly2nb(itl3.sp, row.names = itl3.sp$ITL325NM)

#Warning about some not having neighbours - which is right, but let's just check it's the right ones
#That said, I can't see any without neighbours in the list, confused
for(i in 1:length(neighbours)) print(paste0(itl3.sp$ITL325NM[i]," -- neighbours: ", paste0(neighbours[[i]], collapse = " ") ))

#Ah - neighbours with none are set to zero, not an empty list element


#So, before we get onto full contig matrix things
#Let's just see what the general distribution of neighbouring pair econ correlations are

#We can work through each list element to get all the pairs...
#Though actually, turning into a full list of paired elements could be quicker

#So I prob want three things from it, staring at the Leeds / Bradford spearman over time plot
#Average spearman for whole time series
#standard dev
#slope (inc CIs, probably)

#If we can get for all pairs, can then compare neighbours to a bootstrap dist of random samples from each
#182^2 / 2 = 16471 rows, plenty for a distribution!
pairsofplaces <- expand.grid(unique(itl3.2digit.cv$Region_name),unique(itl3.2digit.cv$Region_name), stringsAsFactors = F)

# keep only those with place1 < place2, as quick way to keep unique
#Also takes care of rows with same name twice
pairsofplaces <- pairsofplaces[pairsofplaces$Var1 < pairsofplaces$Var2, ]

#Compare to number of pairs of neighbouring places... 840
length(unlist(neighbours))


#FIND THE FOLLOWING VALUES FOR EACH PAIR, KEEP ALL
#Pairwise spearman correlations over time for ranked GVA
#Adding in average for whole time series
#standard dev
#slope and CI
#Assumes the data already exists

#TEST
# debugonce(pair_spearman_summarystats)
pair_spearman_summarystats(pairsofplaces[50,])


#Run for all unique pairs, stick into dataframe
#https://stackoverflow.com/a/46899569/5023561


# debugonce(pair_spearman_summarystats)
#Not fast...
pairs <- asplit(as.matrix(pairsofplaces), MARGIN = 1)
results <- lapply(pairs, pair_spearman_summarystats)

#Convert to dataframe, add name pairs back in
results.df <- bind_rows(results)
results.df <- bind_cols(pairsofplaces, results.df)

#Check on distributions...
plot(density(results.df$mean, na.rm=T))
#Add in Leeds brad value as line
abline(v=results.df$mean[qg('leeds|bradford',results.df$Var1) & qg('leeds|bradford',results.df$Var2)], col="blue")



plot(density(results.df$sd, na.rm=T))
plot(hist(results.df$sd, na.rm=T))

#slight neg bias to the slopes...
plot(density(results.df$slope, na.rm=T))
abline(v=0, col="blue")
abline(v=results.df$slope[qg('leeds|bradford',results.df$Var1) & qg('leeds|bradford',results.df$Var2)], col="red")


#So remind me what each slope value is showing before we mull what that means:
#One slope is the direction of change for spearman correlations
#Between a pair of places in the UK
#Between the rank of their SIC sectors' GVA output in each year
#So - how have their economic sector output rankings changed together over time?
#If the slope is positive, they have become more similar (pending some SE check!)

#What this seems to suggest is that ON AVERAGE pairs of places have moved further apart

#What we then want to do is see what happens if we compare that to NEIGHBOURING PAIRS of places

#I can just add a "yes these are neighbours" flag to the same DF to begin to pull that out
#Before looking at where Leeds/Bradford/Rest of region sit...



# Generate pairs from spdep neighbour list
neighbouring.pairs <- do.call(rbind, lapply(seq_along(neighbours), function(i) {
  if (length(neighbours[[i]]) == 0) return(NULL)
  cbind(from = i, to = neighbours[[i]])
}))

# Convert to data frame
neighbouring.pairs.df <- as.data.frame(neighbouring.pairs)

#Remove duplicate undirected pairs (e.g., keep only one of 1-2 and 2-1)
pairs_df_sorted <- t(apply(pairs_df, 1, sort))  # sort each row so lower comes first
neighbouring.unique.pairs.df <- unique(as.data.frame(pairs_df_sorted))
colnames(neighbouring.unique.pairs.df) <- c("zone1", "zone2")

#Then get actual place names in there merged in, so we can then pick out those to flag in the full list
neighbouring.unique.pairs.df<- neighbouring.unique.pairs.df %>% 
  left_join(
    itl3 %>% st_set_geometry(NULL) %>% mutate(id = 1:nrow(itl3)) %>% select(id,ITL325NM),
    by = c('zone1' = 'id')
    ) %>% 
  rename(zone1_name = ITL325NM) %>% 
  left_join(
    itl3 %>% st_set_geometry(NULL) %>% mutate(id = 1:nrow(itl3)) %>% select(id,ITL325NM),
    by = c('zone2' = 'id')
    ) %>% 
  rename(zone2_name = ITL325NM) %>% 
  mutate(neighbourflag = 1)#merge this in to create flag
  

#Next: using the named neighbouring pairs
#Flag which are neighbours in the full list of pairs
#Have to check both pair orders, as not ordered alphabetically though unique
results.df <- results.df %>% 
  left_join(
    neighbouring.unique.pairs.df %>% select(zone1_name,zone2_name,neighbourflag),
    by = c('Var1' = 'zone1_name', 'Var2' = 'zone2_name')
  )

#Repeat for other way round, it'll create a copy neighbour column
results.df <- results.df %>% 
  left_join(
    neighbouring.unique.pairs.df %>% select(zone1_name,zone2_name,neighbourflag),
    by = c('Var1' = 'zone2_name', 'Var2' = 'zone1_name')
  )

#Create final neighbour flag column from both of those
results.df <- results.df %>% 
  mutate(
    neighbourflag = case_when(
      neighbourflag.x == 1 | neighbourflag.y == 1 ~ 1,
      .default = 0
    )
  ) %>% 
  select(-neighbourflag.x,-neighbourflag.y)

#We lost two pairs somewhere?  
table(results.df$neighbourflag)
  


#THAT'S THE FINAL PAIRS THING WITH FLAGGED NEIGHBOURS...

#Quick t test

t.test(x = results.df$mean[results.df$neighbourflag==1], y = results.df$mean[results.df$neighbourflag==0])

#roughly the same from lm presumably?
lm(data  = results.df, formula = mean ~ neighbourflag) %>% summary
plot(results.df$mean ~ results.df$neighbourflag)

#confirm means... tick
mean(results.df$mean[results.df$neighbourflag==1], na.rm = T)
mean(results.df$mean[results.df$neighbourflag==0], na.rm = T)

#And can do this...
p = results.df %>%
  ggplot(aes(x = factor(neighbourflag), y = mean)) +
  theme(panel.background = element_rect(color = "grey70")) +
  coord_flip() 

p + stat_slabinterval(side = "right") +
    labs(title = "stat_slabinterval()", subtitle = "side = 'left'") +
    geom_hline(yintercept = results.df$mean[qg('leeds|bradford',results.df$Var1) & qg('leeds|bradford',results.df$Var2)], colour = 'blue') +
    geom_hline(yintercept = results.df$mean[qg('leeds|wakefield',results.df$Var1) & qg('leeds|wakefield',results.df$Var2)], colour = 'green') +
    geom_hline(yintercept = results.df$mean[qg('leeds|kirklees',results.df$Var1) & qg('leeds|kirklees',results.df$Var2)], colour = 'red')

#Centring on Bradford
p + stat_slabinterval(side = "right") +
    labs(title = "stat_slabinterval()", subtitle = "side = 'left'") +
    geom_hline(yintercept = results.df$mean[qg('leeds|bradford',results.df$Var1) & qg('leeds|bradford',results.df$Var2)], colour = 'blue') +
    geom_hline(yintercept = results.df$mean[qg('bradford|wakefield',results.df$Var1) & qg('bradford|wakefield',results.df$Var2)], colour = 'green') +
    geom_hline(yintercept = results.df$mean[qg('bradford|kirklees',results.df$Var1) & qg('bradford|kirklees',results.df$Var2)], colour = 'red')




p = results.df %>%
  ggplot(aes(x = factor(neighbourflag), y = slope)) +
  theme(panel.background = element_rect(color = "grey70")) +
  coord_flip()

p + stat_slabinterval(side = "right") +
    labs(title = "stat_slabinterval()", subtitle = "side = 'left'") +
    geom_hline(yintercept = results.df$slope[qg('leeds|bradford',results.df$Var1) & qg('leeds|bradford',results.df$Var2)], colour = 'blue') +
geom_hline(yintercept = results.df$slope[qg('leeds|wakefield',results.df$Var1) & qg('leeds|wakefield',results.df$Var2)], colour = 'green') +
  geom_hline(yintercept = results.df$slope[qg('leeds|kirklees',results.df$Var1) & qg('leeds|kirklees',results.df$Var2)], colour = 'red')

#Centred on Bradford
p = results.df %>%
  ggplot(aes(x = factor(neighbourflag), y = slope)) +
  theme(panel.background = element_rect(color = "grey70")) +
  coord_flip()

p + stat_slabinterval(side = "right") +
    labs(title = "stat_slabinterval()", subtitle = "side = 'left'") +
    geom_hline(yintercept = results.df$slope[qg('leeds|bradford',results.df$Var1) & qg('leeds|bradford',results.df$Var2)], colour = 'blue') +
geom_hline(yintercept = results.df$slope[qg('bradford|wakefield',results.df$Var1) & qg('bradford|wakefield',results.df$Var2)], colour = 'green') +
  geom_hline(yintercept = results.df$slope[qg('bradford|kirklees',results.df$Var1) & qg('bradford|kirklees',results.df$Var2)], colour = 'red')


#Explore just neighbours
results.df %>% filter(neighbourflag == 1) %>% View

#Idea as a way to make a map:
#Find average of the means (argh) for each place - 
#That will be a summary of how similar they've been economically to neighbouring places... and can make a map from that

#Thusly
neighbourpairmeanmean <- results.df %>% 
  filter(neighbourflag == 1) %>% 
  group_by(Var1) %>% 
  summarise(meanmean = mean(mean, na.rm=T)) %>% 
  rename(place = Var1)

#Do the same for the other side then keep only unique name places
#Should cover all places, nearly? (apart from few without any neighbours)
neighbourpairmeanmean2 <- results.df %>% 
  filter(neighbourflag == 1) %>% 
  group_by(Var2) %>% 
  summarise(meanmean = mean(mean, na.rm=T))%>% 
  rename(place = Var2)

#Yeah, missing two, which should be the two islands (wight and anglesea)
allneighbourpairmeanmeans <- rbind(
  neighbourpairmeanmean,neighbourpairmeanmean2
) %>% 
  distinct(place, .keep_all = T)


#Errr map
itl3.meanmeanz <- itl3 %>% 
  select(place = ITL325NM) %>% 
  left_join(
    allneighbourpairmeanmeans
  )

tmap_mode('view')

tm_shape(itl3.meanmeanz) +
  tm_polygons('meanmean', 
              fill.scale = tm_scale_intervals(
                n = 7,
                style = "quantile"
              ), alpha = 0.75
              )








#GET CONTIG MATRIX
contiguity.matrix <- nb2mat(neighbours, zero.policy = T)

#Weights add to 1 over rows.
apply(contiguity.matrix,1,sum)


#Link values to spatial data prior to finding average neighbouring values

#GET SPATIAL LAG VALUES
#MULTIPLY CONTIG MATRIX BY PERCENT EMPLOYED
#TO GET AVERAGE EMPLOYMENT IN NEIGHBOURING ZONES
spatial.lag <- contiguity.matrix %*% city.wards$percentEmployed



## Proportion plots from that data----


itl3.2digit.cp <- itl3.2digit.cp %>% 
  group_by(Region_name,SIC07_description) %>%
  mutate(
    gva_movingav = rollapply(value,smoothband,mean,align='center',fill=NA),
    sector_regional_proportion_movingav = rollapply(sector_regional_proportion,smoothband,mean,align='center',fill=NA),
    sector_regional_percent_movingav = sector_regional_proportion_movingav * 100
  ) %>% 
  ungroup()

#Put Leeds and Bradford on different axes
leedsbradford <- itl3.2digit.cp %>% 
  filter(
    qg('leeds|bradford',Region_name),
    !is.na(sector_regional_proportion_movingav),
    sector_regional_percent_movingav > 1
  ) %>% 
  select(Region_name,SIC07_description,year,gva_movingav,sector_regional_percent_movingav) %>% 
  pivot_wider(names_from = Region_name, values_from = c(gva_movingav,sector_regional_percent_movingav)) %>% 
  mutate(
    gva_both = paste0('GVA Leeds: ',gva_movingav_Leeds %>% round(2) , '\nGVA Bradford: ', gva_movingav_Bradford %>% round(2))
  )


p <- twod_generictimeplot_multipletimepoints(
  df = leedsbradford,
  category_var = SIC07_description,
  x_var = sector_regional_percent_movingav_Leeds,
  y_var = sector_regional_percent_movingav_Bradford,
  label_var = gva_movingav_Leeds,
  timevar = year,
  times = c(1999:2022) 
)

# p + theme(aspect.ratio=1) +
p + coord_fixed() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, size = 3) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab(paste0('Leeds')) +
  ylab(paste0('Bradford'))





## SLOPE GRIDS FOR THE LATEST DATA----

#Grabbing code from YPERN_dataexplore/rmarkdown/SIC_sectors_growthgrids.Rmd

### SECTIONS

#1998 to 2023 available

# debugonce(get_slope_and_se_safely)
slopes.log1523 <- get_slope_and_se_safely(data = itl3.sections.cv %>% filter(year %in% 2015:2023), Region_name,SIC07_description, y = log(value), x = year)

p <- slopeDiffGrid(slope_df = slopes.log1523, confidence_interval = 95, column_to_grid = Region_name, column_to_filter = SIC07_description , filterval = 'Information and communication')

p + coord_fixed()

#Kay, that's the principle - very hard to read! But... let's output em all and have a look
#Including for all two digit


#Let's nab 'proportion of slopes' plot from the SYMCA doc to do this -->






## GVA SLOPES: COUNT THEM, FIND PROPORTIONS, COMBINE----

# Via GVA_region_by_sector_explore.R in data_explore

#Add in LQ details
# itl3.sections.cv <- itl3.sections.cv %>% 
  

#Use the slope diff grids to count which are statistically higher or lower than others
#Both within WY and compared to elsewhere
#And make proportion plots, counting up which sectors stand out in their growth slopes
#Ideally for raw GVA and then GVA per job
#Can potentially do for different CIs

#Stick all in function to repeat easily
plotSlopeCounts <- function(df,placename,startdate,enddate){

  slopes.log <- get_slope_and_se_safely(data = df %>% filter(year %in% startdate:enddate), Region_name,SIC07_description, y = log(value), x = year)
  
  # p <- slopeDiffGrid(slope_df = slopes.log1521, confidence_interval = 95, column_to_grid = SIC07_description, column_to_filter = Region_name, filterval = placename)
  # 
  # p + coord_fixed()
  
  confinteral = 95
  
  #Ah, good ol' past me put this in (returnddata = T)
  slopes.data <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = confinteral, column_to_grid = SIC07_description, column_to_filter = Region_name, filterval = placename, returndata = T)
  
  
  #Just need to count CIs overlap along one dimension of the grid (i.e. not its inverse at 90 degrees). 
  #So need the correct unique pairs don't we?
  #gridcol2 is on the y axis and has the 'if this sector positively different, show green' values.
  #Same values then - it's just for gridcol2 sectors or places that we're counting the number of sigs
  #Trickier for all sectors for all places, different loop needed to pull out SY, but step at a time
  sy_slopediffcount <- slopes.data %>% 
    mutate(slopetype = case_when(
      !CIs_overlap & slopediff > 0 ~ 'sig pos',
      !CIs_overlap & slopediff < 0 ~ 'sig neg',
      .default = "not sig"
    )) %>% 
    mutate(slopetype = factor(slopetype, levels = c('sig pos','sig neg','not sig'))) %>% 
    # mutate(slopetype = factor(slopetype)) %>% 
    group_by(gridcol2,slopetype) %>% 
    summarise(count = n()) %>% 
    complete(slopetype, fill = list(count = 0)) %>% 
    group_by(gridcol2) %>% 
    mutate(percent = (count / sum(count)) * 100) %>% 
    mutate(source = paste0(placename,' internal'))
  
  
  
  
  #For SY vs places, bit trickier. Have to run for each sector and pull out SY values each time
  getsectorslopecounts <- function(sector){
    
    slopes.data <- slopeDiffGrid(slope_df = slopes.log, confidence_interval = confinteral, column_to_grid = Region_name, column_to_filter = SIC07_description, filterval = sector, returndata = T)
    
    #Again pulling out values from grid2 perspective, for South Yorkshire each time
    slopes.data <- slopes.data %>% filter(gridcol2 == placename)
    
    #Now count slopes in same way
    sector_slopediffcount <- slopes.data %>% 
      mutate(slopetype = case_when(
        !CIs_overlap & slopediff > 0 ~ 'sig pos',
        !CIs_overlap & slopediff < 0 ~ 'sig neg',
        .default = "not sig"
      )) %>% 
      mutate(slopetype = factor(slopetype, levels = c('sig pos','sig neg','not sig'))) %>% 
      group_by(slopetype) %>% 
      summarise(count = n()) %>% 
      complete(slopetype, fill = list(count = 0)) %>% 
      mutate(percent = (count / sum(count)) * 100) %>% 
      mutate(gridcol2 = sector, source = paste0(placename,' to other places'))#add in sector name
    
  }
  
  allsectorslopecounts <- purrr::map(unique(slopes.log$SIC07_description), getsectorslopecounts) %>% bind_rows()
  
  
  #both
  allslopecounts <- rbind(sy_slopediffcount,allsectorslopecounts) %>% rename(sector = gridcol2) 
  
  #Do separately to make bespoke factor order from subset
  # allslopecounts <- allslopecounts %>% mutate(
  #     sector = factor(sector, ordered = T, levels = unique(itl2.cvs$SIC07_description)[order(allslopecounts %>% filter(slopetype=='sig pos', source == 'SY to other places') %>% select(percent) %>% ungroup() %>% pull)]),
  #   )
  
  #order(allslopecounts %>% filter(slopetype=='sig pos', source == 'SY to other places') %>% select(percent) %>% ungroup() %>% pull)
  
  
  #For version where only >2% of economy per sector. Or label and split
  # ordered.itl <- itl3.sections.cv %>% filter(
  #   grepl(x = Region_name, pattern = 'South Y'),
  #   year == 2023
  #   # year == 2019
  # ) %>% 
  #   mutate(regional_percent = sector_regional_proportion *100) %>% 
  #   select(SIC07_description,regional_percent, LQ) %>% 
  #   # arrange(-LQ) %>% 
  #   arrange(-regional_percent) %>%
  #   slice(1:length(unique(itl2.cps$SIC07_description))) %>% 
  #   rename(sector = SIC07_description)
  # 
  # 
  # allslopecounts <- allslopecounts %>% 
  #   left_join(ordered.itl2.cps, by = 'sector') %>% 
  #   mutate(lessthan2percent = ifelse(regional_percent < 2, 'Less than 2% GVA', 'More than 2% GVA')) 
  
  #Add slope colours and values back in then use for axis text as in grids
  #Slopes match to sectors, so can take from any source with those in here
  allslopecounts <- allslopecounts %>% 
    left_join(
      slopes.data %>% select(gridcol2,slopecolour_y,slopetwo_percent,min.citwo_percent,max.citwo_percent) %>% distinct(gridcol2, .keep_all = T) %>% rename(sector = gridcol2), by = 'sector'
    ) %>% 
    ungroup() %>% 
    mutate(
      sector = gsub(x = sector, pattern = ' and ', replacement = ' / '),
      sector = gsub(x = sector, pattern = 'of |activities|equipment|products', replacement = '')
    ) 
  
  
  # #Get values of percent for correct unique order for factor
  # o <- allslopecounts %>% filter(slopetype=='sig pos', source == 'SY to other places') %>% select(count) %>% ungroup() %>% pull
  # order(o)
  # sort(o)
  # 
  # View(allslopecounts %>% filter(slopetype=='sig pos', source == 'SY to other places'))
  # 
  # View(data.frame(o,order(o)))
  # 
  # x = sample.int(10, 10, replace = F)
  # View(data.frame(x,order(x)))
  # x = runif(10)
  # View(data.frame(x,order(x)))
  
  
  # allslopecounts <- allslopecounts %>%
  #   mutate(
  #     sector = paste0(sector,' (',slopetwo_percent,'% CI: ',min.citwo_percent,'%,',max.citwo_percent,'%)'),
  #     sector = factor(sector, ordered = T, levels = unique(sector)[order(allslopecounts %>% filter(slopetype=='sig pos', source == paste0(placename,' to other places')) %>% arrange(sector) %>% select(count) %>% pull)])
  #   )#loses factor
  # 
  
  #Version with no factor order, to keep alphabetical...
  allslopecounts <- allslopecounts %>%
    mutate(
      sector = paste0(sector,' (',slopetwo_percent,'% CI: ',min.citwo_percent,'%,',max.citwo_percent,'%)')
    )
  
  
  # unique(allslopecounts$sector)[order(allslopecounts %>% filter(slopetype=='sig pos', source == 'SY to other places') %>% select(percent) %>% ungroup() %>% pull)]
  
  
  
  
  #Plot. One for neg one for pos
  # ggplot() +
  #   geom_bar(data = allslopecounts %>% filter(slopetype == 'sig neg', sector!= 'Real estate activities'), aes(x = sector, y = -percent, fill = source), stat = 'identity', position = 'dodge', alpha = 0.7) +
  #   geom_bar(data = allslopecounts %>% filter(slopetype == 'sig pos', sector!= 'Real estate activities'), aes(x = sector, y = percent, fill = source), stat = 'identity', position = 'dodge') +
  #   geom_hline(yintercept = 0, size = 2) +
  #   # scale_fill_distiller(type = 'qual', direction = -1) +
  #   # scale_fill_brewer(palette = 'Dark2', direction = 1) +
  #   scale_fill_brewer(palette = 'Paired', direction = 1) +
  #   coord_flip() +
  #   facet_wrap(~lessthan2percent, ncol = 1, scales = 'free_y')
  
  
  
  #Removing < 2%
  
  #Pull out slope colours
  slopecolours_y <- allslopecounts %>% 
    filter(slopetype == 'sig neg', !grepl('Real estate',sector,ignore.case = T)) %>% 
    # filter(slopetype == 'sig neg', !grepl('Real estate',sector,ignore.case = T), regional_percent > 2) %>% 
    distinct(sector, .keep_all = T) %>% 
    arrange(sector) %>% #will arrange by factor
    select(slopecolour_y) %>% 
    pull
  
  p <- ggplot() +
    geom_bar(data = allslopecounts %>% filter(slopetype == 'sig neg', !grepl('Real estate',sector,ignore.case = T)), 
             aes(x = sector, y = -percent, fill = source), stat = 'identity', position = 'dodge', alpha = 0.7) +
    geom_bar(data = allslopecounts %>% filter(slopetype == 'sig pos', !grepl('Real estate',sector,ignore.case = T)), 
             aes(x = sector, y = percent, fill = source), stat = 'identity', position = 'dodge') +
    geom_hline(yintercept = 0, size = 2) +
    # scale_fill_distiller(type = 'qual', direction = -1) +
    # scale_fill_brewer(palette = 'Dark2', direction = 1) +
    scale_fill_brewer(palette = 'Paired', direction = 1) +
    coord_flip() +
    theme_bw() +
    theme(
      axis.text.y = element_text(colour = slopecolours_y),
      legend.title = element_blank()
    ) +
    ylab('negative << Percent of slopes with significant differences >> positive') +
    xlab('Sector (text gives yearly change + 95% confidence intervals, bold text are significant trends)') 
  
  #Orig with regional % filter still in place
  # ggplot() +
  #   geom_bar(data = allslopecounts %>% filter(slopetype == 'sig neg', !grepl('Real estate',sector,ignore.case = T), regional_percent > 2), 
  #            aes(x = sector, y = -percent, fill = source), stat = 'identity', position = 'dodge', alpha = 0.7) +
  #   geom_bar(data = allslopecounts %>% filter(slopetype == 'sig pos', !grepl('Real estate',sector,ignore.case = T), regional_percent > 2), 
  #            aes(x = sector, y = percent, fill = source), stat = 'identity', position = 'dodge') +
  #   geom_hline(yintercept = 0, size = 2) +
  #   # scale_fill_distiller(type = 'qual', direction = -1) +
  #   # scale_fill_brewer(palette = 'Dark2', direction = 1) +
  #   scale_fill_brewer(palette = 'Paired', direction = 1) +
  #   coord_flip() +
  #   theme_bw() +
  #   theme(
  #     axis.text.y = element_text(colour = slopecolours_y),
  #     legend.title = element_blank()
  #   ) +
  #   ylab('negative << Percent of slopes with significant differences >> positive') +
  #   xlab('Sector (text gives yearly change + 95% confidence intervals, bold text are significant trends)') 

  return(list(plot = p, data = allslopecounts))
  
}


leeds.2dig <- plotSlopeCounts(itl3.2digit.cv, 'Leeds',2014,2023)
bradford.2dig <- plotSlopeCounts(itl3.2digit.cv, 'Bradford',2014,2023)

leeds.2dig$plot + bradford.2dig$plot


leeds.sections <- plotSlopeCounts(itl3.sections.cv, 'Leeds',2014,2023)
bradford.sections <- plotSlopeCounts(itl3.sections.cv, 'Bradford',2014,2023)

leeds.sections$plot + bradford.sections$plot


# # leeds.sections <- plotSlopeCounts(itl3.sections.cv, 'Leeds',2014,2023)
# bradford.sections <- plotSlopeCounts(itl3.sections.cv, 'Sheffield',2014,2023)

# leeds.sections + bradford.sections


#Look at all those correlated across both...
#For 2 digit
# leeds.2dig$data %>% View


both <- rbind(leeds.2dig$data,bradford.2dig$data)

#Convert internal / external into own column
#Need to separate out sector name from numbers too - otherwise sectors don't match
both <- both %>% 
  mutate(source = gsub('to other places','external',source)) %>% 
  separate_wider_delim(source, delim = ' ', names = c('place','internal_external')) %>% 
  separate_wider_delim(sector, delim = '(', names = c('sector','CIs')) %>% 
  mutate(CIs = gsub("\\)","",CIs))

#Then - we want the two places on their own axes, thus...
both.wide.pos <- both %>%
  filter(slopetype == 'sig pos',internal_external == 'external') %>%
  select(sector,place,percent) %>%
  pivot_wider(names_from = place, values_from = percent)

firstplace <- both %>%
  filter(slopetype == 'sig pos', place == unique(both$place)[1]) %>%
  select(place,sector,percent,internal_external)







#Plot
ggplot(both, aes(x = ))







