#Checks on 2025 regional econ data
#Already output using regionalGVA_processing.R - see there for the zone / sector changes implemented this year

#Getting some basics.
#Taking from online guide then using latest data
#Can't just use full online guide as much of it uses BRES linked...
library(tidyverse)
library(sf)#for geo stuff
library(tmap)#for mapping 
library(zoo)#for making smoothed moving averages
library(patchwork)#combine ggplots easily
library(plotly)#For interactive plots
library(RColorBrewer)

source('functions/misc_functions.R')

#Set ggplot theme
# theme_set(theme_light())




#Chained vol real term change vals over time----

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


# SAME FOR ITL3----

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


# REPEAT LEEDS/BRADFORD BUT DO SECTION PROPORTIONS OVER TIME (FOR BETTER COMPARISON)----

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






# SIC 2 DIGIT - REPEAT LEEDS/BRADFORD BUT DO PROPORTIONS OVER TIME (FOR BETTER COMPARISON)----

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

place = itl3.2digit.cp$Region_name[qg('Leeds',itl3.2digit.cp$Region_name)] %>% unique

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





# LEEDS V BRADFORD LQ PLOT FOR SECTIONS----

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





# LEEDS V BRADFORD LQ PLOT FOR 2 DIGIT----

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



#Proportion plots from that data----


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











