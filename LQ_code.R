knitr::opts_chunk$set(echo = TRUE, warning = F, error = F, message = F, comment=NA)

# knitr::purl(input = "sector_locationquotients_and_proportions.qmd", output = "LQ_code.R",documentation = 0)
# 

library(tidyverse)
library(sf)
library(tmap)
library(plotly)
library(zoo)#For moving average function
library(ggrepel)#For self-adjusting plot labels
source('functions/misc_functions.R')
options(scipen = 99)#Avoids scientific notation

itl2.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL2_SIC_2DIGIT_LONG_2022.csv')


lq1998 <- add_location_quotient_and_proportions(
  df = itl2.cp %>% filter(year == 1998),
  regionvar = Region_name,
  lq_var = SIC07_description,
  valuevar = value
)


itl2.cp <- itl2.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()


itl2.cp %>% filter(
  Region_name == 'South Yorkshire',
  year == max(year)#get latest year in data
  ) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:10)


itl2.cp %>% filter(
  Region_name == 'Greater Manchester',
  year == max(year)#get latest year in data
  ) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:10)


itl2.cp %>% filter(
  Region_name == 'Merseyside',
  year == 2021
  ) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) %>% 
  slice(1:10)


#Find the geographical variation of sectors using the LQ spread
LQspread <- itl2.cp %>% 
  filter(year == max(year)) %>% #get latest data
  group_by(SIC07_description) %>% 
  summarise(LQ_spread = diff(range(LQ))) %>% 
  arrange(-LQ_spread)

#Show top 5
LQspread[1:5,]

#Load ITL2 map data using the sf library
itl2.geo <- st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

#Join map data to a subset of the GVA data
sector_LQ_map <- itl2.geo %>% 
  right_join(
    itl2.cp %>% filter(
      year==max(year),#get latest data
      SIC07_description == LQspread$SIC07_description[4]#picking out the fourth highest geographical spread sector
      ),
    by = c('ITL221CD'='ITL_code')
  )


#Plot map
tm_shape(sector_LQ_map) +
  tm_polygons('LQ_log', n = 9) +
  tm_layout(title = 'LQ spread of\nBasic metals\nAcross ITL2 regions', legend.outside = T)


#Use
#LQ_slopes %>% filter(slope==0)
#To see which didn't get slopes (only 8 rows in the current data)
LQ_slopes <- compute_slope_or_zero(
  data = itl2.cp, 
  Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = year)


#Filter down to a single year
yeartoplot <- itl2.cp %>% filter(year == max(year))#use latest year

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('Region_name','SIC07_description')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- itl2.cp %>% 
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

place = 'Merseyside'

#Get a vector with sectors ordered by the place's LQs, descending order
#Use this next to factor-order the SIC sectors
sectorLQorder <- itl2.cp %>% filter(
  Region_name == place,
  year == max(year)#use latest data
) %>% 
  arrange(-LQ) %>% 
  select(SIC07_description) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC07_description <- factor(yeartoplot$SIC07_description, levels = sectorLQorder, ordered = T)

# #Code for saving here, won't run
# ggsave(plot = p, filename = paste0('miscimages/gva_',gsub(' ','',place),'_plot.png'), width = 10, height = 14)
# 

# Reduce to SY LQ 1+
lq.selection <- yeartoplot %>% filter(
  Region_name == place,
  # slope > 1,#LQ grew relatively over time
  LQ > 1
  )

#Keep only sectors that were LQ > 1 from the main plotting df
yeartoplot <- yeartoplot %>% filter(
  SIC07_description %in% lq.selection$SIC07_description
)


# p <- LQ_baseplot(df = yeartoplot, alpha = 0.1, sector_name = SIC07_description,
#                  LQ_column = LQ, change_over_time = slope)
# 
# p <- addplacename_to_LQplot(df = yeartoplot, placename = 'Merseyside',
#                             plot_to_addto = p, shapenumber = 16,
#                             min_LQ_all_time = min_LQ_all_time, max_LQ_all_time = max_LQ_all_time,#Range bars won't appear if either of these not included
#                             value_column = value, sector_regional_proportion = sector_regional_proportion,#Sector size numbers won't appear if either of these not included
#                             region_name = Region_name,#The next four, the function needs them all
#                             sector_name = SIC07_description,
#                             change_over_time = slope,
#                             LQ_column = LQ
#                             )
# 
# p

# #Code for saving here, won't run
# ggsave(plot = p, filename = paste0('miscimages/gva_',gsub(' ','',place),'_plot_LQmorethan1.png'), width = 9, height = 9)
# 

# #Repeat but overlay other places
# p <- LQ_baseplot(df = yeartoplot, alpha = 0, sector_name = SIC07_description,
#                  LQ_column = LQ, change_over_time = slope)
# 
# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p,
#                             placename = 'Greater Manchester', shapenumber = 23,
#                             region_name = Region_name,#The next four, the function needs them all
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
# 
# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p,
#                             placename = 'South Yorkshire', shapenumber = 22,
#                             region_name = Region_name,
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
# 
# p <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p,
#                             placename = place, shapenumber = 16,
#                             min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
#                             value_column = value, sector_regional_proportion = sector_regional_proportion,#include numbers
#                             region_name = Region_name,
#                             sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ)
# p <- p +
#   annotate(
#     "text",
#     label = "Greater Manchester: diamonds\nSouth Yorkshire: squares",
#     x = 0.05, y = 'Manufacture of rubber and plastic products',
# 
#   )
# 
# p

# ggsave(plot = p, filename = paste0('miscimages/gva_',gsub(' ','',place),'_plot_LQmorethan1_3places.png'), width = 9, height = 9)
# 

#Pick a sector to plot separately for all places
#Use grepl as a shortcut to search for sector names
sector <- itl2.cp$SIC07_description[grepl('petroleum', itl2.cp$SIC07_description ,ignore.case = T)] %>% unique

timeplot <- itl2.cp %>% 
  filter(SIC07_description == sector) 

#Use zoo's rollapply function to get a moving average
timeplot <- timeplot %>% 
  group_by(Region_name) %>% 
  arrange(year) %>% 
  mutate(
    LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),
    percent_movingav = rollapply(sector_regional_proportion * 100,3,mean,align='right',fill=NA)
  )

#Or pick top size values
#Largest % in 2021
largest_percents <- timeplot %>% 
  filter(year == max(year)) %>% #latest data
  arrange(-percent_movingav)

#Keep only the top ten places and order them
timeplot <- timeplot %>% 
  mutate(Region_name = factor(Region_name, ordered = T, levels = largest_percents$Region_name)) %>% 
  filter(Region_name %in% largest_percents$Region_name[1:10])

#Mark the ITL of interest so it can be clearer in the plot
timeplot <- timeplot %>%
  mutate(
    ITL2ofinterest = ifelse(Region_name == place, 'ITL of interest','other'),
  )

ggplot(timeplot %>% 
         rename(`ITL region` = Region_name) %>% 
         filter(!is.na(percent_movingav)),#remove NAs from dates so the x axis doesn't show them
       aes(x = year, y = percent_movingav, colour = `ITL region`, size = ITL2ofinterest, linetype = ITL2ofinterest, group = `ITL region`)) +
  geom_point() +
  geom_line() +
  scale_size_manual(values = c(2.5,1)) +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('Regional GVA percent') +
  guides(size = "none", linetype = "none") +
  ggtitle(
    paste0(sector,'\n', place, ' highlighted')
      ) +
    theme(plot.title = element_text(face = 'bold'))


sector <- itl2.cp$SIC07_description[grepl('motor vehicles', itl2.cp$SIC07_description ,ignore.case = T)] %>% unique

timeplot <- itl2.cp %>% 
  filter(SIC07_description == sector) 

timeplot <- timeplot %>% 
  group_by(Region_name) %>% 
  arrange(year) %>% 
  mutate(
    LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),
    percent_movingav = rollapply(sector_regional_proportion * 100,3,mean,align='right',fill=NA)
  )


timeplot <- timeplot %>%
  mutate(
    ITL2ofinterest = ifelse(Region_name == place, 'ITL of interest','other'),
  )

p <- ggplot(timeplot %>% 
         rename(`ITL region` = Region_name) %>% 
         filter(!is.na(percent_movingav)),#remove NAs from dates so the x axis doesn't show them, 
       aes(x = year, y = percent_movingav, colour = ITL2ofinterest, size = ITL2ofinterest, group = `ITL region`)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  ylab('Regional GVA percent (log 10)') +
  scale_size_manual(values = c(2,0.5)) +
  scale_colour_manual(values = c('black','grey')) +
  ggtitle(
    paste0(sector,'\n', place, ' highlighted')
      ) +
    theme(plot.title = element_text(face = 'bold'))

p


# ggplotly(p, tooltip = c("ITL region"))

#Look just at place of interest
#And arrange by the 'growth' slope.
place_slopes <- yeartoplot %>% 
  filter(Region_name == place) %>% 
  arrange(-slope)

#Use that to filter the main df and order sectors by which slope is largest
timeplot.sectors <- itl2.cp %>% 
  filter(Region_name == place) %>% 
  mutate(SIC07_description = factor(SIC07_description, ordered = T, levels = place_slopes$SIC07_description))

#Moving averages
timeplot.sectors <- timeplot.sectors %>% 
  group_by(SIC07_description) %>% 
  arrange(year) %>% 
  mutate(
    LQ_movingav = rollapply(LQ,3,mean,align='right',fill=NA),
    percent_movingav = rollapply(sector_regional_proportion * 100,3,mean,align='right',fill=NA)
  )

#Filter down to top ten LQ growth sectors
timeplot.sectors <- timeplot.sectors %>% 
  filter(
    SIC07_description %in% place_slopes$SIC07_description[1:10]
  )

#Plot GVA percent of the largest LQ growth sectors
ggplot(timeplot.sectors %>% 
         rename(Sector = SIC07_description) %>% 
         filter(!is.na(percent_movingav)),#remove NAs from dates so the x axis doesn't show them
       aes(x = year, y = percent_movingav, colour = Sector, group = Sector)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('GVA percent') +
  guides(size = "none", linetype = "none") +
   ggtitle(
    paste0('Top ten sectors by LQ growth trend\n', place)
      ) +
    theme(plot.title = element_text(face = 'bold'))




# p <- twod_proportionplot(
#   df = itl2.cp,
#   x_regionnames = place,
#   y_regionnames = unique(itl2.cp$Region_name[itl2.cp$Region_name != place]),
#   regionvar = Region_name,
#   category_var = SIC07_description,
#   valuevar = value,
#   timevar = year,
#   start_time = 1998,
#   end_time = max(itl2.cp$year), #latest timepoint
#   compasspoints_to_display = c('SE','NE')
# )
# 
# #add some extras
# p <- p +
#   xlab(paste0(place, ' GVA proportion')) +
#   ylab(paste0('UK GVA proportion (MINUS ',place,')')) +
#   coord_fixed(xlim = c(0.1,12), ylim = c(0.1,12)) +  # good for log scale
#   scale_y_log10() +
#   scale_x_log10()
# 
# p
# 

# ggsave(plot = p, filename = paste0('miscimages/2D_LCR_plot1_log.png'), width = 12, height = 10)
# 

# #Northern England
# north <- itl2.cp$Region_name[grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y', itl2.cp$Region_name, ignore.case = T)] %>% unique
# 
# #South England
# south <- itl2.cp$Region_name[!grepl('Greater Manc|Merseyside|West Y|Cumbria|Cheshire|Lancashire|East Y|North Y|Tees|Northumb|South Y|Scot|Highl|Wales|Ireland', itl2.cp$Region_name, ignore.case = T)] %>% unique
# 
# p <- twod_proportionplot(
#   df = itl2.cp,
#   regionvar = Region_name,
#   category_var = SIC07_description,
#   valuevar = value,
#   timevar = year,
#   start_time = 2008,
#   end_time = max(itl2.cp$year), #latest timepoint
#   # start_time = 1998,
#   # end_time = 2007,
#   compasspoints_to_display = c('SE','SW'),
#   # compasspoints_to_display = c('NE','NW'),
#   y_regionnames = north,
#   x_regionnames = south
# )
# 
# #add these after
# p <- p +
#   xlab('Southern England GVA proportions') +
#   ylab('North England GVA proportions') +
#   scale_y_log10() +
#   scale_x_log10() +
#   coord_fixed(xlim = c(0.1,11), ylim = c(0.1,11))# good for log scale
# 
# p
# 

# ggsave(plot = p, filename = paste0('miscimages/2D_NS_plot2_t2.png'), width = 12, height = 10)
# 
