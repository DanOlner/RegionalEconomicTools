#A few bits of data wrangling taken from different places
library(tidyverse)
library(sf)#for geo stuff
library(tmap)#for mapping 
library(zoo)#for making smoothed moving averages
library(patchwork)#combine ggplots easily
library(plotly)#For interactive plots

source('functions/misc_functions.R')

#Set ggplot theme
theme_set(theme_light())


# SOUTH YORKSHIRE PERCENT MANUF PLOT----

gva.jobs.ITL2 <- read_csv('https://raw.githubusercontent.com/DanOlner/RegionalEconomicTools/refs/heads/gh-pages/data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_chainedvolume_ITL2_SIC_SECTION_MINUSimputedrent_2022.csv') 

#Also want to filter out < 5% size zones, don't want silly percent change values
#% size will take care of job size too, or should

#But we need CP for this to get proportions...
itl2.cp <- read_csv('https://raw.githubusercontent.com/DanOlner/RegionalEconomicTools/refs/heads/gh-pages/data/regionalGVA/regionalGVA_currentprices_ITL2_SIC_SECTION_LONG_2022.csv') %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows() %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    sector_regional_proportion_movingav = rollapply(sector_regional_proportion,3,mean,align='center',fill=NA),
    LQ_movingav = rollapply(LQ,3,mean,align='center',fill=NA)
  ) %>% 
  ungroup()
  
sectorname = itl2.cp$SIC07_description[qg('manuf',itl2.cp$SIC07_description)] %>% unique

mostrecentvals <- itl2.cp %>% filter(
  SIC07_description == sectorname,
  year == max(year)-1
) %>% 
  mutate(regional_percent = sector_regional_proportion_movingav *100, LQ = LQ_movingav) %>%#movinav version 
  # mutate(regional_percent = sector_regional_proportion_movingav *100) %>% 
  select(Region_name,regional_percent, LQ) %>%
  arrange(-regional_percent) %>% 
  print(n = 40)

#VIZ PERCENT CHANGE
#Currently movin av version

#Reduce to places where sector makes up more than x% of the regional econ
#Or use this version to keep all
# itl2.viz <- itl2.gvaperjob.movingavs

smoothband = 3

gva.jobs.ITL2 <- gva.jobs.ITL2 %>% 
  mutate(gvaperjob = gva/JOBCOUNT_FULLTIME) %>%
  group_by(GEOGRAPHY_NAME,SIC07_description) %>%
  mutate(
    jobcount_movingav = rollapply(JOBCOUNT_FULLTIME,smoothband,mean,align='center',fill=NA),
    gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
    `gva/job moving av` = rollapply(gvaperjob * 1000,smoothband,mean,align='center',fill=NA)
  ) %>% 
  ungroup()



itl2.viz <- gva.jobs.ITL2 %>% 
  filter(
    GEOGRAPHY_NAME %in%  mostrecentvals$Region_name[mostrecentvals$regional_percent > 5],
    # Region_name %in%  mostrecentvals$Region_name[mostrecentvals$LQ > 1],
    SIC07_description == sectorname
  )



#Add into 2D percent plot
p <- twod_percentplot(
  df = itl2.viz, 
  category_var = GEOGRAPHY_NAME,
  x_var = gva_movingav,
  y_var = jobcount_movingav,
  timevar = DATE,
  label_var = `gva/job moving av`,
  category_var_value_to_highlight = 'South Yorkshire',
  start_time = 2016,
  end_time = 2021
)

p <- p + xlab("GVA percent change 2015-17 to 2020-22 average") +
ylab("FT JOBS percent change 2015-17 to 2020-22 average")

ggsave(plot = p, filename = 'local/outputs/sy_percentplot.png', width = 14, height = 10)


#Wiggle plot

#Reduce to smaller list

itl2.viz <- gva.jobs.ITL2 %>% 
  filter(
    GEOGRAPHY_NAME %in%  mostrecentvals$Region_name[mostrecentvals$regional_percent >= 12],
    # Region_name %in%  mostrecentvals$Region_name[mostrecentvals$LQ > 1],
    SIC07_description == sectorname
  )


p <- twod_generictimeplot_multipletimepoints(
  df = itl2.viz %>% filter(
    qg('manuf',SIC07_description)
  ),
  category_var = GEOGRAPHY_NAME,
  # x_var = gva,
  # y_var = JOBCOUNT_FULLTIME,
  # label_var = gvaperjobFT,
  x_var = gva_movingav,
  y_var = jobcount_movingav,
  label_var = `gva/job moving av`,
  timevar = DATE,
  times = c(2016:2021) 
)

p + theme(aspect.ratio=1) +
  # scale_x_log10() +
  # scale_y_log10() +
  xlab(paste0("GVA (",smoothband," year moving average)")) +
  ylab(paste0("Job count FT (",smoothband," year moving average)"))





