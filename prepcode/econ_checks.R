#Checks on data wrangling from joined GVA / BRES data
library(tidyverse)
library(zoo)
library(ggrepel)
source('functions/misc_functions.R')


# USING LATEST DATA FOR GVA VS JOBS PERCENT CHANGE PLOTS----

gva.bres <- read_csv('data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_chainedvolume_ITL2_SIC_SECTION_MINUSimputedrent_2022.csv')

#Taken from YPERN_dataexplore Rmd SYMCA_growth_DanOlner...



#First, let's process and smooth
smoothband = 3

gva.bres <- gva.bres %>% 
  mutate(gvaperFT = (gva / JOBCOUNT_FULLTIME) * 1000000 ) %>% #Back to pounds from millions
  group_by(GEOGRAPHY_NAME,SIC07_description) %>% #dates are already in order for lag calcs
  mutate(
    gva_movingav = rollapply(gva,smoothband,mean,align='center',fill=NA),
    jobcount_FT_movingav = rollapply(JOBCOUNT_FULLTIME,smoothband,mean,align='center',fill=NA),
    gvaperFT_movingav_1000s = rollapply(gvaperFT/1000,smoothband,mean,align='center',fill=NA)
  )
  

#good for 3 year avs
starty=2016
endy=2021

p <- twod_generictimeplot_normalisetozero(
  df = gva.bres %>% filter(SIC07_description=='Information and communication') %>% mutate(`gva/FT 1000s` = gvaperFT/1000),
  category_var = GEOGRAPHY_NAME,
  x_var = gva_movingav,
  y_var = jobcount_FT_movingav,
  timevar = DATE,
  label_var = `gva/FT 1000s`,
  category_var_value_to_highlight = 'South Yorkshire',
  start_time = starty,
  end_time = endy
)


xrange_adjust = diff(range(p[[2]]$x_pct_change)) * 0.1
yrange_adjust = diff(range(p[[2]]$y_pct_change)) * 0.1

#keep these for next plot
xmin = min(p[[2]]$x_pct_change) - xrange_adjust
xmax = ifelse(max(p[[2]]$x_pct_change) > 0,max(p[[2]]$x_pct_change) + xrange_adjust,0)#hack for health, need to make generic
ymin = min(p[[2]]$y_pct_change) - yrange_adjust
ymax = max(p[[2]]$y_pct_change) + yrange_adjust 

p[[1]] + coord_fixed(
  xlim = c(
    xmin,xmax
  ),
  ylim = c(
    ymin,ymax
  )
) + annotate("text", x = 90, y = -80, label = paste0(starty,' to ',endy), size = 10)
