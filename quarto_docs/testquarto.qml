---
title: "Yorkshire and The Humber sector analysis (November 2025)"
author:
  - name: "Dan Olner"
format: 
  html:
    output-file: "Yorkshire_n_Humber_sectors_Nov2025"
    fig-width: 8
    fig-height: 8
execute:
  echo: false
  message: false
  warning: false
fig-responsive: false
collapse: false
navbar: false
sidebar: false
---

```{r setup}
library(tidyverse)
# library(spdep)
library(sf)#for geo stuff
# library(tmap)#for mapping 
library(zoo)#for making smoothed moving averages
library(patchwork)#combine ggplots easily
library(plotly)#For interactive plots
# # library(ggdist)
# library(tidyr)
# library(distributional)
library(RColorBrewer)

source('../functions/misc_functions.R')
source('../functions/adhoc_functions.R')
#source('functions/misc_functions.R')
#source('functions/adhoc_functions.R')

options(scipen = 999)

#Set ggplot theme
theme_set(theme_light())

corecities = readRDS('../data/corecitiesvector.rds')

```

# Introduction


# GROWTH TRENDS IN GVA, JOBS AND GVA PER JOB


## Full data range: 2015 to 2023


```{r, fig-growthdata_cv_andperFT}
#| fig-height: 12 
#| fig-width: 9.5

#Counting sigs. Statisticians having kittens. Let's do it.
itl3.2digit.cv <- read_csv('../data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_2DIGIT_LONG_2023.csv') %>% filter(!qg('agri|Owner-occupiers|membership|activities of households',SIC07_description))

#Add in shorter regional GVA SIC text
shortsectornames <- read_csv('../data/shortsectornames_for_regionalGVA_2digitSICs.csv')

#check match, tick
# table(unique(itl3.2digit.cv$SIC07_description) %in% shortsectornames$SIC07_description)
itl3.2digit.cv <- itl3.2digit.cv %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )

#CV first. No NeweyWest
ynh.2dig <- plotSlopeCounts(
  df = itl3.2digit.cv,
  placename = 'Sheffield',
  startdate = 2014,
  enddate = 2023,
  date_colname = year,
  region_colname = Region_name,
  sector_colname = SIC07_description_shortened,
  value_colname = value,
  conf_interval = 95
)




#And for GVA per job
bres.gva.2digit.2023 <- readRDS('../local/data/BRES2024_linkedtoITL3_2025_GVAsiccodes_withgvaattached.rds') %>% 
  filter(JOBCOUNT > 0, !qg('membership|activities of households|agri', SIC07_description)) %>% 
  mutate(gvaperjob = gva / JOBCOUNT)

#check match, tick
# table(unique(bres.gva.2digit.2023$SIC07_description) %in% shortsectornames$SIC07_description)
bres.gva.2digit.2023 <- bres.gva.2digit.2023 %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )





#CHeck this has same list of sectors as the CV data above... tick
#(With imputed rent removed from CV)
# table(unique(bres.gva.2digit.2023$SIC07_description) %in% itl3.2digit.cv$SIC07_description)
# unique(bres.gva.2digit.2023$SIC07_description)[!unique(bres.gva.2digit.2023$SIC07_description) %in% itl3.2digit.cv$SIC07_description]
ynh.gvaperjob.2dig <- plotSlopeCounts(
  df = bres.gva.2digit.2023,
  placename = 'Sheffield',
  startdate = 2015,
  enddate = 2023,
  date_colname = year,
  region_colname = Region_name,
  sector_colname = SIC07_description_shortened,
  value_colname = gvaperjob,#Note, this gets log'd in the function, don't do it here
  conf_interval = 95,
  includesectorname_on_axis = T
)

#Check sector match before alterations
# ynh.2dig$plot + ynh.gvaperjob.2dig$plot


#Axes match - remove from RHS
ynh.gvaperjob.2dig$plot <- ynh.gvaperjob.2dig$plot +
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position = "bottom"
        ) +
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank()) +
  guides(fill = F) +
  ggtitle("GVA/FT") 

#And only need the one legend!
ynh.2dig$plot <- ynh.2dig$plot +
  ggtitle("GVA") +
  theme(
    legend.position = "bottom",
    axis.title.y=element_blank()#add this explanation in figure text
  )

ynh.2dig$plot + ynh.gvaperjob.2dig$plot

```

 