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
  



# sectorname = itl2.cp$SIC07_description[qg('manuf',itl2.cp$SIC07_description)] %>% unique
sectorname = itl2.cp$SIC07_description[qg('information',itl2.cp$SIC07_description)] %>% unique

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
    GEOGRAPHY_NAME %in%  mostrecentvals$Region_name[mostrecentvals$regional_percent > 4.9],
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

ggsave(plot = p, filename = 'local/outputs/sy_percentplot.png', width = 17, height = 8)
# ggsave(plot = p, filename = 'local/outputs/sy_percentplot.png', width = 14, height = 10)


#Wiggle plot----

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





# PRODUCTIVTY / SECTOR PLOT----

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
      grepl('scientific',SIC07_description,ignore.case = T) ~ 'Sci/techn',
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
    # filter(qg('constr|manuf|scientific|information|transport|health|food|entertainment|educ',SIC07_description)) %>%
    filter(qg('constr|manuf|scientific|information|health|entertainment|educ|real|electr',SIC07_description)) %>%
    mutate(
      DATE = DATE - 2000,#Make dates 2 digit, more readable on axis
      placetoshow = qg('south y',GEOGRAPHY_NAME),#get leicester ITL2
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

#ggplotly for interactive
#ggplotly(p, tooltip = 'GEOGRAPHY_NAME', width = 1100, height = 700)

ggsave('local/outputs/sy_prodsector2.png', width = 11, height = 5)




# Newey West SE tests for time series sector data----

#https://chatgpt.com/c/684d7594-3aa8-8013-88a4-1aa8c5671ea5?model=gpt-4o
# install.packages("sandwich")
# install.packages("lmtest")

library(sandwich)
library(lmtest)

#Get chained volume data...
df.cv <- read_csv('https://raw.githubusercontent.com/DanOlner/RegionalEconomicTools/refs/heads/gh-pages/data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_SECTION_MINUSimputedrent_LONG_2023.csv')

df.cv[df.cv < 0] = 0

# Example: linear model on log-transformed values
model <- lm(log(value) ~ year, data = df.cv %>% filter(qg('sheffield',Region_name),qg('manuf',SIC07_description)))

# Compute Newey-West (HAC) standard errors
# The lag argument sets the maximum autocorrelation lag to account for
nw_se <- NeweyWest(model, lag = 1, prewhite = TRUE)

# Run a robust coefficient test
coeftest(model, vcov. = nw_se)

summary(model)


#Test added as option to function
slopes.log1523 <- get_slope_and_se_safely(data = df.cv %>% filter(year %in% 2015:2023), Region_name,SIC07_description, y = log(value), x = year, neweywest = F)

slopes.log1523.nw <- get_slope_and_se_safely(data = df.cv %>% filter(year %in% 2015:2023), Region_name,SIC07_description, y = log(value), x = year, neweywest = T)

plot(slopes.log1523$slope,slopes.log1523.nw$slope)
plot(slopes.log1523$se,slopes.log1523.nw$se)

table(is.na(slopes.log1523$se))
table(is.na(slopes.log1523.nw$se))



# Quick proportion plot----

#Using this data
df = read_csv('https://bit.ly/rtasteritl3noir')

#Core cities versus rest
#Each needs to be in own column
corecities = c("Tyneside","Manchester","Liverpool","Sheffield","Leeds","Nottingham","North Nottinghamshire","South Nottinghamshire","Birmingham","Bristol, City of","Cardiff and Vale of Glamorgan","Glasgow City","Belfast")

df = df %>% 
  mutate(
    corecity = ifelse(
      Region_name %in% corecities,
      "Corecity",
      "Other"
    )
  ) 

#Check
df$Region_name[df$corecity == "Corecity"] %>% unique


#Sum SIC sections for each of those groups
df = df %>%
  group_by(year,corecity,SIC07_description) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()


#Find proportions for each of those groupings
df = df %>%
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = corecity,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()

smoothband = 3

#find moving average,then make each its own column
df = df %>% 
  group_by(corecity,SIC07_description) %>% 
  mutate(
  sector_regional_percent_movingav = rollapply(
    sector_regional_proportion * 100,smoothband,mean,align='center',fill=NA
  ),
  gva_movingav = rollapply(
    value,smoothband,mean,align='center',fill=NA
  )
  ) %>% 
  ungroup()


#Keep key years and look
startyear = 1999
endyear = 2019

plotdata = df %>% 
  filter(year %in% c(startyear:endyear)) %>% 
  select(corecity,gva_movingav,SIC07_description,year,sector_regional_percent_movingav) %>% 
  pivot_wider(names_from = corecity, values_from = c(gva_movingav,sector_regional_percent_movingav))

# plotdata = df %>% 
#   filter(year %in% c(startyear,endyear)) %>% 
#   select(corecity,SIC07_description,year,sector_regional_percent_movingav) %>% 
#   pivot_wider(names_from = corecity, values_from = c(sector_regional_percent_movingav))


p <- twod_generictimeplot_multipletimepoints(
  df = plotdata,
  category_var = SIC07_description,
  x_var = sector_regional_percent_movingav_Corecity,
  y_var = sector_regional_percent_movingav_Other,
  label_var = gva_movingav_Corecity,
  timevar = year,
  # times = c(1999:2022) #3 yr moving av endpoints
  times = c(startyear:endyear) #5 yr moving av endpoints
  
)

# p + theme(aspect.ratio=1) +
p + coord_fixed() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, size = 3) + 
  # scale_x_log10() +
  # scale_y_log10() +
  xlab(paste0('Core cities')) +
  ylab(paste0('Rest of UK'))






# debugonce(twod_generictimeplot)
p <- twod_generictimeplot(
  df = plotdata,
  category_var = SIC07_description,
  x_var = sector_regional_percent_movingav_Corecity,
  y_var = sector_regional_percent_movingav_Other,
  label_var = gva_movingav_Corecity,
  timevar = year,
  start_time = startyear,
  end_time = endyear
  # compasspoints_to_display = c('NE')
)

# p + theme(aspect.ratio=1) +
p <- p + coord_fixed() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, size = 3) + 
  # scale_x_log10() +
  # scale_y_log10() +
  xlab(paste0('Core city')) +
  ylab(paste0('Other'))

p



# CHECKING BRES 2023 AND LATEST (2023) ONS DATA MATCH----

#As of July 2025, we've already got the latest up-to-2023 BRES data downloaded.
#But I think it uses ITL 2021, which means there won't be a perfect match with ITL 2025 now used in ONS GVA latest.

#Let's check
itl3.sections.cp <- read_csv('data/regionalGVA/regionalGVA_currentprices_ITL3_SIC_SECTION_LONG_2023.csv')

#BRES latest... get from local file
bres.sections.itl3 <- read_csv('data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_SECTION.csv')

#That's a lot of difference!
table(unique(itl3.sections.cp$ITL_code) %in% unique(bres.sections.itl3$GEOGRAPHY_CODE))

#What's changed? Compare...
itl3.2021 <- st_read('data/ITL_geographies/International_Territorial_Level_3_January_2021_UK_BUC_V3_2022_6920195468392554877/ITL3_JAN_2021_UK_BUC_V3.shp') %>% st_set_geometry(NULL)

itl3.2025 <- st_read("~/Dropbox/MapPolygons/UK/2025/International_Territorial_Level_3_(January_2025)_Boundaries_UK_BGC_V2.geojson") %>% st_set_geometry(NULL)

table(unique(bres.sections.itl3$GEOGRAPHY_CODE) %in% unique(itl3.2021$ITL321CD))



#For the current Bradford project, I may well want to have the 5 digit level
#Let's just look at an example file
x <- readRDS("data/BRES/BRES_ALLYEARSWITHDATA_TYPE429_internationalterritoriallevelslevel2asofJan2021_2_Fulltimeemployees_2022_2023.rds")

unique(x$INDUSTRY_TYPE)



# I've now updated BRES_sum_SIC5digit... to include outputting the 5 digit SICs themselves in the same format as the others.
#Let's just dig into those to remind myself what we've got.
#There's 2015-2022 NUTS level, then 2022-23 ITL3 2021 level. No 2025 as I type.

#Just looking at 2022-23 for a couple of SIC sector levels including 5 digits
bres.itl3.2digit.ft = read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_2DIGIT.csv")

bres.itl3.5digit.ft = read_csv("data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_2_Fulltimeemployees_2022_2023_SIC_5DIGIT.csv")

#We know these will match as they nest because we summed them ourselves, didn't use the odd rounded versions for higher SIC levels in the orig data
g(bres.itl3.2digit.ft)
g(bres.itl3.5digit.ft)

#sanity check. They're not in the same order, but all there.
table(unique(bres.itl3.2digit.ft$GEOGRAPHY_CODE) %in% unique(bres.itl3.5digit.ft$GEOGRAPHY_CODE))











