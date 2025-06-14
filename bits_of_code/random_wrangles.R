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


