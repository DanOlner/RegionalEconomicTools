#Explore modelled relationships between essential regional economic variables around employment and output
library(tidyverse)
library(modelsummary)
library(zoo)
library(plotly)
library(sf)
library(tmap)
library(patchwork)
# library(jtools)
source('functions/misc_functions.R')
options(scipen = 999)

# CV GVA/JOBS ITL2 SECTIONS----

#Can maybe do 'all industries' next, but let's have a look at this first. Sections cos 3 groups not available at CV for this combo.

itl2 <- read_csv('data/regionalGVA_plus_BRESjobcounts/regionalGVA_plus_BRESjobcounts_chainedvolume_ITL2_SIC_SECTION_MINUSimputedrent_2022.csv')

#OK, so relationship between raw GVA and jobcount in a particular year across places should be pretty tight.
#Let's look first.
#Pick a random sector to check?

chk <- itl2 %>% filter(qg('manuf', SIC07_description))
chk <- itl2 %>% filter(qg('information', SIC07_description))
chk <- itl2 %>% filter(qg('elec', SIC07_description))

p <- ggplot(chk,aes(y = gva, x = JOBCOUNT_FULLTIME, label = GEOGRAPHY_NAME)) +
# p <- ggplot(chk,aes(y = log(gva), x = log(JOBCOUNT_FULLTIME), label = GEOGRAPHY_NAME)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~DATE)

#Yep. Outliers, but the right relationship!
ggplotly(p, tooltip = 'GEOGRAPHY_NAME')


# Same data: change in GVA vs change in jobs, by place, pool sectors (can flag those later)----

#We already know there’s a direct/obvious relationship between them (last plot). What we want here: are there differences across regions, and where does SY fit in that?



#Let's try with actual percent change between years
#To deal with any large jumps so we're not using slightly off log values to infer % change

#That's good and useful. Some things:
#Drop public sectors - their job/gva relationship is built in by design (GVA is just wage * number)
#Agri is also a problem due to data source
#Rest good?
percentch <- itl2 %>% 
  filter(!qg('agri|health|educ|households|public admin', SIC07_description)) %>% 
  group_by(SIC07_description,GEOGRAPHY_NAME) %>% 
  mutate(
    delta_gva = percent_change(lag(gva),gva),
    delta_jobs = percent_change(lag(JOBCOUNT_FULLTIME),JOBCOUNT_FULLTIME)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(delta_gva))

#Quick look
ggplot(percentch, aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')

#Yorkshire only!
ggplot(percentch %>% filter(qg('yorksh',GEOGRAPHY_NAME)), aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')

#Different orientation - pick one sector, pool
ggplot(percentch, aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~SIC07_description, scales = 'free')


table(is.nan(percentch$delta_gva))
table(is.nan(percentch$delta_jobs))
table(is.na(percentch$delta_gva))
table(is.na(percentch$delta_jobs))
table(is.infinite(percentch$delta_gva))
table(is.infinite(percentch$delta_jobs))#Ah!

percentch$delta_jobs[is.infinite(percentch$delta_jobs)] <- NA

#Model that, broken down by place (already normalised in theory, though...)
# x <- lm(delta_gva ~ delta_jobs + GEOGRAPHY_NAME, data = percentch)
# summary(x)
# jtools::plot_coefs(x,ci_level = .95, colors = "Rainbow")

#Not really what we're after - don't want diff from base
#We know those will overlap

#Just want slopes for each place and plot
#Err have function for this, right?
rez.p <- get_slope_and_se_safely(percentch, y = delta_gva, x = delta_jobs, GEOGRAPHY_NAME) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )

ggplot(rez.p %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = SY, size = SY)) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  scale_colour_manual(values = c('grey','black')) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA')


#Check map of that please...
itl2.geo <- st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp')
# plot(st_geometry(itl2.geo))

table(unique(itl2$ITL_code) %in% itl2.geo$ITL221CD)
table(rez$GEOGRAPHY_NAME %in% itl2.geo$ITL221NM)

#OK, match on code, need to add back into rez
rez <- rez %>% 
  left_join(
    itl2 %>% select(ITL_code,GEOGRAPHY_NAME) %>% distinct(),
    by = 'GEOGRAPHY_NAME'
  )

#Better!
table(rez$ITL_code %in% itl2.geo$ITL221CD)


#Join to geo
itl2.geo <- itl2.geo %>% 
  right_join(
    rez,
    by = c('ITL221CD' = 'ITL_code')
  )


tm_shape(itl2.geo) +
  tm_polygons('slope', n = 6, palette="PRGn")




# REPEAT SAME WITH LOGS SO ELASTICITY INTERPRETATION EASIER----


#1. Get deltas for GVA and jobs. Other options later = smoothing, different windows etc. But basic first.
deltas <- itl2 %>% 
  filter(!qg('agri|health|educ|households|public admin', SIC07_description)) %>% 
  group_by(SIC07_description,GEOGRAPHY_NAME) %>% 
  mutate(
    delta_gva = log(lag(gva)) - log(gva),
    delta_jobs = log(lag(JOBCOUNT_FULLTIME)) - log(JOBCOUNT_FULLTIME)
  ) %>% 
  ungroup() %>% 
  filter(,
         !is.na(delta_gva),
         !is.infinite(delta_jobs)
         )



#Quick look
ggplot(deltas, aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')

#Yorkshire only!
ggplot(deltas %>% filter(qg('yorksh',GEOGRAPHY_NAME)), aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')


#Different orientation - pick one sector, pool
ggplot(deltas, aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~SIC07_description, scales = 'free')




#Get separate slopes for all the ITL2 zones
rez <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, GEOGRAPHY_NAME) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = SY, size = SY)) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  scale_colour_manual(values = c('grey','black')) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')



#Test the result we're getting there for South Yorkshire by comparing larger timepoint shifts
#To see if it roughly holds up. 0.25% for each 1% of job change sounds about right to me.




#Repeat for separate sectors too
rez.sectors <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, SIC07_description) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez.sectors, aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  # scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')




# BREAK INTO TWO PRE/POST COVID TIME PERIODS----

deltas <- deltas %>%
  mutate(pandemic = ifelse(DATE < 2020, "pre","post"))

#Viz first, tho having looked - large values for service sectors post covid I suspect means it's plotting a line through strong jobs and GVA *drops* and so inferring very positive slopes... (note how small pre pandemic is)

#PLACES
ggplot(deltas, aes(x = delta_jobs, y = delta_gva, colour = pandemic)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')

#Yorkshire only!
ggplot(deltas %>% filter(qg('yorksh',GEOGRAPHY_NAME)), aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME+pandemic, scales = 'free', ncol = 2) +
  geom_vline(xintercept = 0, colour = 'green') +
  geom_hline(yintercept = 0, colour = 'green') 


#Different orientation - pick one sector, pool
ggplot(deltas, aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~SIC07_description + pandemic, scales = 'free', ncol = 6) +
  geom_vline(xintercept = 0, colour = 'green') +
  geom_hline(yintercept = 0, colour = 'green') 




rez <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, GEOGRAPHY_NAME, pandemic) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = pandemic, size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')




#Save rez for elsewhere
saveRDS(rez,'data/rez.rds')


#Break into two, recombine
pre <- ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)) %>% filter(pandemic == 'pre'), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  # scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average') +
  ggtitle('PRE-PANDEMIC')

post <- ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)) %>% filter(pandemic == 'post'), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  # scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average') +
  ggtitle('POST-PANDEMIC')


pre + post




#Repeat for separate sectors too
rez.sectors <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, SIC07_description, pandemic) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

# ggplot(rez.sectors, aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
#   # scale_size_manual(values = c(0.5,1)) +
#   # scale_colour_manual(values = c('grey','black')) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')

ggplot(rez.sectors, 
       aes(x = slope, y = fct_reorder(SIC07_description,slope), colour = pandemic)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')


#Plot just pre-pandemic
# ggplot(rez.sectors %>% filter(pandemic == 'pre'), 
ggplot(rez.sectors %>% filter(pandemic == 'post'), 
       aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')






# GOODS V SERVICES FLAG, HOW DIFFERENT IN DIFF PLACES?----

#Pandemic thing is worrying isn't it? Let's keep data from last time, see if can break down by both
#unique(deltas$SIC07_description)

deltas <- deltas %>% 
  mutate(goods_services = ifelse(
    qg('mining|manuf|constru|elec|water', SIC07_description), 'goods','services'
  ))


#Check for odd values breaking regression
table(is.nan(deltas$delta_gva))
table(is.nan(deltas$delta_jobs))
table(is.na(deltas$delta_gva))
table(is.na(deltas$delta_jobs))
table(is.infinite(deltas$delta_gva))
table(is.infinite(deltas$delta_jobs))#Ah!

deltas$delta_jobs[is.infinite(deltas$delta_jobs)] <- NA

# debugonce(get_slope_and_se_safely)
rez <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, GEOGRAPHY_NAME, goods_services) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = goods_services, size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  # facet_wrap(~pandemic) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')



#Break down by pandemic, enough data points?
rez <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, GEOGRAPHY_NAME, goods_services, pandemic) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = goods_services, size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  facet_wrap(~pandemic, scales='free_x') +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')



#BREAK INTO TWO, RECOMBINE (so we can get right order)
pre <- ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)) %>% filter(pandemic == 'pre'), 
              aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = goods_services, size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average') +
  ggtitle("PRE-PANDEMIC")

post <- ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)) %>% filter(pandemic == 'post'), 
              aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = goods_services, size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average') +
  ggtitle("POST-PANDEMIC")

pre+post



#Goods/services on different plots
#flag for pre/post:
# pandemicflag = "post"
pandemicflag = "pre"

#Pull out points for CI lines
cilines <- rez %>%
  ungroup() %>% 
  filter(GEOGRAPHY_NAME == 'South Yorkshire', pandemic %in% pandemicflag, goods_services == 'goods') %>% 
  select(ci95min,ci95max) 

goods <- ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)) %>% filter(pandemic %in% pandemicflag, goods_services == 'goods'), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.3, size = 2) +
  geom_vline(xintercept = cilines$ci95min[1], colour = 'darkgreen', alpha = 0.1, size = 1) +
  geom_vline(xintercept = cilines$ci95max[1], colour = 'darkgreen', alpha = 0.1, size = 1) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average') +
  ggtitle("GOODS")


cilines <- rez %>%
  ungroup() %>% 
  filter(GEOGRAPHY_NAME == 'South Yorkshire', pandemic %in% pandemicflag, goods_services == 'services') %>% 
  select(ci95min,ci95max) 

services <- ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)) %>% filter(pandemic %in% pandemicflag, goods_services == 'services'), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), size = SY)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  geom_vline(xintercept = cilines$ci95min[1], colour = 'darkgreen', alpha = 0.1, size = 1) +
  geom_vline(xintercept = cilines$ci95max[1], colour = 'darkgreen', alpha = 0.1, size = 1) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average') +
  ggtitle("SERVICES")


goods + services


#Repeat for separate sectors too
rez.sectors <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, pandemic, goods_services) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

# ggplot(rez.sectors, aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
#   # scale_size_manual(values = c(0.5,1)) +
#   # scale_colour_manual(values = c('grey','black')) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')

ggplot(rez.sectors, 
       aes(x = slope, y = fct_reorder(goods_services,slope), colour = pandemic)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1, position = position_dodge(width = 0.5)) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')


#Plot just pre-pandemic
ggplot(rez.sectors %>% filter(pandemic == 'pre'), 
       aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')







# TEST LAGGED ELASTICITY----

#e.g. Can imagine transmission to GVA happens quicker in certain sectors.
#Rise in jobs this year could show up stronger in the following year

#Same, but add an extra lag so jobs is a year behind gva
deltas <- itl2 %>% 
  filter(!qg('agri|health|educ|households|public admin', SIC07_description)) %>% 
  group_by(SIC07_description,GEOGRAPHY_NAME) %>% 
  mutate(
    delta_gva = log(lag(gva)) - log(gva),
    delta_jobs = log(lag(JOBCOUNT_FULLTIME)) - log(JOBCOUNT_FULLTIME),
    deltajobs_lastyr = lag(delta_jobs)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(delta_gva), !is.na(deltajobs_lastyr))

#Quick look
ggplot(deltas, aes(x = deltajobs_lastyr, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')

#Yorkshire only!
ggplot(deltas %>% filter(qg('yorksh',GEOGRAPHY_NAME)), aes(x = deltajobs_lastyr, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEOGRAPHY_NAME, scales = 'free')


#Different orientation - pick one sector, pool
ggplot(deltas, aes(x = deltajobs_lastyr, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~SIC07_description, scales = 'free')




#Get separate slopes for all the ITL2 zones
rez <- get_slope_and_se_safely(deltas, y = delta_gva, x = deltajobs_lastyr, GEOGRAPHY_NAME) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez %>% mutate(SY = qg('south y',GEOGRAPHY_NAME)), 
       aes(x = slope, y = fct_reorder(GEOGRAPHY_NAME,slope), colour = SY, size = SY)) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  scale_colour_manual(values = c('grey','black')) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')



#Test the result we're getting there for South Yorkshire by comparing larger timepoint shifts
#To see if it roughly holds up. 0.25% for each 1% of job change sounds about right to me.




#Repeat for separate sectors too
rez.sectors <- get_slope_and_se_safely(deltas, y = delta_gva, x = deltajobs_lastyr, SIC07_description) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))

ggplot(rez.sectors, aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  # scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')




# Try and get some slopes from individual ITL2 zones sectors----

#Which won't be many data points. But let's look at an example (and mull fixed effects)

sy <- deltas %>% filter(GEOGRAPHY_NAME == 'South Yorkshire')

#Look at slopes
ggplot(sy, aes(x = delta_jobs, y = delta_gva)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~SIC07_description, scales = 'free') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  geom_hline(yintercept = 0, colour = 'black', alpha = 0.5) 




rez.sectors <- get_slope_and_se_safely(sy, y = delta_gva, x = delta_jobs, SIC07_description) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))


ggplot(rez.sectors, aes(x = slope, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min, xmax = ci95max), width = 0.1) +
  # scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('1% ^ in full time workers is associated with x% ^ in GVA on average')








# APPLY ELASTICITIES TO SOUTH YORKSHIRE ACTUAL JOB COUNT AND GVA NUMBERS----


#Get SY job counts and percentages...
itl2.lq <- itl2 %>% 
  split(.$DATE) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = GEOGRAPHY_NAME,
      lq_var = SIC07_description,
      valuevar = JOBCOUNT_FULLTIME) %>% 
  bind_rows() %>% 
  mutate(
    jobcountFT_regionalpercent = sector_regional_proportion * 100
  ) %>% 
  group_by(GEOGRAPHY_NAME,SIC07_description) %>% 
  mutate(
    movingav_jobcountFT_regionalpercent = rollapply(jobcountFT_regionalpercent,3,mean,align='center',fill=NA),
    movingav_jobcountFT = rollapply(JOBCOUNT_FULLTIME,3,mean,align='center',fill=NA),
    movingav_gva = rollapply(gva,3,mean,align='center',fill=NA)
  ) %>% 
  ungroup()

#Confirm sums correctly to 100% of jobs per region/year... tick
# itl2.lq %>%
#   group_by(DATE,GEOGRAPHY_NAME) %>%
#   summarise(sum(jobcountFT_regionalpercent))
sy <- itl2.lq %>% 
  filter(GEOGRAPHY_NAME == 'South Yorkshire')



rez.sectors <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, SIC07_description, pandemic) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(!is.na(slope))



#Just latest year 3 year moving av
sy.latest <- sy %>% filter(DATE == 2021) %>% select(SIC07_description,gva,movingav_jobcountFT_regionalpercent,movingav_jobcountFT,movingav_gva)


#Add in elasticities for pre-COVID sectors for UK as a whole, to prep if...then
#Keeping only sectors we have elasticities for
sy.latest <- sy.latest %>% right_join(rez.sectors %>% filter(pandemic == 'pre'), by = 'SIC07_description')

#Then some very basic sums to get from "1% inc in jobs means x% inc in GVA" and apply to these numbers

#1. What one job as a percent of total jobs here?
sy.latest <- sy.latest %>% 
  # mutate(onejob_iswhatfraction = (1/movingav_jobcountFT)*100 )
  mutate(onejob_iswhatfraction = 1/movingav_jobcountFT )

#2. Elasticity is just a multiplier of job % change, so can multiply up by that to get GVA change if one extra job
sy.latest <- sy.latest %>% 
  mutate(
    onejob_assocdwithhowmuchextraGVA_pointestimate =  onejob_iswhatfraction * movingav_gva * slope,
    onejob_assocdwithhowmuchextraGVA_min95 =  onejob_iswhatfraction * movingav_gva * ci95min,
    onejob_assocdwithhowmuchextraGVA_max95 =  onejob_iswhatfraction * movingav_gva * ci95max
  )

#Look at reduced version
# sy.latest %>% select(SIC07_description,movingav_jobcountFT,movingav_gva,slope,onejob_iswhatfraction,onejob_assocdwithhowmuchextraGVA_pointestimate) %>% View

#Sanity checks on single number
#Manufacturing in SY, approx values
#55K jobs
#3500M GVA
#Elasticity jobs/gva is 0.125 (1% job ^ = 0.125% * GVA)

#1 job is (1/55000)*100 % of total jobs = 0.0018% of total manuf jobs
#So 1 job going up: 
#0.0018 * 0.125 * 3500 * 1000000
#= 0.7875M

#0.7875 * 55000

#Oh I shouldn't be doing percent, needs to be proportion. A percent is not a fraction!
#(1/55000) * 0.125 * 3500 * 1000000
#7954
#Which would have each job be currently... about right
#(7954 * 55000)/1000000



#Let's plot that...
ggplot(sy.latest, 
       aes(x = onejob_assocdwithhowmuchextraGVA_pointestimate * 1000000, 
           y = fct_reorder(SIC07_description,onejob_assocdwithhowmuchextraGVA_pointestimate))) +
  geom_point() +
  geom_errorbar(aes(xmin = onejob_assocdwithhowmuchextraGVA_min95* 1000000, 
                    xmax = onejob_assocdwithhowmuchextraGVA_max95* 1000000), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('One extra full time worker is associated with ^ in GVA in a year, on average')


#Flag highs and split out
ggplot(sy.latest %>% mutate(highs = qg('financ|mining|real est',SIC07_description)), 
       aes(x = onejob_assocdwithhowmuchextraGVA_pointestimate * 1000000, 
           y = fct_reorder(SIC07_description,onejob_assocdwithhowmuchextraGVA_pointestimate))) +
  geom_point() +
  geom_errorbar(aes(xmin = onejob_assocdwithhowmuchextraGVA_min95* 1000000, 
                    xmax = onejob_assocdwithhowmuchextraGVA_max95* 1000000), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  facet_wrap('highs', scales = 'free') +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('One extra full time worker is associated with ^ in GVA in a year, on average')







# GET PRE PANDEMIC SLOPES FOR ITL2S, FOR GOODS AND SERVICES, AND FIND SY DISTANCE TO MEAN----

#For then applying to elasticity values

#These are the slopes we want
rez.pre <- get_slope_and_se_safely(deltas, y = delta_gva, x = delta_jobs, GEOGRAPHY_NAME, goods_services, pandemic) %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  ) %>% 
  filter(
    !is.na(slope),
    pandemic == 'pre'
    )

#Get mean, and then we just need SY point estimate and CI distance to that mean
means <- rez.pre %>% 
  group_by(goods_services) %>% 
  summarise(meanslope = mean(slope)) %>% 
  ungroup() %>% 
  left_join(
    rez.pre %>% ungroup() %>% filter(qg('south y',GEOGRAPHY_NAME)) %>% select(goods_services,slope,ci95min,ci95max)
  ) %>% 
  mutate(
    sy_slopemultiple = slope/meanslope,
    sy_ci95min_multiple = ci95min/meanslope,
    sy_ci95max_multiple = ci95max/meanslope
  )



#Add goods/services flag back into the SY job/GVA values from last section
sy.latest <- sy.latest %>%
  mutate(goods_services = ifelse(
    qg('mining|manuf|constru|elec|water', SIC07_description), 'goods','services'
    ))


#Add the multipliers in
sy.latest <- sy.latest %>%
  left_join(
    means %>% select(goods_services,sy_slopemultiple:sy_ci95max_multiple),
    by = 'goods_services'
  )


#Then multiply up...
sy.latest <- sy.latest %>%
  mutate(
    ADJ_onejob_assocdwithhowmuchextraGVA_pointestimate = onejob_assocdwithhowmuchextraGVA_pointestimate * sy_slopemultiple * 1000000,
    ADJ_onejob_assocdwithhowmuchextraGVA_min95 = onejob_assocdwithhowmuchextraGVA_min95 * sy_ci95min_multiple * 1000000,
    ADJ_onejob_assocdwithhowmuchextraGVA_max95 = onejob_assocdwithhowmuchextraGVA_max95 * sy_ci95max_multiple* 1000000
  )


#And what does that look like??
ggplot(sy.latest, 
       aes(x = ADJ_onejob_assocdwithhowmuchextraGVA_pointestimate, 
           y = fct_reorder(SIC07_description,ADJ_onejob_assocdwithhowmuchextraGVA_pointestimate))) +
  geom_point() +
  geom_errorbar(aes(xmin = ADJ_onejob_assocdwithhowmuchextraGVA_min95, 
                    xmax = ADJ_onejob_assocdwithhowmuchextraGVA_max95), width = 0.1) +
  scale_size_manual(values = c(0.5,1)) +
  # scale_colour_manual(values = c('grey','black')) +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('One extra full time workers is associated with ^ in GVA on average')










#From that, we can then take a guess at what the non-labour part of GVA output change is
#What's the difference between these values and overall GVA per job in these sectors?

#Already have those figures here for latest period (smoothed)
sy.latest <- sy.latest %>% 
  mutate(
    gvaperjob_movingav = (movingav_gva / movingav_jobcountFT) * 1000000,
    percentdiff_to_deltaonejob = percent_change(ADJ_onejob_assocdwithhowmuchextraGVA_pointestimate,gvaperjob_movingav)
  )

#Sanity check
sy.latest %>% 
  select(
    SIC07_description,gvaperjob_movingav,ADJ_onejob_assocdwithhowmuchextraGVA_pointestimate,percentdiff_to_deltaonejob
  ) %>% 
  arrange(-percentdiff_to_deltaonejob) %>% View






# GROSS FIXED CAPITAL FORMATION----

#And how it also changes over time.
#After a bunch of digging - see YPERN_planner2024 - there's only one good source from ONS via a request.
#Other regional version delayed as I write.

#One we're using (I think current prices but it doesn't say):
#https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/adhocs/13655regionalgrossfixedcapitalformationitl1anditl22000to2019

#So for 2021 ITL2 (will match above handily):
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/regionalaccounts/grossdisposablehouseholdincome/adhocs/13655regionalgrossfixedcapitalformationitl1anditl22000to2019/regionalgfcf20002019itlcodes.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

gfcf <- readxl::read_excel(path = p1f,range = "Rounded GFCF ITL2!A4:X496")

names(gfcf) <- gsub(x = names(gfcf), pattern = ' ', replacement = '_')

#Just going to use all sectors for now...
gfcf = gfcf %>% 
  filter(SIC07_description == 'Total GFCF') %>%
  pivot_longer(`2000`:names(gfcf)[length(names(gfcf))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

#Compare sector bins...
# unique(gfcf$SIC07_description)
# unique(itl2$SIC07_description)





#Might be able to apply deflators from GVA data?
#For 'all industries' at any rate
#This should be correct ITL2s...
url2 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/previous/v10/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx'
p2f <- tempfile(fileext=".xlsx")
download.file(url2, p2f, mode="wb") 

#Table 2d: ITL2, implied deflators, 2019 equals 100 [note 3]
itl2.deflators <- readxl::read_excel(path = p2f,range = "Table2d!A2:AB3938")

names(itl2.deflators) <- gsub(x = names(itl2.deflators), pattern = ' ', replacement = '_')

itl2.deflators = itl2.deflators %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl2.deflators)[length(names(itl2.deflators))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

#Matching ITL2s? Tick.
table(unique(gfcf$ITL2_code) %in% unique(itl2.deflators$ITL_region_code))



#Add deflators in (as proportion) to GFCF data so can just multiply through
#Then need to invert to get re-inflator!
gfcf = gfcf %>% 
  left_join(
    itl2.deflators %>% mutate(deflator = value / 100) %>% select(ITL_region_code,year,deflator),
    by = c('ITL2_code' = 'ITL_region_code','year')
  ) %>% 
  mutate(
    reinflator = 1/ deflator,
    adjusted_value = value * reinflator
  )





#We now need:
#CV GVA data for ITL2s for ALL INDUSTRIES (can't sum from above cos chained volume)
#And then also a sum of BRES job numbers for all industries from above

#CV GVA should come from that same Excel sheet as above to match, so that's handy
itl2.cv.all = readxl::read_excel(path = p2f,range = "Table2b!A2:AB3938")

names(itl2.cv.all) <- gsub(x = names(itl2.cv.all), pattern = ' ', replacement = '_')

itl2.cv.all = itl2.cv.all %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl2.cv.all)[length(names(itl2.cv.all))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))


#Then get sum of jobs for each year and place
alljobs <- itl2 %>% 
  select(year = DATE, ITL_region_code = ITL_code, JOBCOUNT_FULLTIME) %>% 
  group_by(year,ITL_region_code) %>% 
  summarise(JOBCOUNT_FULLTIME = sum(JOBCOUNT_FULLTIME)) %>% 
  ungroup()
  

#Merge those into ITL2 gva all industry totals
itl2.cv.all.jobs = itl2.cv.all %>% 
  inner_join(
    alljobs, by = c('year','ITL_region_code')
  )


#Then can add GFCF in
gfcf.gva.jobs = itl2.cv.all.jobs %>% 
  inner_join(
    gfcf %>% select(ITL_region_code = ITL2_code, year, gfcf = value, gfcf_cv = adjusted_value),
    by = c('year','ITL_region_code')
  )






  


#Then do the same as above: get yearly deltas, but now for all three...
deltas <- gfcf.gva.jobs %>% 
  group_by(ITL_region_code) %>% 
  mutate(
    delta_gva = log(lag(value)) - log(value),
    delta_gfcf = log(lag(gfcf_cv)) - log(gfcf_cv),
    delta_jobs = log(lag(JOBCOUNT_FULLTIME)) - log(JOBCOUNT_FULLTIME)
  ) %>% 
  ungroup() %>% 
  filter(,
         !is.na(delta_gva),
         !is.infinite(delta_jobs)
  )



#What's the rel for the entire set of pair points
#Separately then together?
summary(lm(data = deltas, formula = delta_gva ~ delta_gfcf))

#This not terribly surprising given GVA is largely wages
#In fact it almost makes sense to subtract change in wages from it to see what's left
summary(lm(data = deltas, formula = delta_gva ~ delta_jobs))

#Just out of interest...?
summary(lm(
  data = deltas %>% mutate(deltagva_minus_deltajobs = delta_gva - delta_jobs), 
  formula = deltagva_minus_deltajobs ~ delta_gfcf))

#Both
summary(lm(data = deltas, formula = delta_gva ~ delta_jobs + delta_gfcf))



#Just for SY now -- can't be enough data points here but let's see
#Yeah, no!
summary(lm(data = deltas %>% filter(qg('south y', ITL_region_name)), formula = delta_gva ~ delta_jobs + delta_gfcf))


#Looksee
ggplot(deltas %>% mutate(deltagva_minus_deltajobs = delta_gva - delta_jobs), 
       # aes(x = delta_gfcf, y = deltagva_minus_deltajobs)) +
       aes(x = delta_gfcf, y = delta_gva)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm')






# GFCF - PULL OUT MORE DATA BY USING SECTORS THAT WE HAVE DATA FOR---

# There are a few SIC sections in the GFCF data that will match across jobs and GVA
#Look at those
gfcf <- readxl::read_excel(path = p1f,range = "Rounded GFCF ITL2!A4:X496")

names(gfcf) <- gsub(x = names(gfcf), pattern = ' ', replacement = '_')

#Just going to use all sectors for now...
gfcf = gfcf %>% 
  # filter(SIC07_description == 'Total GFCF') %>%
  pivot_longer(`2000`:names(gfcf)[length(names(gfcf))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

#Compare sector bins...
unique(gfcf$SIC07_description)
unique(itl2$SIC07_description)

#Agri - not really enough data in different places
#Which leaves:
#Manuf / construction / ICT / finance + insurance

#Not a lot but better than nothing, and a decent cross section of physical + services

#So: keep those sectors...
gfcf = gfcf %>% filter(qg('manuf|construction|information|financ', SIC07_description))

#Mutate names to match other ITL2 data
#gfcf$SIC07_description %>% unique
gfcf = gfcf %>%
  mutate(
    SIC07_description = case_when(
      SIC07_description == 'Of which Manufacturing' ~ 'Manufacturing',
      .default = SIC07_description#Rest match I think
    )
  )

#Tick
table(unique(gfcf$SIC07_description) %in% itl2$SIC07_description)


#And same for ITL2 CV
#Deflators first
#Actually, just going to try without for now, on basis that year to year % changes won't be too far off

# url2 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/previous/v10/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx'
# p2f <- tempfile(fileext=".xlsx")
# download.file(url2, p2f, mode="wb") 
# 
# #Table 2d: ITL2, implied deflators, 2019 equals 100 [note 3]
# itl2.deflators <- readxl::read_excel(path = p2f,range = "Table2d!A2:AB3938")
# 
# names(itl2.deflators) <- gsub(x = names(itl2.deflators), pattern = ' ', replacement = '_')
# 
# itl2.deflators = itl2.deflators %>% 
#   filter(SIC07_description %in% c('Manufacturing','Construction','Information and communication','Financial and insurance activities')) %>% 
#   pivot_longer(`1998`:names(itl2.deflators)[length(names(itl2.deflators))], names_to = 'year', values_to = 'value') %>% #get most recent year
#   mutate(year = as.numeric(year))


#Select industries we want from the ITL2 data
#itl2$SIC07_description %>% unique
itl2.sub <- itl2 %>% 
  filter(SIC07_description %in% c('Manufacturing','Construction','Information and communication','Financial and insurance activities'))



#Then can add GFCF in
itl2.sub.gfcf = itl2.sub %>% 
  select(year = DATE, ITL_region_code = ITL_code, Region_name = GEOGRAPHY_NAME, SIC07_description, gva, JOBCOUNT_FULLTIME) %>% 
  inner_join(
    gfcf %>% select(ITL_region_code = ITL2_code, year, gfcf = value, SIC07_description),
    by = c('year','ITL_region_code','SIC07_description')
  )

#save!
saveRDS(itl2.sub.gfcf,'data/misc/gfcf_jobs_sectors.rds')





#Then do the same as above: get yearly deltas, but now for all three...
deltas <- itl2.sub.gfcf %>% 
  group_by(ITL_region_code, SIC07_description) %>% 
  mutate(
    delta_gva = log(lag(gva)) - log(gva),
    delta_gfcf = log(lag(gfcf)) - log(gfcf),
    delta_jobs = log(lag(JOBCOUNT_FULLTIME)) - log(JOBCOUNT_FULLTIME)
  ) %>% 
  ungroup() %>% 
  filter(,
         !is.na(delta_gva),
         !is.infinite(delta_jobs)
  )


#Right, few more data points!
#Eyeball
#Check on conversion back to % change
deltas = deltas %>% 
  mutate(across(c(delta_gva:delta_jobs), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))



ggplot(deltas,aes(x = delta_gfcf_percentperyear, y = delta_gva_percentperyear)) +
# ggplot(deltas,aes(x = delta_gfcf, y = delta_gva)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm')

ggplot(deltas %>% filter(qg('south y', Region_name)),aes(x = delta_gfcf_percentperyear, y = delta_gva_percentperyear)) +
# ggplot(deltas,aes(x = delta_gfcf, y = delta_gva)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm')


#Pairs
summary(lm(data = deltas, formula = delta_gva_percentperyear ~ delta_gfcf_percentperyear))

#All
# summary(lm(data = deltas, formula = delta_gva ~ delta_jobs + delta_gfcf))
summary(lm(data = deltas, formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear))
summary(lm(data = deltas, formula = delta_gva ~ delta_jobs + delta_gfcf))

summary(lm(data = deltas %>% filter(qg('south y', Region_name)), formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear))
summary(lm(data = deltas %>% filter(qg('south y', Region_name)), formula = delta_gva ~ delta_jobs + delta_gfcf))


#Yes I know. But also...
ggplot(deltas %>% filter(between(delta_gfcf_percentperyear,-25,25)),
       aes(x = delta_gfcf_percentperyear, y = delta_gva_percentperyear)) +
  # ggplot(deltas,aes(x = delta_gfcf, y = delta_gva)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm')

#And! Sectors?
ggplot(deltas 
       %>% filter(between(delta_gfcf_percentperyear,-25,25))
       ,
       aes(x = delta_gfcf_percentperyear, y = delta_gva_percentperyear)) +
  # ggplot(deltas,aes(x = delta_gfcf, y = delta_gva)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~SIC07_description)




#i'm not sure rel should be stronger for GFCF than job count...
summary(lm(data = deltas %>% filter(between(delta_gfcf_percentperyear,-25,25)), 
           formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear))

#Sector breakdown
diffsectors = function(sectorname){
  print(sectorname)
  print(summary(lm(data = deltas %>% filter(SIC07_description == sectorname), 
             formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear)))
  # print(summary(lm(data = deltas %>% filter(between(delta_gfcf_percentperyear,-25,25), SIC07_description == sectorname), 
  #            formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear)))
}

map(unique(deltas$SIC07_description),diffsectors)



#Function for extracting coeffs from those...
#Test getting right numbers
# model1 = summary(lm(data = deltas, formula = delta_gva ~ delta_jobs + delta_gfcf))
# coef(model1)[3]
# model1$coefficients[3, 2]

diffsectors_coefs = function(sectorname){
  
  model1 = summary(lm(data = deltas %>% filter(SIC07_description == sectorname), 
                   formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear))
  
  slopejobs <- coef(model1)[2]
  sejobs <- model1$coefficients[2, 2]
  slopegfcf <- coef(model1)[3]
  segfcf <- model1$coefficients[3, 2]
  
  line1 = list(sector = sectorname, coef = 'jobs', slope = slopejobs, se = sejobs)
  line2 = list(sector = sectorname, coef = 'gfcf', slope = slopegfcf, se = segfcf)
  
  return(bind_rows(line1,line2))
  
}

allz = map(unique(deltas$SIC07_description),diffsectors_coefs) %>% bind_rows()







summary(lm(data = deltas %>% filter(
  between(delta_gfcf_percentperyear,-25,25),
  qg('south y', Region_name)
  ),formula = delta_gva_percentperyear ~ delta_jobs_percentperyear + delta_gfcf_percentperyear))






# COMPARE JOBS / GFCF RATIOS FOR DIFFERENT PLACES----

#To put SY in some context
#Use FT jobs here but could use hours also

#Sum to totals / use all industries from above
#This is inflation adjusted
# gfcf.gva.jobs

#Ratio might look odd. Difference to national average might be good, though that should probably be weighted.
#Hmm that intoduces extra weirdness. Let's just use the ratio, it's more direct.

#Use non-adjusted val?
gfcf.gva.jobs = gfcf.gva.jobs %>% 
  mutate(
    jobs_to_gfcf_ratio = JOBCOUNT_FULLTIME / gfcf
    # sizegroup = cut_number(jobs_to_gfcf_ratio,4)#can't use this, breaks apart...
  )

#Let's see if that needs smoothing to pick up patterns
p = ggplot(gfcf.gva.jobs, aes(x = year, y = jobs_to_gfcf_ratio, group = ITL_region_name)) +
  geom_point() +
  geom_line() 
  # facet_wrap(~sizegroup, scales = 'free')

ggplotly(p, tooltip = 'ITL_region_name')

#I think inflation adjustment may be wrong, let's not use it!
#Keep orig gfcf, use ranks instead






#I should probably check that relationship holds if we use hours...
# url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
# p1f <- tempfile(fileext=".xlsx")
# download.file(url1, p1f, mode="wb")
# 
# #"Productivity Hours Worked per Week; ITL2 and ITL3 subregions (constrained to ITL1), 2004 - 2023"
# hoursworked <- readxl::read_excel(path = p1f,range = "Productivity Hours!A5:W247")
# 
# names(hoursworked) <- gsub(x = names(hoursworked), pattern = ' ', replacement = '_')
# 
# hoursworked = hoursworked %>%
#   filter(ITL_level == 'ITL2') %>% 
#   pivot_longer(Hours_2004:Hours_2023, names_to = 'year', values_to = 'hoursworkedperweek') %>% #get most recent year
#   mutate(
#     year = substr(year,7,10),
#     year = as.numeric(year)
#   ) %>% 
#   select(-ITL_level)
# 
# #That may be 2025 ITL zones? Yep, need older hours worked sheet
# table(unique(hoursworked$ITL_code) %in% gfcf.gva.jobs$ITL_region_code)


#2022 version (from 2024!)
url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/previous/v12/labourproductivityitls.xls'
p1f <- tempfile(fileext=".xls")
download.file(url1, p1f, mode="wb")

#"Productivity Hours Worked per Week; ITL2 and ITL3 subregions (constrained to ITL1), 2004 - 2023"
hoursworked <- readxl::read_excel(path = p1f,range = "Productivity Hours!A5:V239")

names(hoursworked) <- gsub(x = names(hoursworked), pattern = ' ', replacement = '_')

hoursworked = hoursworked %>%
  filter(ITL_level == 'ITL2') %>% 
  pivot_longer(Hours_2004:Hours_2022, names_to = 'year', values_to = 'hoursworkedperweek') %>% #get most recent year
  mutate(
    year = substr(year,7,10),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)

#That may be 2025 ITL zones? Yep, need older hours worked sheet
# table(unique(hoursworked$ITL_code) %in% gfcf.gva.jobs$ITL_region_code)
#Good good...
table(unique(gfcf.gva.jobs$ITL_region_code) %in% unique(hoursworked$ITL_code))


#Add in hours per week
gfcf.gva.jobs = gfcf.gva.jobs %>% 
  left_join(
    hoursworked %>% rename(ITL_region_code = ITL_code) %>% select(ITL_region_code,year,hoursworkedperweek),
    by = c('ITL_region_code','year')
  )

#save for elsewhere
saveRDS(gfcf.gva.jobs, 'data/misc/gfcf_jobs.rds')




#While I'm here... how strongly is the FT job / weekly hours link?
#Yeah pretty inseparable really, nothing shocking
ggplot(gfcf.gva.jobs, aes(x = hoursworkedperweek, y = JOBCOUNT_FULLTIME)) +
  geom_point() +
  geom_smooth(method = 'lm')



#Find ratio again
gfcf.gva.jobs = gfcf.gva.jobs %>% 
  mutate(
    hoursperweek_to_gfcf_ratio = hoursworkedperweek / gfcf
  )



p = ggplot(gfcf.gva.jobs, aes(x = year, y = hoursperweek_to_gfcf_ratio, group = ITL_region_name)) +
  geom_point() +
  geom_line() 
# facet_wrap(~sizegroup, scales = 'free')

ggplotly(p, tooltip = 'ITL_region_name')

#It occurs to me - the slope may be an artifact, if inflation not adjusted correctly.

#So let's tell myself what that ratio means when the number's high v low
#Let's just look at the numbers for a top and bottom example
gfcf.gva.jobs %>% filter(ITL_region_name == 'South Yorkshire', year == 2019) %>% select(hoursworkedperweek,gfcf,hoursperweek_to_gfcf_ratio)
gfcf.gva.jobs %>% filter(qg('berkshire',ITL_region_name), year == 2019) %>% select(hoursworkedperweek,gfcf,hoursperweek_to_gfcf_ratio)



#Probably the wrong way round really. We'd be better with "GFCF per hour worked", right?
gfcf.gva.jobs = gfcf.gva.jobs %>% 
  mutate(
    gfcf_perweeklyhour_worked = (gfcf / hoursworkedperweek) * 1000000
  )



p = ggplot(gfcf.gva.jobs, aes(x = year, y = gfcf_perweeklyhour_worked, group = ITL_region_name)) +
  geom_point() +
  geom_line() 
# facet_wrap(~sizegroup, scales = 'free')

ggplotly(p, tooltip = 'ITL_region_name')


#So a way to show this for South Yorkshire? Shift in rank?
#Rank moving average, do moving av first
gfcf.gva.jobs.movingavrank = gfcf.gva.jobs %>% 
  group_by(ITL_region_name) %>% 
  mutate(
    gfcf_perweeklyhour_worked_movingav = rollapply(gfcf_perweeklyhour_worked,3,mean,align='center',fill=NA)
  ) %>% 
  filter(!is.na(gfcf_perweeklyhour_worked_movingav)) %>% #remove years without data before ranking
  group_by(year) %>% 
  mutate(
    rankpos = rank(-gfcf_perweeklyhour_worked_movingav)
    ) %>% 
  ungroup()

#Check for one year... tick
gfcf.gva.jobs.movingavrank %>% filter(year == 2018) %>% View


#Moving av rank position change, mark SY
p = ggplot(
  gfcf.gva.jobs.movingavrank %>% mutate(SY = ITL_region_name == 'South Yorkshire'), 
  aes(x = year, y = rankpos, group = ITL_region_name)) +
  geom_point() +
  geom_line() 

ggplotly(p, tooltip = 'ITL_region_name')



#Maaaap
itl2.geo <- st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

#Join map data to a subset of the GVA data
gfcfratiomap <- itl2.geo %>% 
  right_join(
    gfcf.gva.jobs.movingavrank %>% filter(
      year==max(year)
    ),
    by = c('ITL221CD'='ITL_region_code')
  )


#Plot map
tm_shape(gfcfratiomap) +
  # tm_polygons('gfcf_perweeklyhour_worked', fill.scale = tm_scale(n = 9)) +
  tm_polygons('gfcf_perweeklyhour_worked_movingav', fill.scale = tm_scale_intervals(style = "jenks", n = 9)) +
  tm_layout(title = paste0('GFCF per weekly hour worked\n3 yr average\nITL2 regions ',gfcfratiomap$year[1]), legend.outside = T)




