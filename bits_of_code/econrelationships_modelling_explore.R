#Explore modelled relationships between essential regional economic variables around employment and output
library(tidyverse)
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


















