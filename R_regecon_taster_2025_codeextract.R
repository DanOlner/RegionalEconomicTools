knitr::opts_chunk$set(fig.height = 8, fig.width = 10, echo = TRUE, warning = F, error = F, message = F, comment=NA)



2+2

sqrt(64)



2+2

sqrt(64)


x = 64

x

# library(tidyverse)

#Load some functions and two libraries (and install those libraries if they're not already present)
source('https://bit.ly/rtasterfunctions')


# LOAD ITL3 GVA DATA----

#ITL3 zones, SIC sections, current prices, 2023 latest data (minus imputed rent)
df = read_csv('https://bit.ly/rtasteritl3noir')

#This one has imputed rent included if you want it
# df = read_csv('https://bit.ly/rtasteritl3')



unique(df$SIC07_description)

# unique(df$Region_name)[order(unique(df$Region_name))]

df %>% 
  filter(year == max(year)) %>% 
  group_by(SIC07_description) %>% 
  summarise(
    median_gva = median(value),
    min_gva = min(value),
    max_gva = max(value)
    ) %>% 
  arrange(-median_gva)

# #Piping image from
# #https://s3.amazonaws.com/conference-handouts/2015-nctm-boston/pdfs/Fun%20Functions%20Handout%202013.pdf


#Boooo, nested brackets...
sqrt(log10(10000))

#Wooo! Lovely piped functions!
10000 %>% sqrt %>% log10


# LOCATION QUOTIENTS/PROPORTIONS----

add_location_quotient_and_proportions(
  df %>% filter(year == max(year)),
  regionvar = Region_name,
  lq_var = SIC07_description,
  valuevar = value
) %>% glimpse

#Finding location quotients for each year in the data 
#And then overwriting the df variable with the result
df = df %>%
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()

# #View a single place...
# df %>% filter(
#   qg('rotherh',Region_name),
#   year == max(year)#get latest year in data
# ) %>%
#   mutate(regional_percent = sector_regional_proportion *100) %>%
#   select(SIC07_description,regional_percent, LQ) %>%
#   arrange(-LQ)
# 

#View a single place...
df %>% filter(
  qg('rotherham',Region_name),
  year == max(year)#get latest year in data
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) 


# PLOT STRUCTURAL CHANGE OVER TIME IN ITL3 ZONE----

#Processing before plot

#1. Set window for rolling average: 3 years
smoothband = 3

#2. Make new column with rolling average in (applying to all places and sectors)
#Years are already in the right order, so will roll apply across those correctly
df = df %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    sector_regional_prop_movingav = rollapply(
      sector_regional_proportion,smoothband,mean,align='center',fill=NA
      )
  ) %>% 
  ungroup()

#3. Pick out the name of the place / ITl3 zone to look at
#We only need a bit of the name...
place = getdistinct('doncas',df$Region_name)

#Check we found the single place we wanted (some search terms will pick up none or more than one)
place



# top 12 sectors in latest year
top12sectors = df %>%
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>%
  arrange(-sector_regional_prop_movingav) %>%
  slice_head(n= 12) %>%
  select(SIC07_description) %>%
  pull()

# #Plot!
# ggplot(df %>%
#          filter(
#            Region_name == place,
#            SIC07_description %in% top12sectors,
#            !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
#            ),
#        aes(x = year, y = sector_regional_prop_movingav * 100, colour =
#              fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
#   geom_point() +
#   geom_line() +
#   scale_color_brewer(palette = 'Paired', direction = 1) +
#   ylab('Sector: percent of regional economy') +
#   labs(colour = paste0('Largest 12 sectors in ',max(df$year))) +
#   ggtitle(paste0(place, ' GVA % of economy over time (', smoothband,' year moving average)'))
# 

#Plot! 
ggplot(df %>%
         filter(
           Region_name == place,
           SIC07_description %in% top12sectors,
           !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
           ),
       aes(x = year, y = sector_regional_prop_movingav * 100, colour =
             fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('Sector: percent of regional economy') +
  labs(colour = paste0('Largest 12 sectors in ',max(df$year))) +
  ggtitle(paste0(place, ' GVA % of economy over time (', smoothband,' year moving average)'))


# CHAINED VOLUME ITL3 DATA----

df.cv = read_csv('https://bit.ly/rtasteritl3cvnoir')

#Get the text for an ITL3 zone by searching for a bit of the name
place = getdistinct('barns',df$Region_name)

#Check we got just one place!
place


# startyear = 2014
# endyear = 2023
# 
# #Pick out single slopes to illustrate
# ggplot(
#   df.cv %>% filter(
#     year %in% startyear:endyear,
#     Region_name == place,
#     qg('information|professional', SIC07_description)
#   ),
#   aes(x = year, y = value, colour = SIC07_description)
# ) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = 'lm') +
#   ggtitle(paste0(place, ' selected sectors\nGVA change over time (chained volume)'))
# 

startyear = 2014
endyear = 2023

#Pick out single slopes to illustrate
ggplot(
  df.cv %>% filter(
    year %in% startyear:endyear,
    Region_name == place,
    qg('information|professional', SIC07_description)
  ),
  aes(x = year, y = value, colour = SIC07_description)
) +
  geom_line() +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle(paste0(place, ' selected sectors\nGVA change over time (chained volume)'))


#Get all slopes for sectors in that place
slopes = get_slope_and_se_safely(
  df.cv %>% filter(
    year %in% startyear:endyear,
    Region_name == place,
    !qg('households', SIC07_description)
    ), 
  SIC07_description, 
  y = log(value), 
  x = year, 
  neweywest = T)


#Add 95% confidence intervals around the slope
slopes = slopes %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )

# #Adjust to get percentage change per year
# slopes = slopes %>%
#   mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))
# 
# #Plot the slope with those confidence intervals
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(SIC07_description,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   coord_cartesian(xlim = c(-25,25)) +
#   xlab('Av. % change per year') +
#   ylab("") +
#   ggtitle(paste0(place, ' broad sectors av. chained volume GVA % change per year\n',startyear,'-',endyear,', 95% conf intervals'))
# 


#Adjust to get percentage change per year from log slopes
slopes = slopes %>% 
  mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))

#Plot the slope with those confidence intervals
ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  coord_cartesian(xlim = c(-25,25)) +
  xlab('Av. % change per year') +
  ylab("") +
  ggtitle(paste0(place, ' broad sectors av. chained volume GVA % change per year\n',startyear,'-',endyear,', 95% conf intervals'))
  


# EXAMPLE OF GETTING DATA DIRECTLY (GET NATIONAL SECTION DATA)----

#2025:
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

itl2.all <- readxl::read_excel(path = p1f,range = "Table 2a!A2:AD4556")

names(itl2.all) <- gsub(x = names(itl2.all), pattern = ' ', replacement = '_')

itl2.all = itl2.all %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl2.all)[length(names(itl2.all))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))



startyear = 2011
endyear = 2023

#Get all slopes for sectors in that place
slopes = get_slope_and_se_safely(
  itl2.all %>% filter(year %in% c(startyear:endyear)), 
  Region_name,
  y = log(value), 
  x = year, 
  neweywest = T)


#Add 95% confidence intervals around the slope
slopes = slopes %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )


# #Adjust to get percentage change per year from log slopes
# slopes = slopes %>%
#   mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))
# 
# #Plot the slope with those confidence intervals
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   xlab('Av. % change per year') +
#   ylab("") +
#   ggtitle('All industries in ITL2 zones, chained volume GVA\nAv. % change per year\n2014-2023, 95% conf intervals')
# 
# 

#Adjust to get percentage change per year from log slopes
slopes = slopes %>% 
  mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))

#Plot the slope with those confidence intervals
ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('Av. % change per year') +
  ylab("") +
  ggtitle('All industries in ITL2 zones, chained volume GVA\nAv. % change per year\n2014-2023, 95% conf intervals')



# n <- length(unique(df$SIC07_description))
# set.seed(12)
# qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# 
# randomcols <- col_vector[10:(10+(n-1))]
# 
# #Do once!
# getplot <- function(place){
# 
#   df = df %>%
#     filter(!qg("Activities of households|agri",SIC07_description)) %>%
#     mutate(
#       SIC07_description = case_when(
#         grepl('defence',SIC07_description,ignore.case = T) ~ 'public/defence',
#         grepl('support',SIC07_description,ignore.case = T) ~ 'admin/support',
#         grepl('financ',SIC07_description,ignore.case = T) ~ 'finance/insu',
#         grepl('agri',SIC07_description,ignore.case = T) ~ 'Agri/mining',
#         grepl('electr',SIC07_description,ignore.case = T) ~ 'power/water',
#         grepl('information',SIC07_description,ignore.case = T) ~ 'ICT',
#         grepl('manuf',SIC07_description,ignore.case = T) ~ 'Manuf',
#         grepl('other',SIC07_description,ignore.case = T) ~ 'other',
#         grepl('scientific',SIC07_description,ignore.case = T) ~ 'Scientific',
#         grepl('real estate',SIC07_description,ignore.case = T) ~ 'Real est',
#         grepl('transport',SIC07_description,ignore.case = T) ~ 'Transport',
#         grepl('entertainment',SIC07_description,ignore.case = T) ~ 'Entertainment',
#         grepl('human health',SIC07_description,ignore.case = T) ~ 'Health/soc',
#         grepl('food service activities',SIC07_description,ignore.case = T) ~ 'Food/service',
#         grepl('wholesale',SIC07_description,ignore.case = T) ~ 'Retail',
#         .default = SIC07_description
#       )
#     )
# 
#   top12sectors = df %>%
#     filter(
#       Region_name == place,
#       year == max(year) - 1#to get 3 year moving average value
#     ) %>%
#     arrange(-sector_regional_prop_movingav) %>%
#     slice_head(n= 12) %>%
#     select(SIC07_description) %>%
#     pull()
# 
# 
#   ggplot(df %>%
#            filter(
#              Region_name == place,
#              SIC07_description %in% top12sectors,
#              !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
#            ),
#          aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
#     geom_point() +
#     geom_line() +
#     scale_color_manual(values = setNames(randomcols,unique(df$SIC07_description))) +#set manual pastel colours matching sector name
#     ylab('Sector: percent of regional economy') +
#     theme(plot.title = element_text(face = 'bold')) +
#     labs(colour = 'SIC section') +
#     # scale_y_log10() +
#     ggtitle(paste0(place, ' GVA % of economy over time\nTop 12 sectors in ',max(df$year),'\n (', smoothband,' year moving average)'))
# 
# 
# }
# 
# places = getdistinct('doncas|roth|barns|sheff',df$Region_name)
# 
# plots <- map(places, getplot)
# 
# patchwork::wrap_plots(plots)
# 

n <- length(unique(df$SIC07_description))
set.seed(12)
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

randomcols <- col_vector[10:(10+(n-1))]

#Do once!
getplot <- function(place){
  
  df = df %>% 
    filter(!qg("Activities of households|agri",SIC07_description)) %>% 
    mutate(
      SIC07_description = case_when(
        grepl('defence',SIC07_description,ignore.case = T) ~ 'public/defence',
        grepl('support',SIC07_description,ignore.case = T) ~ 'admin/support',
        grepl('financ',SIC07_description,ignore.case = T) ~ 'finance/insu',
        grepl('agri',SIC07_description,ignore.case = T) ~ 'Agri/mining',
        grepl('electr',SIC07_description,ignore.case = T) ~ 'power/water',
        grepl('information',SIC07_description,ignore.case = T) ~ 'ICT',
        grepl('manuf',SIC07_description,ignore.case = T) ~ 'Manuf',
        grepl('other',SIC07_description,ignore.case = T) ~ 'other',
        grepl('scientific',SIC07_description,ignore.case = T) ~ 'Scientific',
        grepl('real estate',SIC07_description,ignore.case = T) ~ 'Real est',
        grepl('transport',SIC07_description,ignore.case = T) ~ 'Transport',
        grepl('entertainment',SIC07_description,ignore.case = T) ~ 'Entertainment',
        grepl('human health',SIC07_description,ignore.case = T) ~ 'Health/soc',
        grepl('food service activities',SIC07_description,ignore.case = T) ~ 'Food/service',
        grepl('wholesale',SIC07_description,ignore.case = T) ~ 'Retail',
        .default = SIC07_description
      )
    )
  
  top12sectors = df %>%
    filter(
      Region_name == place,
      year == max(year) - 1#to get 3 year moving average value
    ) %>%
    arrange(-sector_regional_prop_movingav) %>%
    slice_head(n= 12) %>%
    select(SIC07_description) %>%
    pull()
  
  
  ggplot(df %>% 
           filter(
             Region_name == place, 
             SIC07_description %in% top12sectors,
             !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
           ), 
         aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = setNames(randomcols,unique(df$SIC07_description))) +#set manual pastel colours matching sector name
    ylab('Sector: percent of regional economy') +
    theme(plot.title = element_text(face = 'bold')) +
    labs(colour = 'SIC section') +
    # scale_y_log10() +
    ggtitle(paste0(place, ' GVA % of economy over time\nTop 12 sectors in ',max(df$year),'\n (', smoothband,' year moving average)'))
  
  
}

places = getdistinct('doncas|roth|barns|sheff',df$Region_name)

plots <- map(places, getplot)

patchwork::wrap_plots(plots)


# 
# #* **Follow [this little guide here](https://danolner.github.io/posts/setting_up_w_r_online/#if-using-rstudio-online-set-up-a-posit.cloud-account-and-create-your-rstudio-project){target="_blank"} to do that (opens in new tab).**
# 
# 
# knitr::purl(input = "R_regecon_taster_2025_revealjs.qmd", output = "R_regecon_taster_2025_codeextract.R",documentation = 0)
# 
# #Mark time periods in the data to group by
# itl2.all = itl2.all %>%
#   mutate(
#     timeperiod = case_when(
#       year %in% c(1998:2010) ~ '1998 to 2010',
#       year %in% c(2011:2023) ~ '2011 to 2023'
#     )
#   )
# 
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope), colour = timeperiod)) +
#   geom_point(position = position_dodge(width = 0.5)) +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), position = position_dodge(width = 0.5), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   coord_cartesian(xlim = c(-5,5)) +
#   xlab('Av. % change per year') +
#   ylab("") +
#   ggtitle(paste0(place, ' broad sectors av. chained volume GVA % change per year\n',startyear,'-',endyear,', 95% conf intervals'))
# 
# 
