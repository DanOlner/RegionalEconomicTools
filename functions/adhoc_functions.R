#AD HOC FUNCTIONS 
#E.G. random plot repeaters in different places that will only get used once
#But nevertheless might need to be debuggable
library(tidyverse)



#FUNCTION FOR EACH BRES JOB GROUPING
#TO OUTPUT GGPLOT LQs
#Used here:
LQplot_BRES_groupsof5digit <- function(df_singletwodigitgrouping){
  
  #If I could plot both and space them out, that would be good (could get Bradford change showing too)
  p <- LQ_baseplot(df = df_singletwodigitgrouping, alpha = 0.15, shape = 0, sector_name = SIC_5DIGIT_NAME_SHORT, 
                   LQ_column = LQ_movingav, change_over_time = slope)
  
  #Don't try if no values (but keep base plot...)
  
  p <- addplacename_to_LQplot(df = df_singletwodigitgrouping, plot_to_addto = p,
                              placename = place, shapenumber = 16,
                              min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                              value_column = jobcount_movingav, sector_regional_proportion = sector_regional_proportion_movingav,
                              region_name = GEOGRAPHY_NAME,
                              sector_name = SIC_5DIGIT_NAME_SHORT, change_over_time = slope, LQ_column = LQ_movingav,
                              value_col_ismoney = F,
                              text = 7)
    

  #Will (should!) be single value
  #Add in other details
  totjobs = df_singletwodigitgrouping %>% filter(GEOGRAPHY_NAME == 'Bradford') %>% summarise(totjobs = sum(jobcount_movingav)) %>% select(totjobs) %>% pull
  totjobsLQmorethan1 = df_singletwodigitgrouping %>% filter(GEOGRAPHY_NAME == 'Bradford',LQ_movingav >= 1) %>% summarise(totjobs = sum(jobcount_movingav)) %>% select(totjobs) %>% pull
  totjobsLQlessthan1 = df_singletwodigitgrouping  %>% filter(GEOGRAPHY_NAME == 'Bradford',LQ_movingav < 1) %>% summarise(totjobs = sum(jobcount_movingav)) %>% select(totjobs) %>% pull
  
  totjobs.percent = df_singletwodigitgrouping %>% filter(GEOGRAPHY_NAME == 'Bradford')%>% summarise(totjobspercent = sum(sector_regional_proportion_movingav * 100)) %>% select(totjobspercent) %>% pull %>% round(2)
  totjobsLQmorethan1.percent = df_singletwodigitgrouping %>% filter(GEOGRAPHY_NAME == 'Bradford',LQ_movingav >= 1) %>% summarise(totjobspercent = sum(sector_regional_proportion_movingav * 100)) %>% select(totjobspercent) %>% pull%>% round(2)
  totjobsLQlessthan1.percent = df_singletwodigitgrouping  %>% filter(GEOGRAPHY_NAME == 'Bradford',LQ_movingav < 1) %>% summarise(totjobspercent = sum(sector_regional_proportion_movingav * 100)) %>% select(totjobspercent) %>% pull%>% round(2)
  
  
  
  p + ggtitle(
    paste0(
      unique(df_singletwodigitgrouping$SIC_2DIGIT_NAME_SHORT),' (2 digit)\n',
      totjobs,' FT jobs (',totjobs.percent,'%)\n',
      'LQ >= 1: ',totjobsLQmorethan1,' jobs (',totjobsLQmorethan1.percent,'%)\nLQ < 1: ',totjobsLQlessthan1,' jobs (',totjobsLQlessthan1.percent,'%)'
    )
    ) 
  
  
}



#Plot repeater for Bradford QML output
#Here: https://github.com/DanOlner/RegionalEconomicTools/blob/f91ff96d13740e6efe43d2ccb6ae7239766b9326/quarto_docs/Bradford_sectorclusters.qmd#L235
# Can overwrite default names
plotprod <- function(df,vartouse, placestoadd = c("Bradford","Calderdale and Kirklees","Leeds","Wakefield")){
  
  vartouse = enquo(vartouse) 
  
  # print(paste0('Grouping:',unique(df$sectorgrouping))) 
  
  #find max bradford value and use that to set y axis max 
  #filter out any infs...
  # df <- df %>% filter(!is.infinite(quo_name(vartouse))) 
  df <- df %>% filter(!is.infinite(!!vartouse)) 
  
  # maxfory = max(df$!!vartouse[df$Region_name == 'Bradford'])
  maxfory = df %>%
    filter(Region_name == 'Bradford') %>% 
    select(!!vartouse) %>%
    filter(!!vartouse == max(!!vartouse)) %>% 
    pull
  
  #Multiplier for max y axis
  maxfory <- maxfory * 1.5
    
  # placestoadd = unique(df$Region_name[qg('Leeds|Bradford',df$Region_name)])
  # placestoadd = unique(df$Region_name[qg('Leeds|Bradford|kirklees|wakef',df$Region_name)])
  
  ggplot() +
    geom_point(#Rest of UK first
      data =  df,
      aes(x = DATE, y = !!vartouse, group = Region_name),
      alpha = 0.15, size = 1) +
    coord_cartesian(ylim = c(0,maxfory)) +
    geom_point(#Then Leeds and Bradford
      data =  df %>% filter(Region_name %in% placestoadd),
      aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name, shape = Region_name),
      # aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name),
      alpha = 0.8, size = 2) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    geom_line(#Then Leeds and Bradford
      data =  df %>% filter(Region_name %in% placestoadd),
      aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name),
      alpha = 0.5, size = 1) +
    geom_point(#Then Leeds and Bradford
      data =  df %>% filter(Region_name == 'Bradford'),
      aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name, shape = Region_name),
      # aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name),
      alpha = 1, size = 3) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    geom_line(#Then Leeds and Bradford
      data =  df %>% filter(Region_name == 'Bradford'),
      aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name),
      alpha = 0.5, size = 1) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    scale_shape_manual(values = c(17,16,16,16), name="") +
    xlab('year') +
    ylab('GVA/job (1000s)') +
    # ylab('GVA/job PPM of UK economy') +
    facet_wrap(~SIC07_description_shortened, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10)))
  
}



#Plot repeater for general GVA per job data
# Can overwrite default names
plotprod_generic <- function(df,vartouse, placestoadd){
  
  vartouse = enquo(vartouse) 
  
  # print(paste0('Grouping:',unique(df$sectorgrouping))) 
  
  #find max bradford value and use that to set y axis max 
  #filter out any infs...
  # df <- df %>% filter(!is.infinite(quo_name(vartouse))) 
  df <- df %>% filter(!is.infinite(!!vartouse)) 
  
  # maxfory = max(df$!!vartouse[df$Region_name == 'Bradford'])
  maxfory = df %>%
    filter(Region_name %in% placestoadd) %>% 
    select(!!vartouse) %>%
    filter(!!vartouse == max(!!vartouse)) %>% 
    pull
  
  #Multiplier for max y axis
  # maxfory <- maxfory * 1.5
    
  # placestoadd = unique(df$Region_name[qg('Leeds|Bradford',df$Region_name)])
  # placestoadd = unique(df$Region_name[qg('Leeds|Bradford|kirklees|wakef',df$Region_name)])
  
  ggplot() +
    geom_point(#Rest of UK first
      data =  df,
      aes(x = DATE, y = !!vartouse, group = Region_name),
      alpha = 0.15, size = 1) +
    coord_cartesian(ylim = c(0,maxfory)) +
    geom_point(
      data =  df %>% filter(Region_name %in% placestoadd),
      aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name, shape = Region_name),
      # aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name),
      alpha = 0.8, size = 2) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    geom_line(
      data =  df %>% filter(Region_name %in% placestoadd),
      aes(x = DATE, y = !!vartouse, group = Region_name, colour = Region_name),
      alpha = 0.5, size = 1) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    scale_shape_manual(values = c(17,16,16,16), name="") +
    xlab('year') +
    ylab('GVA/job (1000s)') +
    # ylab('GVA/job PPM of UK economy') +
    facet_wrap(~SIC07_description_shortened, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10)))
  
}


# For BRES 5 digit, return a summarised job count of different SIC code levels based on digit length
bres_countjobs_by_SICdigitlevel = function(digitlevel,bres){
  
  bres = bres %>% 
    mutate(newsic = str_sub(SIC_5DIGIT_CODE,1,digitlevel)) %>% 
    group_by(DATE,GEOGRAPHY_NAME,newsic) %>% 
    summarise(
      JOBCOUNT = sum(JOBCOUNT)
    ) %>% 
    ungroup()
  
  names(bres)[names(bres) == 'newsic'] = paste0('sic',digitlevel)
  
  return(bres)
  
}


# Function up LQ and hanger-on bits and bobs
# So can all be repeated easily for e.g. different SIC code levels
# Currently only for BRES stuff in misc_checks.R, while doing indstrat work
getLQs_and_attachedstuff = function(bres_df){
  
  # cat("BRES level: ", bres_df %>% select(contains('sic')) %>% distinct() %>% pull(),"\n")
  levelcolname = names(bres_df)[3]
  
  cat("BRES level: ", levelcolname,"\n")
  
  islq <- bres_df %>% 
    group_split(DATE) %>% 
    map(
      add_location_quotient_and_proportions,
      regionvar = GEOGRAPHY_NAME,
      lq_var = !!sym(levelcolname),#string name to symbol
      valuevar = JOBCOUNT
    ) %>% 
    bind_rows() 
  
  LQ_slopes <- compute_slope_or_zero(
    data = islq, 
    GEOGRAPHY_NAME, !!sym(levelcolname),#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = DATE)
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- islq %>% filter(DATE == max(DATE))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('GEOGRAPHY_NAME', levelcolname)
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- islq %>% 
    group_by(GEOGRAPHY_NAME, !!sym(levelcolname)) %>% 
    summarise(
      min_LQ_all_time = min(LQ, na.rm = T),
      max_LQ_all_time = max(LQ, na.rm = T)
    ) %>% 
    mutate(
      min_LQ_all_time = ifelse(is.infinite(min_LQ_all_time),NA,min_LQ_all_time),
      max_LQ_all_time = ifelse(is.infinite(max_LQ_all_time),NA,max_LQ_all_time)
    )
  
  # table(is.infinite(minmaxes$min_LQ_all_time))
  # table(is.infinite(minmaxes$max_LQ_all_time))
  
  #Join min and max
  yeartoplot <- yeartoplot %>% 
    left_join(
      minmaxes,
      by = c('GEOGRAPHY_NAME', levelcolname)
    )
  
  return(list(lqs = islq, yeartoplot = yeartoplot))
  
}

# Check for and list any SICS that have other SICs nested inside them
# So we can avoid double-counting total IS-8 percentages.
checksic = function(sicname) {
  
  uniquesics[which(qg(paste0('^',sicname),uniquesics))]
  
  # if(x[[1]]!=sicname) print(x)
}


# For getting a list of SICs to drop to keep uniques within each IS8 grouping
droplist_foreachIS8 = function(is8name) {
  
  uniquesics <<- unique(lqs_for_indstrat %>% filter(indstrat_code == is8name) %>% select(sic) %>% pull)
  
  dropfirst = map(uniquesics, checksic)
  dropfirst = map(dropfirst, ~.x[-1])
  # Removing these will leave only ones we can sum to get total IndStrat jobs
  dropthese = unlist(dropfirst)
  
  return(list(is8 = is8name, dropthese = dropthese))
  
}



