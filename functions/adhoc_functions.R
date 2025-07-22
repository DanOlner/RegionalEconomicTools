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
plotprod <- function(df,vartouse){
  
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
  placestoadd = unique(df$Region_name[qg('Leeds|Bradford|kirklees|wakef',df$Region_name)])
  
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
