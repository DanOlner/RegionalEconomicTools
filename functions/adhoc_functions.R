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



# Return 2D percent change plot, specifically for ynh plotting
persector_jobsgva_percentchangeplot_ynh = function(sector, sectorpercentcutoff = 1.5){
  
    placestokeep <- bres.gva.2d %>% 
      filter(year == max(year), SIC07_description == sector) %>% 
      filter(sector_regional_propfrom_CP * 100 > sectorpercentcutoff) %>%#Keep only places where this sector makes up 1%+ of reg econ
      select(placename_shortest) %>% 
      distinct() %>%
      pull
    
    # Check we've got some places and some ynh places
    if(length(placestokeep) > 0 & mean(ynhshortnames %in% placestokeep) > 0){
      
      p <- twod_percentplot(
        df = bres.gva.2d %>% filter(SIC07_description == sector, placename_shortest %in% placestokeep),
        category_var = placename_shortest,
        x_var = gva_movingav,
        y_var = jobcount_movingav,#
        # y_var = JOBS_sector_regional_percent_movingav,#this shows structural change better - jobs have grown nominally in most sectors (but breaks GVA/job diagonal)
        timevar = year,
        label_var = `gva/job`,
        category_var_value_to_highlight = ynhshortnames,
        label_only_highlightedplaces = T,
        start_time = 2016,
        end_time = 2022,
        returndata = T,
        backgroundvectoralpha = 0.2,
        # useboxoverlayforlabels = T,
        overlay_arrowsize = 1
      )
      
      # Get range if percents just for any ynh itl3s present
      ynh_data = p[[2]] %>% filter(placename_shortest %in% ynhshortnames)
      
      xminmax = range(ynh_data$x_pct_change)
      yminmax = range(ynh_data$y_pct_change)
      
      # If no neg values, adjust
      # # if(xminmax[1] > 0) xminmax[1] = -20
      # if(yminmax[1] > 0) yminmax[1] = -20
      
      # Test values for within padding range
      # xminmax = c(-10,10)
      
      # Make sure each axis has at least a bit of padding
      padding = 20
      # xminmax = ifelse(abs(xminmax) < 20, 20 * (xminmax / abs(xminmax)), xminmax)
      
      xminmax <- pmax(pmin(xminmax, c(-padding, Inf)), c(-Inf, padding))
      yminmax <- pmax(pmin(yminmax, c(-padding, Inf)), c(-Inf, padding))
      
      # Zoom in on ynh places
      # p[[1]] = p[[1]] + coord_fixed()
      p[[1]] = p[[1]] + coord_fixed(xlim = xminmax * 1.2, ylim = yminmax * 1.2)
      
      p[[1]] <- p[[1]] + 
        # ggtitle(paste0(sector,': ', round(bradjobs/1000,1), 'K jobs in Bradford, ',bradpercentjobs,'% of tot')) +
        xlab("GVA % change 2015/17 av to 2021/23 av") +
        ylab("JOB COUNT % change 2015/17 av to 2021/23 av") 
        # ggtitle(sector)
      # coord_cartesian(xlim = c(-50,130), ylim = c(-30,100))
      
      return(p[[1]])
      
    }#End if length placestokeep
    
    return(NULL)
    
}




# FOR AUTOMATED QMD CREATION IN YNH_AUTOQMD
# Make 3 plots (or 2 if no data for 2D bres plot)
# And return in list
ynh_autoplots = function(sector){
  
  # Get ITL1 for Y&H with other ITL1s in background
  # Will just be a single row, don't need to order (yet)
  # Will order for final output by LQ in Y&H...
  LQ_slopes <- compute_slope_or_zero(
    data = itl1.summedtoitl3SICs, 
    Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = year)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- itl1.summedtoitl3SICs %>% filter(year == max(year))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- itl1.summedtoitl3SICs %>% 
    group_by(Region_name, SIC07_description) %>% 
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
      by = c('Region_name', 'SIC07_description')
    )
  
  # Then just plot for that single sector
  place = 'Yorkshire and The Humber'
  
  # Get LQ range to display
  # Here, range will be LQs for places for this sector 
  # not the range bars, most likely, though we may need to get whichever is highest/lowest
  # lqmin = yeartoplot %>% filter(SIC07_description == sector) %>% 
  #   filter(LQ == min(LQ)) %>% 
  #   pull(LQ)
  lq_range = range(yeartoplot$LQ[yeartoplot$SIC07_description == sector])
  
  # Display money value and regional prop on x axis
  yaxisdisplay = paste0(
    "£",
    round(yeartoplot %>% filter(SIC07_description == sector, Region_name == place) %>% pull(gva_movingav),0),
    "M, ",
    round(yeartoplot %>% filter(SIC07_description == sector, Region_name == place) %>% select(sector_regional_proportion) * 100,2),
    "%"
  )
  
  
  p <- LQ_baseplot(df = yeartoplot %>% filter(SIC07_description == sector), alpha = 0.8, shape = 0, sector_name = SIC07_description, LQ_column = LQ, change_over_time = slope, labelcolumn = placename_short, horriblehack = T)
  
  p <- addplacename_to_LQplot(df = yeartoplot %>% filter(SIC07_description == sector), plot_to_addto = p, maxLQvalmultiplier = 20,#hide it!
                              placename = place, shapenumber = 16,
                              min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                              value_column = gva_movingav, sector_regional_proportion = sector_regional_proportion,
                              region_name = Region_name,
                              sector_name = SIC07_description, change_over_time = slope, LQ_column = LQ,
                              text = 7, value_col_ismoney = T)
  
  p <- p + 
    # coord_cartesian(xlim = c(0.1,7)) +
    theme(
      # axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title = element_text(hjust = 0.5,face = "bold", size = 14)
    ) +
    ylab(yaxisdisplay) +
    # ggtitle(sector) +
    coord_cartesian(xlim = lq_range)
  
  
  
  
  # Get ITL3 plot made for the places in Y&H
  LQ_slopes <- compute_slope_or_zero(
    data = itl3.ynh, 
    Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = year)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- itl3.ynh %>% filter(year == max(year))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- itl3.ynh %>% 
    group_by(Region_name, SIC07_description) %>% 
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
      by = c('Region_name', 'SIC07_description')
    )
  
  # Join other SIC levels to it so we can break down into production / other
  # I think I have a lookup for that, though that will need a tweak due to one extra sector in ITL1
  
  # Have just upyeard regionalGVA_siccode_lookupmaker.R for ITL1 as well...
  # (Forgot I had that!)
  # gvalookup_itl3 = read_csv('data/siclookup_forregionalGVAcategories_ITL3.csv')
  
  # Confirm... tick
  # table(unique(gvalookup_itl1$SIC07_code) %in% unique(islq$SIC07_code))
  # yeartoplot <- yeartoplot %>%
  #   left_join(
  #     gvalookup_itl3 %>% select(-SIC07_description),
  #     by = 'SIC07_code'
  #   )
  
  
  # Make a column with the amount and % in to display as part of labels
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = paste0(
        placename_shorter,
        ": £",
        round(gva_movingav,0),
        "M, ",
        round(sector_regional_proportion * 100,2),
        "%"
      )
    )
  
  
  # Filter down just to the sector we're displaying
  # so we can get order correct
  yeartoplot = yeartoplot %>% filter(SIC07_description == sector)
  
  # placeLQorder <- yeartoplot %>% 
  #   arrange(-LQ) %>% 
  #   pull(displayregions) 
  
  #Turn the sector column into a factor and order by LQs
  # yeartoplot$displayregions <- factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = fct_reorder(displayregions,LQ_log,.desc = T)
    )
  
  # factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  
  
  # Get range to display on x axis
  # Use min and max of minmaxes here
  plot_range = minmaxes %>% filter(SIC07_description == sector) %>% 
    select(min_LQ_all_time:max_LQ_all_time) %>% 
    pivot_longer(min_LQ_all_time:max_LQ_all_time, names_to = 'cols', values_to = 'vals') %>% 
    ungroup() %>% 
    reframe(range = range(vals)) %>% 
    pull(range) 
  
  if(plot_range[1]==0) plot_range[1] = 0.1# Avoid log infs
  
  # debugonce(LQ_baseplot)
  p2 <- LQ_baseplot(df = yeartoplot, alpha = 1, sector_name = displayregions, 
                    LQ_column = LQ, change_over_time = slope, horriblehack = T)
  
  # debugonce(addplacename_to_LQplot)
  p2 <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p2, maxLQvalmultiplier = 20,#Hide it!
                               placename = sector, shapenumber = 16,
                               min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                               value_column = gva_movingav, sector_regional_proportion = sector_regional_proportion,
                               region_name = SIC07_description,
                               sector_name = displayregions, change_over_time = slope, LQ_column = LQ,
                               text = 7, value_col_ismoney = T)
  
  p2 <- p2 +
    coord_cartesian(xlim = plot_range) +
    ggtitle("")
  
  
  # ALSO GET JOBS/GVA 2D PLOT (worked out below)
  p3 = persector_jobsgva_percentchangeplot_ynh(sector)
  
  return(list(p,p2,p3))
  
  # If we have a 2D plot (i.e. at least one ynh place has 2%+ jobs in this sector)
  # if(!is.null(p3)){
  #   
  #   plottosave = p / p2 / p3 + patchwork::plot_layout(heights = c(2, 10, 10))
  #   
  #   # Remove punctuation and spaces from filename
  #   ggsave(paste0('local/outputs/ynh_sectorLQplots/',gsub('[[:punct:]]| ','',sector),'.png'), plot = plottosave, width = 9, height = 17)
  #   
  # } else {
  #   
  #   plottosave = p / p2 + patchwork::plot_layout(heights = c(2, 10))
  #   
  #   # Remove punctuation and spaces from filename
  #   ggsave(paste0('local/outputs/ynh_sectorLQplots/',gsub('[[:punct:]]| ','',sector),'.png'), plot = plottosave, width = 9, height = 11)
  #   
  # 
  
# }
  
}





# Ad hoc for getting 5 digit subsectors in SIC90 visualised
# For SY LAs vs core cities
# From both BRES and companies house job counts
sic90subsector_lqs = function(data,sector){
  
  # This is year in BRES data but for companies house is just difference between two timepoints
  # slopetimevar = enquo(slopetimevar)
  # Nope! Is called 'year' in CH data and will function the same
  
  # Get ITL3 plot made for the places in Y&H
  LQ_slopes <- compute_slope_or_zero(
    data = data, 
    Region_name, SIC07_description,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = year)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- data %>% filter(year == max(year))#use latest year
  
  
  # Filter down just to the sector we're displaying
  # so we can get order correct
  yeartoplot = yeartoplot %>% filter(SIC07_description == sector)
  
  
  # Filter to just yorkshire and core cities
  # corecities = readRDS('data/corecitiesvector.rds')
  
  # check... false will be Belfast
  table(corecities %in% c(yeartoplot$Region_name))
  
  # FILTER PLACE
  yeartoplot = yeartoplot %>% 
    filter(
      Region_name %in% c(corecities,toupper(c('Barnsley','Doncaster','Rotherham','Sheffield')))
      # Region_name %in% c(corecities,c('Barnsley','Doncaster','Rotherham','Sheffield'))
    )
  
  # Tick, I think
  unique(yeartoplot$Region_name)
  
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'SIC07_description')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- data %>% 
    group_by(Region_name, SIC07_description) %>% 
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
      by = c('Region_name', 'SIC07_description')
    )
  
  
  # Make a column with the amount and % in to display as part of labels
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = paste0(
        Region_name,
        ": ",
        # round(JOBCOUNT/1000,2),
        JOBCOUNT,
        ", ",
        round(sector_regional_proportion * 100,2),
        "%"
      )
    )
  
  # placeLQorder <- yeartoplot %>% 
  #   arrange(-LQ) %>% 
  #   pull(displayregions) 
  
  #Turn the sector column into a factor and order by LQs
  # yeartoplot$displayregions <- factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = fct_reorder(displayregions,LQ_log,.desc = T)
    )
  
  # factor(yeartoplot$displayregions, levels = placeLQorder, ordered = T)
  
  
  # Get range to display on x axis
  # Use min and max of minmaxes here
  plot_range = minmaxes %>% filter(SIC07_description == sector) %>% 
    select(min_LQ_all_time:max_LQ_all_time) %>% 
    pivot_longer(min_LQ_all_time:max_LQ_all_time, names_to = 'cols', values_to = 'vals') %>% 
    ungroup() %>% 
    reframe(range = range(vals)) %>% 
    pull(range) 
  
  if(plot_range[1]==0) plot_range[1] = 0.1# Avoid log infs
  
  # debugonce(LQ_baseplot)
  # Horrible hack because I added a sector order line that breaks the older version
  # But is needed for this one... le sigh
  p2 <- LQ_baseplot(df = yeartoplot, alpha = 1, sector_name = displayregions, 
                    LQ_column = LQ, change_over_time = slope, horriblehack = TRUE)
  
  # debugonce(addplacename_to_LQplot)
  p2 <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p2, maxLQvalmultiplier = 20,#Hide it!
                               placename = sector, shapenumber = 16,
                               min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                               value_column = JOBCOUNT, sector_regional_proportion = sector_regional_proportion,
                               region_name = SIC07_description,
                               sector_name = displayregions, change_over_time = slope, LQ_column = LQ,
                               text = 7, value_col_ismoney = T)
  
  p2 <- p2 +
    coord_cartesian(xlim = plot_range) +
    ggtitle(sector)
  
  p2
  
}



# Ad-hoc for working out IS-8 LQs separately for each grouping
is8_lqplot = function(is8name){
  
  # g(indstrat_sums)
  # Turn into an LQ-friendly df with the IS-8 as sector 1 and 'all other jobs here' as sector 2
  this_is8 = indstrat_sums %>%
    filter(indstrat_code == is8name) %>% 
    select(year = DATE,Region_name = GEOGRAPHY_NAME,totaljobs,jobs_minus_thisIS8)#where totaljobs is the IS-8 count and jobs_minus is all others
  
  # Now we just make these two columns long as their own sectors
  this_is8 = this_is8 %>% 
    pivot_longer(totaljobs:jobs_minus_thisIS8, names_to = 'sector', values_to = 'jobcount')
  
  
  # LQ from that for each year
  # And keep only the sector we want to view
  this_is8 = this_is8 %>%
    group_split(year) %>% 
    map(
      add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = sector,
      valuevar = jobcount
    ) %>% 
    bind_rows() %>% 
    filter(sector == 'totaljobs')
  
  
  # Pick manually to get just Y&H places
  # ynh itl3 names gets us most of the way
  
  itl3.ynh = tryCatch(
    readRDS('local/data/itl3ynh.rds'),
    error = function(e) readRDS('../local/data/itl3ynh.rds')
  )
  
  ynhnames = unique(itl3.ynh$Region_name)
  
  ynhnames = c(ynhnames[-c(2,11)], 'Calderdale','Kirklees','Lincolnshire','North Lincolnshire')
  
  # Tick
  table(ynhnames %in% this_is8$Region_name)
  
  # Keep only those
  this_is8 = this_is8 %>% 
    filter(
      Region_name %in% ynhnames
    )
  
  
  # Enshortenemateifyadoodoo
  # ynhshortnames - not that short? And also won't match LAs here
  unique(this_is8$Region_name)
  
  this_is8 = this_is8 %>%
    mutate(
      Region_name = case_when(
        Region_name == 'Lincolnshire' ~ 'L\'shire',
        qg('North linc',Region_name) ~ 'N L\'shire',
        qg('North york',Region_name) ~ 'N Yorks',
        qg('East riding',Region_name) ~ 'East Riding',
        .default = Region_name
      )
    )
  
  
  
  # Get ITL3 plot made for the places in Y&H
  LQ_slopes <- compute_slope_or_zero(
    data = this_is8, 
    Region_name, sector,#slopes will be found within whatever grouping vars are added here
    y = LQ_log, x = year)
  
  
  #Filter down to a single year... we may want to smooth years, let's see
  yeartoplot <- this_is8 %>% filter(year == max(year))#use latest year
  
  #Add slopes into data to get LQ plots
  yeartoplot <- yeartoplot %>% 
    left_join(
      LQ_slopes,
      by = c('Region_name', 'sector')
    )
  
  #Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
  minmaxes <- this_is8 %>% 
    group_by(Region_name, sector) %>% 
    summarise(
      min_LQ_all_time = min(LQ, na.rm = T),
      max_LQ_all_time = max(LQ, na.rm = T)
    ) %>% 
    mutate(
      min_LQ_all_time = ifelse(is.infinite(min_LQ_all_time),NA,min_LQ_all_time),
      max_LQ_all_time = ifelse(is.infinite(max_LQ_all_time),NA,max_LQ_all_time)
    )
  
  
  
  #Join min and max
  yeartoplot <- yeartoplot %>% 
    left_join(
      minmaxes,
      by = c('Region_name', 'sector')
    )
  
  # Join other SIC levels to it so we can break down into production / other
  # I think I have a lookup for that, though that will need a tweak due to one extra sector in ITL1
  
  # Have just upyeard regionalGVA_siccode_lookupmaker.R for ITL1 as well...
  # (Forgot I had that!)
  # gvalookup_itl3 = read_csv('data/siclookup_forregionalGVAcategories_ITL3.csv')
  
  # Confirm... tick
  # table(unique(gvalookup_itl1$SIC07_code) %in% unique(islq$SIC07_code))
  # yeartoplot <- yeartoplot %>%
  #   left_join(
  #     gvalookup_itl3 %>% select(-indstrat_code),
  #     by = 'SIC07_code'
  #   )
  
  
  # Make a column with the amount and % in to display as part of labels
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = paste0(
        Region_name,
        ": ",
        round(jobcount/1000,1),
        "K, ",
        round(sector_regional_proportion * 100,2),
        "%"
      )
    )
  
  
  yeartoplot = yeartoplot %>% 
    mutate(
      displayregions = fct_reorder(displayregions,LQ_log,.desc = T)
    )
  
  # factor(yeartoplot.sub$displayregions, levels = placeLQorder, ordered = T)
  
  
  # Get range to display on x axis
  # Use min and max of minmaxes here
  plot_range = minmaxes %>% 
    select(min_LQ_all_time:max_LQ_all_time) %>% 
    pivot_longer(min_LQ_all_time:max_LQ_all_time, names_to = 'cols', values_to = 'vals') %>% 
    ungroup() %>% 
    reframe(range = range(vals)) %>% 
    pull(range) 
  
  if(plot_range[1]==0) plot_range[1] = 0.1# Avoid log infs
  
  # debugonce(LQ_baseplot)
  p2 <- LQ_baseplot(df = yeartoplot, alpha = 1, sector_name = displayregions, 
                    LQ_column = LQ, change_over_time = slope, horriblehack = T)
  
  # debugonce(addplacename_to_LQplot)
  p2 <- addplacename_to_LQplot(df = yeartoplot, plot_to_addto = p2, maxLQvalmultiplier = 20,#Hide it!
                               placename = unique(yeartoplot$sector), shapenumber = 16,
                               min_LQ_all_time = min_LQ_all_time,max_LQ_all_time = max_LQ_all_time,#Include minmax
                               value_column = jobcount, sector_regional_proportion = sector_regional_proportion,
                               region_name = sector,
                               sector_name = displayregions, change_over_time = slope, LQ_column = LQ,
                               text = 7, value_col_ismoney = T)
  
  p2 +
    coord_cartesian(xlim = plot_range) +
    ggtitle(is8name)
  
  
  
}





















