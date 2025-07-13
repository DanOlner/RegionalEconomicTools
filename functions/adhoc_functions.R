#AD HOC FUNCTIONS 
#E.G. random plot repeaters in different places that will only get used once
#But nevertheless might need to be debuggable
library(tidyverse)

#Plot repeater for Bradford QML output
#Here: 
plotprod <- function(df){
  
  #find max bradford value and use that to set y axis max
  #filter out any infs...
  df <- df %>% filter(!is.infinite(job_output_propofUKtotal_movingav))
  
  maxfory = max(df$job_output_propofUKtotal_movingav[df$Region_name == 'Bradford'])
  
  ggplot() +
    geom_jitter(#Rest of UK first
      data =  df,
      aes(x = DATE, y = job_output_propofUKtotal_movingav, group = Region_name),
      alpha = 0.3, size = 1,
      width = 0.1) +
    coord_cartesian(ylim = c(0,maxfory * 2)) +
    geom_point(#Then Leeds and Bradford
      data =  df %>% filter(Region_name %in% c('Leeds','Bradford')),
      aes(x = DATE, y = job_output_propofUKtotal_movingav, group = Region_name, colour = Region_name, shape = Region_name),
      alpha = 1, size = 5) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    geom_line(#Then Leeds and Bradford
      data =  df %>% filter(Region_name %in% c('Leeds','Bradford')),
      aes(x = DATE, y = job_output_propofUKtotal_movingav, group = Region_name, colour = Region_name),
      alpha = 0.5, size = 1) +
    scale_colour_brewer(palette = 'Set1', direction = 1, name="") +
    scale_shape_manual(values = c(16,18), name="") +
    xlab('year') +
    facet_wrap(~SIC07_description_shortened, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10)))
  
}
