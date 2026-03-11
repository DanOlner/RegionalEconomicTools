#
library(tidyverse)
library(plotly)

# Via DFE: https://www.gov.uk/government/publications/neet-and-participation-local-authority-figures
# Which is only for 16-17 year olds
# ONS LFS no longer does below national level...
neet = read_csv('local/data/participation-in-education-training-and-neet-age-16-to-17-by-local-authority_2024-25/data/ud_neet_characteristics.csv')
#neet2 = read_csv('local/data/participation-in-education-training-and-neet-age-16-to-17-by-local-authority_2024-25/data/ud_participation_by_type.csv')

# Check LAs there...
neet %>% filter(grepl('York',la_name)) %>% distinct() %>% View

# Filter to what we need to plot
neetsub = neet %>% 
  filter(Age == '16-17',Characteristic_grouping == 'Total') %>% 
  select(
    time_period,la_name,NEETprop,Notknownprop
  ) %>% 
  pivot_longer(NEETprop:Notknownprop,names_to = 'var', values_to = 'proportion') %>% 
  # mutate(yny = ifelse(la_name %in% c('York','North Yorkshire'), la_name, 'other')) %>% 
  mutate(proportion = as.numeric(proportion))

p = ggplot() +
  geom_jitter(data = neetsub, aes(x = time_period, y = proportion), colour = 'black', alpha = 0.2, width = 0.2) +
  geom_line(data = neetsub %>% filter(la_name %in% c('York','North Yorkshire')), aes(x = time_period, y = proportion, colour = la_name), size = 0.35) +
  geom_point(data = neetsub %>% filter(la_name %in% c('York','North Yorkshire')), aes(x = time_period, y = proportion, colour = la_name), size = 3) +
  facet_wrap(~var, scales = 'free_y')

pp = ggplotly(p, tooltip = 'proportion') %>% layout(width = 1100, height = 900)

pp

htmlwidgets::saveWidget(pp, "docs/miscplots/NEETplot.html", selfcontained = TRUE)

