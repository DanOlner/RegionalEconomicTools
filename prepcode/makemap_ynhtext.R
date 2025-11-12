# Make map
hexsummary.sector = hexsummary %>% filter(SIC07_description_from_ITL3 == sector)

#Link that back into the grid squares...
#Use right join to drop empties
sq.ch <- sq %>% 
  right_join(
    hexsummary.sector,
    by = 'id'
  ) %>% 
  filter(totalemployees_thisyear >= 10) %>% 
  mutate(
    emp_percentchange = percent_change(totalemployees_thisyear,totalemployees_lastyear)
  )

sq.ch <- sq.ch %>%
  mutate(hovertext = paste0("Firm count: ",totalfirms, ", id: ",id))

# OK, maybe not bivariate!
# tm_basemap("OpenStreetMap") +
#   tm_shape(mask) +#add in masking layer for rest of basemap
#   tm_polygons(col = 'white', fill = 'white') +
  tm_shape(itl3.ynh.zones, is.main = TRUE) +
  # tm_polygons(fill = '#a6baa8') +
  tm_polygons(fill = 'white', fill_alpha = 0.6) +
  # tm_shape(sq.ch %>% filter(emp_percentchange < 100)) +
  tm_shape(sq.ch %>% mutate(emp_percentchange = ifelse(emp_percentchange > 100, 100, emp_percentchange))) +#Cap at 100 for display purposes; those can be "100%+ increase"
  # tm_shape(sq.ch %>% filter(totalemployees_thisyear > 50)) +
  # tm_shape(sq.ch) +
  tm_polygons(
    fill = "emp_percentchange", id="hovertext",
    # fill = "totalemployees_thisyear", id="hovertext",
    fill.scale = tm_scale_intervals(style = "pretty", n = 3, values = "matplotlib.rd_yl_bu"),
    # fill.scale = tm_scale_intervals(style = "kmeans", n = 4, values = "matplotlib.rd_yl_bu"),
    col_alpha = 0.5
  ) +
  tm_shape(itl3.ynh.zones) +
  tm_borders(col_alpha = 0.3)
