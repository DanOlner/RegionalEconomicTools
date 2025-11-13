#CV first. No NeweyWest
ynh.2dig <- plotSlopeCounts(
  df = itl3.2digit.cv,
  placename = itl3,
  startdate = 2014,
  enddate = 2023,
  date_colname = year,
  region_colname = Region_name,
  sector_colname = SIC07_description_shortened,
  value_colname = value,
  conf_interval = 95
)

ynh.gvaperjob.2dig <- plotSlopeCounts(
  df = bres.gva.2digit.2023,
  placename = itl3,
  startdate = 2015,
  enddate = 2023,
  date_colname = year,
  region_colname = Region_name,
  sector_colname = SIC07_description_shortened,
  value_colname = gvaperjob,#Note, this gets log'd in the function, don't do it here
  conf_interval = 95,
  includesectorname_on_axis = T
)

#Axes match - remove from RHS
ynh.gvaperjob.2dig$plot <- ynh.gvaperjob.2dig$plot +
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position = "bottom"
  ) +
  # axis.text.x=element_blank(),
  # axis.ticks.x=element_blank()) +
  guides(fill = F) +
  ggtitle("GVA/FT") 

#And only need the one legend!
ynh.2dig$plot <- ynh.2dig$plot +
  ggtitle("GVA") +
  theme(
    legend.position = "bottom",
    axis.title.y=element_blank()#add this explanation in figure text
  )

ynh.2dig$plot + ynh.gvaperjob.2dig$plot
