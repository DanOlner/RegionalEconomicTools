# This example will run as-is in posit.cloud (free version)
# using the tidyverse template.

# Jobs and GVA per job cumulative plot
# So area (job x gvaperjob) = total GVA for that sector
# Which is a little bit circular as we just divide gva by jobs to get one of the axes
# But as with the other productivity plots, that proves visually quite useful
library(tidyverse)
library(RColorBrewer)

# If not present, install the ggrepel library 
# Then load it
# Will use for better labelling
if(!require(ggrepel)){
  install.packages("ggrepel")
  library(ggrepel)
}

# GET THE DATA
# Here's some I made earlier (Blue Peter style)
# Current BRES latest is 2024 though GVA is still on 2023
# Here we'll use a 3 year moving average for a bit more consistency over time
# Already has shortened sector names included for a neater plot
bres.gva.2d = readRDS(gzcon(url('https://github.com/DanOlner/RegionalEconomicTools/raw/refs/heads/gh-pages/data/bresgva2d_2023.rds')))



# GET SOME BESPOKE SECTOR-SPECIFIC COLOURS

# Get some consistent sector colours so each sector is the same
# A larger number than the standard brewer palettes need so let's get our own
# Nabbed from https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
n <- length(unique(bres.gva.2d$SIC07_description_shortened))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colourstouse <- col_vector[1:(1+(n-1))]



# GET THE ITL3 ZONES TO INCLUDE IN THE PLOT

# Use a lookup to get ITL3 names from within a specific ITL2
itl.lookup = read_csv('https://raw.githubusercontent.com/DanOlner/RegionalEconomicTools/refs/heads/gh-pages/data/LAD_(December_2024)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv')

# Check match between BRES data ITL3 codes and this lookup
table(unique(bres.gva.2d$ITL_code) %in% unique(itl.lookup$ITL325CD))

# Note, some falses the other way round because the lookup is UK-wide
# But BRES data is GB-only
# table(unique(itl.lookup$ITL325CD) %in% unique(bres.gva.2d$ITL_code))

# Open up the itl.lookup to search for place names
# Either with this code or click on its name in the environment panel top right
# itl.lookup %>% View




# We can use the lookup to get a list of ITL3s for specific ITL2s or ITL1s even
listofplaces = itl.lookup %>% 
  # filter(ITL225NM == 'West Yorkshire') %>% #Either look for direct match
  filter(ITL225NM == 'South Yorkshire') %>% #Either look for direct match
  # filter(grepl('west yorks',ITL225NM, ignore.case = T)) %>% #Or search for string
  pull(ITL325NM)

# Example for some other places
# Uncomment as appropriate
# listofplaces = itl.lookup %>%
  # filter(grepl('south yorks',ITL225NM, ignore.case = T)) %>%
  # filter(grepl('north yorks',ITL225NM, ignore.case = T)) %>%
  # pull(ITL325NM)

# Or if we want, we can set ITL3 names directly from the main dataset with a part-string match
# For example, this pulls out some specific cities
# In grepl, we're separating places with an OR symbol - |
# Also includes a second grepl line to say 'but not places with 'greater' in the name
# In this case, that exclues the various greater Manchester ITL3s picked up in the 1st grepl
# And a couple of 'nottinghamshires'
# listofplaces = bres.gva.2d %>%
#   select(Region_name) %>% 
#   distinct() %>% 
#   filter(
#     grepl('Leeds|Manch|Sheff|Nottingh|Bristol|glasgow',Region_name, ignore.case = T),
#     !grepl("greater|shire", Region_name, ignore.case = TRUE)
#     ) %>% 
#   pull() 

# Pick four random
listofplaces = sample(unique(bres.gva.2d$Region_name),12)
# Add in any extras


# listofplaces = c(listofplaces,
#                  bres.gva.2d %>%
#                    select(Region_name) %>%
#                    distinct() %>%
#                    filter(
#                      grepl('Blackpool|talbot',Region_name, ignore.case = T),
#                      # grepl('Leeds|Manch|Sheff|Nottingh|Bristol|glasgow',Region_name, ignore.case = T),
#                      # !grepl("greater|shire", Region_name, ignore.case = TRUE)
#                      ) %>%
#                    pull()
#                  )

# And you can of course set that string directly if you want to use the exact names
# Which you could get, for instance, by Viewing the data and searching in it
# listofplaces = c("Bristol, City of","Leeds","Manchester","Nottingham","Sheffield","Glasgow City")

# Keep just those places and pick out the latest available year
# Which is smoothed average of 2021 to 2023 (appears as 2022 in data)
places = bres.gva.2d %>% 
  filter(
    Region_name %in% listofplaces, 
    year == max(year)
  )



# MAKE THE DATA FOR THE PLOT AND PLOT IT! 

# Create data for plotting cumulative job blocks
plot.df = places %>%
  group_by(Region_name) %>%
  arrange(-`gva/job`) %>%
  mutate(xmin = cumsum(lag(jobcount_movingav, default = 0)),
         xmax = xmin + jobcount_movingav,
         ymin = 0,
         ymax = `gva/job`) %>%
  ungroup()

# Split label types based on size of job count
# So we can use two label types
# Neither does the job well - both together work alright when split
textcutoffsize = 1300

# Start plot - add extra plotting elements to p before finally plotting
# Note: control the layout in facet_wrap with ncol and nrow
p = ggplot() +
  geom_rect(data = plot.df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SIC07_description_shortened), color = "black", size =0.25) +
  labs(
    y = "GVA per FT job (1000s)", 
    x = "Job count", 
    title = "GVA-per-FT-job x job count for SIC sectors (Area = total GVA)"
  ) +
  scale_fill_manual(values = setNames(colourstouse,unique(bres.gva.2d$SIC07_description_shortened))) +
  guides(fill = F) +
  coord_flip(ylim = c(0,300)) +
  facet_wrap(~Region_name, scales = 'free_y', ncol = 6)

# Add basic text labels if block big enough
p = p + geom_text(
  data = plot.df %>% filter(jobcount_movingav > textcutoffsize), 
  aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = SIC07_description_shortened), size = 3) 

# Use ggrepel if too small
p = p + geom_text_repel(
  data = plot.df %>% filter(jobcount_movingav <= textcutoffsize),
  aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = SIC07_description_shortened),
  alpha=0.6,
  size = 3,
  box.padding = 0.5,
  ylim = c(175,NA),
  max.overlaps = 9999
)

# Plot!
# You may need to click on the 'Plot' pane to see it.
# Use the zoom button in the Plots panel to get a resizeable pop-out version
p

# SAVE!
# The save will be visible in the files tab.
# Download in the files tab on the right.
# ggsave('gvajobsblocks.png', width = 14, height = 12)