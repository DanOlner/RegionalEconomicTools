# Jobs and GVA per job cumulative plot
# So area (job x gvaperjob) = total GVA for that sector
# Which is a little bit circular as we just divide gva by jobs to get one of the axes
# But as with the other productivity plots that proves visually quite useful
library(tidyverse)
source('functions/misc_functions.R')
# theme_set(theme_grey())


# Current BRES latest is 2024 though GVA is still on 2023
# Here we'll use the moving avs for a bit more consistency over time
bres.gva.2d = readRDS('local/data/bresgva2d.rds') %>% 
  filter(!qg('households|agri|membership', SIC07_description))

shortsectornames <- read_csv('data/shortsectornames_for_regionalGVA_2digitSICs.csv')

bres.gva.2d <- bres.gva.2d %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )

place = bres.gva.2d %>% 
  filter(qg('sheffield|barnsley|doncaster|rotherham',Region_name), year == max(year))
# filter(qg("Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan",Region_name), year == max(year))
  # filter(Region_name %in% corecities, DATE == max(DATE)) %>% 
  # filter(qg('bradford|kirkees|calderdale|wakefield|leeds',Region_name), year == max(year))

# Supply a way to select ITL3s from within a specific ITL2


plot.df = place %>%
  # filter(productionsector == 'production') %>%
  # group_by(Region_name,productionsector) %>%
  group_by(Region_name) %>%
  arrange(-`gva/job`) %>%
  # arrange(-JOBCOUNT) %>%
  mutate(xmin = cumsum(lag(jobcount_movingav, default = 0)),
         xmax = xmin + jobcount_movingav,
         ymin = 0,
         ymax = `gva/job`) %>%
  ungroup()

#OK, apply to actual sector data
#Get some consistent sector colours first
n <- length(unique(bres.gva.2d$SIC07_description_shortened))
set.seed(12)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))

# randomcols <- sample(col_vector, n)
# n <- length(unique(itl3.sections.cv$SIC07_description))
randomcols <- col_vector[1:(1+(n-1))]

# Labels - will have to restrict ggrepel ones to those under a certain height.

ggplot(plot.df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SIC07_description_shortened), color = "black", size =0.25) +
  geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = SIC07_description_shortened), size = 3) +
  # labs(y = "GVA", x = "Job count", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  labs(y = "GVA per job (1000s)", x = "Job count", title = "Sectors by GVA-per-job and Jobs (Area = total GVA)") +
  # labs(x = "GVA", y = "Job count", title = "Sectors by GVA and Jobs (Area = GVA × Jobs)") +
  # theme_minimal() +
  scale_fill_manual(values = setNames(randomcols,unique(bres.gva.2d$SIC07_description_shortened))) +
  guides(fill = F) +
  coord_flip(ylim = c(0,300)) +
  # facet_wrap(~Region_name+productionsector, scales = 'free', ncol = 2)
  facet_wrap(~Region_name, scales = 'free_y', ncol = 5)

#So close but not quite. Could just use for upper labels?
# p + geom_text_repel(
#   aes(x = (xmin + xmax)/2.05, y = (ymin + ymax)/2, label = SIC07_description_shortened),
#   alpha=1,
#   size = 3,
#   nudge_x = .05,
#   box.padding = 1,
#   nudge_y = 0.05,
#   segment.curvature = -0.1,
#   segment.ncp = 0.3,
#   segment.angle = 20,
#   max.overlaps = 9999
# )




# Let's try and make a version with split label types based on size
textcutoffsize = 2000

p = ggplot() +
  geom_rect(data = plot.df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SIC07_description_shortened), color = "black", size =0.25) +
  labs(y = "GVA per job (1000s)", x = "Job count", title = "Sectors by GVA-per-job and Jobs (Area = total GVA)") +
  scale_fill_manual(values = setNames(randomcols,unique(bres.gva.2d$SIC07_description_shortened))) +
  guides(fill = F) +
  coord_flip(ylim = c(0,300)) +
  facet_wrap(~Region_name, scales = 'free_y', ncol = 5)

p = p + geom_text(
  data = plot.df %>% filter(jobcount_movingav > textcutoffsize), 
  aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = SIC07_description_shortened), size = 3) 

p = p + geom_text_repel(
  data = plot.df %>% filter(jobcount_movingav <= textcutoffsize),
  aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = SIC07_description_shortened),
  alpha=1,
  size = 3,
  # nudge_x = .05,
  box.padding = 0.5,
  # min.segment.length = 2,
  ylim = c(175,NA),
  # nudge_y = 0.05,
  # segment.curvature = -0.1,
  # segment.ncp = 0.3,
  # segment.angle = 20,
  max.overlaps = 9999
)

p





