#MISC TESTS
library(tidyverse)
library(nomisr)
library(stringdist)

# TEST FUZZY MATCHING ON UK REGION NAMES----

#Using the two sets from ITL3 and BRES NUTS3 2016 above

#This does not do the most amazing job... wouldn't get good matches

# 1) Define cleaning function
clean_region_name <- function(x) {
  x %>%
    tolower() %>%
    gsub("&", "and", .) %>%
    gsub("[[:punct:]]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

# 2) Apply cleaning
df1_clean <- ITL3 %>%
  mutate(cleaned_itl3 = clean_region_name(Region_name))

df2_clean <- z %>%
  mutate(cleaned_nuts3 = clean_region_name(GEOGRAPHY_NAME))

# 3) Option A: Directly compute string distances between df1_clean and df2_clean
#    This is for illustration; typically you'd do a loop or cross join to compare each pair.

# Cross join, get all pairs
df_cross <- tidyr::crossing(df1_clean %>% select(cleaned_itl3), df2_clean %>% select(cleaned_nuts3))

# Compute Jaro-Winkler distances:
df_matches <- df_cross %>%
  mutate(distance_jw = stringdist(cleaned_itl3, cleaned_nuts3, method = "jw")) %>%
  # or compute similarity = 1 - distance_jw if you prefer similarity
  mutate(similarity_jw = 1 - distance_jw)

# Then pick best match for each row in df1_clean by highest similarity:
df_best_matches <- df_matches %>%
  group_by(cleaned_itl3) %>% #arbitrary, could group by other
  slice_max(similarity_jw, n = 1) %>%
  ungroup()




# Test alternative rolling average methods----

#Been using zoo's rollapply but is there a native tidyverse option?
#E.g. https://stackoverflow.com/a/75687758/5023561
#Oh that's another library anyway... 
#https://cran.r-project.org/web/packages/slider/vignettes/slider.html
