# Script to get NOMIS data via NOMISR package
# And then save for use in Quarto

# See https://docs.evanodell.com/nomisr/articles/introduction.html
# For NOMISR essentials

#Load some functions (and install some packages if not present)
source('https://bit.ly/pipelinesetup')

# Load libraries (if running for first time)
library(tidyverse)
library(nomisr)

# STEP 1: Get info on all available datasets
nomisinfo = nomis_data_info() %>% 
  select(id, description.value, name.value)

# If we look inside that downloaded dataframe, we can search for the dataset code we want to use
# Let's search for 'employment' in RStudio...

# Now we can get metadata for the BRES data, having found the code
a <- nomis_get_metadata(id = "NM_189_1")

#Pick on some of those for a closer look
#Note, MEASURE in the actual downloaded data is "MEASURE_NAME" column...
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")

# This one's especially useful - we need it to get the right geography code
geogz = nomis_get_metadata(id = "NM_189_1", concept = "GEOGRAPHY", type = "type")

print(geogz, n = 60)


# From the metadata, we're going to pull out the geography codes
# That we need to put into NOMISR
# for JUST the core cities again
# Slightly different names / shapes in BRES, but mostly the same
# Again, here's one I made earlier...
corecities.bres = readRDS(gzcon(url('https://github.com/DanOlner/RegionalEconomicTools/raw/refs/heads/gh-pages/data/corecities_bres.rds')))


# USE LOCAL AUTHORITIES TO GET ALL TIMEPOINTS
# ITL3 2021 zones in the BRES data only have 2022-2024
# "TYPE424 local authorities: district / unitary (as of April 2023)"
# Belfast won't be there cos BRES is GB but otherwise good
placeid <- nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "TYPE424") %>% 
  filter(label.en %in% corecities.bres) %>% select(id) %>% pull

# NOW WE GET THE ACTUAL DATA
# Using many of the codes we just looked at
# Check how long it takes too...

# p.s. if you want to save time, I've also Blue Petered this data
# You'll have the option to use that in the Quarto doc rather than run NOMISR

# I'm also not sure if the NOMIS API will be made sad by us all trying to download at the same time...

x = Sys.time()

# So the actual GET DATA function
# We pass in the various values we got from above
# Note also the option to set the year - we don't do that, so we get all of them
# We also select only some columns, which does save a bit of download time
bres <- nomis_get_data(id = "NM_189_1",  geography = placeid,
                       # time = "latest",#Can use to get specific timepoint. If left out, we'll get em all
                       # MEASURE = 1,#Count of jobs
                       MEASURE = 2,#'Industry percentage'
                       MEASURES = 20100,#Just gives value (of percent) - redundant but lowers data download
                       EMPLOYMENT_STATUS = 2,#Full time jobs
                       select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
)

Sys.time() - x

# That takes 25-30 seconds in posit's R
# No fun if you're trying to check changes - don't want to run that every time
# Get the data in a different script and store somewhere the quarto doc can reload easily

# Save for use in quarto
# The RDS format is quick and compact
saveRDS(bres,'bres_nomisdatasave.rds')
