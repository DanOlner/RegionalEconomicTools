#ONS LOCAL SESSION 27 NOV 2025: TEST CODE
#PIPELINE ESSENTIALS
library(tidyverse)
library(xml2)
source('functions/misc_functions.R')

# Get zip list from nomis website for bulk census download----


website_basename = "https://www.nomisweb.co.uk"

doc = read_html(paste0(website_basename,"/sources/census_2021_bulk"))

# Extract all <a> tags with hrefs containing "census2021*.zip"
zip_links = xml_attr(
  xml_find_all(doc, "//a[contains(@href, 'census2021') and contains(@href, '.zip')]"),
  "href"
)

#Set global timeout to very bigly indeed
#ten hours per file covers all eventualities!
options(timeout = 36000)

url1 = paste0(website_basename,zip_links[1])
p1f = tempfile(fileext=".zip")
download.file(url1, p1f, mode="wb")

# List what's in that but don't unzip
filenames = unzip(p1f, list = TRUE)

#Stick into dataframe
census2021data = read_csv(unzip(p1f, files = getdistinct('utla',filenames$Name)))
