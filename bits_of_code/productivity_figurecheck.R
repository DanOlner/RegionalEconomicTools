#Check match between ITL2 vs MCA productivity figures
library(tidyverse)

#For both, getting table A4: 
#"Table A4: Current Price (unsmoothed) GVA (B) per hour worked (£); Combined Authorities and Economic Enterprise Regions, 2004 - 2023"


#Get ITL2 productivity numbers----

url1 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivitygvaperhourworkedandgvaperfilledjobindicesbyuknuts2andnuts3subregions/current/labourproductivityitls1.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb")

#Use current prices in £ sheet
prodITL2 <- readxl::read_excel(path = p1f,range = "A4!A5:W247")

names(prodITL2) <- gsub(x = names(prodITL2), pattern = ' ', replacement = '_')

#Tidy
prodITL2 = prodITL2 %>% 
  filter(ITL_level == 'ITL2') %>% 
  pivot_longer(Pounds_2004:Pounds_2023, names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(
    year = substr(year,8,11),
    year = as.numeric(year)
  ) %>% 
  select(-ITL_level)


#Get version of numbers from MCA / EA sheet----

url2 <- 'https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivityindicesbycombinedauthoritiesandeconomicenterpriseregions/current/labourproductivitycaeer2.xlsx'
p2f <- tempfile(fileext=".xlsx")
download.file(url2, p2f, mode="wb")

#Use current prices in £ sheet
prodMCA <- readxl::read_excel(path = p2f,range = "A4!A5:V22")#Gets just MCA values

names(prodMCA) <- gsub(x = names(prodMCA), pattern = ' ', replacement = '_')

#Tidy
prodMCA = prodMCA %>% 
  filter(area_code != 'UKX') %>% 
  pivot_longer(Pounds_2004:Pounds_2023, names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(
    year = substr(year,8,11),
    year = as.numeric(year)
  ) 
