#BRES API download
library(tidyverse)
library(nomisr)
source('functions/data_process_functions.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD LATEST BRES DATA FOR ITL2 ZONES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Find latest available year for all BRES data
#(Though note, below, some geographies at time of writing have no data in certain years)
time <- nomis_get_metadata(id = "NM_189_1", concept = "TIME")
latestyear <- as.numeric(time$id[length(time$id)])

#2015 is earliest in BRES data
yearstoget = 2015:latestyear

#TYPE437 is nuts 2016 level 3 - Bournemouth, Christchurch and Poole don't match ITL3, the rest does (see intersection checks in misc_checks.R )
#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2 2021
#TYPE429 is ITL2 (2021)
#TYPE428 is ITL3 (2021)

#Use this to view all geography types
#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 40)

#Looking for some other ways to reduce download size (industry never works...)
# nomis_get_metadata(id = "NM_189_1", concept = "INDUSTRY")
# nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
# nomis_get_metadata(id = "NM_189_1", concept = "TIME")

#Reminder of employee cats in the BRES data
## Employees: An employee is anyone aged 16 years or over that an organisation directly pays from its payroll(s), in return for carrying out a full-time or part-time job or being on a training scheme. It excludes voluntary workers, self-employed, working owners who are not paid via PAYE. 
# Full-time employees: those working more than 30 hours per week.
# Part-time employees: those working 30 hours or less per week.
# Employment includes employees plus the number of working owners. BRES therefore includes self-employed workers as long as they are registered for VAT or Pay-As-You-Earn (PAYE) schemes. Self employed people not registered for these, along with HM Forces and Government Supported trainees are excluded.
#And note, no gig economy jobs: 
#https://www.ons.gov.uk/aboutus/transparencyandgovernance/freedomofinformationfoi/workersinthegigeconomy





#TYPE438 is nuts 2016 level 2 - geography fully matches ITL2 2021
#FULL TIME (run "EMPLOYMENT_STATUS" line above to see correct codes )
#debugonce(runBRESdownloader.fortheseyears)
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE438", EMPLOYMENT_STATUS = 2)
#PART TIME
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE438", EMPLOYMENT_STATUS = 3)
#"EMPLOYEES"
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE438", EMPLOYMENT_STATUS = 4)

#TYPE437 is nuts 2016 level 3 - geography only doesn't match ITL3 for Bournemouth/Poole/Christchurch, rest does
#FULL TIME
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE437", EMPLOYMENT_STATUS = 2)
#PART TIME
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE437", EMPLOYMENT_STATUS = 3)
#"EMPLOYEES"
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE437", EMPLOYMENT_STATUS = 4)

#ITL2 and 3 (data only available for 2022 and 2023)
#Full time for both
#Run this version to prove data not available for anything but 2022 to 2023
# runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE429", EMPLOYMENT_STATUS = 2)

# ITL2
#FULL TIME
runBRESdownloader.fortheseyears(years = 2022:latestyear, geography = "TYPE429", EMPLOYMENT_STATUS = 2)
#PART TIME
runBRESdownloader.fortheseyears(years = 2022:latestyear, geography = "TYPE429", EMPLOYMENT_STATUS = 3)
#"EMPLOYEES"
runBRESdownloader.fortheseyears(years = 2022:latestyear, geography = "TYPE429", EMPLOYMENT_STATUS = 4)

# ITL3
#FULL TIME
runBRESdownloader.fortheseyears(years = 2022:latestyear, geography = "TYPE428", EMPLOYMENT_STATUS = 2)
#PART TIME
runBRESdownloader.fortheseyears(years = 2022:latestyear, geography = "TYPE428", EMPLOYMENT_STATUS = 3)
#"EMPLOYEES"
runBRESdownloader.fortheseyears(years = 2022:latestyear, geography = "TYPE428", EMPLOYMENT_STATUS = 4)



# These are local authorities (county/unitary)
# Will either use as is, or drop in e.g. the four LAs in SY (where ITL3 2021 only has BDR combined)
#debugonce(runBRESdownloader.fortheseyears)
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE423", EMPLOYMENT_STATUS = 2)
#PART TIME
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE423", EMPLOYMENT_STATUS = 3)
#"EMPLOYEES"
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE423", EMPLOYMENT_STATUS = 4)


# Also do district / unitary...
#debugonce(runBRESdownloader.fortheseyears)
# FULL TIME
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE432", EMPLOYMENT_STATUS = 2)
#PART TIME
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE432", EMPLOYMENT_STATUS = 3)
#"EMPLOYEES"
runBRESdownloader.fortheseyears(years = yearstoget, geography = "TYPE432", EMPLOYMENT_STATUS = 4)




