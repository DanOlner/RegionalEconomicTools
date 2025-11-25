# Packages for the ONS Local pipelines session
# 
library(tidyverse)

cat('Checking on missing packages, installing if we need to. Starting timer...\n')

x = Sys.time()

# Some NOMIS prerequisites not right versions installed...
if(!require(cli)){
  install.packages("cli")
}

if(!require(vctrs)){
  install.packages('vctrs')
}

if(!require(devtools)){
  install.packages('devtools')
}

# Then we can get NOMSIR
devtools::install_github("ropensci/nomisr")

cat('Packages installed. Time taken: ', Sys.time() - x)


#HELPER FUNCTIONS
#reduce need to type glimpse every time...
g <- function(x) glimpse(x)
v <- function(x) View(x)

#Wrap grepl to do tidier version of this when e.g. filtering for terms
#gq = "grepl quick!"
qg <- function(...) grepl(..., ignore.case = T)

#Same as above but returning distinct values
getdistinct <- function(...) grep(..., ignore.case = T, value = T) %>% unique

