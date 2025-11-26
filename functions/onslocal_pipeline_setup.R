# Packages/helpers for the ONS Local pipelines session
# 27th Nov 2025
message('Checking on missing packages, installing if we need to. Starting timer...')

x = Sys.time()

# Some NOMIS prerequisites not right versions installed...
if(!require(tidyverse)){
  install.packages("tidyverse")
}

if(!require(pryr)){
  install.packages("pryr")
}

if(!require(devtools)){
  install.packages('devtools')
}

# Then we can get NOMISR
devtools::install_github("ropensci/nomisr")

message('Packages installed. Time taken:')

message(Sys.time() - x)

#HELPER FUNCTIONS
#reduce need to type glimpse every time...
g <- function(x) glimpse(x)
v <- function(x) View(x)

#Wrap grepl to do tidier version of this when e.g. filtering for terms
#gq = "grepl quick!"
qg <- function(...) grepl(..., ignore.case = T)

#Same as above but returning distinct values
getdistinct <- function(...) grep(..., ignore.case = T, value = T) %>% unique


# Get linear slopes by group
#Version that returns slope and SE (for 2D LM only...)
get_slope_and_se_safely <- function(data, ..., y, x, neweywest = F) {
  
  groups <- quos(...)  
  y <- enquo(y) 
  x <- enquo(x) 
  
  #Function to compute slope
  get_slope_and_se <- function(data) {
    
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    
    if(neweywest){
      
      nw_se <- sandwich::NeweyWest(model, lag = 1, prewhite = TRUE)
      rez <- lmtest::coeftest(model, vcov. = nw_se)
      
      slope <- coef(rez)[2]
      se <- rez[2,2]
      
    } else {
      
      slope <- coef(model)[2]
      se <- summary(model)$coefficients[2, 2]
      
    }
    
    return(list(slope = slope, se = se))
    
    # return(c(coef(model)[2],summary(model)[[4]]['x','Std. Error']))
  }
  
  #Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope_and_se, otherwise = list(slope = NA, se = NA))
  
  #Group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(result = map(data, safe_get_slope)) %>% 
    mutate(slope = map_dbl(result, "slope"),
           se = map_dbl(result, "se")) %>%
    select(-data, -result)
  
}

