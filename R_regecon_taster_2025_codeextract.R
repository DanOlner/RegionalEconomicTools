#' ---
#' title: "Using R for regional economic analysis, taster session (17th June 2025)"
#' sidebar: false
#' code-overflow: wrap
#' execute:
#'   echo: true
#'   warning: false
#'   error: false
#' format:
#'   revealjs:
#'     slide-number: true
#'     code-line-numbers: false
#'     code-overflow: wrap
#'     css: styles.css
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.height = 8, fig.width = 10, echo = TRUE, warning = F, error = F, message = F, comment=NA)


#' 
#' 
#' 
#' 
#' ## Before we start...
#' 
#' * You've got two ways to engage with this session: 
#' 
#' 1. Just watch my screencast,let it wash over you and then come back to the materials in your own time. (These slides as well as ONS recording.)
#' 2. Have a go at running the R code yourself as we go. **If you're trying #2 - you'll need a copy of these slides open.** You'll have got the link via email - it's also in the chat, or just type `bit.ly/rtasterslides` into your browser address bar.
#' 
#' ---
#' 
#' * If you want to do some coding but didn't have a chance to run through the little R setup guide, **you've got some time now while I do all the pre-amble! The next slide takes you through how to get RStudio working in the browser using posit.cloud.**
#' * Open the slide deck in your own browser using the chat link, so you can keep the instructions on-screen while I carry on (the link to posit.cloud is in those online slides).
#' 
#' `bit.ly/rtasterslides` again, just in case...
#' 
#' ---
#' 
#' ::: {.callout-tip icon=false}
#' 
#' ## Get up and running with RStudio in the cloud
#' 
#' * [Create an account at posit.cloud](https://login.posit.cloud/login){target="_blank"} using the 'sign up' box. **It'll ask you to check your email for a verification link** so do that before proceeding. 
#' * Once verified, go back to the posit.cloud tab and log in. You should see a message: "You are logged into Posit. Please select your destination". Choose 'Posit Cloud' (not Posit Connect Cloud). **That'll take you to your online workspace.**
#' * Click the **new project** button, top right
#' * Select '**new project from template**' (as in the pic below) and then "Data Analysis in R with the tidyverse" (if not already selected). This template comes pre-installed with the tidyverse package, which we'll be using. Select then click OK down at the bottom. **This will open your RStudio project where we'll do the coding.** 
#' 
#' ::: {.center}
#' ![](https://danolner.github.io/posts/setting_up_w_r_online/newproject.png){width="200px"}
#' :::
#' 
#' :::
#' 
#' ----
#' 
#' 
#' 
#' ## Introduction
#' 
#' We'll be doing a **whistlestop tour** of using R for digging into UK regional economics.
#' 
#' 
#' 
#' 
#' ## Goals for the session
#' 
#' * To actually **get you started using R and RStudio** if you haven't before.
#' * 
#' 
#' ---
#' 
#' * First, apologies - because this is just a taster, we won't be able to check in on everyone's progress to make sure it's working or making sense! I've designed the guide so it can be followed step by step - but if we lose you along the way, we've also got a poll and ways to stay in touch, if I can help to support outside of this session.
#' 
#' ---
#' 
#' * If this happens, we'll still run through some of things R can do, and we can set up support later...
#' * That could include smaller groups where making sure it's working will be possible!
#' * The aim: this should work for anyone completely new to RStudio. May veer from insultingly simplistic to horribly complex
#' 
#' ---
#' 
#' * This is a whistlestop tour - if you're new to any of this, don't worry about it making sense on first go! I'll just try to highlight what R can do, and provide pointers to learning more.
#' * Sometimes things won't work... [explain]
#' * Bugs and errors, explain.
#' * I will pick out one or two basic R things to highlight, but missing a load... [e.g. piping / functions]
#' * Other things...
#' 
#' ## The plan
#' 
#' * We'll **work through these slides together**, sometimes typing or copying code from the slides into RStudio.
#' * So I'll be **flitting between the slides and RStudio myself** to do the same steps you'll be trying.
#' * That could be vexing if you're trying to look at slides! So **make sure you have your own version of the slides open the a browser.** I'll post a link to it in the chat.
#' * Each slide is numbered - I'll say where we're at as we go on
#' 
#' ---
#' 
#' * If you **don't fancy constantly flitting between slides and RStudio** I've also made a single copy of the whole script we'll work through, which you can get here (click on the little clipboard symbol top-right to copy, and then when we make a script below, paste the whole thing in).
#' * I'll provide that link again below after we've made a script.
#' * Numbers in the comments on the script will (should!) match the slides so you can just follow along in RStudio.
#' 
#' ## The slides
#' 
#' * Once you have your own copy of the slides open, note the **three-bar button, bottom left**. 
#' * That lets you see the full contents
#' 
#' 
#' # CODING TIME!
#' 
#' ## Making a new R script in RStudio
#' 
#' **RStudio is where you'll be doing all the R loveliness** (though see also the spangly new [positron](https://positron.posit.co/){target="_blank"}, I haven't tried yet).
#' 
#' Before looking at RStudio, let's **make a new R script, where we'll do the coding**.
#' 
#' 
#' ::: {.callout-note icon=false}
#' 
#' ## Task: Open a new R script in RStudio
#' 
#' * In the **file** menu in RStudio
#' * Use **new file** and **R script** (note: it tells you the keyboard shortcut for this)
#' 
#' ::: {.center}
#' ![](miscimages/Screenshot from 2025-06-12 11-48-14.png){width="200px"}
#' :::
#' 
#' :::
#' 
#' ---
#' 
#' After that new file's been made, RStudio should look something like the pic below.
#' 
#' ![](https://danolner.github.io/posts/setting_up_w_r_online/rstudio_online.png)
#' 
#' 
#' # Tour of RStudio / getting set up
#' 
#' ## 1. The console
#' 
#' The **console** is **bottom left**: all R commands end up being run here. You either **run code directly in the console** or **send code from your R script.**
#' 
#' ::: {.callout-note icon=false}
#' 
#' ## Task: enter some commands directly into the console
#' 
#' **Click in the console area** so the cursor is on the command line. Type some random sums (or copy, see next slide) - **press enter and you'll see the results directly in the console.**
#' 
#' Note that second one: `sqrt` is a **function** and arguments go into the brackets. More on that in a mo.
#' 
## -------------------------------------------------------------------------------------------------------------------------------

2+2

sqrt(64)


#' 
#' :::
#' 
#' ---
#' 
#' ::: {.callout-tip}
#' ## Copying code from these slides into RStudio
#' 
#' All code blocks here **have a little clipboard symbol if you hover over them** to quickly copy so you can paste it all into RStudio. This will come in especially handy with larger code blocks later...
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------

2+2

sqrt(64)


#' 
#' :::
#' 
#' 
#' ## 2. Giving things names
#' 
#' We can assign all manner of things a **name** to then use them elsewhere: values, strings, lists - **and, crucially, data objects like all the UK's regional GVA.** But these **all use the same method.**
#' 
#' ::: {.callout-note icon=false}
#' 
#' ## Task: Still in the console, assign a value to a name
#' 
## -------------------------------------------------------------------------------------------------------------------------------
x = 64

#' 
#' You'll see that assignment appear **top right in RStudio**. It can be retrieved just with:
#' 
## -------------------------------------------------------------------------------------------------------------------------------
x

#' 
#' :::
#' 
#' 
#' ## 3. Using an R script
#' 
#' **We'll now stick some code into that newly created R script we made earlier in the top left of RStudio.** 
#' 
#' All code will run in the console - what we do with scripts is just send our written/saved code *to* the console. 
#' 
#' Let's test this by **adding code to load the tidyverse library**.
#' 
#' ::: {.callout-tip}
#' ## What's the tidyverse?
#' 
#' It's a basket of tools that have become part of the R language - they all fit together to make consistent workflows **much** easier.
#' 
#' The online book [R for Data Science](https://r4ds.hadley.nz/) is a great source to learn it, by the person who started it.
#' 
#' :::
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' 
#' ## Task: add a line of code to the R script
#' 
#' Type or paste the following text at the top of the newly opened R script in the top left panel. 
#' 
## ----eval = F-------------------------------------------------------------------------------------------------------------------
# library(tidyverse)

#' 
#' When you've put that in, **the script title will go red**, showing it can now be saved (it should look something like the image below).
#' 
#' ::: {.center}
#' ![](https://danolner.github.io/posts/setting_up_w_r_online/script1.png)
#' :::
#' 
#' * **Save the script** either with the CTRL + S shortcut, or file > save. Give it whatever name you like, but note that it **saves into your self-contained project folder.**
#' 
#' :::
#' 
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' 
#' ## Task: Run the `library(tidyverse)` line
#' 
#' **Now we need to actually run that line of code... we can do this in a couple of ways:**
#' 
#' 1. In your R script, if **no code text is highlighted/selected** RStudio will **Run the code line by line** (or chunk by chunk - we'll cover that in the taster session).
#' 2. **If a block of text is highlighted**, the whole block will run. So e.g. if you select-all in a script and then run it, **the entire script will run.**
#' 
#' Let's do #1: **Run the code line by line**.
#' 
#' * **To test this, we'll load the tidyverse library with the code we just pasted in** (which is just one line of code at the moment!) Put the cursor at the end of the `libary(tidyverse)` line in the script (if it's not there already), either with the mouse or keyboard. (Keyboard navigation is mostly the same as any other text editor like Word, but [here's a full shortcut list](https://support.posit.co/hc/en-us/articles/200711853-Keyboard-Shortcuts-in-the-RStudio-IDE) if useful.)
#' * Once there, either use the 'run' button, top right of the script window, or (much easier if you're doing this repeatedly) **press CTRL+ENTER to run it.**
#' 
#' :::
#' 
#' 
#' ---
#' 
#' **If all is well, you should see the text below in the console - the tidyverse library is now loaded. Huzzah!** 
#' 
#' 
#' ::: {.center}
#' ![](miscimages/tidyverseloaded.png)
#' :::
#' 
#' 
#' 
#' # Finally - some economic data! Phew.
#' 
#' ## Well nearly...
#' 
#' **Just one more thing first before actual economic data, sorry. We're just going to **load some functions (and install a couple of libraries) that we'll need.
#' 
#' If of interest (or you want to check the code isn't stealing your bank details!) code for these functions are [on the github site here](https://github.com/DanOlner/RegionalEconomicTools/blob/gh-pages/functions/misc_functions.R){target="_blank"}.
#' 
#' ::: {.callout-note icon=false}
#' 
#' ## Task: In the R script, stick this code below the `library(tidyverse)` line and run it with CTRL+ENTER or the run command
#' 
#' As this will **install a couple of libraries for the first time** it'll take about **25-30 seconds** to run.
#' 
#' **Note: comments in R begin with a #hash.**
#' 
## -------------------------------------------------------------------------------------------------------------------------------
#Load some functions and two libraries (and install those libraries if they're not already present)
source('https://bit.ly/rtasterfunctions')


#' 
#' ::: 
#' 
#' 
#' 
#' ---
#' 
#' **Now we can load some regional GVA data**.
#' 
#' ::: {.callout-note icon=false}
#' ## Tasks: Load in regional economic data, then look at it in RStudio.
#' 
#' This line takes a **comma-separated variable file from the web** and names it / assigns it to `df`. **Paste it into your script (below the other code we've added) and run it.**
#' 
#' If you **look at the environment pane, top right**. `df` should now be there (as in the pic below). **Click anywhere on that line in the environment pane** - it'll open a **new tab in RStudio** with a view of the data attached to `df`.
#' 
## -------------------------------------------------------------------------------------------------------------------------------
# LOAD ITL3 GVA DATA----

#ITL3 zones, SIC sections, current prices, 2023 latest data (minus imputed rent)
df = read_csv('https://bit.ly/rtasteritl3noir')

#This one has imputed rent included if you want it
# df = read_csv('https://bit.ly/rtasteritl3')



#' 
#' :::
#' 
#' 
#' ::: {.center}
#' ![](miscimages/env.png)
#' :::
#' 
#' 
#' ## What data have we got here?
#' 
#' This is from the latest ONS **"Regional gross value added (balanced) by industry: all International Territorial Level (ITL) regions"** (The Excel file is [available here](https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry){target="_blank"}). Released April 2025, it's got data up to 2023. It's got:
#' 
#' * GVA values from 1998 to 2023, broken down by:
#' * ITL3 zones i.e. local authorities rather than MCAs
#' * SIC sections: 18 broad sector categories (max in this data is 45 categories)
#' 
#' Plus...
#' 
#' ---
#' 
#' * This is **current price** data: what the pound value of GVA was **in each separate year**, unadjusted for inflation
#' * This data **can be summed**, unlike the inflation-adjusted (chained volume) data (where each sector in each place is inflation-adjusted separately, so can't be summed)
#' * Because it can be summed, this current price data is **great for digging into structural change** (e.g. "How have sector proportions changed over time?") But not so good for real growth analysis.
#' 
#' Let's start looking at the data in R...
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task: look at what SIC sector categories are in the data
#' 
#' We can do this by pulling out the unique values from a column.
#' 
#' In R, there are **different ways to refer to columns.** Here's a key one: use the object's name, then a dollar sign. R will then let you autocomplete the column name.
#' 
## -------------------------------------------------------------------------------------------------------------------------------
unique(df$SIC07_description)

#' 
#' :::
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task: do the same for place names
#' 
#' We can use the same method to get the full list of ITL3 place names too (though we'll use an easier method to pick them out later). Here, we also stick them into alphabetical order.
#' 
## ----eval=F---------------------------------------------------------------------------------------------------------------------
# unique(df$Region_name)[order(unique(df$Region_name))]

#' 
#' :::
#' 
#' ---
#' 
#' **"R is brilliant" exhibit A: we can use it to automate/repeat tedious things.**
#' 
#' Here's a sample of what the data looks like in the original Excel file. Different SIC sector levels are nested, there's one column per year (in R, we've made year a single 'tidy' column)
#' 
#' ::: {.center}
#' ![](miscimages/regionalxlexample.png)
#' :::
#' 
#' 
#' ---
#' 
#' * The data we've attached to the `df` object comes from that ONS Excel sheet
#' * I've used R to get it directly from the web and then extract each CSV separately that might be needed
#' * Each of those nested sector groups gets separated out: goods/services, sections, the full SIC list (45 sectors in this data)
#' * The code for that [is here](https://github.com/DanOlner/RegionalEconomicTools/blob/gh-pages/prepcode/regionalGVA_processing.R){target="_blank"}.
#' * (Is this an example of R code that could be useful to run in powerBI to wrangle data for use?)
#' 
#' 
#' ## Wrangling the data
#' 
#' * We have our sector/region GVA data in R now - but **how to wrangle it by place and sector, to start getting useful info out of it?** 
#' * Here's an example of using the `tidyverse` to do this - we'll group by sector for the latest year to see its median and range
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task: Wrangle the data to get some summary numbers
#' 
#' We'll talk about what's going on here a bit more in the next slide. For now - copy and run the code.
#' 
#' **Note: RStudio will run the whole block if press CTRL + ENTER with cursor anywhere in it.**
#' 
## -------------------------------------------------------------------------------------------------------------------------------
df %>% 
  filter(year == max(year)) %>% 
  group_by(SIC07_description) %>% 
  summarise(
    median_gva = median(value),
    min_gva = min(value),
    max_gva = max(value)
    ) %>% 
  arrange(-median_gva)

#' 
#' 
#' :::
#' 
#' 
#' 
#' 
#' ## Piping interlude!
#' 
## ----eval=F, echo=F-------------------------------------------------------------------------------------------------------------
# #Piping image from
# #https://s3.amazonaws.com/conference-handouts/2015-nctm-boston/pdfs/Fun%20Functions%20Handout%202013.pdf

#' 
#' * That last block of code was **piping output of one function into the input of the next**, chaining them together. This is super-powerful and useful.
#' 
#' ![](miscimages/function_machines.png)
#' 
#' ---
#' 
#' Here's the principle with a silly toy example using two functions. Dead simple here, but with larger processing chains it quickly becomes indispensible. It'll crop up a lot as we go.^[R geek footnote: magrittr pipe not native pipe because only one shortcut in RStudio + magrittr pipe more versatile.]
#' 
#' ::: {.callout-note icon=false}
#' ## Task: compare standard R function to 'piping in' the argument
#' 
#' If typing these out, the keyboard shortcut in RStudio for the pipe operator is **CTRL + SHIFT + M**.
#' 
## -------------------------------------------------------------------------------------------------------------------------------

#Boooo, nested brackets...
sqrt(log10(10000))

#Wooo! Lovely piped functions!
10000 %>% sqrt %>% log10


#' 
#' :::
#' 
#'  
#' 
#' ## Getting sector proportions and location quotients
#' 
#' * Now we'll **wrangle the data using a function we loaded earlier** Blue Peter style (may be showing age there!) 
#' * This function adds in columns for **what proportion of each place's economy each sector makes up** and, using that, what the **location quotient (LQ)** is for each sector in each place.
#' 
#' ![](miscimages/heresoneImadeearlier.png)
#' 
#' 
#' ---
#' 
#' ##  {.center}
#' 
#' ::: {.callout-note}
#' ## Location quotient (LQ)
#' 
#' * In case the LQ is new to anyone: it's just **the proportion of a sector in a place, over its proportion nationally.**
#' * So it's **less than one** if a sector is less concentrated in a place than the UK average / more than 1 if higher (and 0.5 and 2 if it's half or double the UK concentration).
#' * [This ONS LQ excel sheet](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/locationquotientdataandindustrialspecialisationforlocalauthorities){target="_blank"} has a really clear explanation in the notes.
#' * I've made a page on other ways to use the LQ in R [here](https://danolner.github.io/RegionalEconomicTools/sector_locationquotients_and_proportions.html), if of interest.
#' 
#' :::
#' 
#' 
#' ----
#' 
#' ::: {.callout-note icon=false}
#' ## Task: run this function and `glimpse` the result in the console.
#' 
#' We stick **four arguments** into the function - including the data itself, which here we filter to the latest year.
#' 
#' It's then piped to `glimpse`, which lets us look at all columns at once by **sticking them on their side** (a nice way to check our results look sane).
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------
# LOCATION QUOTIENTS/PROPORTIONS----

add_location_quotient_and_proportions(
  df %>% filter(year == max(year)),
  regionvar = Region_name,
  lq_var = SIC07_description,
  valuevar = value
) %>% glimpse

#' 
#' :::
#' 
#' 
#' 
#' ----
#' 
#' ##  {.center}
#' 
#' ::: {.callout-tip}
#' 
#' Once the code from the next slide is in RStudio, if you **`CTRL + mouse click` on the function name**, it'll take you to the code (if you loaded them above). You'll see it's a not-too-elaborate piping operation of the type we were just looking at.
#' 
#' The power of functions is in packaging this kind of 'need often' task into flexible, reusable form.
#' 
#' :::
#' 
#' ---
#' 
#' 
#' 
#' In the **next code block** we're running the same code but doing **four extra things** that we chain together:
#' 
#' 1. We `group_split` the data into years (one separate dataframe per year)
#' 2. `map` each year bundle into the `add_location_quotient_and_proportions` function
#' 3. `bind_rows` it all back together
#' 4. We also **overwrite `df`** with the result (first line) so we've kept it (rather than, in the last example, just outputting to the console).
#' 
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task: find LQs and proportions for each year and keep the result
#' 
#' Once run, we can also **look at the result again via the environment panel**.
#' 
#' (Note: you'll probably **get a warning** - this is due to a single negative GVA value, it won't affect anything here.)
#' 
#' While we're here: **highlight just this bit of code: `df %>% group_split(year)` and then run**. We can see what just that chunk is doing exactly what the function name suggests.
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------
#Finding location quotients for each year in the data 
#And then overwriting the df variable with the result
df = df %>%
  group_split(year) %>%
  map(add_location_quotient_and_proportions,
      regionvar = Region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()

#' 
#' :::
#' 
#' ---
#' 
#' **Now let's do something with result: **
#' 
#' * **Pick out one place or local authority**: you'll only need some of the place name you want - here I'm using `rotherh` to find Rotherham, but do change it to other places.
#' * For the latest year in the data, **pull out the location quotient and sector proportion for that place**
#' * And then order, highest to lowest.
#' 
#' ::: {.callout-note icon=false}
#' ## Task: 
#' 
## ----eval=F---------------------------------------------------------------------------------------------------------------------
# #View a single place...
# df %>% filter(
#   qg('rotherh',Region_name),
#   year == max(year)#get latest year in data
# ) %>%
#   mutate(regional_percent = sector_regional_proportion *100) %>%
#   select(SIC07_description,regional_percent, LQ) %>%
#   arrange(-LQ)
# 

#' 
#' :::
#' 
#' ---
#' 
#' **Output from that code... **
#' 
#' * Rotherham's largest sector specialism (highest LQ) is **manufacturing** for the latest year (and nearly 19% of its economy). 
#' 
## ----echo=F---------------------------------------------------------------------------------------------------------------------
#View a single place...
df %>% filter(
  qg('rotherham',Region_name),
  year == max(year)#get latest year in data
) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ) 


#' 
#' 
#' ---
#' 
#' ::: {.callout-tip}
#' ## Cheatsheets!
#' 
#' There are a bunch of excellent **cheat sheet documents** available for many of R's features. You can easily access them via RStudio's `help` menu. 
#' 
#' The **[data tranformation with dplyr](https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-transformation.pdf){target="_blank"}** sheet covers a lot of the functions we're using in piping operations.
#' 
#' There's also an excellent **[ggplot cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-visualization.pdf){target="_blank"}**, among many others. We'll come back to this one...
#' 
#' :::
#' 
#' 
#' ---
#' 
#' 
#' 
#' ## Visualising: how has the structure of places' economies changed over time?
#' 
#' Let's plot some of the data. We'll look at how an ITL3 zone's broad sectors have **changed their proportion over time**, to see how those economies have changed structurally.
#' 
#' We'll do this in **3 stages**:
#' 
#' 1. Add in a **moving average** to smooth the data a little
#' 2. Pick out the top 12 broad sectors in the latest year
#' 3. Plot!
#' 
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task: apply the moving average to our `df` data object and pick out the placename we want
#' 
#' You can look at the available ITL3 zones at [geoportal.statistics.gov.uk here](https://geoportal.statistics.gov.uk/datasets/1121eb5cb03245d787e97426f9ebe699_2/explore?location=52.100456%2C-0.493217%2C6.92){target="_blank"}, if useful for checking names. (Opens in new tab.)
#' 
## -------------------------------------------------------------------------------------------------------------------------------
# PLOT STRUCTURAL CHANGE OVER TIME IN ITL3 ZONE----

#Processing before plot

#1. Set window for rolling average: 3 years
smoothband = 3

#2. Make new column with rolling average in (applying to all places and sectors)
#Years are already in the right order, so will roll apply across those correctly
df = df %>% 
  group_by(Region_name,SIC07_description) %>% 
  mutate(
    sector_regional_prop_movingav = rollapply(
      sector_regional_proportion,smoothband,mean,align='center',fill=NA
      )
  ) %>% 
  ungroup()

#3. Pick out the name of the place / ITl3 zone to look at
#We only need a bit of the name...
place = getdistinct('doncas',df$Region_name)

#Check we found the single place we wanted (some search terms will pick up none or more than one)
place



#'   
#' :::
#' 
#' ---
#' 
#' 
#' 
#' ::: {.callout-note icon=false}
#' ## Task: for our chosen place, pick out the largest proportion sectors in the most recent year
#' 
#' Here, we `filter` to get our place and year, `arrange` in reverse order of moving average, `slice` off the top 12 and `pull` out the sector names.
#'   
## -------------------------------------------------------------------------------------------------------------------------------
# top 12 sectors in latest year
top12sectors = df %>%
  filter(
    Region_name == place,
    year == max(year) - 1#to get 3 year moving average value
  ) %>%
  arrange(-sector_regional_prop_movingav) %>%
  slice_head(n= 12) %>%
  select(SIC07_description) %>%
  pull()

#' 
#' :::
#' 
#' ---
#' 
#' ## GGPLOT interlude!
#' 
#' * In the next slide, we finally make an actual chart. Phew. 
#' * There's a lot going on in `ggplot`, the package we'll be using to visualise data - but it's worth mentioning that its **principles are actually clear and consistent.**
#' * You'll see it has an `aes` argument - that's short for **aesthetics**. An aesthetic is any single distinct plot element - like the x or y axis, colour, shape, size etc. 
#' 
#' ---
#' 
#' * The rule is simple: **you map one column to one aesthetic.** This is another **tidy data** principle, and its the reason - for example - why we have years all their own column, so year can be mapped to one axis.
#' * We'll see later with geographical zones - categorical variables also get mapped to single aesthetics.
#' * The [ggplot cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-visualization.pdf){target="_blank"} shows, for **all plot types**, what aesthetics are available.
#' 
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task: plot smoothed sector proportion over time using `ggplot`
#' 
#' 
## ----plot1code------------------------------------------------------------------------------------------------------------------
#| eval: false

# #Plot!
# ggplot(df %>%
#          filter(
#            Region_name == place,
#            SIC07_description %in% top12sectors,
#            !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
#            ),
#        aes(x = year, y = sector_regional_prop_movingav * 100, colour =
#              fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
#   geom_point() +
#   geom_line() +
#   scale_color_brewer(palette = 'Paired', direction = 1) +
#   ylab('Sector: percent of regional economy') +
#   labs(colour = paste0('Largest 12 sectors in ',max(df$year))) +
#   ggtitle(paste0(place, ' GVA % of economy over time (', smoothband,' year moving average)'))
# 

#' 
#' :::
#' 
#' Resulting plot is on on next slide -->
#' 
#' 
#' ---
#' 
## ----plot1output----------------------------------------------------------------------------------------------------------------
#| echo: false

#Plot! 
ggplot(df %>%
         filter(
           Region_name == place,
           SIC07_description %in% top12sectors,
           !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
           ),
       aes(x = year, y = sector_regional_prop_movingav * 100, colour =
             fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Paired', direction = 1) +
  ylab('Sector: percent of regional economy') +
  labs(colour = paste0('Largest 12 sectors in ',max(df$year))) +
  ggtitle(paste0(place, ' GVA % of economy over time (', smoothband,' year moving average)'))


#' 
#' 
#' 
#' ## Real GVA growth over time
#' 
#' Current prices are great and everything - loads of value being able to examine relative change, and things actually adding up. **But they're not real prices**^[Though Diane Coyle quotes Schelling 1958: "What we call 'real' magnitudes are not completely real; only the money magnitudes are real. The 'real' ones are hypothetical." Coyle 2025 p.178], so can't be used to understand **real growth over time**.
#' 
#' For this, we have to use **chained volume** data that adjusts for inflation...
#' 
#' ---
#' 
#' So next we **load data for the same sectors and ITL3 zones, but it's chained volume GVA figures**, inflation-adjusted to 2022 money value.
#' 
#' We'll then **pick out a single place to examine**. I've chosen Barnsley - but note: **you can use a small part of a local authority / place name to return the full name that we'll use to filter the data**. 
#' 
#' Check it's returned what you want though. For example, `glouc` will return a couple of different places with 'Gloucester' in. 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Tasks: Load in the chained volume data for ITL3 zones and SIC sections, then pick a place name to examine
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------
# CHAINED VOLUME ITL3 DATA----

df.cv = read_csv('https://bit.ly/rtasteritl3cvnoir')

#Get the text for an ITL3 zone by searching for a bit of the name
place = getdistinct('barns',df$Region_name)

#Check we got just one place!
place


#' 
#' 
#' :::
#' 
#' 
#' ---
#' 
#' Now we'll **examine sector growth trends**, looking at average growth over time. To start with, let's pick out a couple of sectors for our chosen place.
#' 
#' ::: {.callout-note icon=false}
#' ## Task: Plot trends over time for a couple of sectors using ggplot.
#' 
#' Set a start/end year (here, the most recent decade). **Note: two sector names are searched for here**, separating with a `|` vertical bar meaning `or`. Reminder: you can look at the full list of available sectors with **unique(df.cv$SIC07_description)**
#' 
## ----eval = F-------------------------------------------------------------------------------------------------------------------
# startyear = 2014
# endyear = 2023
# 
# #Pick out single slopes to illustrate
# ggplot(
#   df.cv %>% filter(
#     year %in% startyear:endyear,
#     Region_name == place,
#     qg('information|professional', SIC07_description)
#   ),
#   aes(x = year, y = value, colour = SIC07_description)
# ) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = 'lm') +
#   ggtitle(paste0(place, ' selected sectors\nGVA change over time (chained volume)'))
# 

#' 
#' :::
#' 
#' 
#' ---
#' 
## ----echo = F-------------------------------------------------------------------------------------------------------------------
#| fig-width: 12

startyear = 2014
endyear = 2023

#Pick out single slopes to illustrate
ggplot(
  df.cv %>% filter(
    year %in% startyear:endyear,
    Region_name == place,
    qg('information|professional', SIC07_description)
  ),
  aes(x = year, y = value, colour = SIC07_description)
) +
  geom_line() +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle(paste0(place, ' selected sectors\nGVA change over time (chained volume)'))


#' 
#' 
#' 
#' ---
#' 
#' 
#' Next, we're going to **find linear trends for every sector in our chosen place and compare them, including some confidence intervals**. To do this we'll:
#' 
#' * Use another pre-made function loaded earlier that **finds linear slopes and standard errors for each group** (each sector in this case), using log values so change is comparable.
#' * Do a bit of wrangling and then **plot average % change per year for each sector.**
#' 
#' ---
#' 
#' 
#' ::: {.callout-note icon=false}
#' ## Task: Get slopes and confidence intervals for each sector in our chosen place
#' 
#' The function has a 'use [Newey West estimators](https://en.wikipedia.org/wiki/Newey%E2%80%93West_estimator){target="_blank"}' option - better standard errors for sticking lines through time series data (**open contibutions to improve trend analysis here would be very welcome!**)
#' 
## -------------------------------------------------------------------------------------------------------------------------------
#Get all slopes for sectors in that place
slopes = get_slope_and_se_safely(
  df.cv %>% filter(
    year %in% startyear:endyear,
    Region_name == place,
    !qg('households', SIC07_description)
    ), 
  SIC07_description, 
  y = log(value), 
  x = year, 
  neweywest = T)


#Add 95% confidence intervals around the slope
slopes = slopes %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )

#' 
#' 
#' :::
#' 
#' ---
#' 
#' ::: {.callout-note icon=false}
#' ## Task:
#' 
#' 
## ----eval = F-------------------------------------------------------------------------------------------------------------------
# #Adjust to get percentage change per year
# slopes = slopes %>%
#   mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))
# 
# #Plot the slope with those confidence intervals
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(SIC07_description,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   coord_cartesian(xlim = c(-25,25)) +
#   xlab('Av. % change per year') +
#   ylab("") +
#   ggtitle(paste0(place, ' broad sectors av. chained volume GVA % change per year\n',startyear,'-',endyear,', 95% conf intervals'))
# 

#' 
#' 
#' :::
#' 
#' Plot in next slide -->
#' 
#' 
#' ---
#' 
## ----echo = F-------------------------------------------------------------------------------------------------------------------

#Adjust to get percentage change per year from log slopes
slopes = slopes %>% 
  mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))

#Plot the slope with those confidence intervals
ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(SIC07_description,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  coord_cartesian(xlim = c(-25,25)) +
  xlab('Av. % change per year') +
  ylab("") +
  ggtitle(paste0(place, ' broad sectors av. chained volume GVA % change per year\n',startyear,'-',endyear,', 95% conf intervals'))
  


#' 
#' 
#' 
#' ## More growth analysis: ITL2 zones
#' 
#' Let's actually run through extracting data from an online Excel sheet. It's not too much work. We'll use it to look at **growth rates for the entire economies of mayoral authorities and other ITL2 zones.**
#' 
#' **We use the `readxl` package - this is already installed in posit.cloud RStudio. If you're working in RStudio on your own machine, here's some code to check if you have it installed and install it if not.** 
#' 
#' 
#' ::: {.callout-note icon=false}
#' ## Task: Check if readxl is installed and install if not
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------
# EXAMPLE OF GETTING DATA DIRECTLY (GET NATIONAL SIC SECTION DATA)----

#Check readxl is already installed, install if not.
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}


#' 
#' :::
#' 
#' 
#' 
#' 
#' ---
#' 
#' Then onto getting the data directly and processing it...
#' 
#' 
#' ::: {.callout-note icon=false}
#' ## Task: Copy and run this code to get data directly from the ONS regional/industry GVA Excel file
#' 
#' We download a temporary local copy and then extract from a sheet and cell range we define using the `readxl` package.
#' 
## -------------------------------------------------------------------------------------------------------------------------------
#2025:
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

itl2.all <- read_excel(path = p1f,range = "Table 2a!A2:AD4556")

names(itl2.all) <- gsub(x = names(itl2.all), pattern = ' ', replacement = '_')

itl2.all = itl2.all %>% 
  filter(SIC07_description == 'All industries') %>% 
  pivot_longer(`1998`:names(itl2.all)[length(names(itl2.all))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))


#' 
#' :::
#' 
#' 
#' 
#' ---
#' 
#' Now we have the data in the `itl.all` object in the form we want, we'll do as we did above for sectors in one place - **find linear slopes for each ITL2 zone and some confidence intervals**.
#' 
#' ::: {.callout-note icon=false}
#' ## Task: choose a time window and find OLS slopes for each ITL2 zone
#' 
## -------------------------------------------------------------------------------------------------------------------------------
startyear = 2011
endyear = 2023

#Get all slopes for sectors in that place
slopes = get_slope_and_se_safely(
  itl2.all %>% filter(year %in% c(startyear:endyear)), 
  Region_name,
  y = log(value), 
  x = year, 
  neweywest = T)


#Add 95% confidence intervals around the slope
slopes = slopes %>% 
  mutate(
    ci95min = slope - (se * 1.96),
    ci95max = slope + (se * 1.96)
  )


#' 
#' :::
#' 
#' 
#' 
#' ---
#' 
#' Finally, we plot the slopes to show average percent change over time...
#' 
#' ::: {.callout-note icon=false}
#' ## Task: plot each ITL2's linear slopes with confidence intervals 
#' 
#' 
## ----eval=F---------------------------------------------------------------------------------------------------------------------
# #Adjust to get percentage change per year from log slopes
# slopes = slopes %>%
#   mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))
# 
# #Plot the slope with those confidence intervals
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope))) +
#   geom_point() +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   xlab('Av. % change per year') +
#   ylab("") +
#   ggtitle('All industries in ITL2 zones, chained volume GVA\nAv. % change per year\n2014-2023, 95% conf intervals')
# 
# 

#' 
#' :::
#' 
#' 
#' ---
#' 
#' 
## ----echo=F---------------------------------------------------------------------------------------------------------------------
#Adjust to get percentage change per year from log slopes
slopes = slopes %>% 
  mutate(across(c(slope,ci95min,ci95max), ~(exp(.) - 1) * 100, .names = '{.col}_percentperyear'))

#Plot the slope with those confidence intervals
ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope))) +
  geom_point() +
  geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), width = 0.1) +
  geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
  xlab('Av. % change per year') +
  ylab("") +
  ggtitle('All industries in ITL2 zones, chained volume GVA\nAv. % change per year\n2014-2023, 95% conf intervals')



#' 
#' ---
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # BONUS MATERIAL
#' 
#' ## Multiple proportion plots with consistent sector colours
#' 
#' ::: {.callout-note icon=false}
#' ## Task: 
#' 
## -------------------------------------------------------------------------------------------------------------------------------
#| eval: false

# n <- length(unique(df$SIC07_description))
# set.seed(12)
# qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# 
# randomcols <- col_vector[10:(10+(n-1))]
# 
# #Do once!
# getplot <- function(place){
# 
#   df = df %>%
#     filter(!qg("Activities of households|agri",SIC07_description)) %>%
#     mutate(
#       SIC07_description = case_when(
#         grepl('defence',SIC07_description,ignore.case = T) ~ 'public/defence',
#         grepl('support',SIC07_description,ignore.case = T) ~ 'admin/support',
#         grepl('financ',SIC07_description,ignore.case = T) ~ 'finance/insu',
#         grepl('agri',SIC07_description,ignore.case = T) ~ 'Agri/mining',
#         grepl('electr',SIC07_description,ignore.case = T) ~ 'power/water',
#         grepl('information',SIC07_description,ignore.case = T) ~ 'ICT',
#         grepl('manuf',SIC07_description,ignore.case = T) ~ 'Manuf',
#         grepl('other',SIC07_description,ignore.case = T) ~ 'other',
#         grepl('scientific',SIC07_description,ignore.case = T) ~ 'Scientific',
#         grepl('real estate',SIC07_description,ignore.case = T) ~ 'Real est',
#         grepl('transport',SIC07_description,ignore.case = T) ~ 'Transport',
#         grepl('entertainment',SIC07_description,ignore.case = T) ~ 'Entertainment',
#         grepl('human health',SIC07_description,ignore.case = T) ~ 'Health/soc',
#         grepl('food service activities',SIC07_description,ignore.case = T) ~ 'Food/service',
#         grepl('wholesale',SIC07_description,ignore.case = T) ~ 'Retail',
#         .default = SIC07_description
#       )
#     )
# 
#   top12sectors = df %>%
#     filter(
#       Region_name == place,
#       year == max(year) - 1#to get 3 year moving average value
#     ) %>%
#     arrange(-sector_regional_prop_movingav) %>%
#     slice_head(n= 12) %>%
#     select(SIC07_description) %>%
#     pull()
# 
# 
#   ggplot(df %>%
#            filter(
#              Region_name == place,
#              SIC07_description %in% top12sectors,
#              !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
#            ),
#          aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
#     geom_point() +
#     geom_line() +
#     scale_color_manual(values = setNames(randomcols,unique(df$SIC07_description))) +#set manual pastel colours matching sector name
#     ylab('Sector: percent of regional economy') +
#     theme(plot.title = element_text(face = 'bold')) +
#     labs(colour = 'SIC section') +
#     # scale_y_log10() +
#     ggtitle(paste0(place, ' GVA % of economy over time\nTop 12 sectors in ',max(df$year),'\n (', smoothband,' year moving average)'))
# 
# 
# }
# 
# places = getdistinct('doncas|roth|barns|sheff',df$Region_name)
# 
# plots <- map(places, getplot)
# 
# patchwork::wrap_plots(plots)
# 

#' 
#' 
#' :::
#' 
#' ---
#' 
## -------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| fig-width: 12
#| fig-height: 8

n <- length(unique(df$SIC07_description))
set.seed(12)
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

randomcols <- col_vector[10:(10+(n-1))]

#Do once!
getplot <- function(place){
  
  df = df %>% 
    filter(!qg("Activities of households|agri",SIC07_description)) %>% 
    mutate(
      SIC07_description = case_when(
        grepl('defence',SIC07_description,ignore.case = T) ~ 'public/defence',
        grepl('support',SIC07_description,ignore.case = T) ~ 'admin/support',
        grepl('financ',SIC07_description,ignore.case = T) ~ 'finance/insu',
        grepl('agri',SIC07_description,ignore.case = T) ~ 'Agri/mining',
        grepl('electr',SIC07_description,ignore.case = T) ~ 'power/water',
        grepl('information',SIC07_description,ignore.case = T) ~ 'ICT',
        grepl('manuf',SIC07_description,ignore.case = T) ~ 'Manuf',
        grepl('other',SIC07_description,ignore.case = T) ~ 'other',
        grepl('scientific',SIC07_description,ignore.case = T) ~ 'Scientific',
        grepl('real estate',SIC07_description,ignore.case = T) ~ 'Real est',
        grepl('transport',SIC07_description,ignore.case = T) ~ 'Transport',
        grepl('entertainment',SIC07_description,ignore.case = T) ~ 'Entertainment',
        grepl('human health',SIC07_description,ignore.case = T) ~ 'Health/soc',
        grepl('food service activities',SIC07_description,ignore.case = T) ~ 'Food/service',
        grepl('wholesale',SIC07_description,ignore.case = T) ~ 'Retail',
        .default = SIC07_description
      )
    )
  
  top12sectors = df %>%
    filter(
      Region_name == place,
      year == max(year) - 1#to get 3 year moving average value
    ) %>%
    arrange(-sector_regional_prop_movingav) %>%
    slice_head(n= 12) %>%
    select(SIC07_description) %>%
    pull()
  
  
  ggplot(df %>% 
           filter(
             Region_name == place, 
             SIC07_description %in% top12sectors,
             !is.na(sector_regional_prop_movingav)#Remove NAs created by 3 year moving average
           ), 
         aes(x = year, y = sector_regional_prop_movingav * 100, colour = fct_reorder(SIC07_description,-sector_regional_prop_movingav) )) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = setNames(randomcols,unique(df$SIC07_description))) +#set manual pastel colours matching sector name
    ylab('Sector: percent of regional economy') +
    theme(plot.title = element_text(face = 'bold')) +
    labs(colour = 'SIC section') +
    # scale_y_log10() +
    ggtitle(paste0(place, ' GVA % of economy over time\nTop 12 sectors in ',max(df$year),'\n (', smoothband,' year moving average)'))
  
  
}

places = getdistinct('doncas|roth|barns|sheff',df$Region_name)

plots <- map(places, getplot)

patchwork::wrap_plots(plots)


#' 
#' 
#' 
#' 
#' ---
#' 
#' 
#' 
#' 
## ----cuttinz, eval=F, echo=F----------------------------------------------------------------------------------------------------
# 
# #* **Follow [this little guide here](https://danolner.github.io/posts/setting_up_w_r_online/#if-using-rstudio-online-set-up-a-posit.cloud-account-and-create-your-rstudio-project){target="_blank"} to do that (opens in new tab).**
# 
# #Extract just code
# knitr::purl(input = "R_regecon_taster_2025_revealjs.qmd", output = "R_regecon_taster_2025_codeextract.R",documentation = 0)
# 
# #Mark time periods in the data to group by
# itl2.all = itl2.all %>%
#   mutate(
#     timeperiod = case_when(
#       year %in% c(1998:2010) ~ '1998 to 2010',
#       year %in% c(2011:2023) ~ '2011 to 2023'
#     )
#   )
# 
# ggplot(slopes, aes(x = slope_percentperyear, y = fct_reorder(Region_name,slope), colour = timeperiod)) +
#   geom_point(position = position_dodge(width = 0.5)) +
#   geom_errorbar(aes(xmin = ci95min_percentperyear, xmax = ci95max_percentperyear), position = position_dodge(width = 0.5), width = 0.1) +
#   geom_vline(xintercept = 0, colour = 'black', alpha = 0.5) +
#   coord_cartesian(xlim = c(-5,5)) +
#   xlab('Av. % change per year') +
#   ylab("") +
#   ggtitle(paste0(place, ' broad sectors av. chained volume GVA % change per year\n',startyear,'-',endyear,', 95% conf intervals'))
# 
# 

#' 
