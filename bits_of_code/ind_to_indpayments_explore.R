# Via https://www.nomisweb.co.uk/sources/i2i_payflows
# Download data from here:
# https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/industrytoindustrypaymentflowsuk/2017to2024experimentaldata
library(tidyverse)
library(patchwork)
library(circlize)#For chord diagram (non-interactive)
library(networkD3)#For interactive version
source('functions/misc_functions.R')

theme_set(theme_light())


# NATIONAL / SIC 5-DIGIT----

# CSV downloaded to local/data/
i2i = read_csv('local/data/ukindustrytoindustrypaymentflowssic5january2017tonovember2024_FINAL.csv')

# 1.6GB
# Monthly...
# pryr::object_size(i2i)

i2i = i2i %>% 
  mutate(
    value = as.numeric(`Value (£)`),
    `Number of transactions` = as.numeric(`Number of transactions`),
    avval_pertransaction = value/`Number of transactions`
  )

# Err
length(unique(i2i$`Payer (5-digit SIC)`))

# View the ones that went NA, what happened there?
nas = i2i %>% filter(is.na(value))

# Check if it's date-related... nope, seem pretty evenly spread
table(nas$Date)

# Going to assume [c] means too low to include

# Keep non-NA and use
# Set year date to group by
i2i = i2i %>% 
  filter(!is.na(value)) %>% 
  mutate(
    year = floor_date(Date,'year')
  )

# 2024 doesn't have december, which will make that year a little unrepresentative
unique(i2i$year)
unique(i2i$Date)


# Sum by year (but drop 2024 cos of missing month
i2i.yr = i2i %>% 
  filter(year != '2024-01-01') %>% 
  group_by(year,`Payer (5-digit SIC)`,`Payee (5-digit SIC)`) %>% 
  summarise(
    value = sum(value),
    num_transactions = sum(`Number of transactions`)
  ) %>% 
  ungroup()

# Add in other SIC levels - just sections for now
siclookup = read_csv('https://github.com/DanOlner/RegionalEconomicTools/raw/refs/heads/gh-pages/data/SIClookup.csv')

# Add in to both payer and payee
i2i.yr = i2i.yr %>% 
  left_join(
    siclookup %>% select(SIC_5DIGIT_CODE,SIC_section_payer = SIC_SECTION_NAME),
    by = c('Payer (5-digit SIC)' = 'SIC_5DIGIT_CODE')
  ) %>% 
  left_join(
    siclookup %>% select(SIC_5DIGIT_CODE,SIC_section_payee = SIC_SECTION_NAME),
    by = c('Payee (5-digit SIC)' = 'SIC_5DIGIT_CODE')
  )

# Label all NAs as unknown/other
i2i.yr = i2i.yr %>% 
  mutate(
    SIC_section_payer = ifelse(is.na(SIC_section_payer),'other',SIC_section_payer),
    SIC_section_payee = ifelse(is.na(SIC_section_payee),'other',SIC_section_payee)
  )

# Group by section, add up spend
i2i.yr.sections = i2i.yr %>% 
  group_by(SIC_section_payer,SIC_section_payee,year) %>% 
  summarise(
    value = sum(value),
    num_transactions = sum(num_transactions)
  )


# Pick a single year
i2i.sections.2023 = i2i.yr.sections %>% filter(year == '2023-01-01')

# Get flows ready for sticking into chordDiagram
flows <- i2i.sections.2023 %>%
  select(SIC_section_payer,SIC_section_payee,value) %>% 
  mutate(value = value / 1000000) %>% 
  filter(SIC_section_payer!='other',SIC_section_payee!='other') %>% 
  ungroup()

# Shorten
flows$SIC_section_payer = reduceSICnames(flows$SIC_section_payer,'section')
flows$SIC_section_payee = reduceSICnames(flows$SIC_section_payee,'section')

# Filter some more!
flows = flows %>% 
  filter(across(contains('SIC_section_pay'), ~!.x %in% c('Households','Extraterr','Other')))

# MAKE THE DIAGRAM
chordDiagram(
  flows,
  grid.col = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(flows$SIC_section_payer))),
  directional = 1
  )



# NetworkD3 version (less good but interactive)
# Enwidenate... https://stackoverflow.com/a/60793668
# flows.matrix = flows %>% pivot_wider(names_from = SIC_section_payer, values_from = value)
# Or...
flows.matrix = data.table::dcast(as.data.table(flows), SIC_section_payee ~ SIC_section_payer, value.var = 'value')

# Work out the same. We just need to stick the first row into names...?
rownames(flows.matrix) = flows.matrix$SIC_section_payee
flows.matrix = flows.matrix %>% select(-SIC_section_payee)

# Is OK, doesn't show bidirectional...
chordNetwork(
  Data = flows.matrix,
  labels = rownames(flows.matrix),
  height = 1000,
  width = 1000,
  labelDistance = 150
  )


# NOPE!!
# Test sankey example
# Load energy projection data
# Load energy projection data
# URL <- paste0(
#   "https://cdn.rawgit.com/christophergandrud/networkD3/",
#   "master/JSONdata/energy.json")
# Energy <- jsonlite::fromJSON(URL)
# # Plot
# sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               units = "TWh", fontSize = 12, nodeWidth = 30)
# 
# # Right, so in theory...
# sankeyNetwork(Links = data.frame(flows), Nodes = data.frame(rownames(flows.matrix)), Source = "SIC_section_payer",
#               Target = "SIC_section_payee", Value = "value", NodeID = "rownames.flows.matrix.",
#               units = "£M", fontSize = 12, nodeWidth = 30)
# 
# # Nope. Let's see if matching the orig format more exactly works
# flows.sankey =  flows %>% 
#     mutate(
#       SIC_section_payee_numeric = as.numeric(factor(SIC_section_payee))-1,
#       SIC_section_payer_numeric = as.numeric(factor(SIC_section_payer))-1
#       ) %>% 
#   select(SIC_section_payer_numeric,SIC_section_payee_numeric,value) %>% 
#   data.frame()
# 
# nodes.sankey = data.frame(
#   name = as.character(factor(flows$SIC_section_payee))
# )      
# 
# sankeyNetwork(Links = flows.sankey, Nodes = nodes.sankey, Source = "SIC_section_payer_numeric",
#               Target = "SIC_section_payee_numeric", Value = "value", NodeID = "name",
#               units = "£M", fontSize = 12, nodeWidth = 30)



# ITL1 / 2 DIGIT----

# What we'll try to do here:
# Proportion being spent within region vs in other regions
# Could even do prop in this region / neighbouring regions / further afield / london
# i2i = read_csv('local/data/sic2_region_2019_2025_nomis.csv')
# 
# # 2.64gb.
# pryr::object_size(i2i)
# 
# # Convert [c] to NAs and appropriate columns to numeric
# i2i = i2i %>%
#   mutate(
#     across(
#       c(`Payer (2-digit SIC)`,`Payee (2-digit SIC)`,`Value (£)`,`Number of transactions`),
#       ~ ifelse(. == "[c]", NA, .)
#       )) %>%
#   mutate(
#     across(
#         c(`Payer (2-digit SIC)`,`Payee (2-digit SIC)`,`Value (£)`,`Number of transactions`),
#         ~ as.numeric(.)
#     )
# )
# 
# # Better names
# i2i = i2i %>% rename(
#   payer_sic2digit = `Payer (2-digit SIC)`,
#   payer_ITL1 = `Payer Region (ITL-1)`,
#   payee_sic2digit = `Payee (2-digit SIC)`,
#   payee_ITL1 = `Payee Region (ITL-1)`,
#   pounds = `Value (£)`,
#   num_transactions = `Number of transactions`
#   )
# 
# 
# # Save that formatted version
# saveRDS(i2i, 'local/data/sic2_region_2019_2025_nomis_formatted.rds')


# table(i2i$`Number of transactions`)

# table(i2i$`Payer (2-digit SIC)`)

# Before NA conversion... just checking this number matches
# Yep, NA number is correct. So those are too low to be included
# table(i2i$`Number of transactions` == "[c]")
# table(i2i$`Value (£)` == "[c]")

# After [c] to NA conversion
# table(is.na(i2i$`Number of transactions`))
# table(is.na(i2i$`Value (£)`))


# Let's just do yearly for now, should be able to see trends much more clearly if any

i2i = readRDS('local/data/sic2_region_2019_2025_nomis_formatted.rds')

plot(hist(log(i2i$num_transactions), na.rm = T))

# Check on dates again, may need to miss out the last year if not complete
# Nope, looks all good for up to 2025
table(i2i$Date)

# Drop NA values - at best, they're zero anyway so won't count
# Add in year
i2i = i2i %>% 
  filter(!is.na(pounds)) %>% 
  mutate(
    year = floor_date(Date,'year')
  )

# All fine
unique(i2i$year)
unique(i2i$Date)

# Sum by year per sector and place and flow direction
i2i.yr = i2i %>% 
  group_by(year,payer_sic2digit,payer_ITL1,payee_sic2digit,payee_ITL1) %>% 
  summarise(
    pounds = sum(pounds),
    num_transactions = sum(num_transactions)
  ) %>% 
  ungroup()

saveRDS(i2i.yr, 'local/data/payments_itl2_sic2_yearly.rds')


# OK, small enough to add ITL1 names in now
# itllookup = read_csv("local/LAD_(April_2025)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv") %>% 
#   select(ITL125CD,ITL125NM) %>% 
#   distinct()

# A no, this is using the old GOR codes, not the ITL1 codes.
# And just for England, right?
gor = read_csv("local/data/Regions_(December_2024)_Names_and_Codes_in_EN.csv") %>% select(RGN24CD,RGN24NM)

# The online version has every UK region... what is occurring?
# Those beginning with W, S and N are presumably Wales, Scotland and NI?
# M, L and S I don't know...
# Turns out channel islands and isle of man
# https://doc.ukdataservice.ac.uk/doc/7961/mrdoc/pdf/7961_nspl_user_guide_2011.pdf
unique(i2i.yr$payer_ITL1)

# Check match
table(gor$RGN24CD %in% i2i.yr$payer_ITL1)

# OK, and can then add others in
gorUK = gor %>% 
  bind_rows(
    tibble(
      RGN24CD = c(
        "N99999999","S99999999","W99999999"
      ),
      RGN24NM = c("Northern Ireland","Scotland","Wales")
    )
  )


# Join twice to get label in for payer and payee
i2i.yr = i2i.yr %>% 
  left_join(
    gorUK %>% 
      rename(
        payer_ITL1 = RGN24CD,
        payer_ITL1name = RGN24NM
        ), 
    by = c('payer_ITL1')
  ) %>% 
  left_join(
    gorUK %>% 
      rename(
        payee_ITL1 = RGN24CD,
        payee_ITL1name = RGN24NM
      ), 
    by = c('payee_ITL1')
  )

# Drop NAs from either, not any use to us
i2i.yr = i2i.yr %>% 
  filter(
    !is.na(payer_ITL1name),!is.na(payee_ITL1name)
  )


# DOING SUMS----

# Right, getting there.
# Let's find proportion local vs elsewhere per place / year
# Note, we could also do:
# Proportion from 'rest of UK' being spent here, change over time

# But here we're after:
# Proportion spent internally vs spent outside of this region
# Spent internally = payer is same as payee
# Spent elsewhere = payer is paying that to other regions
# And we want those in their own column for ease of division

# And then probably to index over time
# As proportion spent here would be a function of that economy's size

# Let's also sum SICs for now
# Otherwise it's fairly hefy matrix
i2i.spent_here = i2i.yr %>% 
  filter(payer_ITL1name == payee_ITL1name) %>% 
  # filter(
  #   between(payer_sic2digit,10,33),#Manufacturing is between 10 and 33
  #   between(payee_sic2digit,10,33)
  #   ) %>% #added filter to just look money moving within at one sector or sectors
  group_by(year,payer_ITL1name) %>% 
  summarise(pounds_spent_here = sum(pounds)) %>% 
  ungroup()

# Same data for 'spent outside of region'
i2i.spent_outside = i2i.yr %>% 
  filter(payer_ITL1name != payee_ITL1name) %>%
  # filter(
  #   between(payer_sic2digit,10,33),#Manufacturing is between 10 and 33
  #   between(payee_sic2digit,10,33)
  # ) %>% #added filter to just look money moving within at one sector or sectors
  group_by(year,payer_ITL1name) %>% 
  summarise(pounds_spent_externally = sum(pounds)) %>% 
  ungroup()

# Join and make ratio
i2i.inoutspending = i2i.spent_here %>% 
  left_join(
    i2i.spent_outside, by = c('year','payer_ITL1name')
  ) %>% 
  mutate(
    in_over_out = pounds_spent_here/pounds_spent_externally
  )


# Before indexing, check how those ratios look by themselves
p1 = ggplot(
  i2i.inoutspending,
  aes(x = year, y = in_over_out, colour = fct_reorder(payer_ITL1name, -in_over_out))
) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_color_brewer(palette = 'Paired') +
  labs(colour = 'Region')


# Index - put in order, use first entry as reference
# Make sure years are in order 
i2i.inoutspending = i2i.inoutspending %>% 
  group_by(payer_ITL1name) %>% 
  arrange(year) %>% 
  mutate(in_over_out_index = percent_change(first(in_over_out), in_over_out) + 100)


p2 = ggplot(
  i2i.inoutspending,
  aes(x = year, y = in_over_out_index, colour = fct_reorder(payer_ITL1name, -in_over_out))
) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Paired') +
  labs(colour = 'Region')


both = p1 + p2

both


























