# Via https://www.nomisweb.co.uk/sources/i2i_payflows
# Download data from here:
# https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/industrytoindustrypaymentflowsuk/2017to2024experimentaldata
library(tidyverse)
library(circlize)#For chord diagram (non-interactive)
library(networkD3)#For interactive version
source('functions/misc_functions.R')

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


