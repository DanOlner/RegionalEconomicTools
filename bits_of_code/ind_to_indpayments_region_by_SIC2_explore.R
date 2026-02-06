# Via https://www.nomisweb.co.uk/sources/i2i_payflows
# Download data from here:
# https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/industrytoindustrypaymentflowsuk/2017to2024experimentaldata
library(tidyverse)
library(patchwork)
library(circlize)#For chord diagram (non-interactive)
library(networkD3)#For interactive version
source('functions/misc_functions.R')

theme_set(theme_light())


# ITL1 / 2 DIGIT----

# Version broken down by ITL1 region and SIC 2 digit

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


# Saving a reduced version too


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




i2i.yr = readRDS('local/data/payments_itl2_sic2_yearly.rds')


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

# Let's add sections in for ease of filtering
siclookup = read_csv('data/SIClookup.csv')

# Check match (noting the i2i data hasa large SIC2 = 0 values that I'm not sur about)
table(siclookup$SIC_2DIGIT_CODE_NUMERIC %in% i2i.yr$payer_sic2digit)

# Join twice for each of payer and payee
# Should leave 2 digit zeros as NAs for sections
i2i.yr = i2i.yr %>% 
  left_join(
    siclookup %>% select(
      SIC_2DIGIT_CODE_NUMERIC,
      sectioncode_payer = SIC_SECTION_CODE,
      sectionname_payer = SIC_SECTION_NAME) %>% distinct(),
    by = c('payer_sic2digit' = 'SIC_2DIGIT_CODE_NUMERIC')
  ) %>% left_join(
    siclookup %>% select(
      SIC_2DIGIT_CODE_NUMERIC,
      sectioncode_payee = SIC_SECTION_CODE,
      sectionname_payee = SIC_SECTION_NAME) %>% distinct(),
    by = c('payee_sic2digit' = 'SIC_2DIGIT_CODE_NUMERIC')
  ) 
  

# Save version with sections
saveRDS(i2i.yr,'local/data/payments_itl2_sic2_yearly_w_sections.rds')

# Quick check - what proportion of transactions and cash are in the 'we don't know which sector' bucket?
# Bit of a palaver there, but hey ho - about 4-6% of transactions in the 'don't know' SIC bin
i2i.yr %>% 
  mutate(
    payer_siciszero = ifelse(payer_sic2digit == 0, 'zero','the rest'),
    payee_siciszero = ifelse(payee_sic2digit == 0, 'zero','the rest')
    ) %>% 
  select(payer_siciszero,payee_siciszero,pounds,num_transactions) %>% 
  pivot_longer(payer_siciszero:payee_siciszero, names_to = 'type', values_to = 'yesno') %>% 
  group_by(type,yesno) %>% 
  summarise(
    pounds = sum(pounds),
    num_transactions = sum(num_transactions)
  ) %>%
  group_by(type) %>% 
  summarise(
    percent_cash = (pounds/sum(pounds)) * 100,
    percent_transactions = (num_transactions/sum(num_transactions))*100
  )


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
    in_over_out = pounds_spent_here/pounds_spent_externally,
    share_of_money_spend_inregion = pounds_spent_here/(pounds_spent_here + pounds_spent_externally)
    # in_over_out = pounds_spent_externally/pounds_spent_here#Just checking not better other way round
  )


# Consistent colour with different factor order
#Get set pastel colours for each place
n <- length(unique(i2i.inoutspending$payer_ITL1name))
set.seed(12)
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

randomcols <- col_vector[10:(10+(n-1))]

pal = setNames(randomcols, unique(i2i.inoutspending$payer_ITL1name))

# Before indexing, check how those ratios look by themselves
p1 = ggplot(
  i2i.inoutspending %>% mutate(ynh = ifelse(qg('humber',payer_ITL1name), T,F)),
  aes(x = year, y = share_of_money_spend_inregion, colour = fct_reorder(payer_ITL1name, -share_of_money_spend_inregion), size = ynh)
  # aes(x = year, y = in_over_out, colour = fct_reorder(payer_ITL1name, -in_over_out), size = ynh)
) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_size_manual(values = c(0.5,2)) +
  # scale_color_brewer(palette = 'Paired') +
  scale_colour_manual(
    values = pal
    # limits = names(pal)   # locks mapping across plots (don't want this here actually!)
  ) +
  labs(colour = 'Region') +
  guides(size = 'none') +
  ylab('share of internal vs total spending (log10)')


# Index - put in order, use first entry as reference
# Make sure years are in order 
i2i.inoutspending = i2i.inoutspending %>% 
  group_by(payer_ITL1name) %>% 
  arrange(year) %>% 
  mutate(share_of_money_spend_inregion_index = percent_change(first(share_of_money_spend_inregion), share_of_money_spend_inregion) + 100)
  # mutate(in_over_out_index = percent_change(first(in_over_out), in_over_out) + 100)


p2 = ggplot(
  i2i.inoutspending %>% mutate(ynh = ifelse(qg('humber',payer_ITL1name), T,F)),
  aes(x = year, y = share_of_money_spend_inregion_index, colour = fct_reorder(payer_ITL1name, -share_of_money_spend_inregion_index), size = ynh)
  # aes(x = year, y = in_over_out_index, colour = fct_reorder(payer_ITL1name, -in_over_out_index), size = ynh)
) +
  geom_point() +
  geom_line() +
  scale_size_manual(values = c(0.5,2)) +
  # scale_color_brewer(palette = 'Paired') +
  scale_colour_manual(
    values = pal
    # limits = names(pal)   # locks mapping across plots (don't want this here actually!)
  ) +
  labs(colour = 'Region', size = F) +
  guides(size = 'none') +
  ylab('Indexed to 100 in 1st yr')


both = p1 + p2

both = p1 / p2

saveRDS(both,'local/data/ind2indbothplots.rds')

both

# Quick Scotland plot over time...
ggplot(
  # i2i.inoutspending %>% filter(payer_ITL1name == 'Scotland') %>% 
  i2i.inoutspending %>% filter(payer_ITL1name == 'North East') %>% 
    pivot_longer(pounds_spent_here:pounds_spent_externally, names_to = 'source', values_to = 'value'), 
       aes(x = year, y = value, colour = source)) +
  geom_line()

# Actually, let's do em all
ggplot(
  i2i.inoutspending %>% 
    # filter(payer_ITL1name != 'London') %>% 
    pivot_longer(pounds_spent_here:pounds_spent_externally, names_to = 'source', values_to = 'value') %>% 
    mutate(value_billions = value/1000000000), 
       aes(x = year, y = value_billions, colour = source)) +
  geom_line() +
  # scale_y_log10() +
  # facet_wrap(~payer_ITL1name) 
  facet_wrap(~payer_ITL1name, scales = 'free_y')


i2i.inoutspending %>% 
  mutate(across(contains('spent'), ~./10000000000, .names = "div_{.col}")) %>% 
  View




# Function up and save each SIC section
output_per_section_to_samesection = function(section_from,section_to){
  
  i2i.spent_here = i2i.yr %>% 
    filter(payer_ITL1name == payee_ITL1name) %>% 
    filter(
      sectionname_payer == section_from,
      sectionname_payee == section_to
    ) %>% #added filter to just look money moving within at one sector or sectors
    group_by(year,payer_ITL1name) %>% 
    summarise(pounds_spent_here = sum(pounds)) %>% 
    ungroup()
  
  # Same data for 'spent outside of region'
  i2i.spent_outside = i2i.yr %>% 
    filter(payer_ITL1name != payee_ITL1name) %>%
    filter(
      sectionname_payer == section_from,
      sectionname_payee == section_to
    ) %>% #added filter to just look money moving within at one sector or sectors
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
      # in_over_out = pounds_spent_externally/pounds_spent_here#Just checking not better other way round
    )
  
  p = ggplot(
    i2i.inoutspending %>% 
      # filter(payer_ITL1name != 'London') %>% 
      pivot_longer(pounds_spent_here:pounds_spent_externally, names_to = 'source', values_to = 'value') %>% 
      mutate(value_billions = value/1000000000), 
    aes(x = year, y = value_billions, colour = source)) +
    geom_line() +
    # scale_y_log10() +
    # facet_wrap(~payer_ITL1name) 
    facet_wrap(~payer_ITL1name, scales = 'free_y') +
    ggtitle(paste0('From ',section_from,' to ',section_to))
  
    ggsave(paste0('local/outputs/i2i_regional_sections_in_out/',
                  gsub('[[:punct:]]| ','',section_from),'_to_',
                  gsub('[[:punct:]]| ','',section_to),
                  '.png'), plot = p, width = 9, height = 12)
  
  
}

map2(unique(i2i.yr$sectionname_payee)[2:20], unique(i2i.yr$sectionname_payee)[2:20], output_per_section_to_samesection)
map2(unique(i2i.yr$sectionname_payee)[2:20], "Manufacturing", output_per_section_to_samesection)




