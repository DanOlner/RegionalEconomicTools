#Make a lookup for ONS SIC code categorisations from the GVA data (slightly different combos to full list)
library(tidyverse)

#Get the three different SIC code groupings
#Arbitrary files, just need the code/name lists
three <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_3GROUPS_WIDE_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()

#Will be different for ITL1, 2 and 3

# ITL1----

# Getting from orig file... sections
url1 <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p1f <- tempfile(fileext=".xlsx")
download.file(url1, p1f, mode="wb") 

sections <- readxl::read_excel(path = p1f,range = "Table 1c!A2:AD1730") 

#More process-able names with no spaces
names(sections) <- gsub(x = names(sections), pattern = ' ', replacement = '_')

#Keep SIC sections
#This gets all the letters, saves having to manually filter
SIC_sections <- sections$SIC07_code[substr(sections$SIC07_code,2,2) == ' '] %>% unique


#TWO VERSIONS - ONE THAT KEEPS IMPUTED RENT, ONE THAT REMOVES
#To do the latter for SIC sections, just need to replace the SIC section that includes it with the lower level SIC that does not
#This is also then the quickest way to remove imputed rent from the national total and regional totals
#(Reminder - which we can only do using 'current prices' because only those can be re-summed, unlike chained volume measures)

#REPLACE "L (68)" REAL ESTATE ACTIVITIES (WHICH INCLUDES IMPUTED RENT) WITH JUST 68 "Real estate activities, excluding imputed rental"  
#NOTE: "68" WILL NEED REPLACING AGAIN WITH L (68) AFTER FOR LATER MATCHES, BUT PUT EXC IMPUTED RENT NOTE IN
SIC_sections_minusImputedRent = SIC_sections
SIC_sections_minusImputedRent[SIC_sections_minusImputedRent == 'L (68)'] <- '68'

sections <- sections %>% 
  filter(
    SIC07_code %in% SIC_sections,
    !qg('united kingdom|england|extra',Region_name)
  ) 

# Looking OK
# unique(sections$SIC07_code)
# unique(sections$Region_name)

# Enlongen whole thing
sections <- sections %>%  
  pivot_longer(`1998`:names(sections)[length(names(sections))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year))

sections <- sections %>%
  select(SIC07_code, SIC07_description) %>% 
  distinct()



# Getting from orig file... 2 digit
#Table 1c is current prices with ITL1 zones
twodigit <- readxl::read_excel(path = p1f,range = "Table 1c!A2:AD1730") 

#More process-able names with no spaces
names(twodigit) <- gsub(x = names(twodigit), pattern = ' ', replacement = '_')

#WARNING: ONLY CORRECT LIST TO REMOVE FOR ITL1 (only one change from ITL2 though, E (36-39))
#SICs to remove to leave just unique SIC values
#Still works for 2025 as well as 2024 to leave correct highest res SICs
SICremoves = c(
  'Total',
  'A-E',
  'A (1-3)',
  'B (5-9)',
  'C (10-33)',
  'CA (10-12)',
  'CB (13-15)',
  'CC (16-18)',
  'CG (22-23)',
  'CH (24-25)',
  'CL (29-30)',
  'CM (31-33)',
  'E (36-39)',
  'F (41-43)',
  'G-T',
  'G (45-47)',
  'H (49-53)',
  'I (55-56)',
  'J (58-63)',
  'K (64-66)',
  'L (68)',#real estate activities - leaves in "Real estate activities, excluding imputed rental" & "Owner-occupiers' imputed rental" as separate categories
  'M (69-75)',
  'N (77-82)',
  'Q (86-88)',
  'R (90-93)',
  'S (94-96)'
)

twodigit <- twodigit %>% 
  filter(
    !SIC07_code %in% SICremoves,
    !qg('united kingdom|england|extra',Region_name)
  ) 

# Looking OK
# unique(twodigit$SIC07_code)
# unique(twodigit$Region_name)

# Enlongen
twodigit <- twodigit %>%  
  pivot_longer(`1998`:names(twodigit)[length(names(twodigit))], names_to = 'year', values_to = 'value') %>% #get most recent year
  mutate(year = as.numeric(year)) %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()



# PROCESS
# Helper: given a range string (like "A-E" or "10-33") return the sequence.
expand_range <- function(x) {
  if (str_detect(x, "-")) {
    parts <- str_split(x, "-", simplify = TRUE)
    # If both parts are single letters, use LETTERS.
    if (all(str_detect(parts, "^[A-Z]$"))) {
      start <- which(LETTERS == parts[1])
      end <- which(LETTERS == parts[2])
      return(LETTERS[start:end])
    } else { 
      # Otherwise assume numeric range.
      return(seq(as.integer(parts[1]), as.integer(parts[2])))
    }
  } else {
    # No hyphen: if the trimmed value is numeric, return it as an integer.
    trimmed <- str_trim(x)
    num <- suppressWarnings(as.integer(trimmed))
    if (!is.na(num)) {
      return(num)
    } else {
      return(trimmed)
    }
  }
}


## --- 1. Expand the "three" dataset ---
# In "three" the SIC07_code column is something like "A-E" or "G-T".
# We extract and expand that to get one row per letter.
three_expanded <- three %>%
  mutate(letter_range = str_extract(SIC07_code, "^[A-Z]+(-[A-Z]+)?")) %>% 
  mutate(letter = map(letter_range, expand_range)) %>% 
  unnest(letter)

## --- 2. Expand the "sections" dataset ---
# Here the code is like "A (1-3)". We extract the letter and the number-range.
sections_expanded <- sections %>%
  mutate(letter = str_extract(SIC07_code, "^[A-Z]"),
         num_range = str_extract(SIC07_code, "(?<=\\().+(?=\\))"),
         number = map(num_range, expand_range)) %>% 
  unnest(number) %>% 
  mutate(number = as.integer(number))

## --- 3. Expand the "twodigit" dataset ---
# These codes come in several flavours – some are just numbers/ranges,
# others include a letter part (e.g. "B (5-9)" or "CD-CF (19-21)").
# Here we grab the numeric part. (and adjust if some codes mix letter and digit differently.)
twodigit_expanded <- twodigit %>%
  mutate(numeric_part = case_when(
    # If there's a parenthesis, extract what's inside.
    str_detect(SIC07_code, "\\(") ~ str_extract(SIC07_code, "(?<=\\().+(?=\\))"),
    # If there's a hyphen but no parenthesis, assume it's a numeric range (e.g. "2-3").
    str_detect(SIC07_code, "-") ~ SIC07_code,
    # Otherwise, extract only the leading digits (e.g. "68IMP" becomes "68").
    TRUE ~ str_extract(SIC07_code, "^[0-9]+")
  )) %>%
  mutate(numeric_part = str_trim(numeric_part),
         num = map(numeric_part, expand_range)) %>%
  unnest(num) %>%
  mutate(num = as.integer(num))


## --- 4. Joining ---
# The idea is to join the sections to the "three" lookup via the letter variable,
# and then join the sections to the two-digit breakdown via the numeric code.
lookup.itl1 <- sections_expanded %>%
  left_join(three_expanded, by = "letter", suffix = c("_sections", "_three")) %>%
  left_join(twodigit_expanded, by = c("number" = "num"), suffix = c("", "_two"))

# The resulting 'lookup' table now links:
# - The overall three-group category (from 'three_expanded') via the letter.
# - The section (from 'sections_expanded') via its letter and numeric range.
# - The detailed two-digit category (from 'twodigit_expanded') via the numeric code.






# ITL2----

sections <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_SECTION_LONG_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()

twodigit <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_2DIGIT_WIDE_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()


three %>% print(n = 100)
sections %>% print(n = 100)
twodigit %>% print(n = 100)

# Helper: given a range string (like "A-E" or "10-33") return the sequence.
expand_range <- function(x) {
  if (str_detect(x, "-")) {
    parts <- str_split(x, "-", simplify = TRUE)
    # If both parts are single letters, use LETTERS.
    if (all(str_detect(parts, "^[A-Z]$"))) {
      start <- which(LETTERS == parts[1])
      end <- which(LETTERS == parts[2])
      return(LETTERS[start:end])
    } else { 
      # Otherwise assume numeric range.
      return(seq(as.integer(parts[1]), as.integer(parts[2])))
    }
  } else {
    # No hyphen: if the trimmed value is numeric, return it as an integer.
    trimmed <- str_trim(x)
    num <- suppressWarnings(as.integer(trimmed))
    if (!is.na(num)) {
      return(num)
    } else {
      return(trimmed)
    }
  }
}


## --- 1. Expand the "three" dataset ---
# In "three" the SIC07_code column is something like "A-E" or "G-T".
# We extract and expand that to get one row per letter.
three_expanded <- three %>%
  mutate(letter_range = str_extract(SIC07_code, "^[A-Z]+(-[A-Z]+)?")) %>% 
  mutate(letter = map(letter_range, expand_range)) %>% 
  unnest(letter)

## --- 2. Expand the "sections" dataset ---
# Here the code is like "A (1-3)". We extract the letter and the number-range.
sections_expanded <- sections %>%
  mutate(letter = str_extract(SIC07_code, "^[A-Z]"),
         num_range = str_extract(SIC07_code, "(?<=\\().+(?=\\))"),
         number = map(num_range, expand_range)) %>% 
  unnest(number) %>% 
  mutate(number = as.integer(number))

## --- 3. Expand the "twodigit" dataset ---
# These codes come in several flavours – some are just numbers/ranges,
# others include a letter part (e.g. "B (5-9)" or "CD-CF (19-21)").
# Here we grab the numeric part. (and adjust if some codes mix letter and digit differently.)
twodigit_expanded <- twodigit %>%
  mutate(numeric_part = case_when(
    # If there's a parenthesis, extract what's inside.
    str_detect(SIC07_code, "\\(") ~ str_extract(SIC07_code, "(?<=\\().+(?=\\))"),
    # If there's a hyphen but no parenthesis, assume it's a numeric range (e.g. "2-3").
    str_detect(SIC07_code, "-") ~ SIC07_code,
    # Otherwise, extract only the leading digits (e.g. "68IMP" becomes "68").
    TRUE ~ str_extract(SIC07_code, "^[0-9]+")
  )) %>%
  mutate(numeric_part = str_trim(numeric_part),
         num = map(numeric_part, expand_range)) %>%
  unnest(num) %>%
  mutate(num = as.integer(num))


## --- 4. Joining ---
# The idea is to join the sections to the "three" lookup via the letter variable,
# and then join the sections to the two-digit breakdown via the numeric code.
lookup.itl2 <- sections_expanded %>%
  left_join(three_expanded, by = "letter", suffix = c("_sections", "_three")) %>%
  left_join(twodigit_expanded, by = c("number" = "num"), suffix = c("", "_two"))

# The resulting 'lookup' table now links:
# - The overall three-group category (from 'three_expanded') via the letter.
# - The section (from 'sections_expanded') via its letter and numeric range.
# - The detailed two-digit category (from 'twodigit_expanded') via the numeric code.





# Now ITL3----

sections <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_SECTION_LONG_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()

twodigit <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL3_SIC_2DIGIT_WIDE_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()

three %>% print(n = 100)
sections %>% print(n = 100)
twodigit %>% print(n = 100)



three_expanded <- three %>%
  mutate(letter_range = str_extract(SIC07_code, "^[A-Z]+(-[A-Z]+)?")) %>% 
  mutate(letter = map(letter_range, expand_range)) %>% 
  unnest(letter)

## --- 2. Expand the "sections" dataset ---
# Here the code is like "A (1-3)". We extract the letter and the number-range.
sections_expanded <- sections %>%
  mutate(letter = str_extract(SIC07_code, "^[A-Z]"),
         num_range = str_extract(SIC07_code, "(?<=\\().+(?=\\))"),
         number = map(num_range, expand_range)) %>% 
  unnest(number) %>% 
  mutate(number = as.integer(number))

## --- 3. Expand the "twodigit" dataset ---
# These codes come in several flavours – some are just numbers/ranges,
# others include a letter part (e.g. "B (5-9)" or "CD-CF (19-21)").
# Here we grab the numeric part. (You may need to adjust if some codes mix letter and digit differently.)
twodigit_expanded <- twodigit %>%
  mutate(numeric_part = case_when(
    # If there's a parenthesis, extract what's inside.
    str_detect(SIC07_code, "\\(") ~ str_extract(SIC07_code, "(?<=\\().+(?=\\))"),
    # If there's a hyphen but no parenthesis, assume it's a numeric range (e.g. "2-3").
    str_detect(SIC07_code, "-") ~ SIC07_code,
    # Otherwise, extract only the leading digits (e.g. "68IMP" becomes "68").
    TRUE ~ str_extract(SIC07_code, "^[0-9]+")
  )) %>%
  mutate(numeric_part = str_trim(numeric_part),
         num = map(numeric_part, expand_range)) %>%
  unnest(num) %>%
  mutate(num = as.integer(num))


## --- 4. Joining ---
# The idea is to join the sections to the "three" lookup via the letter variable,
# and then join the sections to the two-digit breakdown via the numeric code.
lookup.itl3 <- sections_expanded %>%
  left_join(three_expanded, by = "letter", suffix = c("_sections", "_three")) %>%
  left_join(twodigit_expanded, by = c("number" = "num"), suffix = c("", "_two"))



# Reorganise those and save----

lookup.itl1 <- lookup.itl1 %>% 
  select(SIC07_code, SIC07_description, SIC07_code_sections, SIC07_description_sections, SIC07_code_three, SIC07_description_three) %>% 
  distinct(SIC07_code, .keep_all = T)

lookup.itl2 <- lookup.itl2 %>% 
  select(SIC07_code, SIC07_description, SIC07_code_sections, SIC07_description_sections, SIC07_code_three, SIC07_description_three) %>% 
  distinct(SIC07_code, .keep_all = T)

lookup.itl3 <- lookup.itl3 %>% 
  select(SIC07_code, SIC07_description, SIC07_code_sections, SIC07_description_sections, SIC07_code_three, SIC07_description_three) %>% 
  distinct(SIC07_code, .keep_all = T)

#correct length? Tick
map(list(lookup.itl1,lookup.itl2,lookup.itl3), nrow)

write_csv(lookup.itl1,'data/siclookup_forregionalGVAcategories_ITL1.csv')
write_csv(lookup.itl2,'data/siclookup_forregionalGVAcategories_ITL2.csv')
write_csv(lookup.itl3,'data/siclookup_forregionalGVAcategories_ITL3.csv')







