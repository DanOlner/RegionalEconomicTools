#Make a lookup for ONS SIC code categorisations from the GVA data (slightly different combos to full list)
library(tidyverse)

#Get the three different SIC code groupings
#Arbitrary files, just need the code/name lists
three <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_3GROUPS_WIDE_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()

#Will be different for ITL2 and 3
#Do itl2 first
sections <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_SECTION_LONG_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()

twodigit <- read_csv('data/regionalGVA/regionalGVA_chainedvolume_ITL2_SIC_2DIGIT_WIDE_2022.csv') %>% 
  select(SIC07_code, SIC07_description) %>% 
  distinct()


three %>% print(n = 100)
sections %>% print(n = 100)
twodigit %>% print(n = 100)

# ITL2 first----

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

lookup.itl2 <- lookup.itl2 %>% 
  select(SIC07_code, SIC07_description, SIC07_code_sections, SIC07_description_sections, SIC07_code_three, SIC07_description_three) %>% 
  distinct(SIC07_code, .keep_all = T)

lookup.itl3 <- lookup.itl3 %>% 
  select(SIC07_code, SIC07_description, SIC07_code_sections, SIC07_description_sections, SIC07_code_three, SIC07_description_three) %>% 
  distinct(SIC07_code, .keep_all = T)

#correct length? Tick
map(list(lookup.itl2,lookup.itl3), nrow)

write_csv(lookup.itl2,'data/siclookup_forregionalGVAcategories_ITL2.csv')
write_csv(lookup.itl3,'data/siclookup_forregionalGVAcategories_ITL3.csv')







