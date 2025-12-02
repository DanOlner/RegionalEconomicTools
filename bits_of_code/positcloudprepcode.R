# jobgva cumul plot prep
# Current BRES latest is 2024 though GVA is still on 2023
# Here we'll use the moving avs for a bit more consistency over time
bres.gva.2d = readRDS('local/data/bresgva2d.rds') %>% 
  filter(!qg('households|agri|membership', SIC07_description))

shortsectornames <- read_csv('data/shortsectornames_for_regionalGVA_2digitSICs.csv')

bres.gva.2d <- bres.gva.2d %>% 
  left_join(
    shortsectornames, by = 'SIC07_description'
  )

# Write that data for online use
saveRDS(bres.gva.2d,'data/bresgva2d_2023.rds')