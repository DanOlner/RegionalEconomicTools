# Verify whether the CURRENT ONS regional GVA (balanced) release already
# incorporates the post-Blue-Book-2021 *improved telecoms deflator*.
#
# Decisive test (Abdirahman/Coyle/Heys/Stewart 2020): over 2010-2017 the OLD
# telecoms deflator ROSE ~+3% (and real telecoms GVA fell ~8%). If the current
# file's *implied* deflator FALLS over that window, the correction is baked in.
#
# Companion to claude/deflators_real_vs_nominal.md (§12A) and
# bits_of_code/currentprices_v_chainedvolume.R.

suppressMessages({library(readxl); library(dplyr); library(tidyr)})

url <- 'https://www.ons.gov.uk/file?uri=/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry/current/regionalgrossvalueaddedbalancedbyindustryandallinternationalterritoriallevelsitlregions.xlsx'
p <- tempfile(fileext='.xlsx')
download.file(url, p, mode='wb', quiet=TRUE)

cat('=== Sheets ===\n'); print(excel_sheets(p))

read_one <- function(sheet){
  d <- read_excel(p, range = paste0(sheet,'!A2:AD1730'))
  names(d) <- gsub(' ', '_', names(d))
  d %>% filter(Region_name == 'United Kingdom', SIC07_code == '61')
}

cp  <- read_one('Table 1c')   # current prices (£m)
cv  <- read_one('Table 1a')   # chained volume (index, 2022 = 100)

cat('\n=== SIC 61 row found ===\n')
cat('CP desc:', cp$SIC07_description, '| CV desc:', cv$SIC07_description, '\n')

yrs <- grep('^[0-9]{4}$', names(cp), value=TRUE)
cat('Years:', min(yrs), '..', max(yrs), '\n')

long <- tibble(
  year = as.integer(yrs),
  cp   = as.numeric(cp[1, yrs]),
  cv   = as.numeric(cv[1, yrs])
) %>% mutate(deflator_raw = cp / cv)

# normalise implied deflator to 100 in latest year
lastyr <- max(long$year)
base_def <- long$deflator_raw[long$year==lastyr]
long <- long %>% mutate(deflator = 100 * deflator_raw / base_def)

g   <- function(y) long$deflator[long$year==y]
cvv <- function(y) long$cv[long$year==y]
cpv <- function(y) long$cp[long$year==y]

cat('\n=== SIC 61 (Telecommunications), UK ===\n')
print(as.data.frame(long %>% filter(year %in% c(1998,2010,2017,lastyr))))

cat('\n--- Chained-volume (real) multiples ---\n')
cat('CV 1998 value (index):', round(cvv(1998),3), '\n')
cat('Real multiple 1998 ->', lastyr, ':', round(cvv(lastyr)/cvv(1998),1), 'x\n')

cat('\n--- Implied GVA deflator (index, ', lastyr, '=100) ---\n', sep='')
cat('1998:', round(g(1998),1), ' | 2010:', round(g(2010),1),
    ' | 2017:', round(g(2017),1), ' | ', lastyr, ':', round(g(lastyr),1), '\n')

pc <- function(a,b) round(100*(g(b)/g(a)-1),1)
cat('\n*** DECISIVE TEST (Coyle window) ***\n')
cat('Implied deflator change 2010 -> 2017:', pc(2010,2017), '%  ',
    '(Coyle: OLD deflator was about +3% here)\n')
cat('Real GVA change 2010 -> 2017       :', round(100*(cvv(2017)/cvv(2010)-1),0), '%  ',
    '(Coyle: OLD figures showed about -8%)\n')
cat('Implied deflator change 1998 ->', lastyr, ':', pc(1998,lastyr), '%\n')

cat('\n--- Nominal (current price) check ---\n')
cat('CP multiple 1998 ->', lastyr, ':', round(cpv(lastyr)/cpv(1998),2), 'x\n')
