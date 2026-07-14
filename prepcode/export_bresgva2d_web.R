# Export bresgva2d_2023.rds to a slim columnar JSON for the docs/webblocks web page.
# One-off build step: run this whenever the source RDS updates.
#
# Output: docs/webblocks/data/bresgva2d_web.json
# Encoding is columnar + dictionary-based to keep the file small:
#   - `sectors`  : sector short names (index = sector id), in the SAME order the
#                  R plot uses (unique() over the data) so colours line up.
#   - `colours`  : hex colour per sector, reproduced exactly from the R script's
#                  RColorBrewer qualitative concatenation.
#   - `regions`  : per ITL3 area: name + ITL3 code + parent ITL2/ITL1 names,
#                  so the UI can offer "add all of Yorkshire & Humber" style picks.
#   - `years`    : year values (index = year id).
#   - `cols`     : parallel arrays (one entry per data row): region idx, sector idx,
#                  year idx, jobs (moving avg), gva/job, gva. NA gva -> null.

library(tidyverse)
library(RColorBrewer)
library(jsonlite)

# Run from the repo root, e.g.:  Rscript prepcode/export_bresgva2d_web.R
bres <- readRDS("data/bresgva2d_2023.rds")

# --- sector dictionary + colours (match the plot's ordering exactly) ---------
# The R plot builds colours with setNames(colourstouse, unique(SIC07_description_shortened)),
# so the canonical sector order is unique() over the column, NOT sorted.
sectors <- unique(bres$SIC07_description_shortened)
n <- length(sectors)

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colours <- unname(col_vector[1:n])

# --- region dictionary, enriched with ITL2/ITL1 parents ----------------------
lookup <- read_csv(
  "data/LAD_(April_2025)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv",
  show_col_types = FALSE
) %>%
  select(ITL325CD, ITL225NM, ITL125NM) %>%
  distinct()

regions_tbl <- bres %>%
  distinct(Region_name, ITL_code) %>%
  left_join(lookup, by = c("ITL_code" = "ITL325CD")) %>%
  arrange(Region_name)

regions <- lapply(seq_len(nrow(regions_tbl)), function(i) {
  list(
    name = regions_tbl$Region_name[i],
    itl3 = regions_tbl$ITL_code[i],
    itl2 = regions_tbl$ITL225NM[i] %||% NA,
    itl1 = regions_tbl$ITL125NM[i] %||% NA
  )
})

# --- years dictionary --------------------------------------------------------
years <- sort(unique(bres$year))

# --- columnar row arrays -----------------------------------------------------
region_idx <- match(bres$Region_name, regions_tbl$Region_name) - 1L
sector_idx <- match(bres$SIC07_description_shortened, sectors) - 1L
year_idx <- match(bres$year, years) - 1L

round_or_null <- function(x, digits) {
  x <- round(x, digits)
  # jsonlite writes NA as null when na = "null"
  x
}

payload <- list(
  meta = list(
    source = "data/bresgva2d_2023.rds",
    generated = as.character(Sys.Date()),
    note = "ITL3-level BRES jobs x regional GVA-per-job, 3-year moving average."
  ),
  sectors = sectors,
  colours = colours,
  years = years,
  regions = regions,
  cols = list(
    region = region_idx,
    sector = sector_idx,
    year = year_idx,
    jobs = round_or_null(bres$jobcount_movingav, 1),
    gvaperjob = round_or_null(bres$`gva/job`, 2),
    gva = round_or_null(bres$gva_movingav, 2)
  )
)

out_dir <- "docs/webblocks/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "bresgva2d_web.json")

write_json(payload, out_path, na = "null", auto_unbox = TRUE, digits = NA)

cat("Wrote", out_path, "\n")
cat("Size:", round(file.info(out_path)$size / 1024, 1), "KB\n")
cat("Regions:", nrow(regions_tbl), " Sectors:", n, " Years:", length(years),
    " Rows:", nrow(bres), "\n")
