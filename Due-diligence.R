library(dplyr)
library(openxlsx)
library(readr)

setwd("/Users/mc1405/RCode/HDF code/")
output_path = "/Users/mc1405/RCode/HDF code/Output"

# Paths
exc_file <- "mayuko_data/Investment_case_capping_2024-11-27.xlsx"
csv_file <- file.path(output_path, "hiv_nonfung_base_c.csv")

# Read the Excel (sheet: Non-fungible)
nonfung_raw <- read.xlsx(exc_file, sheet = "Non-fungible", colNames = TRUE)

# Read the CSV
hiv_nonfung <- read_csv(csv_file, show_col_types = FALSE)

# Merge by ISO (Excel) and country (CSV)
merged_hiv <- nonfung_raw %>%
  left_join(hiv_nonfung, by = c("ISO" = "country"))

# View result
head(merged_hiv)

merged_hiv <- merged_hiv %>%
  mutate(
    ratio_base_to_cost = HIV_base_DAH_c / cost
  )
