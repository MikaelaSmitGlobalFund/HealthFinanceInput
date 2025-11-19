# -----------------------------------------
# Compare to reference data by Mayuko
# First Version: 19 Nov 2025 (cleaned)
# -----------------------------------------

library(dplyr)
library(openxlsx)
library(readr)

setwd("/Users/mc1405/RCode/HDF code/")
output_path = "/Users/mc1405/RCode/HDF code/Output"

# Paths
exc_file <- "mayuko_data/Investment_case_capping_2024-11-27.xlsx"
hiv_file <- file.path(output_path, "hiv_nonfung_base_c.csv")
tb_file <- file.path(output_path, "tb_nonfung_base_c.csv")
mal_file <- file.path(output_path, "mal_nonfung_base_c.csv")

# Read reference sheet
nonfung_raw <- read.xlsx(exc_file, sheet = "Non-fungible", colNames = TRUE)

# Read the CSV
hiv_nonfung <- read_csv(hiv_file, show_col_types = FALSE)
tb_nonfung <- read_csv(tb_file, show_col_types = FALSE)
mal_nonfung <- read_csv(mal_file, show_col_types = FALSE)

# HIV: Merge by ISO (Excel) and country (CSV)
merged_hiv <- nonfung_raw %>%
  left_join(hiv_nonfung, by = c("ISO" = "country"))

merged_hiv <- merged_hiv %>%
  mutate(
    ratio_base_to_cost = HIV_base_DAH_c / cost
  )

# TB: Merge by ISO (Excel) and country (CSV)
merged_tb <- nonfung_raw %>%
  left_join(tb_nonfung, by = c("ISO" = "country"))

merged_tb <- merged_tb %>%
  mutate(
    ratio_base_to_cost = TB_base_DAH_c / cost
  )

# TB: Merge by ISO (Excel) and country (CSV)
merged_mal <- nonfung_raw %>%
  left_join(mal_nonfung, by = c("ISO" = "country"))

merged_mal <- merged_mal %>%
  mutate(
    ratio_base_to_cost = Mal_base_DAH_c / cost
  )



# ---------------------------
# Helper comparison function
# ---------------------------
build_merge_summary <- function(df_ref, df_model, ref_colname) {
  
  merged <- df_ref %>%
    left_join(df_model, by = c("ISO" = "country")) %>%
    mutate(
      ratio = !!as.symbol(ref_colname) / cost,
      status_icon = case_when(
        !is.na(!!as.symbol(ref_colname)) & !is.na(cost) & abs(ratio - 1) < 1e-6 ~ "🟢 MATCH",
        !is.na(!!as.symbol(ref_colname)) & !is.na(cost) & abs(ratio - 1) >= 1e-6 ~ "🟠 VALUE DIFFERENT",
        !is.na(!!as.symbol(ref_colname)) & is.na(cost) ~ "🔴 MODEL MISSING",
        is.na(!!as.symbol(ref_colname)) & !is.na(cost) ~ "🟡 EXTRA IN MODEL",
        TRUE ~ "⚪ BOTH MISSING"
      )
    )
  
  # Summary table
  summary <- merged %>%
    count(status_icon, name = "n_countries") %>%
    arrange(desc(n_countries))
  
  list(merged = merged, summary = summary)
}
  
# ---------------------------------------------------------
# Run for each disease
# ---------------------------------------------------------
hiv_out <- build_merge_summary(nonfung_raw, hiv_nonfung, "HIV_base_DAH_c")
tb_out  <- build_merge_summary(nonfung_raw, tb_nonfung,  "TB_base_DAH_c")
mal_out <- build_merge_summary(nonfung_raw, mal_nonfung, "Mal_base_DAH_c")

# ---------------------------------------------------------
# Save Excel files (icons preserved!)
# ---------------------------------------------------------

## HIV
wb_hiv <- createWorkbook()
addWorksheet(wb_hiv, "Merged")
addWorksheet(wb_hiv, "Summary")
writeData(wb_hiv, "Merged",  hiv_out$merged)
writeData(wb_hiv, "Summary", hiv_out$summary)
saveWorkbook(wb_hiv, file.path(output_path, "HIV_nonfung_compare.xlsx"), overwrite = TRUE)

## TB
wb_tb <- createWorkbook()
addWorksheet(wb_tb, "Merged")
addWorksheet(wb_tb, "Summary")
writeData(wb_tb, "Merged",  tb_out$merged)
writeData(wb_tb, "Summary", tb_out$summary)
saveWorkbook(wb_tb, file.path(output_path, "TB_nonfung_compare.xlsx"), overwrite = TRUE)

## MALARIA
wb_mal <- createWorkbook()
addWorksheet(wb_mal, "Merged")
addWorksheet(wb_mal, "Summary")
writeData(wb_mal, "Merged",  mal_out$merged)
writeData(wb_mal, "Summary", mal_out$summary)
saveWorkbook(wb_mal, file.path(output_path, "MAL_nonfung_compare.xlsx"), overwrite = TRUE)

cat("✔ Excel files saved with icons:\n",
    "- HIV_nonfung_compare.xlsx\n",
    "- TB_nonfung_compare.xlsx\n",
    "- MAL_nonfung_compare.xlsx\n")

