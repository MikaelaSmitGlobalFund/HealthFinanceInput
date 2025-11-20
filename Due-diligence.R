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

hiv_fun_file  <- file.path(output_path, "hiv_fung_incl_econ_17.csv")
tb_fun_file   <- file.path(output_path, "tb_fung_incl_econ_17.csv")
mal_fun_file  <- file.path(output_path, "mal_fung_incl_econ_17.csv")

# Read reference sheet
nonfung_raw <- read.xlsx(exc_file, sheet = "Non-fungible", colNames = TRUE)
fung_raw <- read.xlsx(exc_file, sheet = "Fungible", colNames = TRUE)

# Read the CSV
hiv_nonfung <- read_csv(hiv_file, show_col_types = FALSE)
tb_nonfung <- read_csv(tb_file, show_col_types = FALSE)
mal_nonfung <- read_csv(mal_file, show_col_types = FALSE)

hiv_fung <- read_csv(hiv_fun_file, show_col_types = FALSE)
tb_fung  <- read_csv(tb_fun_file,  show_col_types = FALSE)
mal_fung <- read_csv(mal_fun_file, show_col_types = FALSE)


# ---------------------------
# NON-FUNGIBLE COMPARISON
# ---------------------------
# HIV: Merge by ISO (Excel) and country (CSV)
merged_hiv <- nonfung_raw %>%
  select(ISO, HIV_base_DAH_c) %>%     # keep only ISO + fungible HIV
  left_join(hiv_nonfung, by = c("ISO" = "country")) %>%   # add model output
  mutate(
    ratio_base_to_cost = HIV_base_DAH_c / cost        # NEW ratio
  )


# TB: Merge by ISO (Excel) and country (CSV)
merged_tb <- nonfung_raw %>%
  select(ISO, TB_base_DAH_c) %>%                        # keep only ISO + TB reference
  left_join(tb_nonfung, by = c("ISO" = "country")) %>%  # add model output
  mutate(
    ratio_base_to_cost = TB_base_DAH_c / cost           # compute ratio
  )

# TB: Merge by ISO (Excel) and country (CSV)
merged_mal <- nonfung_raw %>%
  select(ISO, Mal_base_DAH_c) %>%                         # keep only ISO + Malaria reference
  left_join(mal_nonfung, by = c("ISO" = "country")) %>%   # add model output
  mutate(
    ratio_base_to_cost = Mal_base_DAH_c / cost            # compute ratio
  )

# ---------------------------
# FUNGIBLE COMPARISON
# ---------------------------
# HIV: Merge by ISO (Excel) and country (CSV)
merged_hiv_fung <- fung_raw %>%
  select(ISO, Fungible_amount_HIV) %>%                   # keep only ISO + reference HIV fungible
  left_join(hiv_fung, by = c("ISO" = "country")) %>%     # merge model output
  mutate(
    ratio_base_to_cost = Fungible_amount_HIV / cost      # compare reference vs model
  )

# TB: Merge by ISO (Excel) and country (CSV)
merged_tb_fung <- fung_raw %>%
  select(ISO, Fungible_amount_TB) %>%                    # only ISO + TB reference
  left_join(tb_fung, by = c("ISO" = "country")) %>%      # merge model output
  mutate(
    ratio_base_to_cost = Fungible_amount_TB / cost       # ratio
  )

# Malaria: Merge by ISO (Excel) and country (CSV)
merged_mal_fung <- fung_raw %>%
  select(ISO, Fungible_amount_Mal) %>%                   # ISO + Malaria reference
  left_join(mal_fung, by = c("ISO" = "country")) %>%     # correct merge
  mutate(
    ratio_base_to_cost = Fungible_amount_Mal / cost      # ratio
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
hiv_out$merged <- hiv_out$merged %>%
  select(ISO, HIV_base_DAH_c, cost, ratio, status_icon)
tb_out$merged <- tb_out$merged %>%
  select(ISO, TB_base_DAH_c, cost, ratio, status_icon)
mal_out$merged <- mal_out$merged %>%
  select(ISO, Mal_base_DAH_c, cost, ratio, status_icon)


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


# ---------------------------------------------------------
# Run fungible comparisons
# ---------------------------------------------------------

hiv_fung_out$merged <- hiv_fung_out$merged %>%
  select(ISO, Fungible_amount_HIV, cost, ratio, status_icon)

tb_fung_out$merged <- tb_fung_out$merged %>%
  select(ISO, Fungible_amount_TB, cost, ratio, status_icon)

mal_fung_out$merged <- mal_fung_out$merged %>%
  select(ISO, Fungible_amount_Mal, cost, ratio, status_icon)



# ---------------------------------------------------------
# Save Excel files for fungible comparisons
# ---------------------------------------------------------

## HIV fungible
wb_hiv_f <- createWorkbook()
addWorksheet(wb_hiv_f, "Merged")
addWorksheet(wb_hiv_f, "Summary")
writeData(wb_hiv_f, "Merged",  hiv_fung_out$merged)
writeData(wb_hiv_f, "Summary", hiv_fung_out$summary)
saveWorkbook(wb_hiv_f, file.path(output_path, "HIV_fungible_compare.xlsx"), overwrite = TRUE)

## TB fungible
wb_tb_f <- createWorkbook()
addWorksheet(wb_tb_f, "Merged")
addWorksheet(wb_tb_f, "Summary")
writeData(wb_tb_f, "Merged",  tb_fung_out$merged)
writeData(wb_tb_f, "Summary", tb_fung_out$summary)
saveWorkbook(wb_tb_f, file.path(output_path, "TB_fungible_compare.xlsx"), overwrite = TRUE)

## Malaria fungible
wb_mal_f <- createWorkbook()
addWorksheet(wb_mal_f, "Merged")
addWorksheet(wb_mal_f, "Summary")
writeData(wb_mal_f, "Merged",  mal_fung_out$merged)
writeData(wb_mal_f, "Summary", mal_fung_out$summary)
saveWorkbook(wb_mal_f, file.path(output_path, "Mal_fungible_compare.xlsx"), overwrite = TRUE)

cat("✔ Fungible Excel files saved:\n",
    "- HIV_fungible_compare.xlsx\n",
    "- TB_fungible_compare.xlsx\n",
    "- Mal_fungible_compare.xlsx\n")

