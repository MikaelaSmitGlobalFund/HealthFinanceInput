# ----------------------------
# Resource Need Extraction
# First Version: 14 Nov 2025
# Clean version
# ----------------------------

rm(list = ls())

# ---- Central Switchboard ----
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed


# ---- Install packages if necessary ----
if(firstrun>0) {
  install.packages("dplyr")
  install.packages("data.table")
  installed.packages("tidyverse")
  installed.packages("readr")
  installed.packages("openxlsx")
  installed.packages("here")
}

library(dplyr) # require instead
library(data.table)   # For like function (%like%)
library(tidyr)
library(readr)      # for read_csv()
library(openxlsx)   # for read.xlsx() that keeps empty rows
library(here)


# ---- Working directory ----
if (computer ==1){
  setwd("/Users/mc1405/RCode/HDF code/")
  output_path = "/Users/mc1405/RCode/HDF code/Output"
}


# ---- Helper functions ----
# Clean Excel import keeping empty rows
read_excel_keep_empty <- function(path, sheet = 1){
  openxlsx::read.xlsx(
    xlsxFile       = path,
    sheet          = sheet,
    detectDates    = TRUE,
    skipEmptyRows  = FALSE,   # <-- keep empty rows
    skipEmptyCols  = FALSE,   # <-- keep empty columns
    na.strings     = c("", "NA")
  )
}

# Numeric cleaner
to_num <- function(x) {
  suppressWarnings({
    x <- gsub(",", "", x)                  # remove commas
    x <- gsub("\\$", "", x)                # remove $
    x <- gsub(" ", "", x)                  # remove spaces
    x <- gsub("[()]", "-", x)              # turn (2000) into -2000
    x <- gsub("[^0-9eE\\.-]", "", x)       # keep only numbers or scientific notation
    as.numeric(x)
  })
}


# ---- Load data  ----
model_dir <- file.path(getwd(), "model_output")

hiv_path     <- file.path(model_dir, "HIV cost impact results 12nov24.csv")
malaria_path <- file.path(model_dir, "output.xlsx")
hbc_path     <- file.path(model_dir, "HBC_results_OneFile.xlsx")


df_hiv2 <- readr::read_csv(hiv_path, show_col_types = FALSE) %>%
  mutate(
    Total_cost = to_num(Total_cost),
    PLHIV      = to_num(PLHIV)
  )

df_malaria2 <- read_excel_keep_empty(malaria_path) %>%
  mutate(
    total_cost    = to_num(total_cost),
    cost_vaccine  = to_num(cost_vaccine),
    vaccine_compete = to_num(vaccine_compete)
  )

df_tb2 <- read_excel_keep_empty(hbc_path) %>%
  mutate(
    Costs       = to_num(Costs),
    vacc_costs  = to_num(vacc_costs)
  )


# 1) MALARIA: keep iso3, year, and generate costs minus vaccine costs
#    filter Scenario == "PF_20_CC" AND vaccine_compete == 0
df_malaria_rn <- df_malaria2 %>%
  filter(scenario == "PF_20_CC", vaccine_compete == 0) %>%
  mutate(RN_cost = total_cost - cost_vaccine) %>%
  select(iso3, year, RN_cost)

df_malaria_rn_sum <- df_malaria_rn %>%
  filter(year %in% 2027:2029) %>%
  group_by(iso3) %>%
  summarise(RN = sum(RN_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)


# 2) TB: keep iso3, year, and generate costs minus vaccine costs
#    filter Scenario == "PF_09"
df_tb_rn <- df_tb2 %>%
  filter(Scenario == "PF_09") %>%
  mutate(RN_cost = Costs - vacc_costs) %>%
  select(iso3, year, RN_cost)

df_tb_rn_sum <- df_tb_rn %>%
  filter(year %in% 2027:2029) %>%
  group_by(iso3) %>%
  summarise(RN = sum(RN_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)


# 3) HIV: Keep last Step before PLHIV NA
hiv_steps <- df_hiv2 %>%
  filter(grepl("^Step\\d+$", scenario)) %>%
  mutate(
    step_num  = as.integer(sub("^Step", "", scenario)),
    PLHIV_num = PLHIV
  ) %>%
  arrange(iso3, year, step_num)

# Limit to Step 7–13 only
hiv_steps_valid <- hiv_steps %>%
  filter(step_num >= 7, step_num <= 13)

last_valid <- hiv_steps_valid %>%
  group_by(iso3, year) %>%
  summarise(
    last_valid_step = {
      prefix_ok <- cumall(!is.na(PLHIV_num))
      if (any(prefix_ok)) {
        max(step_num[prefix_ok], na.rm = TRUE)
      } else {
        NA_integer_
      }
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(last_valid_step))

df_hiv_rn <- df_hiv2 %>%
  mutate(step_num = as.integer(sub("^Step","",scenario))) %>%
  inner_join(last_valid, by = c("iso3","year")) %>%
  filter(step_num == last_valid_step) %>%
  select(iso3, year, Total_cost) %>%
  distinct()

# Now sum for relevant years: 
df_hiv_rn_sum <- df_hiv_rn %>%
  filter(year %in% 2027:2029) %>%
  group_by(iso3) %>%
  summarise(RN = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)



# Save summed RN data for 2027–2029 using the existing output_path
write.csv(df_hiv_rn_sum,
          file = file.path(output_path, "HIV_RN_2027_2029.csv"),
          row.names = FALSE)

write.csv(df_tb_rn_sum,
          file = file.path(output_path, "TB_RN_2027_2029.csv"),
          row.names = FALSE)

write.csv(df_malaria_rn_sum,
          file = file.path(output_path, "Malaria_RN_2027_2029.csv"),
          row.names = FALSE)
