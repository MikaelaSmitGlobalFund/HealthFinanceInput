# First Version: 14th November 2025 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will extract the RN from the modellers files
# Central switchboard gives you option of choosing which diseases to run


rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# Set Year reference
start_year_min     = 2014                 # This cuts off the first year and removes years were HIVneg with the lag are wrong
start_year_base    = 2021                 # This year is the base year of reporting, in this case 2021
end_year_all       = 2023                 # This is the year of latest partner data
end_year_sdg       = 2030                 # This is the final year of prediction

# Install packages if necessary
if(firstrun>0) {
  install.packages("dplyr")
  install.packages("data.table")
  installed.packages("tidyverse")
  installed.packages("readr")
  installed.packages("openxlsx")
  installed.packages("here")
  installed.packages("janitor")
}

library(dplyr) # require instead
library(data.table)   # For like function (%like%)
library(tidyr)
library(readr)      # for read_csv()
library(openxlsx)   # for read.xlsx() that keeps empty rows
library(here)
library(janitor)

# Set computer, wd and load data
if (computer ==1){
  setwd("/Users/mc1405/RCode/HDF code/")
  output_path = "/Users/mc1405/RCode/HDF code/OuOutput"
}
  
# Load the model data
model_dir <- file.path(getwd(), "model_output")

# Helper: read Excel while KEEPING blank rows/cols
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

# File paths
hiv_path     <- file.path(model_dir, "HIV cost impact results 15oct24.csv")
malaria_path <- file.path(model_dir, "output_2024_09_13.xlsx")
hbc_path     <- file.path(model_dir, "HBC_results_OneFile_2024_10_15.xlsx")

# Load the data
df_hiv2     <- readr::read_csv(hiv_path, show_col_types = FALSE)       # CSV
df_malaria2 <- read_excel_keep_empty(malaria_path)                     # XLSX
df_tb2      <- read_excel_keep_empty(hbc_path)                         # XLSX with blank


# 1) MALARIA: keep iso3, year, total_cost, cost_vaccine
#    filter Scenario == "PF_20_CC" AND vaccine_compete == 0
df_malaria_rn <- df_malaria2 %>%
  filter(scenario == "PF_20_CC", vaccine_compete == 0) %>%
  transmute(iso3, year, total_cost, cost_vaccine)

# 2) TB (HBC): keep iso3, year, Costs, vacc_costs
#    filter Scenario == "PF_09"
df_tb_rn <- df_tb2 %>%
  filter(Scenario == "PF_09") %>%
  transmute(iso3, year, Costs, vacc_costs)


# 3) HIV: Keep only Step-scenarios and parse step number + numeric PLHIV
hiv_steps <- df_hiv2 %>%
  filter(grepl("^Step\\d+$", scenario)) %>%
  mutate(
    step_num  = as.integer(sub("^Step", "", scenario)),
    # robust numeric: strip non-digits except . and -
    PLHIV_num = suppressWarnings({
      x <- gsub("[^0-9.\\-]", "", as.character(PLHIV))
      x[x == ""] <- NA_character_
      as.numeric(x)
    })
  ) %>%
  arrange(iso3, year, step_num)

# HIV: For each iso3-year: take the longest leading run of non-NA PLHIV,
#    then choose the highest step within that run (i.e., last valid step)
last_valid <- hiv_steps %>%
  group_by(iso3, year) %>%
  summarise(
    last_valid_step = {
      prefix_ok <- cumall(!is.na(PLHIV_num))
      if (any(prefix_ok, na.rm = TRUE)) {
        max(step_num[prefix_ok], na.rm = TRUE)
      } else {
        NA_integer_
      }
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(last_valid_step))

# HIV: Build the RN table by joining back and filtering to that final step
df_hiv_rn <- df_hiv2 %>%
  mutate(step_num = suppressWarnings(as.integer(sub("^Step", "", scenario)))) %>%
  inner_join(last_valid, by = c("iso3", "year")) %>%
  filter(step_num == last_valid_step) %>%
  select(iso3, year, Total_cost) %>%
  arrange(iso3, year) %>%
  distinct()

# Now sum for relevant years: 
df_hiv_rn_sum <- df_hiv_rn %>%
  filter(year >= 2027 & year <= 2029) %>%
  group_by(iso3) %>%
  summarise(RN = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)

df_tb_rn_sum <- df_tb_rn %>%
  filter(year >= 2027 & year <= 2029) %>%
  group_by(iso3) %>%
  summarise(RN = sum(Costs, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)

df_malaria_rn_sum <- df_malaria_rn %>%
  filter(year >= 2027 & year <= 2029) %>%
  group_by(iso3) %>%
  summarise(RN = sum(total_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)
