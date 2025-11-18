# First Version: 18th November 2025 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will extract the domestic data from Stephen's files
# Central switchboard gives you option of choosing which diseases to run


rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# Install packages if necessary
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


# Set computer, wd and load data
if (computer ==1){
  setwd("/Users/mc1405/RCode/HDF code/")
  output_path = "/Users/mc1405/RCode/HDF code/Output"
}

# ---- File paths ----
proj_dir <- file.path(getwd(), "stephen_data")  # or your actual folder
hiv_proj_file     <- file.path(proj_dir, "HIV Results Update for Mikaela's workflow testing_08_10_2024.xlsx")
tb_proj_file      <- file.path(proj_dir, "TB Results Update for Mikaela's workflow testing_08_10_2024.xlsx")
malaria_proj_file <- file.path(proj_dir, "Malaria DRM Results for Mikaela workflow testing_08_10_2024.xlsx")

# ---- Helper: clean numeric ----
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9eE\\.-]", "", as.character(x))))
}

# ---- 1. HIV ----
hiv_raw <- read.xlsx(
  hiv_proj_file,
  sheet    = "HIV Capped",
  startRow = 3,        # skip the MA/NA junk row
  colNames = TRUE
)

# Fix duplicated names
names(hiv_raw) <- make.names(names(hiv_raw), unique = TRUE)

# Identify the correct uncapped DRMH columns
names(hiv_raw) <- make.names(names(hiv_raw), unique = TRUE)
econ_col  <- grep("Sum.of.DRMH_ggte$",   names(hiv_raw), value = TRUE)[1]
dipi50_col<- grep("Sum.of.DRMHdipi50$",  names(hiv_raw), value = TRUE)[1]
dipi80_col<- grep("Sum.of.DRMHdipi80$",  names(hiv_raw), value = TRUE)[1]

# Filter for correct period and limit to data needed
hiv_drm <- hiv_raw %>%
  filter(!is.na(Period), Period != "GC7") %>%    # keep GC8 only
  transmute(
    iso3 = ISO,
    Period,
    econgrowth_uncap = to_num(.data[[econ_col]]),
    dipi50_uncap     = to_num(.data[[dipi50_col]]),
    dipi80_uncap     = to_num(.data[[dipi80_col]])
  )

hiv_drm <- hiv_drm %>%
  filter(!is.na(iso3), iso3 != "")


# ---- 2.TB ----
tb_raw <- read.xlsx(
  tb_proj_file,
  sheet    = "TB DRM Results",
  colNames = TRUE
)

# Fix duplicated names
names(tb_raw) <- make.names(names(tb_raw), unique = TRUE)

# Identify the correct uncapped DRMH columns
econ_col   <- grep("DRMT_HH_ggte$", names(tb_raw), ignore.case = TRUE, value = TRUE)[1]
dipi50_col <- grep("DRMTdipi50$",   names(tb_raw), ignore.case = TRUE, value = TRUE)[1]
dipi80_col <- grep("DRMTdipi80$",   names(tb_raw), ignore.case = TRUE, value = TRUE)[1]

# Filter for correct period and limit to data needed
tb_drm <- tb_raw %>%
  filter(!is.na(period), period == "GC8") %>%
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(.data[[econ_col]]),
    dipi50_uncap     = to_num(.data[[dipi50_col]]),
    dipi80_uncap     = to_num(.data[[dipi80_col]])
  ) %>%
  filter(!is.na(iso3), iso3 != "")

# Sum over GC8 period
tb_drm <- tb_drm %>%
  group_by(iso3) %>%
  summarise(
    econgrowth_uncap = sum(econgrowth_uncap, na.rm = TRUE),
    dipi50_uncap     = sum(dipi50_uncap,     na.rm = TRUE),
    dipi80_uncap     = sum(dipi80_uncap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso3)



# ---- 3. Malaria ----
mal_raw <- read.xlsx(
  malaria_proj_file,
  sheet    = "Uncapped",
  colNames = TRUE
)

# Fix duplicated names
names(mal_raw) <- make.names(names(mal_raw), unique = TRUE)

# Identify the correct uncapped DRMH columns
econ_col   <- grep("^DRMM_ggte$",  names(mal_raw), value = TRUE)
dipi50_col <- grep("^DRMMdipi50$", names(mal_raw), value = TRUE)
dipi80_col <- grep("^DRMMdipi80$", names(mal_raw), value = TRUE)


# Filter for correct period and limit to data needed
mal_drm <- mal_raw %>%
  filter(!is.na(period), period == "GC8") %>%
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(.data[[econ_col]]),
    dipi50_uncap     = to_num(.data[[dipi50_col]]),
    dipi80_uncap     = to_num(.data[[dipi80_col]])
  ) %>%
  filter(!is.na(iso3), iso3 != "")     # remove blanks


# Sum over GC8 period
mal_drm <- mal_drm %>%
  group_by(iso3) %>%
  summarise(
    econgrowth_uncap = sum(econgrowth_uncap, na.rm = TRUE),
    dipi50_uncap     = sum(dipi50_uncap,     na.rm = TRUE),
    dipi80_uncap     = sum(dipi80_uncap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso3)

# Save
write.csv(hiv_drm,
          file = file.path(output_path, "HIV_DRM.csv"),
          row.names = FALSE)

write.csv(tb_drm,
          file = file.path(output_path, "TB_DRM.csv"),
          row.names = FALSE)

write.csv(mal_drm,
          file = file.path(output_path, "Malaria_DRM.csv"),
          row.names = FALSE)
