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
dfm_file     <- file.path(proj_dir, "For Mikaela Testing HTM through 2035 with Oct 24 WEO.xlsx")

# ---- Helper: clean numeric ----
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9eE\\.-]", "", as.character(x))))
}

# ---- 1. HIV ----
hiv_raw <- read.xlsx(
  dfm_file,
  sheet    = "HIV",
  colNames = TRUE
)

# Fix duplicated names
names(hiv_raw) <- make.names(names(hiv_raw), unique = TRUE)

# Extract and clean HIV DRM for GC8 only
hiv_drm <- hiv_raw %>%
  filter(!is.na(period), period == "GC08") %>%
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(DRMHggte),
    dipi50_uncap     = to_num(DRMHdipi50),
    dipi80_uncap     = to_num(DRMHdipi80)
  ) %>%
  filter(!is.na(iso3), iso3 != "")

# Sum over GC8 period
hiv_drm <- hiv_drm %>%
  group_by(iso3) %>%
  summarise(
    econgrowth_uncap = sum(econgrowth_uncap, na.rm = TRUE),
    dipi50_uncap     = sum(dipi50_uncap,     na.rm = TRUE),
    dipi80_uncap     = sum(dipi80_uncap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso3)


# ---- 2.TB ----
tb_raw <- read.xlsx(
  dfm_file,
  sheet    = "TB",
  colNames = TRUE
)

# Fix duplicated names
names(tb_raw) <- make.names(names(tb_raw), unique = TRUE)

# Identify the correct uncapped DRMH columns
tb_drm <- tb_raw %>%
  filter(!is.na(period), period == "GC08") %>%
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(DRMT_HH_ggte),
    dipi50_uncap     = to_num(DRMTdipi50),
    dipi80_uncap     = to_num(DRMTdipi80)
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
  dfm_file,
  sheet    = "Malaria",
  colNames = TRUE
)

# Fix duplicated names
names(mal_raw) <- make.names(names(mal_raw), unique = TRUE)

# Identify the correct uncapped DRMH columns
mal_drm <- mal_raw %>%
  filter(!is.na(Period), Period == "GC08") %>%
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(DRMM_ggte),
    dipi50_uncap     = to_num(DRMMdipi50),
    dipi80_uncap     = to_num(DRMMdipi80)
  ) %>%
  filter(!is.na(iso3), iso3 != "")


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
