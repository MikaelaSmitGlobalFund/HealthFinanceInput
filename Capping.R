# First Version: 17th November 2025 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will extract the DAH from the Stephen's files
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

# Map file names
dah_alloc_path   <- file.path(output_path, "DAH_Allocated.csv")
dah_unalloc_path <- file.path(output_path, "DAH_Unallocated.csv")

hiv_rn_path      <- file.path(output_path, "HIV_RN_2027_2029.csv")
tb_rn_path       <- file.path(output_path, "TB_RN_2027_2029.csv")
mal_rn_path      <- file.path(output_path, "Malaria_RN_2027_2029.csv")

hiv_drm_path     <- file.path(output_path, "HIV_DRM.csv")
tb_drm_path      <- file.path(output_path, "TB_DRM.csv")
mal_drm_path     <- file.path(output_path, "Malaria_DRM.csv")

# Load data
# DAH data
dah_alloc   <- read.csv(dah_alloc_path,   stringsAsFactors = FALSE)
dah_unalloc <- read.csv(dah_unalloc_path, stringsAsFactors = FALSE)

# Rename ISO3 → iso3 if present
if ("ISO3" %in% names(dah_alloc)) {
  dah_alloc <- dah_alloc %>% rename(iso3 = ISO3)
}

if ("ISO3" %in% names(dah_unalloc)) {
  dah_unalloc <- dah_unalloc %>% rename(iso3 = ISO3)
}

# RN (resource need) data
hiv_rn <- read.csv(hiv_rn_path, stringsAsFactors = FALSE)
tb_rn  <- read.csv(tb_rn_path,  stringsAsFactors = FALSE)
mal_rn <- read.csv(mal_rn_path, stringsAsFactors = FALSE)

# DRM (domestic resource mobilization) data
hiv_drm <- read.csv(hiv_drm_path, stringsAsFactors = FALSE)
tb_drm  <- read.csv(tb_drm_path,  stringsAsFactors = FALSE)
mal_drm <- read.csv(mal_drm_path, stringsAsFactors = FALSE)

# Helper function
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9eE\\.\\-]", "", as.character(x))))
}


# Let's cap
# ---- 1) HIV ____
# Merge RN + DRM + DAH allocated by iso3
hiv_df <- hiv_rn %>%
  mutate(iso3 = as.character(iso3)) %>%
  left_join(hiv_drm %>% mutate(iso3 = as.character(iso3)),
            by = "iso3") %>%
  left_join(dah_alloc %>%
              mutate(iso3 = as.character(iso3)) %>%
              select(iso3, Alloc_H),
            by = "iso3")

# Make sure numeric & replace NA with 0 for sums
hiv_df <- hiv_df %>%
  mutate(
    RN                = to_num(RN),
    econgrowth_uncap  = to_num(econgrowth_uncap),
    dipi50_uncap      = to_num(dipi50_uncap),
    dipi80_uncap      = to_num(dipi80_uncap),
    Alloc_H           = to_num(Alloc_H)
  ) %>%
  mutate(across(c(RN, econgrowth_uncap, dipi50_uncap, dipi80_uncap, Alloc_H),
                ~ tidyr::replace_na(.x, 0)))

# Sum domestic + allocated DAH (per scenario)
hiv_df <- hiv_df %>%
  mutate(
    summed_nonfung_econgrowth = econgrowth_uncap + Alloc_H,
    summed_nonfung_dipi50     = dipi50_uncap     + Alloc_H,
    summed_nonfung_dipi80     = dipi80_uncap     + Alloc_H
  )

hiv_df <- hiv_df %>%
  mutate(
    # capped at RN
    econgrowth_capped = pmin(summed_nonfung_econgrowth, RN),
    dipi50_capped     = pmin(summed_nonfung_dipi50,     RN),
    dipi80_capped     = pmin(summed_nonfung_dipi80,     RN),

    # deltas = amount above RN (negative → 0)
    delta_econgrowth  = pmax(summed_nonfung_econgrowth - RN, 0),
    delta_dipi50      = pmax(summed_nonfung_dipi50     - RN, 0),
    delta_dipi80      = pmax(summed_nonfung_dipi80     - RN, 0)
  )

# Save the non-fungible files
hiv_nonfung_base_c <- hiv_df %>%
  select(country = iso3,
         cost    = econgrowth_capped)

write.csv(
  hiv_nonfung_base_c,
  file.path(output_path, "hiv_nonfung_base_c.csv"),
  row.names = FALSE
)


hiv_nonfung_dipi50_c <- hiv_df %>%
  select(country = iso3,
         cost    = dipi50_capped)

write.csv(
  hiv_nonfung_dipi50_c,
  file.path(output_path, "hiv_nonfung_dipi50_c.csv"),
  row.names = FALSE
)


hiv_nonfung_dipi80_c <- hiv_df %>%
  select(country = iso3,
         cost    = dipi80_capped)

write.csv(
  hiv_nonfung_dipi80_c,
  file.path(output_path, "hiv_nonfung_dipi80_c.csv"),
  row.names = FALSE
)


