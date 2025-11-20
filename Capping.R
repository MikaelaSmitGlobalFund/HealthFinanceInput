# -----------------------------------------
# Capping
# First Version: 17 Nov 2025 (cleaned)
# -----------------------------------------

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

# Helper function
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9eE\\.\\-]", "", as.character(x))))
}
clean_iso <- function(x) toupper(trimws(as.character(x)))

# Helper function to make a clean scenario label like "11" from "$11b"
scenario_label <- function(s) gsub("[^0-9]", "", s)

# Map file names
dah_alloc_path   <- file.path(output_path, "DAH_Allocated.csv")
dah_unalloc_path <- file.path(output_path, "DAH_Unallocated.csv")

hiv_rn_path      <- file.path(output_path, "HIV_RN_2027_2029.csv")
tb_rn_path       <- file.path(output_path, "TB_RN_2027_2029.csv")
mal_rn_path      <- file.path(output_path, "Malaria_RN_2027_2029.csv")

hiv_drm_path     <- file.path(output_path, "HIV_DRM.csv")
tb_drm_path      <- file.path(output_path, "TB_DRM.csv")
mal_drm_path     <- file.path(output_path, "Malaria_DRM.csv")

gc8_path <- file.path(getwd(), "gideon_data", "CONFIDENTIAL_GC8_Allocation_Projections.csv")

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

# GF funding
gc8 <- read.csv(gc8_path, stringsAsFactors = FALSE) %>%
  mutate(
    Scenario   = as.character(Scenario),
    ISO3       = clean_iso(ISO3),
    Component  = as.character(Component),
    Allocation = to_num(Allocation)
  )


# ================================================================
# ---- 1) HIV NON_FUNGIBLE  ----
# ================================================================
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

# ---- Two-step capping (like Mayuko) ----
# 1) Cap domestic vs RN
# 2) Add DAH, cap again vs RN
# 3) Delta = (domestic_cap + DAH) - RN, floored at 0
hiv_df <- hiv_df %>%
  mutate(
    # 1st cap: domestic vs RN
    econ_dom_cap   = pmin(econgrowth_uncap, RN),
    dipi50_dom_cap = pmin(dipi50_uncap,     RN),
    dipi80_dom_cap = pmin(dipi80_uncap,     RN),
    
    # Combine with DAH
    econ_base_DAH   = econ_dom_cap   + Alloc_H,
    dipi50_base_DAH = dipi50_dom_cap + Alloc_H,
    dipi80_base_DAH = dipi80_dom_cap + Alloc_H,
    
    # 2nd cap: (domestic_cap + DAH) vs RN
    econgrowth_capped = pmin(econ_base_DAH,   RN),
    dipi50_capped     = pmin(dipi50_base_DAH, RN),
    dipi80_capped     = pmin(dipi80_base_DAH, RN),
    
    # Deltas: (domestic_cap + DAH) - RN, but not < 0
    delta_econgrowth  = pmax(econ_base_DAH   - RN, 0),
    delta_dipi50      = pmax(dipi50_base_DAH - RN, 0),
    delta_dipi80      = pmax(dipi80_base_DAH - RN, 0)
  )


# Save the non-fungible files (HIV)
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

# ================================================================
# ---- 2) TB NON_FUNGIBLE  ----
# ================================================================

tb_df <- tb_rn %>%
  mutate(iso3 = as.character(iso3)) %>%
  left_join(tb_drm %>% mutate(iso3 = as.character(iso3)),
            by = "iso3") %>%
  left_join(dah_alloc %>%
              mutate(iso3 = as.character(iso3)) %>%
              select(iso3, Alloc_T),
            by = "iso3")

# ensure numerics
tb_df <- tb_df %>%
  mutate(
    RN                = to_num(RN),
    econgrowth_uncap  = to_num(econgrowth_uncap),
    dipi50_uncap      = to_num(dipi50_uncap),
    dipi80_uncap      = to_num(dipi80_uncap),
    Alloc_T           = to_num(Alloc_T)
  ) %>%
  mutate(across(c(RN, econgrowth_uncap, dipi50_uncap, dipi80_uncap, Alloc_T),
                ~ tidyr::replace_na(.x, 0)))

# For TB in India, use dipi50 as the "econ" path (like Mayuko)
tb_df <- tb_df %>%
  mutate(
    econgrowth_uncap = ifelse(iso3 == "IND", dipi50_uncap, econgrowth_uncap)
  )

# Two-step capping for TB
tb_df <- tb_df %>%
  mutate(
    # 1st cap: domestic vs RN
    econ_dom_cap   = pmin(econgrowth_uncap, RN),
    dipi50_dom_cap = pmin(dipi50_uncap,     RN),
    dipi80_dom_cap = pmin(dipi80_uncap,     RN),
    
    # Combine with DAH
    econ_base_DAH   = econ_dom_cap   + Alloc_T,
    dipi50_base_DAH = dipi50_dom_cap + Alloc_T,
    dipi80_base_DAH = dipi80_dom_cap + Alloc_T,
    
    # 2nd cap
    econgrowth_capped = pmin(econ_base_DAH,   RN),
    dipi50_capped     = pmin(dipi50_base_DAH, RN),
    dipi80_capped     = pmin(dipi80_base_DAH, RN),
    
    # Deltas
    delta_econgrowth  = pmax(econ_base_DAH   - RN, 0),
    delta_dipi50      = pmax(dipi50_base_DAH - RN, 0),
    delta_dipi80      = pmax(dipi80_base_DAH - RN, 0)
  )

# Save TB non-fungible files
tb_nonfung_base_c <- tb_df %>%
  select(country = iso3, cost = econgrowth_capped)

write.csv(tb_nonfung_base_c,
          file.path(output_path, "tb_nonfung_base_c.csv"),
          row.names = FALSE)

tb_nonfung_dipi50_c <- tb_df %>%
  select(country = iso3, cost = dipi50_capped)

write.csv(tb_nonfung_dipi50_c,
          file.path(output_path, "tb_nonfung_dipi50_c.csv"),
          row.names = FALSE)

tb_nonfung_dipi80_c <- tb_df %>%
  select(country = iso3, cost = dipi80_capped)

write.csv(tb_nonfung_dipi80_c,
          file.path(output_path, "tb_nonfung_dipi80_c.csv"),
          row.names = FALSE)


# ================================================================
# ---- 3) MALARIA NON_FUNGIBLE ----
# ================================================================

mal_df <- mal_rn %>%
  mutate(iso3 = as.character(iso3)) %>%
  left_join(mal_drm %>% mutate(iso3 = as.character(iso3)),
            by = "iso3") %>%
  left_join(dah_alloc %>%
              mutate(iso3 = as.character(iso3)) %>%
              select(iso3, Alloc_M),
            by = "iso3")

# ensure numerics
mal_df <- mal_df %>%
  mutate(
    RN                = to_num(RN),
    econgrowth_uncap  = to_num(econgrowth_uncap),
    dipi50_uncap      = to_num(dipi50_uncap),
    dipi80_uncap      = to_num(dipi80_uncap),
    Alloc_M           = to_num(Alloc_M)
  ) %>%
  mutate(across(c(RN, econgrowth_uncap, dipi50_uncap, dipi80_uncap, Alloc_M),
                ~ tidyr::replace_na(.x, 0)))

# Two-step capping for Malaria
mal_df <- mal_df %>%
  mutate(
    # 1st cap: domestic vs RN
    econ_dom_cap   = pmin(econgrowth_uncap, RN),
    dipi50_dom_cap = pmin(dipi50_uncap,     RN),
    dipi80_dom_cap = pmin(dipi80_uncap,     RN),
    
    # Combine with DAH
    econ_base_DAH   = econ_dom_cap   + Alloc_M,
    dipi50_base_DAH = dipi50_dom_cap + Alloc_M,
    dipi80_base_DAH = dipi80_dom_cap + Alloc_M,
    
    # 2nd cap
    econgrowth_capped = pmin(econ_base_DAH,   RN),
    dipi50_capped     = pmin(dipi50_base_DAH, RN),
    dipi80_capped     = pmin(dipi80_base_DAH, RN),
    
    # Deltas
    delta_econgrowth  = pmax(econ_base_DAH   - RN, 0),
    delta_dipi50      = pmax(dipi50_base_DAH - RN, 0),
    delta_dipi80      = pmax(dipi80_base_DAH - RN, 0)
  )

# Save malaria non-fungible files
mal_nonfung_base_c <- mal_df %>%
  select(country = iso3, cost = econgrowth_capped)

write.csv(mal_nonfung_base_c,
          file.path(output_path, "mal_nonfung_base_c.csv"),
          row.names = FALSE)

mal_nonfung_dipi50_c <- mal_df %>%
  select(country = iso3, cost = dipi50_capped)

write.csv(mal_nonfung_dipi50_c,
          file.path(output_path, "mal_nonfung_dipi50_c.csv"),
          row.names = FALSE)

mal_nonfung_dipi80_c <- mal_df %>%
  select(country = iso3, cost = dipi80_capped)

write.csv(mal_nonfung_dipi80_c,
          file.path(output_path, "mal_nonfung_dipi80_c.csv"),
          row.names = FALSE)


# ==================================
# ---- 4) BUILD DFs FOR DELTAS  ----
# ==================================

# (this part is just updated to use the new delta_* we defined above)
hiv_delta <- hiv_df %>%
  mutate(iso3 = clean_iso(iso3)) %>%
  select(iso3, delta_econgrowth, delta_dipi50, delta_dipi80)

tb_delta <- tb_df %>%
  mutate(iso3 = clean_iso(iso3)) %>%
  select(iso3, delta_econgrowth, delta_dipi50, delta_dipi80)

mal_delta <- mal_df %>%
  mutate(iso3 = clean_iso(iso3)) %>%
  select(iso3, delta_econgrowth, delta_dipi50, delta_dipi80)


# Helper function to generate fungible
# ---- Generic builder: allocation + unallocated + delta ----
# Helper function to generate fungible
# ---- Generic builder: allocation + unallocated + delta ----
build_fungible_files <- function(component_name,
                                 delta_df,
                                 unalloc_col,
                                 file_prefix) {
  
  # Filter to this component in Gideon's file
  comp_alloc <- gc8 %>%
    filter(Component == component_name) %>%
    mutate(ISO3 = clean_iso(ISO3))
  
  # List of ALL countries that appear in *any* of:
  # - Gideon allocations (comp_alloc$ISO3)
  # - your DAH unallocated table
  # - your delta table
  all_iso <- union(
    comp_alloc$ISO3,
    union(dah_unalloc$iso3, delta_df$iso3)
  )
  
  # Loop over each Scenario (e.g. "$11b", "$17b")
  for (sc in unique(comp_alloc$Scenario)) {
    sc_lab <- scenario_label(sc)
    
    # Gideon allocation for this scenario
    sc_alloc <- comp_alloc %>%
      filter(Scenario == sc) %>%
      select(ISO3, Allocation)
    
    # start with full country set
    tmp <- data.frame(ISO3 = all_iso) %>%
      # add Gideon allocations (some will be NA)
      left_join(sc_alloc, by = "ISO3") %>%
      # add unallocated DAH
      left_join(
        dah_unalloc %>% select(iso3, !!unalloc_col),
        by = c("ISO3" = "iso3")
      ) %>%
      # add deltas
      left_join(
        delta_df,
        by = c("ISO3" = "iso3")
      ) %>%
      mutate(
        Allocation       = replace_na(Allocation, 0),
        !!unalloc_col    := replace_na(.data[[unalloc_col]], 0),
        delta_econgrowth = replace_na(delta_econgrowth, 0),
        delta_dipi50     = replace_na(delta_dipi50, 0),
        delta_dipi80     = replace_na(delta_dipi80, 0)
      )
    
    # econ growth: Allocation + unalloc + delta_econgrowth
    econ_df <- tmp %>%
      transmute(
        country = ISO3,
        cost    = Allocation + .data[[unalloc_col]] + delta_econgrowth
      )
    
    write.csv(
      econ_df,
      file.path(output_path,
                paste0(file_prefix, "_fung_incl_econ_", sc_lab, ".csv")),
      row.names = FALSE
    )
    
    # dipi50
    dipi50_df <- tmp %>%
      transmute(
        country = ISO3,
        cost    = Allocation + .data[[unalloc_col]] + delta_dipi50
      )
    
    write.csv(
      dipi50_df,
      file.path(output_path,
                paste0(file_prefix, "_fung_incl_dipi50_", sc_lab, ".csv")),
      row.names = FALSE
    )
    
    # dipi80
    dipi80_df <- tmp %>%
      transmute(
        country = ISO3,
        cost    = Allocation + .data[[unalloc_col]] + delta_dipi80
      )
    
    write.csv(
      dipi80_df,
      file.path(output_path,
                paste0(file_prefix, "_fung_incl_dipi80_", sc_lab, ".csv")),
      row.names = FALSE
    )
  }
}



# ================================================================
# 5) Build fungible-including-delta files per disease & scenario
# ================================================================

# HIV/AIDS
build_fungible_files(
  component_name = "HIV/AIDS",
  delta_df       = hiv_delta,
  unalloc_col    = "Unalloc_H",
  file_prefix    = "hiv"
)

# Tuberculosis
build_fungible_files(
  component_name = "Tuberculosis",
  delta_df       = tb_delta,
  unalloc_col    = "Unalloc_T",
  file_prefix    = "tb"
)

# Malaria
build_fungible_files(
  component_name = "Malaria",
  delta_df       = mal_delta,
  unalloc_col    = "Unalloc_M",
  file_prefix    = "mal"
)
