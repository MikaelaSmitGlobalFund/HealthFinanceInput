# -----------------------------------------
# 04_extract_DRM.R
# Extract uncapped Domestic Resource Mobilisation (DRM)
# for HIV, TB, and Malaria from the HTM workbook.
#
# This script:
#   1. Loads the HTM Excel model ("For Mikaela...xlsx")
#   2. Extracts DRM for each disease for GC8 (GC08)
#   3. Cleans numerics, handles duplicates, sums per ISO3
#   4. Saves clean DRM datasets for use in capping
#
# All file paths, GC8 name, and helper functions come from:
#   - 00_settings.R
#   - 01_helpers.R
#
# Output:
#   OUTPUT_DIR/HIV_DRM.csv
#   OUTPUT_DIR/TB_DRM.csv
#   OUTPUT_DIR/Malaria_DRM.csv
# -----------------------------------------

rm(list = ls())

# Auto-load settings & helpers if they are not loaded yet
if (!exists("ROOT_DIR")) {
  if (file.exists("00_settings.R")) {
    # working directory is probably RCode/
    source("00_settings.R")
  } else if (file.exists("RCode/00_settings.R")) {
    # working directory is project root
    source("RCode/00_settings.R")
  } else {
    stop("Cannot find 00_settings.R. Check your working directory.")
  }
}

if (!exists("to_num")) {
  if (file.exists("01_helpers.R")) {
    source("01_helpers.R")
  } else if (file.exists("RCode/01_helpers.R")) {
    source("RCode/01_helpers.R")
  } else {
    stop("Cannot find 01_helpers.R. Check your working directory.")
  }
}

# Always work from the project root defined in 00_settings.R
setwd(ROOT_DIR)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)


# Path to the HTM workbook (contains DRM fields)
drm_file <- file.path(STEPHEN_DIR, FILE_DRM)
assert_file(drm_file)     # check file exists


# ====================================================
#                 HIV DRM extraction
# ====================================================
# The "HIV" sheet contains fields:
#   - ISO (country code)
#   - DRMHggte   → economic-growth DRM (uncapped)
#   - DRMHdipi50 → dipi50 scenario DRM
#   - DRMHdipi80 → dipi80 scenario DRM
#
# We keep only rows where period == GC_NAME (e.g. "GC08")
# Then sum all GC08 rows per ISO3, ensuring numeric cleaning.
# ====================================================

hiv_raw <- read.xlsx(
  drm_file,
  sheet    = "HIV",
  colNames = TRUE
)

# Remove duplicate column names that Excel sometimes generates
names(hiv_raw) <- make.names(names(hiv_raw), unique = TRUE)

hiv_drm <- hiv_raw %>%
  filter(!is.na(period), period == GC_NAME) %>%     # only GC8 rows
  transmute(
    iso3             = ISO,                         # rename to standard field
    econgrowth_uncap = to_num(DRMHggte),
    dipi50_uncap     = to_num(DRMHdipi50),
    dipi80_uncap     = to_num(DRMHdipi80)
  ) %>%
  filter(!is.na(iso3), iso3 != "") %>%              # drop empty rows
  group_by(iso3) %>%                                # in case multiple GC8 rows exist
  summarise(
    econgrowth_uncap = sum(econgrowth_uncap, na.rm = TRUE),
    dipi50_uncap     = sum(dipi50_uncap,     na.rm = TRUE),
    dipi80_uncap     = sum(dipi80_uncap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso3)



# ====================================================
#                 TB DRM extraction
# ====================================================
# TB sheet fields used:
#   - DRMT_HH_ggte
#   - DRMTdipi50
#   - DRMTdipi80
#
# Same process as HIV.
# ====================================================

tb_raw <- read.xlsx(
  drm_file,
  sheet    = "TB",
  colNames = TRUE
)

names(tb_raw) <- make.names(names(tb_raw), unique = TRUE)

tb_drm <- tb_raw %>%
  filter(!is.na(period), period == GC_NAME) %>%     # GC8 only
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(DRMT_HH_ggte),
    dipi50_uncap     = to_num(DRMTdipi50),
    dipi80_uncap     = to_num(DRMTdipi80)
  ) %>%
  filter(!is.na(iso3), iso3 != "") %>%
  group_by(iso3) %>%
  summarise(
    econgrowth_uncap = sum(econgrowth_uncap, na.rm = TRUE),
    dipi50_uncap     = sum(dipi50_uncap,     na.rm = TRUE),
    dipi80_uncap     = sum(dipi80_uncap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso3)



# ====================================================
#             Malaria DRM extraction
# ====================================================
# Sheet name uses "Period" (capital P), not "period".
# Fields:
#   - DRMM_ggte
#   - DRMMdipi50
#   - DRMMdipi80
# ====================================================

mal_raw <- read.xlsx(
  drm_file,
  sheet    = "Malaria",
  colNames = TRUE
)

names(mal_raw) <- make.names(names(mal_raw), unique = TRUE)

mal_drm <- mal_raw %>%
  filter(!is.na(Period), Period == GC_NAME) %>%     # GC8 only
  transmute(
    iso3             = ISO,
    econgrowth_uncap = to_num(DRMM_ggte),
    dipi50_uncap     = to_num(DRMMdipi50),
    dipi80_uncap     = to_num(DRMMdipi80)
  ) %>%
  filter(!is.na(iso3), iso3 != "") %>%
  group_by(iso3) %>%
  summarise(
    econgrowth_uncap = sum(econgrowth_uncap, na.rm = TRUE),
    dipi50_uncap     = sum(dipi50_uncap,     na.rm = TRUE),
    dipi80_uncap     = sum(dipi80_uncap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso3)



# ====================================================
#                  Save Outputs
# ====================================================
# These CSV files are consumed by:
#   05_capping_and_fungible.R
# ====================================================

write.csv(hiv_drm,
          file = file.path(OUTPUT_DIR, FILE_HIV_DRM),
          row.names = FALSE)

write.csv(tb_drm,
          file = file.path(OUTPUT_DIR, FILE_TB_DRM),
          row.names = FALSE)

write.csv(mal_drm,
          file = file.path(OUTPUT_DIR, FILE_MAL_DRM),
          row.names = FALSE)
