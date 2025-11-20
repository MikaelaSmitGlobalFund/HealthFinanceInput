# -----------------------------------------
# 00_settings.R
# Central configuration for capping pipeline
# -----------------------------------------

# ---- Core paths ----
ROOT_DIR <- "/Users/mc1405/RCode/HDF code"
OUTPUT_DIR <- file.path(ROOT_DIR, "Output")
STEPHEN_DIR <- file.path(ROOT_DIR, "stephen_data")
MODEL_DIR   <- file.path(ROOT_DIR, "model_output")
GIDEON_DIR  <- file.path(ROOT_DIR, "gideon_data")

# ---- Core input files ----
# DRM workbook 
FILE_DRM        <- "For Mikaela Testing HTM through 2035 with Oct 24 WEO.xlsx"

# Modelling outputs
FILE_HIV_MODEL  <- "HIV cost impact results 7nov24.csv"
FILE_TB_MODEL   <- "HBC_results_OneFile.xlsx"
FILE_MAL_MODEL  <- "output.xlsx"

# DAH workbook
FILE_DAH        <- "DAH.xlsx"

# GF allocation file (Gideon)
FILE_GF_ALLOC   <- "CONFIDENTIAL_GC8_Allocation_Projections.csv"

# ---- GC & modelling parameters ----
# Name used in HTM sheets (period column): this *must* match "GC08" in the Excel
GC_NAME       <- "GC08"

# Calendar years used in modelling (for RN sums)
GC_START_YEAR <- 2027L
GC_YEARS      <- GC_START_YEAR:(GC_START_YEAR + 2L)   # e.g. 2027:2029

# ---- Output filenames (relative to OUTPUT_DIR) ----
# RN outputs (no years in filename to keep future-proof)
FILE_HIV_RN  <- "HIV_RN.csv"
FILE_TB_RN   <- "TB_RN.csv"
FILE_MAL_RN  <- "Malaria_RN.csv"

# DAH outputs
FILE_DAH_ALLOC <- "DAH_Allocated.csv"
FILE_DAH_UNAL  <- "DAH_Unallocated.csv"

# DRM outputs
FILE_HIV_DRM <- "HIV_DRM.csv"
FILE_TB_DRM  <- "TB_DRM.csv"
FILE_MAL_DRM <- "Malaria_DRM.csv"

# Non-fungible outputs (econ growth main path)
FILE_HIV_NONFUNG_BASE <- "hiv_nonfung_base_c.csv"
FILE_TB_NONFUNG_BASE  <- "tb_nonfung_base_c.csv"
FILE_MAL_NONFUNG_BASE <- "mal_nonfung_base_c.csv"
