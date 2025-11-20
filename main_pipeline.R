# -----------------------------------------
# main_pipeline.R
# Master script to run the full capping pipeline
#
# Order:
#  1. Extract RN (from model output)
#  2. Extract DAH (allocated + unallocated / QZA)
#  3. Extract DRM (HTM workbook)
#  4. Run capping + build non-fungible and fungible files
#
# Note: due-diligence / Mayuko comparison script is kept separate
#       and can be sourced manually if/when needed.
# -----------------------------------------

# You should call this script from the *project root*:
#   setwd("/Users/mc1405/RCode/HDF code")
#   source("RCode/main_pipeline.R")

# 1) Load settings + helpers (in RCode/)
source("RCode/00_settings.R")
source("RCode/01_helpers.R")

cat("Step 1: Extracting Resource Needs (RN)...\n")
source("RCode/02_extract_RN.R")

cat("Step 2: Extracting DAH allocations and QZA unallocated...\n")
source("RCode/03_extract_DAH.R")

cat("Step 3: Extracting DRM from HTM workbook...\n")
source("RCode/04_extract_DRM.R")

cat("Step 4: Running capping and building fungible files...\n")
source("RCode/05_capping_and_fungible.R")

cat("\n✔ Pipeline finished.\n")
cat("Non-fungible & fungible CSVs are in: ", OUTPUT_DIR, "\n")

# If you later want to run due-diligence comparison:
# setwd(ROOT_DIR); source("RCode/06_due_diligence_compare.R")
