# -----------------------------------------
# 01_helpers.R
# Shared helper functions & library setup
# -----------------------------------------

# ---- Libraries ----
suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(readr)
  library(openxlsx)
  library(readxl)
})

# ---- Numeric cleaners ----
to_num <- function(x) {
  suppressWarnings({
    x <- as.character(x)
    x <- gsub(",", "", x)
    x <- gsub("\\$", "", x)
    x <- gsub("\\s+", "", x)
    x <- gsub("[()]", "-", x)               # (2000) → -2000
    x <- gsub("[^0-9eE.+\\-]", "", x)       # keep digits, + - . e E .
    as.numeric(x)
  })
}

# Slightly stricter version used in some places if desired
to_num_simple <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9eE\\.\\-]", "", as.character(x))))
}

# ---- ISO cleaner ----
clean_iso <- function(x) {
  toupper(trimws(as.character(x)))
}

# ---- Scenario label: "$17b" → "17" ----
scenario_label <- function(s) {
  gsub("[^0-9]", "", s)
}

# ---- Excel with empty rows kept ----
read_excel_keep_empty <- function(path, sheet = 1) {
  openxlsx::read.xlsx(
    xlsxFile       = path,
    sheet          = sheet,
    detectDates    = TRUE,
    skipEmptyRows  = FALSE,
    skipEmptyCols  = FALSE,
    na.strings     = c("", "NA")
  )
}

# ---- Simple file existence check ----
assert_file <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
}
