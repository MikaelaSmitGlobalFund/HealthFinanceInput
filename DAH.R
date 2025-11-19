# -----------------------------------------
# DAH extraction and allocation
# First Version: 17 Nov 2025 (cleaned)
# -----------------------------------------

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


# ---- Working directory & output path ----
if (computer ==1){
  setwd("/Users/mc1405/RCode/HDF code/")
  output_path = "/Users/mc1405/RCode/HDF code/Output"
}


# ---- Helper function robust numeric conversion ----
to_num <- function(x) {
  suppressWarnings({
    x <- as.character(x)
    x <- gsub(",", "", x)          # remove commas
    x <- gsub("\\$", "", x)        # remove $
    x <- gsub("\\s+", "", x)       # remove spaces
    x <- gsub("[()]", "-", x)      # (2000) → -2000
    x <- gsub("[^0-9eE.+\\-]", "", x)  # KEEP digits, + - . e E
    as.numeric(x)
  })
}


# ---- Load data ----
dah_dir <- file.path(getwd(), "stephen_data")

dah_file <- list.files(
  dah_dir,
  pattern   = "^DAH \\(2\\)\\.xlsx$",
  full.names = TRUE
)[1]

stopifnot(length(dah_file) == 1)

# Read in Allocated data 
direct_dah_raw <- read.xlsx(
  xlsxFile      = dah_file,
  sheet         = "Direct_DAH_GFelig",
  detectDates   = TRUE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE
)

# Clean & keep relevant columns
DAH_Allocated <- direct_dah_raw %>%
  select(
    ISO,
    inH, inT, inM,
    Direct_H, Direct_T, Direct_M,
    everything()
  ) %>%
  mutate(
    Direct_H = to_num(Direct_H),
    Direct_T = to_num(Direct_T),
    Direct_M = to_num(Direct_M),
    inH      = as.integer(inH),
    inT      = as.integer(inT),
    inM      = as.integer(inM)
  ) %>%
  # drop rows where country not eligible for any disease
  filter(!(coalesce(inH, 0) == 0 &
             coalesce(inT, 0) == 0 &
             coalesce(inM, 0) == 0)) %>%
  rename(ISO3 = ISO) %>%
  arrange(ISO3) %>%
  select(ISO3, inH, inT, inM, Direct_H, Direct_T, Direct_M)


# Read in QZA dara 
vals_raw <- read.xlsx(
  dah_file,
  sheet    = "DAH_QZA",
  rows     = 166,
  cols     = 21:23,     # U, V, W
  colNames = FALSE,
  rowNames = FALSE
)

# Convert 1x3 row into numeric vector (billions)
nums_bil <- to_num(unlist(vals_raw))
stopifnot(length(nums_bil) == 3)

# Convert from billions to absolute USD
nums_usd <- nums_bil * 1e9

# Build small DAH_QZA summary table
DAH_QZA_all <- data.frame(
  Disease    = c("HIV", "TB", "MAL"),
  DAH_Billion = nums_bil,
  DAH_USD     = nums_usd
)

HIV_total <- DAH_QZA_all$DAH_USD[DAH_QZA_all$Disease == "HIV"]
TB_total  <- DAH_QZA_all$DAH_USD[DAH_QZA_all$Disease == "TB"]
MAL_total <- DAH_QZA_all$DAH_USD[DAH_QZA_all$Disease == "MAL"]


# --- Helper: distribute total proportionally by Direct_ column for eligible countries ---
distribute <- function(df, elig_col, weight_col, total_amount) {
  stopifnot("ISO3" %in% names(df))
  
  elig <- suppressWarnings(as.integer(df[[elig_col]]))
  elig[is.na(elig)] <- 0
  
  w <- to_num(df[[weight_col]])
  w[is.na(w)] <- 0
  
  idx <- which(elig == 1)
  alloc <- numeric(nrow(df))
  
  if (length(idx) > 0 && !is.na(total_amount) && total_amount != 0) {
    s <- sum(w[idx], na.rm = TRUE)
    if (s > 0) {
      alloc[idx] <- total_amount * (w[idx] / s)
    } else {
      # All eligible weights are zero: split equally
      alloc[idx] <- total_amount / length(idx)
    }
  }
  
  tibble(ISO3 = df$ISO3, unalloc = alloc)
}


# Build per-disease unallocated splits (by ISO3)
HIV_unalloc <- distribute(DAH_Allocated, "inH", "Direct_H", HIV_total) %>%
  rename(Unalloc_H = unalloc)

TB_unalloc  <- distribute(DAH_Allocated, "inT", "Direct_T", TB_total) %>%
  rename(Unalloc_T = unalloc)

MAL_unalloc <- distribute(DAH_Allocated, "inM", "Direct_M", MAL_total) %>%
  rename(Unalloc_M = unalloc)


# Combine into final table keyed on ISO3
DAH_Unallocated <- DAH_Allocated %>%
  select(ISO3) %>%
  distinct() %>%
  left_join(HIV_unalloc, by = "ISO3") %>%
  left_join(TB_unalloc,  by = "ISO3") %>%
  left_join(MAL_unalloc, by = "ISO3") %>%
  mutate(across(c(Unalloc_H, Unalloc_T, Unalloc_M), ~replace_na(.x, 0))) %>%
  arrange(ISO3)


# Save
write.csv(DAH_Unallocated,
          file.path(output_path, "DAH_Unallocated.csv"),
          row.names = FALSE)

# Clean and save country-specific, non-fungible allocations
DAH_Allocated <- DAH_Allocated %>%
  select(ISO3, Direct_H, Direct_T, Direct_M) %>%
  rename(Alloc_H = Direct_H) %>%
  rename(Alloc_T = Direct_T) %>%
  rename(Alloc_M = Direct_M)


write.csv(DAH_Allocated,
          file = file.path(output_path, "DAH_Allocated.csv"),
          row.names = FALSE)

