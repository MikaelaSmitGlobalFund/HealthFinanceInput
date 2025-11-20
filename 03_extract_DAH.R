# -----------------------------------------
# 03_extract_DAH.R
# DAH extraction and QZA allocation
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


dah_file <- file.path(STEPHEN_DIR, FILE_DAH)
assert_file(dah_file)

# ---- Read Direct DAH ----
direct_dah_raw <- read.xlsx(
  xlsxFile      = dah_file,
  sheet         = "Direct_DAH_GFelig",
  detectDates   = TRUE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE
)

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
  filter(!(coalesce(inH, 0) == 0 &
             coalesce(inT, 0) == 0 &
             coalesce(inM, 0) == 0)) %>%
  rename(ISO3 = ISO) %>%
  arrange(ISO3) %>%
  select(ISO3, inH, inT, inM, Direct_H, Direct_T, Direct_M)

# ---- QZA totals (DAH_QZA) ----
vals_raw <- read.xlsx(
  dah_file,
  sheet    = "DAH_QZA",
  rows     = 166,
  cols     = 21:23,
  colNames = FALSE,
  rowNames = FALSE
)

nums_bil <- to_num(unlist(vals_raw))
stopifnot(length(nums_bil) == 3)

nums_usd <- nums_bil * 1e9

DAH_QZA_all <- data.frame(
  Disease     = c("HIV", "TB", "MAL"),
  DAH_Billion = nums_bil,
  DAH_USD     = nums_usd
)

HIV_total <- DAH_QZA_all$DAH_USD[DAH_QZA_all$Disease == "HIV"]
TB_total  <- DAH_QZA_all$DAH_USD[DAH_QZA_all$Disease == "TB"]
MAL_total <- DAH_QZA_all$DAH_USD[DAH_QZA_all$Disease == "MAL"]

# ---- Distribute QZA proportionally ----
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
      alloc[idx] <- total_amount / length(idx)
    }
  }
  
  tibble(ISO3 = df$ISO3, unalloc = alloc)
}

HIV_unalloc <- distribute(DAH_Allocated, "inH", "Direct_H", HIV_total) %>%
  rename(Unalloc_H = unalloc)

TB_unalloc  <- distribute(DAH_Allocated, "inT", "Direct_T", TB_total) %>%
  rename(Unalloc_T = unalloc)

MAL_unalloc <- distribute(DAH_Allocated, "inM", "Direct_M", MAL_total) %>%
  rename(Unalloc_M = unalloc)

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
          file.path(OUTPUT_DIR, FILE_DAH_UNAL),
          row.names = FALSE)

DAH_Allocated_export <- DAH_Allocated %>%
  select(ISO3, Direct_H, Direct_T, Direct_M) %>%
  rename(Alloc_H = Direct_H,
         Alloc_T = Direct_T,
         Alloc_M = Direct_M)

write.csv(DAH_Allocated_export,
          file = file.path(OUTPUT_DIR, FILE_DAH_ALLOC),
          row.names = FALSE)
