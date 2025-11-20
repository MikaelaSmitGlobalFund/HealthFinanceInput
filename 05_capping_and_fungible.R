# -----------------------------------------
# 05_capping_and_fungible.R
# Non-fungible capping + fungible construction (all scenarios)
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


# ---- Load data ----
dah_alloc   <- read.csv(file.path(OUTPUT_DIR, FILE_DAH_ALLOC),   stringsAsFactors = FALSE)
dah_unalloc <- read.csv(file.path(OUTPUT_DIR, FILE_DAH_UNAL),    stringsAsFactors = FALSE)

if ("ISO3" %in% names(dah_alloc))   dah_alloc   <- dah_alloc   %>% rename(iso3 = ISO3)
if ("ISO3" %in% names(dah_unalloc)) dah_unalloc <- dah_unalloc %>% rename(iso3 = ISO3)

hiv_rn <- read.csv(file.path(OUTPUT_DIR, FILE_HIV_RN), stringsAsFactors = FALSE)
tb_rn  <- read.csv(file.path(OUTPUT_DIR, FILE_TB_RN),  stringsAsFactors = FALSE)
mal_rn <- read.csv(file.path(OUTPUT_DIR, FILE_MAL_RN), stringsAsFactors = FALSE)

hiv_drm <- read.csv(file.path(OUTPUT_DIR, FILE_HIV_DRM), stringsAsFactors = FALSE)
tb_drm  <- read.csv(file.path(OUTPUT_DIR, FILE_TB_DRM),  stringsAsFactors = FALSE)
mal_drm <- read.csv(file.path(OUTPUT_DIR, FILE_MAL_DRM), stringsAsFactors = FALSE)

gc8 <- read.csv(file.path(GIDEON_DIR, FILE_GF_ALLOC), stringsAsFactors = FALSE) %>%
  mutate(
    Scenario   = as.character(Scenario),
    ISO3       = clean_iso(ISO3),
    Component  = as.character(Component),
    Allocation = to_num(Allocation)
  )

# ================================================================
# 1) HIV NON-FUNGIBLE
# ================================================================
# put RN, DRM and QZA into one df
# Then cap DRM to RN
# Then sum capped DRM and QZA and cap to RN
hiv_df <- hiv_rn %>%
  mutate(iso3 = as.character(iso3)) %>%
  left_join(hiv_drm %>% mutate(iso3 = as.character(iso3)),
            by = "iso3") %>%
  left_join(dah_alloc %>%
              mutate(iso3 = as.character(iso3)) %>%
              select(iso3, Alloc_H),
            by = "iso3") %>%
  mutate(
    RN                = to_num_simple(RN),
    econgrowth_uncap  = to_num_simple(econgrowth_uncap),
    dipi50_uncap      = to_num_simple(dipi50_uncap),
    dipi80_uncap      = to_num_simple(dipi80_uncap),
    Alloc_H           = to_num_simple(Alloc_H)
  ) %>%
  mutate(across(c(RN, econgrowth_uncap, dipi50_uncap, dipi80_uncap, Alloc_H),
                ~ tidyr::replace_na(.x, 0))) %>%
  mutate(
    econ_dom_cap   = pmin(econgrowth_uncap, RN),
    dipi50_dom_cap = pmin(dipi50_uncap,     RN),
    dipi80_dom_cap = pmin(dipi80_uncap,     RN),
    econ_base_DAH   = econ_dom_cap   + Alloc_H,
    dipi50_base_DAH = dipi50_dom_cap + Alloc_H,
    dipi80_base_DAH = dipi80_dom_cap + Alloc_H,
    econgrowth_capped = pmin(econ_base_DAH,   RN),
    dipi50_capped     = pmin(dipi50_base_DAH, RN),
    dipi80_capped     = pmin(dipi80_base_DAH, RN),
    delta_econgrowth  = pmax(econ_base_DAH   - RN, 0),
    delta_dipi50      = pmax(dipi50_base_DAH - RN, 0),
    delta_dipi80      = pmax(dipi80_base_DAH - RN, 0)
  )

write.csv(
  hiv_df %>% select(country = iso3, cost = econgrowth_capped),
  file.path(OUTPUT_DIR, FILE_HIV_NONFUNG_BASE),
  row.names = FALSE
)

write.csv(
  hiv_df %>% select(country = iso3, cost = dipi50_capped),
  file.path(OUTPUT_DIR, "hiv_nonfung_dipi50_c.csv"),
  row.names = FALSE
)

write.csv(
  hiv_df %>% select(country = iso3, cost = dipi80_capped),
  file.path(OUTPUT_DIR, "hiv_nonfung_dipi80_c.csv"),
  row.names = FALSE
)

# ================================================================
# 2) TB NON-FUNGIBLE
# ================================================================
# put RN, DRM and QZA into one df
# Then cap DRM to RN
# Then sum capped DRM and QZA and cap to RN
tb_df <- tb_rn %>%
  mutate(iso3 = as.character(iso3)) %>%
  left_join(tb_drm %>% mutate(iso3 = as.character(iso3)),
            by = "iso3") %>%
  left_join(dah_alloc %>%
              mutate(iso3 = as.character(iso3)) %>%
              select(iso3, Alloc_T),
            by = "iso3") %>%
  mutate(
    RN                = to_num_simple(RN),
    econgrowth_uncap  = to_num_simple(econgrowth_uncap),
    dipi50_uncap      = to_num_simple(dipi50_uncap),
    dipi80_uncap      = to_num_simple(dipi80_uncap),
    Alloc_T           = to_num_simple(Alloc_T)
  ) %>%
  mutate(across(c(RN, econgrowth_uncap, dipi50_uncap, dipi80_uncap, Alloc_T),
                ~ tidyr::replace_na(.x, 0))) %>%
  mutate(
    # India: use dipi50 as econ path
    econgrowth_uncap = ifelse(iso3 == "IND", dipi50_uncap, econgrowth_uncap),
    econ_dom_cap   = pmin(econgrowth_uncap, RN),
    dipi50_dom_cap = pmin(dipi50_uncap,     RN),
    dipi80_dom_cap = pmin(dipi80_uncap,     RN),
    econ_base_DAH   = econ_dom_cap   + Alloc_T,
    dipi50_base_DAH = dipi50_dom_cap + Alloc_T,
    dipi80_base_DAH = dipi80_dom_cap + Alloc_T,
    econgrowth_capped = pmin(econ_base_DAH,   RN),
    dipi50_capped     = pmin(dipi50_base_DAH, RN),
    dipi80_capped     = pmin(dipi80_base_DAH, RN),
    delta_econgrowth  = pmax(econ_base_DAH   - RN, 0),
    delta_dipi50      = pmax(dipi50_base_DAH - RN, 0),
    delta_dipi80      = pmax(dipi80_base_DAH - RN, 0)
  )

write.csv(
  tb_df %>% select(country = iso3, cost = econgrowth_capped),
  file.path(OUTPUT_DIR, FILE_TB_NONFUNG_BASE),
  row.names = FALSE
)

write.csv(
  tb_df %>% select(country = iso3, cost = dipi50_capped),
  file.path(OUTPUT_DIR, "tb_nonfung_dipi50_c.csv"),
  row.names = FALSE
)

write.csv(
  tb_df %>% select(country = iso3, cost = dipi80_capped),
  file.path(OUTPUT_DIR, "tb_nonfung_dipi80_c.csv"),
  row.names = FALSE
)

# ================================================================
# 3) MALARIA NON-FUNGIBLE
# ================================================================
# put RN, DRM and QZA into one df
# Then cap DRM to RN
# Then sum capped DRM and QZA and cap to RN
mal_df <- mal_rn %>%
  mutate(iso3 = as.character(iso3)) %>%
  left_join(mal_drm %>% mutate(iso3 = as.character(iso3)),
            by = "iso3") %>%
  left_join(dah_alloc %>%
              mutate(iso3 = as.character(iso3)) %>%
              select(iso3, Alloc_M),
            by = "iso3") %>%
  mutate(
    RN                = to_num_simple(RN),
    econgrowth_uncap  = to_num_simple(econgrowth_uncap),
    dipi50_uncap      = to_num_simple(dipi50_uncap),
    dipi80_uncap      = to_num_simple(dipi80_uncap),
    Alloc_M           = to_num_simple(Alloc_M)
  ) %>%
  mutate(across(c(RN, econgrowth_uncap, dipi50_uncap, dipi80_uncap, Alloc_M),
                ~ tidyr::replace_na(.x, 0))) %>%
  mutate(
    econ_dom_cap   = pmin(econgrowth_uncap, RN),
    dipi50_dom_cap = pmin(dipi50_uncap,     RN),
    dipi80_dom_cap = pmin(dipi80_uncap,     RN),
    econ_base_DAH   = econ_dom_cap   + Alloc_M,
    dipi50_base_DAH = dipi50_dom_cap + Alloc_M,
    dipi80_base_DAH = dipi80_dom_cap + Alloc_M,
    econgrowth_capped = pmin(econ_base_DAH,   RN),
    dipi50_capped     = pmin(dipi50_base_DAH, RN),
    dipi80_capped     = pmin(dipi80_base_DAH, RN),
    delta_econgrowth  = pmax(econ_base_DAH   - RN, 0),
    delta_dipi50      = pmax(dipi50_base_DAH - RN, 0),
    delta_dipi80      = pmax(dipi80_base_DAH - RN, 0)
  )

write.csv(
  mal_df %>% select(country = iso3, cost = econgrowth_capped),
  file.path(OUTPUT_DIR, FILE_MAL_NONFUNG_BASE),
  row.names = FALSE
)

write.csv(
  mal_df %>% select(country = iso3, cost = dipi50_capped),
  file.path(OUTPUT_DIR, "mal_nonfung_dipi50_c.csv"),
  row.names = FALSE
)

write.csv(
  mal_df %>% select(country = iso3, cost = dipi80_capped),
  file.path(OUTPUT_DIR, "mal_nonfung_dipi80_c.csv"),
  row.names = FALSE
)

# ==================================
# 4) BUILD DELTA TABLES
# ==================================
hiv_delta <- hiv_df %>%
  mutate(iso3 = clean_iso(iso3)) %>%
  select(iso3, delta_econgrowth, delta_dipi50, delta_dipi80)

tb_delta <- tb_df %>%
  mutate(iso3 = clean_iso(iso3)) %>%
  select(iso3, delta_econgrowth, delta_dipi50, delta_dipi80)

mal_delta <- mal_df %>%
  mutate(iso3 = clean_iso(iso3)) %>%
  select(iso3, delta_econgrowth, delta_dipi50, delta_dipi80)

# ==================================
# 5) FUNGIBLE: allocation + unalloc + delta
# ==================================
build_fungible_files <- function(component_name,
                                 delta_df,
                                 unalloc_col,
                                 file_prefix) {
  
  comp_alloc <- gc8 %>%
    filter(Component == component_name) %>%
    mutate(ISO3 = clean_iso(ISO3))
  
  all_iso <- union(
    comp_alloc$ISO3,
    union(dah_unalloc$iso3, delta_df$iso3)
  )
  
  for (sc in unique(comp_alloc$Scenario)) {
    sc_lab <- scenario_label(sc)
    
    sc_alloc <- comp_alloc %>%
      filter(Scenario == sc) %>%
      select(ISO3, Allocation)
    
    tmp <- data.frame(ISO3 = all_iso) %>%
      left_join(sc_alloc, by = "ISO3") %>%
      left_join(
        dah_unalloc %>% select(iso3, !!unalloc_col),
        by = c("ISO3" = "iso3")
      ) %>%
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
    
    econ_df <- tmp %>%
      transmute(
        country = ISO3,
        cost    = Allocation + .data[[unalloc_col]] + delta_econgrowth
      )
    write.csv(
      econ_df,
      file.path(OUTPUT_DIR,
                paste0(file_prefix, "_fung_incl_econ_", sc_lab, ".csv")),
      row.names = FALSE
    )
    
    dipi50_df <- tmp %>%
      transmute(
        country = ISO3,
        cost    = Allocation + .data[[unalloc_col]] + delta_dipi50
      )
    write.csv(
      dipi50_df,
      file.path(OUTPUT_DIR,
                paste0(file_prefix, "_fung_incl_dipi50_", sc_lab, ".csv")),
      row.names = FALSE
    )
    
    dipi80_df <- tmp %>%
      transmute(
        country = ISO3,
        cost    = Allocation + .data[[unalloc_col]] + delta_dipi80
      )
    write.csv(
      dipi80_df,
      file.path(OUTPUT_DIR,
                paste0(file_prefix, "_fung_incl_dipi80_", sc_lab, ".csv")),
      row.names = FALSE
    )
  }
}

build_fungible_files("HIV/AIDS",     hiv_delta, "Unalloc_H", "hiv")
build_fungible_files("Tuberculosis", tb_delta,  "Unalloc_T", "tb")
build_fungible_files("Malaria",      mal_delta, "Unalloc_M", "mal")
