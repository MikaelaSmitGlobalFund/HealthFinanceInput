# -----------------------------------------
# 02_extract_RN.R
# Resource Need Extraction for HIV/TB/Malaria (2027–2029)
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


# ---- File paths ----
hiv_path     <- file.path(MODEL_DIR, FILE_HIV_MODEL)
malaria_path <- file.path(MODEL_DIR, FILE_MAL_MODEL)
tb_path      <- file.path(MODEL_DIR, FILE_TB_MODEL)

assert_file(hiv_path)
assert_file(malaria_path)
assert_file(tb_path)

# ---- Load and clean model outputs ----
df_hiv2 <- readr::read_csv(hiv_path, show_col_types = FALSE) %>%
  mutate(
    Total_cost = to_num(Total_cost),
    PLHIV      = to_num(PLHIV)
  )

df_malaria2 <- read_excel_keep_empty(malaria_path) %>%
  mutate(
    total_cost     = to_num(total_cost),
    cost_vaccine   = to_num(cost_vaccine),
    vaccine_compete = to_num(vaccine_compete)
  )

df_tb2 <- read_excel_keep_empty(tb_path) %>%
  mutate(
    Costs      = to_num(Costs),
    vacc_costs = to_num(vacc_costs)
  )

# ---- MALARIA RN ----
df_malaria_rn <- df_malaria2 %>%
  filter(scenario == "PF_20_CC", vaccine_compete == 0) %>%
  mutate(RN_cost = total_cost - cost_vaccine) %>%
  select(iso3, year, RN_cost)

df_malaria_rn_sum <- df_malaria_rn %>%
  filter(year %in% GC_YEARS) %>%
  group_by(iso3) %>%
  summarise(RN = sum(RN_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)

# ---- TB RN ----
df_tb_rn <- df_tb2 %>%
  filter(Scenario == "PF_09") %>%
  mutate(RN_cost = Costs - vacc_costs) %>%
  select(iso3, year, RN_cost)

df_tb_rn_sum <- df_tb_rn %>%
  filter(year %in% GC_YEARS) %>%
  group_by(iso3) %>%
  summarise(RN = sum(RN_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)

# ---- HIV RN (Step7–13 prefix rule) ----
hiv_steps <- df_hiv2 %>%
  filter(grepl("^Step\\d+$", scenario)) %>%
  mutate(
    step_num  = as.integer(sub("^Step", "", scenario)),
    PLHIV_num = PLHIV
  ) %>%
  arrange(iso3, year, step_num)

hiv_steps_valid <- hiv_steps %>%
  filter(step_num >= 7, step_num <= 13)

last_valid <- hiv_steps_valid %>%
  group_by(iso3, year) %>%
  summarise(
    last_valid_step = {
      prefix_ok <- cumall(!is.na(PLHIV_num))
      if (any(prefix_ok)) {
        max(step_num[prefix_ok], na.rm = TRUE)
      } else {
        NA_integer_
      }
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(last_valid_step))

df_hiv_rn <- df_hiv2 %>%
  mutate(step_num = as.integer(sub("^Step","",scenario))) %>%
  inner_join(last_valid, by = c("iso3","year")) %>%
  filter(step_num == last_valid_step) %>%
  select(iso3, year, Total_cost) %>%
  distinct()

df_hiv_rn_sum <- df_hiv_rn %>%
  filter(year %in% GC_YEARS) %>%
  group_by(iso3) %>%
  summarise(RN = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3)

# ---- Save RN outputs ----
write.csv(df_hiv_rn_sum,
          file = file.path(OUTPUT_DIR, FILE_HIV_RN),
          row.names = FALSE)

write.csv(df_tb_rn_sum,
          file = file.path(OUTPUT_DIR, FILE_TB_RN),
          row.names = FALSE)

write.csv(df_malaria_rn_sum,
          file = file.path(OUTPUT_DIR, FILE_MAL_RN),
          row.names = FALSE)
