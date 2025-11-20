# This is Mayuko's code downloaded from Sharepoint 
# However it read in DRM from GC7 so I have modified that around line 39
# also changed the file locations to my folders


###R packages
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)
library(lubridate)
library(readxl)
library(scales)
library(glue)

###Path and Raw files to be imported

path <- "/Users/mc1405/RCode/HDF code/"

Mikaela_file<- "For Mikaela Testing HTM through 2035 with Oct 24 WEO.xlsx"
DAH<- "DAH.xlsx"
Modelled_HIV<- "HIV cost impact results 7nov24.csv" #7Nov2024
Modelled_TB<- "HBC_results_OneFile.xlsx" #15Oct2024
Modelled_Mal<- "output.xlsx"  #4nov2024
GF_allocation<- "CONFIDENTIAL_GC8_Allocation_Projections.csv"

###Parameters

GrantCycle= "GC8"
GC_StartYear="2027"
GC_MidYear<- as.numeric(GC_StartYear)+1
GC_EndYear<- as.numeric(GC_StartYear)+2
GFamountScenario="$17b"
today<- today()


#########
###Import raw data
HIV <- read_excel(
  path  = paste0(path, "stephen_data/", Mikaela_file),
  sheet = "HIV_capbyperiod",
  range = cell_cols("A:J"),
  col_names = FALSE          # treat everything as raw data
) %>%
  # keep only rows where col 1 is a 3-letter ISO like "AFG", "MAR", etc.
  filter(str_detect(.[[1]], "^[A-Z]{3}$")) %>%
  transmute(
    ISO        = as.character(.[[1]]),   # col A
    Base_HIV   = as.numeric(.[[9]]),     # col I = GC08 Sum of DRMHggte
    Dipi50_HIV = as.numeric(.[[10]])     # col J = GC08 Sum of DRMHdipi50
  )

Malaria <- read_excel(
  path  = paste0(path, "stephen_data/", Mikaela_file),
  sheet = "Malaria_capbyperiod",
  range = cell_cols("A:J"),
  col_names = FALSE
) %>%
  filter(str_detect(.[[1]], "^[A-Z]{3}$")) %>%
  transmute(
    ISO        = as.character(.[[1]]),
    Base_Mal   = as.numeric(.[[9]]),     # GC08 Sum of DRMMfwd
    Dipi50_Mal = as.numeric(.[[10]])     # GC08 Sum of DRMMdipi50
  )

TB <- read_excel(
  path  = paste0(path, "stephen_data/", Mikaela_file),
  sheet = "TB_capbyperiod",
  range = cell_cols("A:J"),
  col_names = FALSE
) %>%
  filter(str_detect(.[[1]], "^[A-Z]{3}$")) %>%
  transmute(
    ISO        = as.character(.[[1]]),
    Base_TB    = as.numeric(.[[9]]),     # GC08 Sum of DRMT_HH_ggte
    Dipi50_TB  = as.numeric(.[[10]])     # GC08 Sum of DRMTdipi50
  )


DAH_QZA_unallocated<- read_excel(paste0(path, "stephen_data/",DAH), sheet="DAH_QZA", skip=158) %>%
  rename(category="DAH_IHME_nogf_2020 to 2022") %>%
  filter(category=="DAH_QZA_nogf_GFelig 20_22") %>%
  select(HIV, TB, Malaria)%>%
  mutate(HIV_usd= as.numeric(HIV)*1000000000, TB_usd= as.numeric(TB)*1000000000, Malaria_usd=as.numeric(Malaria)*1000000000)

Modelled_impact_HIV<- read_csv(paste0(path, "model_output/", Modelled_HIV)) %>%
  mutate(scenario_num=parse_number(scenario)) %>%
  filter(scenario_num== max(scenario_num)) %>%
  select(iso3, scenario, year, Total_cost, scenario_num)

Modelled_impact_TB<- read_excel(paste0(path,"model_output/",  Modelled_TB)) %>%
  filter(str_detect(Scenario, "PF")) %>%
  mutate(scenario_num=parse_number(Scenario)) %>%
  filter(scenario_num== max(scenario_num)) %>%
  select(iso3, Scenario, year, Costs, vacc_costs, scenario_num)

Modelled_impact_Malaria<- read_excel(paste0(path, "model_output/", Modelled_Mal)) %>%
  filter(str_detect(scenario, "PF")) %>%
  mutate(scenario_num=parse_number(scenario)) %>%
  filter(scenario_num== max(scenario_num)) %>%
  filter(vaccine_compete==0) %>%
  select(iso3, scenario, year, total_cost, cost_vaccine, scenario_num)

Direct_DAH <- read_excel(paste0(path, "stephen_data/", DAH), sheet = "Direct_DAH_GFelig") %>%
  select(ISO, Direct_H, Direct_T, Direct_M) %>%
  filter(!is.na(ISO))

GF_amount <- read_csv(paste0(path,"gideon_data/", GF_allocation))%>%
  filter(Scenario==GFamountScenario )

###################  FUNGIBLE  ####################
##1. Calculate unallocated amounts(QZA) proportional to country allocation of DAH 
###Step 1. Total of HIV, TB, malaria DAH amounts for all countries
Total_Direct_H <- sum(Direct_DAH$Direct_H)
Total_Direct_T <- sum(Direct_DAH$Direct_T)
Total_Direct_M <- sum(Direct_DAH$Direct_M)

##Step 2. Proportion of DAH allocation per country 
Direct_DAH <- Direct_DAH %>%
  mutate(proportional_H= Direct_H/Total_Direct_H) %>%
  mutate(proportional_T= Direct_T/Total_Direct_T) %>%
  mutate(proportional_M= Direct_M/Total_Direct_M) 

#Step 3. Taking values for total unallocated amounts (QZA, non-country specific)
QZA_H<- DAH_QZA_unallocated$HIV_usd
QZA_T<- DAH_QZA_unallocated$TB_usd
QZA_M<- DAH_QZA_unallocated$Malaria_usd

#Step 4. QZA amount per country according to the country proportion for DAH allocation 
Direct_DAH <- Direct_DAH %>%
  mutate(
    H_QZA_proportional_to_DAH = if_else(Direct_H == 0, 0, QZA_H * proportional_H),
    T_QZA_proportional_to_DAH = if_else(Direct_T == 0, 0, QZA_T * proportional_T),
    M_QZA_proportional_to_DAH = if_else(Direct_M == 0, 0, QZA_M * proportional_M))

#Step 5. GF allocation amount
GF_amount_HIV<- GF_amount %>%
  filter(Component== "HIV/AIDS") %>%
  rename(ISO="ISO3", GF_Allocation_HIV="Allocation")
GF_amount_TB<- GF_amount %>%
  filter(Component== "Tuberculosis")%>%
  rename(ISO="ISO3", GF_Allocation_TB="Allocation")
GF_amount_Mal<- GF_amount %>%
  filter(Component== "Malaria")%>%
  rename(ISO="ISO3", GF_Allocation_Mal="Allocation")
GF_amount_combined<- GF_amount_HIV %>%
  full_join(GF_amount_TB, by="ISO") %>%
  full_join(GF_amount_Mal, by="ISO") %>%
  select(ISO, GF_Allocation_HIV, GF_Allocation_TB, GF_Allocation_Mal)



###################  NON-FUNGIBLE  ####################
##1. Calculate the capping variable from the modelled results (scenario with largest amount of impact for GC8)
###HIV capping
df_hiv_y1 <- Modelled_impact_HIV %>%
  filter(year == GC_StartYear) %>%
  rename(Total_cost_y1=Total_cost)  %>%
  select(-scenario_num, -year)

df_hiv_y2 <- Modelled_impact_HIV %>%
  filter(year == GC_MidYear) %>%
  rename(Total_cost_y2=Total_cost)  %>%
  select(-scenario_num, -year)

df_hiv_y3 <- Modelled_impact_HIV %>%
  filter(year == GC_EndYear) %>%
  rename(Total_cost_y3=Total_cost) %>%
  select(-scenario_num, -year)

Modelled_impact_HIV_GCcombined<- df_hiv_y1 %>%
  left_join(df_hiv_y2, by="iso3") %>%
  left_join(df_hiv_y3, by="iso3") %>%
  mutate(Total_cost= rowSums(across(c(Total_cost_y1, Total_cost_y2, Total_cost_y3)))) 

HIV_cap<- Modelled_impact_HIV_GCcombined %>%
  select(iso3, Total_cost) %>%
  rename(ISO=iso3, HIV_cap=Total_cost)

###TB capping
df_tb_y1 <- Modelled_impact_TB %>%
  mutate(Total_cost=(Costs - vacc_costs)) %>%
  filter(year == GC_StartYear) %>%
  rename(Total_cost_y1=Total_cost)  %>%
  select(-scenario_num, -year)

df_tb_y2 <- Modelled_impact_TB %>%
  mutate(Total_cost=(Costs - vacc_costs)) %>%
  filter(year == GC_MidYear) %>%
  rename(Total_cost_y2=Total_cost)  %>%
  select(-scenario_num, -year)

df_tb_y3 <- Modelled_impact_TB %>%
  mutate(Total_cost=(Costs - vacc_costs)) %>%
  filter(year == GC_EndYear) %>%
  rename(Total_cost_y3=Total_cost)  %>%
  select(-scenario_num, -year)

Modelled_impact_TB_GCcombined<- df_tb_y1 %>%
  left_join(df_tb_y2, by="iso3") %>%
  left_join(df_tb_y3, by="iso3") %>%
  mutate(Total_cost= rowSums(across(c(Total_cost_y1, Total_cost_y2, Total_cost_y3)))) 

TB_cap<- Modelled_impact_TB_GCcombined %>%
  select(iso3, Total_cost) %>%
  rename(ISO=iso3, TB_cap=Total_cost)

###Malaria capping
df_mal_y1 <- Modelled_impact_Malaria %>%
  mutate(Total_cost=(total_cost - cost_vaccine)) %>%
  filter(year == GC_StartYear) %>%
  rename(Total_cost_y1=Total_cost)  %>%
  select(-scenario_num, -year)

df_mal_y2 <- Modelled_impact_Malaria %>%
  mutate(Total_cost=(total_cost - cost_vaccine)) %>%
  filter(year == GC_MidYear) %>%
  rename(Total_cost_y2=Total_cost)  %>%
  select(-scenario_num, -year)

df_mal_y3 <- Modelled_impact_Malaria %>%
  mutate(Total_cost=(total_cost - cost_vaccine)) %>%
  filter(year == GC_EndYear) %>%
  rename(Total_cost_y3=Total_cost)  %>%
  select(-scenario_num, -year)

Modelled_impact_Mal_GCcombined<- df_mal_y1 %>%
  left_join(df_mal_y2, by="iso3") %>%
  left_join(df_mal_y3, by="iso3") %>%
  mutate(Total_cost= rowSums(across(c(Total_cost_y1, Total_cost_y2, Total_cost_y3)))) 

Mal_cap<- Modelled_impact_Mal_GCcombined %>%
  select(iso3, Total_cost) %>%
  rename(ISO=iso3, Mal_cap=Total_cost)

##2. Sum of domestic fund/ggte and DAH
Non_fungible<-Direct_DAH %>%
  full_join(HIV, by="ISO") %>%
  full_join(TB, by= "ISO") %>%
  full_join(Malaria, by="ISO")

#For TB in India, use Dipi50 value instead of Base
Non_fungible<- Non_fungible %>%
  mutate(Base_TB = if_else(ISO == "IND", Dipi50_TB, Base_TB))

#First capping - Cap the ggte (domestic cost) with modelled costs
Non_fungible_2 <- Non_fungible %>%
  left_join(HIV_cap, by="ISO") %>%
  left_join(TB_cap, by="ISO") %>%
  left_join(Mal_cap, by="ISO") %>%
  mutate(HIV_base_c= pmin(Base_HIV, HIV_cap), 
         TB_base_c= pmin(Base_TB, TB_cap), 
         Mal_base_c= pmin(Base_Mal, Mal_cap))

Non_fungible_summed<- Non_fungible_2 %>%
  mutate(HIV_base_DAH= if_else(is.na(HIV_base_c), Direct_H, Direct_H+HIV_base_c),
         TB_base_DAH= if_else(is.na(TB_base_c), Direct_T, Direct_T+TB_base_c), 
         Mal_base_DAH= if_else(is.na(Mal_base_c), Direct_M, Direct_M+Mal_base_c)) 

Non_fungible_capped<- Non_fungible_summed %>%
  select(ISO, Direct_H, Direct_T, Direct_M, Base_HIV, Base_TB, Base_Mal, HIV_base_DAH, 
         TB_base_DAH, Mal_base_DAH, HIV_cap, TB_cap, Mal_cap) %>%
  mutate(HIV_base_DAH_c= pmin(HIV_base_DAH, HIV_cap), 
         TB_base_DAH_c= pmin(TB_base_DAH, TB_cap), 
         Mal_base_DAH_c= pmin(Mal_base_DAH, Mal_cap),
         HIV_cap_leftover= HIV_base_DAH - HIV_cap, 
         TB_cap_leftover= TB_base_DAH - TB_cap, 
         Mal_cap_leftover=  Mal_base_DAH - Mal_cap, 
         HIV_cap_leftover= if_else(HIV_cap_leftover>0, HIV_cap_leftover, 0), 
         TB_cap_leftover= if_else(TB_cap_leftover>0, TB_cap_leftover, 0),
         Mal_cap_leftover= if_else(Mal_cap_leftover>0, Mal_cap_leftover, 0)) 

##2. Calculate the capping leftover (difference of sums of domestic growth/ggte and DAH)
Cap_leftover<- Non_fungible_capped %>%
  select(ISO,HIV_cap_leftover,  TB_cap_leftover, Mal_cap_leftover)


#####################################################################################
###########################COMBINE THE FUNGIBLE AMOUNTS##############################
Fungible_combined<- Direct_DAH %>%
  full_join(GF_amount_combined , by="ISO") %>%
  full_join(Cap_leftover, by="ISO") %>%
  mutate(Fungible_amount_HIV = rowSums(across(c(GF_Allocation_HIV, H_QZA_proportional_to_DAH, HIV_cap_leftover)), na.rm = TRUE)) %>%
  mutate(Fungible_amount_TB = rowSums(across(c(GF_Allocation_TB, T_QZA_proportional_to_DAH, TB_cap_leftover)), na.rm = TRUE)) %>%
  mutate(Fungible_amount_Mal = rowSums(across(c(GF_Allocation_Mal, M_QZA_proportional_to_DAH, Mal_cap_leftover)), na.rm = TRUE))
Fungible_combined_selected<- Fungible_combined %>%
  select(ISO, Fungible_amount_HIV,  Fungible_amount_TB, Fungible_amount_Mal )

today=lubridate::today()
write.xlsx(Fungible_combined,
           paste("C:/Users/mtakamiya/OneDrive - The Global Fund/Documents/Investment capping due dilligence/fungible_combined_",today,".xlsx"))


########################NON-FUNGIBLE AMOUNTS#########################################
Non_fungible_capped_selected <- Non_fungible_capped %>%
  select(ISO, HIV_base_DAH_c, TB_base_DAH_c, Mal_base_DAH_c)

###############EXCEL OUTPUT##################################################
output_file<- paste0("Investment_case_capping","_", today, ".xlsx")
Path_file<- file.path(path, output_file)
today<- today()

wb <- createWorkbook()
addWorksheet(wb, "Fungible")
writeData(wb, "Fungible", Fungible_combined_selected)
addWorksheet(wb, "Non-fungible")
writeData(wb, "Non-fungible", Non_fungible_capped_selected)
saveWorkbook(wb, Path_file, overwrite = TRUE)
