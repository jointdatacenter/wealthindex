library(haven)
library(forcats)
library(sf)
library(readxl)
library(writexl)
library(viridis)
library(dplyr)
library(tidyr)
library(stringi)
library(foreign)
library(modelsummary)
library(stargazer)
library(broom)
library(labelled)
library(psych)
library(factoextra)
library(mice)
library(fastDummies)
library(gridExtra)
library("unhcrthemes")
import_lato()
library(GPArotation)
library(Hmisc)
library(gt)
library(webshot2)

main <- read.csv("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/unhcr_ssd_2023_fds_data_v1_1/UNHCR_SSD_2023_FDS_data_main_v2.1.csv")

###CHECK ASSETS ACROSS GROUPS (WHETHER THIS METHODOLOGY IS SUITABLE FOR YOUR DATASET)
#file: check.R

#DATA CLEANING
region_map <- read_excel("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/regions.xlsx") 
main <- main %>%
  left_join(region_map, by = "Intro_camp_label") %>%
  rename(Region = region)

#optional: create aggregate variables for improved drinking water, sanitation, clean cooking fuel (if dataset already has aggregates, skip this step)
main <- main %>%
  mutate(
    Improved_dw = case_when(
      BD01 %in% c(11, 12, 13, 14, 21, 31, 41, 51, 61, 71, 72, 91, 92) ~ 1,
      BD01 %in% c(32, 42, 62, 81, 96) ~ 0,
      TRUE ~ NA_real_  # assigns NA if BD01 has a value not in either list
    ),
    
    Improved_san = case_when(
      SAN01 %in% c(11, 12, 13, 14, 18, 21, 22, 31) ~ 1,
      SAN01 %in% c(23, 41, 51, 95, 96) ~ 0,
      TRUE ~ NA_real_  # assigns NA if SAN01 has a value not in either list
    ),
    
    Clean_cookingfuel = case_when(
      HC01 == 2 ~ 0,                 #no food/drink consumed in household
      HC02 %in% c(1,2,3,4,5) ~ 1,    #solar cooker, electric stove, piped natural gas stove, biogas stove ,lpg/cooking gas stove
      HC04 %in% c(1,11,95)~1,
      HC04 %in% c(2,3,4,5,6,7,8,9,10,12,13,14,96)~0,
      (HC02 %in% c(c(6,7,8,9,10,11,96)))~0,
      TRUE ~ NA_real_
    ),
    
    shared_san_facility = case_when(
      SAN05 %in% c(1)~1,
      SAN05 %in% c(2,98)~0,
      SAN01 %in% c(95,96) ~ 0
    ),
    
    #Proportion of households with access to electricity from TIER 1 or above
    electricity = case_when(
      (HE01 %in% c(1,2) & (HE05_hours >= 1 & HE05_hours != 98)) ~ 1, #If the main source of electricity is national gird or mini-grid which is available at least 1 hour every evening, then Capacity is above tier 1.  
      HE01 %in% c(3,5,6) & HE02a %in% c("A", "B", "C", "D", "E", "F", "G") ~ 1, #If the main source of electricity is solar home system, electric generator or rechargeable batter & the household is able to power at least a small device, then Capacity is above tier 1.
      HE01 == 4 & HE02a %in% c("A", "B")  ~ 1, # If households use a solar lantern, can power either a mobile charger or radio, and use multiple light bulbs, capacity tier is at least 1. 
      TRUE ~ 0 #All other cases, capacity of electricity is below tier 1. 
    ),
    #Crowding (more than 3 household members sleeping in one room)
    memsleep = ifelse(is.na(number_of_hhmembers) | is.na(HH14) | HH14 == 0, number_of_hhmembers/1, number_of_hhmembers / HH14),
    crowding_cat = ifelse(memsleep > 3, 1, 0)
  )

#Land & House ownership - include depending on legal situation in country (do refugees have right to land ownership?)
#calculate agricultural land area (adjust to currencies used)
calculate_land <- function(land09, land21, land21a) {
  if (is.na(land09) || !grepl("B", land09)) {
    return(0)
  }
  if (is.na(land21a)) {
    land21a <- 0
  }
  if (land21a == 2) {
    return(land21)
  } else if (land21a == 1) {
    return(land21 * 0.42)
  } else if (land21a == 3) {
    return(land21 / 10000)
  }
  return(0)
}

main <- main %>%
  rowwise() %>%
  mutate(
    agricultural_land_ha = sum(
      calculate_land(Plot1_Land09, Plot1_Land21, Plot1_Land21a),
      calculate_land(Plot2_Land09, Plot2_Land21, Plot2_Land21a),
      calculate_land(Plot3_Land09, Plot3_Land21, Plot3_Land21a),
      calculate_land(Plot4_Land09, Plot4_Land21, Plot4_Land21a),
      na.rm = TRUE
    )
  ) %>%
  ungroup()%>%
  select(-starts_with("Plot"))

#recode 1/2 to 1/0 and impute NA values
###NAs in House ownership - assume that if household states they own the land that they live on, they also own the house
main <- main %>%
  mutate(House01 = ifelse(is.na(House01) & Land02 == 1, 1, House01))%>%
  mutate(across(
    c(Land01, House01),
    ~ ifelse(. == 1, 1, ifelse(. %in% c(98, 99), NA, 0)) %>% replace_na(0)
  ))

#Housing
##In South Sudan, if HH01 housing type 3 (traditional house such as tukul) or 4 (tent), the respondents were not asked about material of walls (HH02), roof (HH03) and floor (HH06) - this should be fixed in future FDS versions, but in case of high NA counts, worth double checking
###all obs HH01 = 3 (trad. house) we assume walls = mud bricks, roof = thatch, floor = earth/sand
###all obs HH01 = 4 (tent) we assume walls =no walls, roof=no roof, floor = earth/sand
main <- main %>%
  mutate(
    HH02 = ifelse(HH01 == 3 & is.na(HH02), 50, ifelse(HH01 == 4 & is.na(HH02), 11, HH02)),
    HH03 = ifelse(HH01 == 3 & is.na(HH03), 12, ifelse(HH01 == 4 & is.na(HH03), 11, HH03)),
    HH06 = ifelse(HH01 == 3 & is.na(HH06), 11, ifelse(HH01 == 4 & is.na(HH06), 11, HH06))
  )

main <- main %>%
  mutate(
    HH02_WI = case_when(
      HH02 %in% c(11, 12, 13) ~ "natural",
      HH02 %in% c(21, 22, 23, 24, 25, 26, 50) ~ "rudimentary",
      HH02 %in% c(31, 32, 33, 34, 35, 36) ~ "finished",
      TRUE ~ NA_character_
    ),
    HH03_WI = case_when(
      HH03 %in% c(11, 12, 13) ~ "natural",
      HH03 %in% c(21, 22, 23, 24) ~ "rudimentary",
      HH03 %in% c(31, 32, 33, 34, 35, 36) ~ "finished",
      TRUE ~ NA_character_
    ),
    HH06_WI = case_when(
      HH06 %in% c(11, 12) ~ "natural",
      HH06 %in% c(21, 22) ~ "rudimentary",
      HH06 %in% c(31, 32, 33, 34, 35, 36) ~ "finished",
      TRUE ~ NA_character_
    ),
    HH02_finished = case_when(HH02_WI == "finished" ~ 1, TRUE ~ 0),
    HH03_finished = case_when(HH03_WI == "finished" ~ 1, TRUE ~ 0),
    HH06_finished = case_when(HH06_WI == "finished" ~ 1, TRUE ~ 0), 
    HH02_rudimentary = case_when(HH02_WI == "rudimentary" ~ 1, TRUE ~ 0),
    HH03_rudimentary = case_when(HH03_WI == "rudimentary" ~ 1, TRUE ~ 0),
    HH06_rudimentary = case_when(HH06_WI == "rudimentary" ~ 1, TRUE ~ 0), 
    HH02_natural = case_when(HH02_WI == "natural" ~ 1, TRUE ~ 0),
    HH03_natural = case_when(HH03_WI == "natural" ~ 1, TRUE ~ 0),
    HH06_natural = case_when(HH06_WI == "natural" ~ 1, TRUE ~ 0)
  )


#Livestock ownership
##bin all livestock variables
main <- main %>%
  mutate(across(starts_with("Assets04"), ~ ifelse(is.na(.) | . == 99, 0, .))) %>%
  mutate(
    Assets04a_1 = ifelse(Assets04a >= 1 & Assets04a <= 4, 1, 0),
    Assets04a_2 = ifelse(Assets04a >= 5 & Assets04a <= 9, 1, 0),
    Assets04a_3 = ifelse(Assets04a >= 10 & Assets04a != 99, 1, 0),
    Assets04b_1 = ifelse(Assets04b >= 1 & Assets04b <= 4, 1, 0),
    Assets04b_2 = ifelse(Assets04b >= 5 & Assets04b <= 9, 1, 0),
    Assets04b_3 = ifelse(Assets04b >= 10 & Assets04b != 99, 1, 0),
    Assets04c_1 = ifelse(Assets04c >= 1 & Assets04c <= 4, 1, 0),
    Assets04c_2 = ifelse(Assets04c >= 5 & Assets04c <= 9, 1, 0),
    Assets04c_3 = ifelse(Assets04c >= 10 & Assets04c != 99, 1, 0),
    Assets04d_1 = ifelse(Assets04d >= 1 & Assets04d <= 4, 1, 0),
    Assets04d_2 = ifelse(Assets04d >= 5 & Assets04d <= 9, 1, 0),
    Assets04d_3 = ifelse(Assets04d >= 10 & Assets04d != 99, 1, 0),
    Assets04e_1 = ifelse(Assets04e >= 1 & Assets04e <= 4, 1, 0),
    Assets04e_2 = ifelse(Assets04e >= 5 & Assets04e <= 9, 1, 0),
    Assets04e_3 = ifelse(Assets04e >= 10 & Assets04e != 99, 1, 0),
    Assets04f_1 = ifelse(Assets04f >= 1 & Assets04f <= 4, 1, 0),
    Assets04f_2 = ifelse(Assets04f >= 5 & Assets04f <= 9, 1, 0),
    Assets04f_3 = ifelse(Assets04f >= 10 & Assets04f != 99, 1, 0),
    Assets04g_1 = ifelse(Assets04g >= 1 & Assets04g <= 4, 1, 0),
    Assets04g_2 = ifelse(Assets04g >= 5 & Assets04g <= 9, 1, 0),
    Assets04g_3 = ifelse(Assets04g >= 10 & Assets04g != 99, 1, 0)
  )
main <- main %>%
  set_variable_labels(
    Assets04a_1 = "Cows/bulls: 1-4",
    Assets04a_2 = "Cows/bulls: 5-9",
    Assets04a_3 = "Cows/bulls: 10+",
    Assets04b_1 = "Other cattle: 1-4",
    Assets04b_2 = "Other cattle: 5-9",
    Assets04b_3 = "Other cattle: 10+",
    Assets04c_1 = "Horses/donkeys/mules: 1-4",
    Assets04c_2 = "Horses/donkeys/mules: 5-9",
    Assets04c_3 = "Horses/donkeys/mules: 10+",
    Assets04d_1 = "Goats: 1-4",
    Assets04d_2 = "Goats: 5-9",
    Assets04d_3 = "Goats: 10+",
    Assets04e_1 = "Sheep: 1-4",
    Assets04e_2 = "Sheep: 5-9",
    Assets04e_3 = "Sheep: 10+",
    Assets04f_1 = "Barnyard animals: 1-4",
    Assets04f_2 = "Barnyard animals: 5-9",
    Assets04f_3 = "Barnyard animals: 10+",
    Assets04g_1 = "Pigs: 1-4",
    Assets04g_2 = "Pigs: 5-9",
    Assets04g_3 = "Pigs: 10+"
  )
##drop original Asset04 variables
main <- main %>%
  select(-c(Assets04a, Assets04b, Assets04c, Assets04d, Assets04e, Assets04f, Assets04g, Assets03))

#recode all other asset variables and bank account access into 0/1
main <- main %>%
  mutate(across(
    c(HH_30a, starts_with("Assets01"), starts_with("Assets02")),
    ~ ifelse(. == 1, 1, ifelse(. %in% c(98, 99), NA, 0)) %>% replace_na(0)
  ))


#save main
write.csv(main, "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main.csv", row.names = FALSE)
