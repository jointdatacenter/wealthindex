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
# Step 1: Identify asset variables
asset_vars <- grep("^Assets01|^Assets02", names(main), value = TRUE)

##asset tests urban vs.rural
main <- main %>%
  mutate(area_type = case_when(
    Intro_09 == 1 ~ "Rural",
    Intro_09 == 2 ~ "Peri-urban",
    Intro_09 == 3 ~ "Urban",
    TRUE ~ NA_character_
  ))

main_long <- main %>%
  select(all_of(asset_vars), area_type) %>%
  pivot_longer(cols = all_of(asset_vars), names_to = "asset", values_to = "owned") %>%
  filter(!is.na(owned))  # exclude missing values

asset_ownership_summary <- main_long %>%
  group_by(area_type, asset) %>%
  summarise(percent_owned = mean(owned == 1, na.rm = TRUE) * 100, .groups = "drop") %>%
  pivot_wider(names_from = area_type, values_from = percent_owned) %>%
  arrange(asset)

asset_ownership_summary <- asset_ownership_summary %>%
  mutate(across(where(is.numeric), round, 1))



main <- main %>%
  mutate(pop_group = case_when(
    Intro_07_1 == 1 ~ "Refugees",
    Intro_07_1 == 3 ~ "Host community",
    TRUE ~ NA_character_
  ))

main_long_group <- main %>%
  select(all_of(asset_vars), pop_group) %>%
  filter(!is.na(pop_group)) %>%  # exclude missing group info
  pivot_longer(cols = all_of(asset_vars), names_to = "asset", values_to = "owned") %>%
  filter(!is.na(owned))  # exclude missing asset values

asset_ownership_by_group <- main_long_group %>%
  group_by(pop_group, asset) %>%
  summarise(percent_owned = mean(owned == 1, na.rm = TRUE) * 100, .groups = "drop") %>%
  pivot_wider(names_from = pop_group, values_from = percent_owned) %>%
  arrange(asset)

asset_ownership_by_group <- asset_ownership_by_group %>%
  mutate(across(where(is.numeric), round, 1))
