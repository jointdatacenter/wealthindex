#####TESTS


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
