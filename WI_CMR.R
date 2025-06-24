#Cameroon

main <- readRDS("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main_anon_cameroon.rds")



#Data Cleaning

main$Improved_san <- main$type_san_facility
main$Clean_cookingfuel <- main$RBM20802


table(main$Improved_dw, useNA = "ifany") #drinking water from an improved source
table(main$Improved_dw_final, useNA = "ifany") # improved drinking water, less than 30 minute away
table(main$electricity, useNA = "ifany") # hh has electricity
table(main$shared_san_facility, useNA = "ifany") #Do you share this toilet facility with others who are not members of your household?
table(main$Improved_san, useNA = "ifany") #What kind of toilet facility do members of your household use most of the time?
table(main$crowding_cat, useNA = "ifany") # #Crowding index - overcrowded when more than 3 persons share one room to sleep
table(main$Clean_cookingfuel, useNA = "ifany") # clean cooking - primary reliance on clean (cooking) fuels and technology

main$shared_san_facility[is.na(main$shared_san_facility)] <- 0

table(main$Land01, main$Intro_07)
#House and Land ownership
main <- main %>%
  mutate(House01 = ifelse(is.na(House01) & Land02 == 1, 1, House01))%>%
  mutate(across(
    c(Land01, House01),
    ~ ifelse(. == 1, 1, ifelse(. %in% c(98, 99), NA, 0)) %>% replace_na(0)
  ))


#Housing
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
  )%>%
  select(-c(Assets04a, Assets04b, Assets04c, Assets04d, Assets04e, Assets04f, Assets04g, Assets03))

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
    Assets04f_1 = "Chicken or other poultry: 1-4",
    Assets04f_2 = "Chicken or other poultry: 5-9",
    Assets04f_3 = "Chicken or other poultry: 10+",
    Assets04g_1 = "Pigs: 1-4",
    Assets04g_2 = "Pigs: 5-9",
    Assets04g_3 = "Pigs: 10+"
  )



#recode all other asset variables and bank account access into 0/1
main <- main %>%
  mutate(across(
    c(HH_30a,crowding_cat, starts_with("Assets01"), starts_with("Assets02")),
    ~ ifelse(. == 1, 1, ifelse(. %in% c(98, 99), NA, 0)) %>% replace_na(0)
  ))




table(main$samp_strat)

main <- main %>%
  mutate( # country of origin from ID_00 and ID_00_specify
    urref_bin = case_when(
      samp_strat == "Urban refugees (Yaoundé & Douala)" ~ 1, ##ensure to adjust here the country code (where FDS took place)
      TRUE ~ 0))

table(main$urref_bin)


# Calculate the percentage of each variable when the value is 1, grouped by urref_bin
percentages_by_urref_bin <- main %>%
  group_by(urref_bin) %>%
  summarise(across(everything(), ~ mean(.x == 1, na.rm = TRUE) * 100))

# Print the percentages
print(percentages_by_urref_bin)




main_WI <- main %>%
  select(
    # Core service access variables
    uuid,
    urref_bin,
    wgh_strata_spec,
    samp_strat,
    Intro_09,
    Intro_07,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    House01,
    HH_30a,
    
    # Housing materials
    HH02_finished,
    HH03_finished,
    HH06_finished,
    HH02_rudimentary,
    HH03_rudimentary,
    HH06_rudimentary,
    HH02_natural, 
    HH03_natural,
    HH06_natural,
    
    
    starts_with("Assets01"), #household assets
    starts_with("Assets02"), #working assets
    starts_with("Assets04") #livestock
  )

###Now that we have binary variables, we can calculate the wealth index

main_WI_urban <- main %>%
  filter(urref_bin == 1) %>%
  select(
    # Core service access variables
    uuid,
    urref_bin,
    wgh_strata_spec,
    samp_strat,
    Intro_09,
    Intro_07,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    House01,
    HH_30a,
    
    # Housing materials
    HH02_finished,
    HH03_finished,
    HH06_finished,
    HH02_rudimentary,
    HH03_rudimentary,
    HH06_rudimentary,
    HH02_natural, 
    HH03_natural,
    HH06_natural,
    
    
    starts_with("Assets01"), #household assets
    starts_with("Assets02"), #working assets
    starts_with("Assets04") #livestock
  )


###Now that we have binary variables, we can calculate the wealth index

main_WI_rural <- main %>%
  filter(urref_bin == 0) %>%
  select(
    # Core service access variables
    uuid,
    urref_bin,
    wgh_strata_spec,
    samp_strat,
    Intro_09,
    Intro_07,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    House01,
    HH_30a,
    
    # Housing materials
    HH02_finished,
    HH03_finished,
    HH06_finished,
    HH02_rudimentary,
    HH03_rudimentary,
    HH06_rudimentary,
    HH02_natural, 
    HH03_natural,
    HH06_natural,
    
    
    starts_with("Assets01"), #household assets
    starts_with("Assets02"), #working assets
    starts_with("Assets04") #livestock
  )


#remove variables with zero variation
zero_sd_vars_main <- sapply(main_WI[ , 7:ncol(main_WI)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_main <- names(main_WI)[which(zero_sd_vars_main) + 6]  # +6 to account for starting at column 5
main_WI <- main_WI[ , !(names(main_WI) %in% removed_vars_main)]
cat("Removed variables from main:", removed_vars_main, "\n")

zero_sd_vars_urban <- sapply(main_WI_urban[ , 7:ncol(main_WI_urban)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_urban <- names(main_WI_urban)[which(zero_sd_vars_urban) + 6]  # +6 to account for starting at column 5
main_WI_urban <- main_WI_urban[ , !(names(main_WI_urban) %in% removed_vars_urban)]
cat("Removed variables from urban:", removed_vars_urban, "\n")

zero_sd_vars_rural <- sapply(main_WI_rural[ , 7:ncol(main_WI_rural)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_rural <- names(main_WI_rural)[which(zero_sd_vars_rural) + 6]  # +6 to account for starting at column 5
main_WI_rural <- main_WI_rural[ , !(names(main_WI_rural) %in% removed_vars_rural)]
cat("Removed variables from rural:", removed_vars_rural, "\n")



###WI is calculated by using PCA (principal component analysis)
#calc combined PCA
main_pca <- psych::principal(
  main_WI[, 7:ncol(main_WI)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,                 # use covariance matrix (not correlation)
  cor = "tet",
  scores = TRUE                 # return scores
)



#using joint calculation & comparing refugees and host community
main_WI %>%
  filter(Intro_07 %in% c("Refugees", "Host Community"), !is.na(wealth_quintile)) %>%
  mutate(
    wealth_quintile = fct_rev(wealth_quintile),  # Reverse order of quintiles
    Intro_07 = factor(Intro_07, levels = c("Host Community", "Refugees"))  # Flip bar order
  ) %>%
  ggplot(aes(x = Intro_07, fill = wealth_quintile)) +
  geom_bar(position = "fill") +
  ggtitle("Wealth Distribution FDS Cameroon") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +
  theme_unhcr() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_flip()

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/quintiles_CMR.png", width = 6, height = 3, dpi = 300)

