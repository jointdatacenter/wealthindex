main <- read.csv("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main.csv")

main <- main %>%
  mutate(
    urban_bin = case_when(
      Intro_09 == 3 ~ 1, 
      TRUE ~ 0))

table(main$urban_bin)


#select all variables for final dataset
main_WI <- main %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    urban_bin,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    agricultural_land_ha,
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
    
    #Assets
    starts_with("Assets01"),
    starts_with("Assets02"),
    starts_with("Assets04")
  )


main_WI_urban <- main %>%
  filter(urban_bin == 1) %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    urban_bin,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    agricultural_land_ha,
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
    
    #Assets
    starts_with("Assets01"),
    starts_with("Assets02"),
    starts_with("Assets04")
  )


main_WI_rural <- main %>%
  filter(urban_bin == 0) %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    urban_bin,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    agricultural_land_ha,
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
    
    #Assets
    starts_with("Assets01"),
    starts_with("Assets02"),
    starts_with("Assets04")
  )




print(main_WI)
str(main_WI)


#remove variables with zero variation
zero_sd_vars_main <- sapply(main_WI[ , 7:ncol(main_WI)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_main <- names(main_WI)[which(zero_sd_vars_main) + 6]  # +5 to account for starting at column 5
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



#PCA
#calc combined PCA
main_pca <- psych::principal(
  main_WI[, 7:ncol(main_WI)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,                 # use covariance matrix (not correlation)
  cor = "mixed",
  scores = TRUE                 # return scores
)



# Run PCA for urban dataset
main_pca_urban <- psych::principal(
  main_WI_urban[, 7:ncol(main_WI_urban)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,
  cor = "mixed",
  scores = TRUE                 # return scores
)

# Run PCA for rural dataset
main_pca_rural <- psych::principal(
  main_WI_rural[, 7:ncol(main_WI_rural)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,                 # use covariance matrix (not correlation)
  cor = "mixed",
  scores = TRUE                 # return scores
)



#Merge pca scores into main dataset
main_WI$comscore <- main_pca$scores[,1]
main_WI$urbscore <- NA
main_WI$rurscore <- NA


main_WI$urbscore[main_WI$urban_bin == 1] <- main_pca_urban$scores[, 1]
main_WI$rurscore[main_WI$urban_bin == 0] <- main_pca_rural$scores[, 1]




#Run Separate regressions to link urban/rural PCA scores to common pca score
# Urban regression: comscore ~ urbscore
urban_model <- lm(comscore ~ urbscore, data = main_WI, subset = urban_bin == 1)

# Rural regression: comscore ~ rurscore
rural_model <- lm(comscore ~ rurscore, data = main_WI, subset = urban_bin == 0)

# Extract coefficients
urb_const <- coef(urban_model)[1]
urb_coeff <- coef(urban_model)[2]

rur_const <- coef(rural_model)[1]
rur_coeff <- coef(rural_model)[2]


#Construct Combined Wealth index (combscore)
main_WI$combscore <- NA
main_WI$combscore[main_WI$urban_bin == 1] <- urb_const + urb_coeff * main_WI$urbscore[main_WI$urban_bin == 1]
main_WI$combscore[main_WI$urban_bin == 0] <- rur_const + rur_coeff * main_WI$rurscore[main_WI$urban_bin == 0]



#Combined/national quintiles
main_WI <- main_WI %>%
  filter(!is.na(combscore)) %>%
  mutate(
    q_combscore = ntile(combscore, 5),
    q_combscore = factor(q_combscore, levels = 1:5,
                         labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  )

#Common/Join PCA Quintiles
main_WI <- main_WI %>%
  filter(!is.na(comscore)) %>%
  mutate(
    wealth_quintile = ntile(comscore, 5),
    wealth_quintile = factor(wealth_quintile, levels = 1:5, 
                             labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  )

main_WI<- main_WI[!is.na(main_WI$combscore), ]
main_pca_SSD <- main_pca





#using joint calculation & comparing refugees in north and south and host community
main_WI %>%
  filter(group %in% c("Refugees South", "Refugees North", "Host Community North"),
         !is.na(wealth_quintile)) %>%
  mutate(wealth_quintile = fct_rev(wealth_quintile)) %>%  # Reverse order
  ggplot(aes(x = group, fill = wealth_quintile)) +
  geom_bar(position = "fill") +
  xlab("Group") +
  ylab("Percentage") +
  ggtitle("Wealth Distribution FDS South Sudan") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +  # Reverse legend
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_flip()


ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/quintiles_SSD.png", width = 6, height = 4, dpi = 300)



#More Graphs: 
#RUNNING URBAN/RURAL SEPARATELY
main_WI %>%
  filter(group %in% c("Refugees South", "Refugees North", "Host Community North"),
         !is.na(q_combscore)) %>%
  mutate(q_combscore = fct_rev(q_combscore)) %>%  # Reverse order
  ggplot(aes(x = group, fill = q_combscore)) +
  geom_bar(position = "fill") +
  xlab("Group") +
  ylab("Percentage") +
  ggtitle("Wealth Distribution by Group") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +  # Use UNHCR color palette
  theme_unhcr() +
  coord_flip()


main_WI %>%
  filter(Intro_07_1 %in% c(1, 3), !is.na(q_combscore)) %>%
  mutate(
    population_group = case_when(
      Intro_07_1 == 1 ~ "Refugees",
      Intro_07_1 == 3 ~ "Host Community"
    )
  ) %>%
  mutate(q_combscore = fct_rev(q_combscore)) %>%  # Reverse order
  ggplot(aes(x = population_group, fill = q_combscore)) +
  geom_bar(position = "fill") +
  xlab("Population Group") +
  ylab("Percentage") +
  ggtitle("Wealth Distribution: Refugees vs. Host Community") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +  # Use UNHCR color palette
  theme_unhcr() +
  coord_flip()

