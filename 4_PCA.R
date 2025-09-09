main <- read.csv("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main.csv")

main <- main %>%
  mutate(
    incamp_bin = case_when(
      Intro_08 == 1 ~ 1, 
      TRUE ~ 0))

table(main$incamp_bin)



#select all variables for final dataset
main_WI <- main %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    incamp_bin,
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


main_WI_incamp <- main %>%
  filter(incamp_bin == 1) %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    incamp_bin,
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


main_WI_outofcamp <- main %>%
  filter(incamp_bin == 0) %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    incamp_bin,
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
zero_sd_vars_main <- sapply(main_WI[ , 9:ncol(main_WI)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_main <- names(main_WI)[which(zero_sd_vars_main) + 8]  # +5 to account for starting at column 5
main_WI <- main_WI[ , !(names(main_WI) %in% removed_vars_main)]
cat("Removed variables from main:", removed_vars_main, "\n")


#PCA
#calc combined PCA
main_pca <- psych::principal(
  main_WI[, 9:ncol(main_WI)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,                 # use covariance matrix (not correlation)
  cor = "mixed",
  scores = TRUE                 # return scores
)




#Merge pca scores into main dataset
main_WI$comscore <- main_pca$scores[,1]

#Inspect loadings 
main_pca$loadings


#Common/Join PCA Quintiles
main_WI <- main_WI %>%
  filter(!is.na(comscore)) %>%
  mutate(
    wealth_quintile = ntile(comscore, 5),
    wealth_quintile = factor(wealth_quintile, levels = 1:5, 
                             labels = c("Poorest", "Quintile 2", "Quintile 3", "Quintile 4", "Richest"))
  )

write.csv(main_WI, "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main_WI.csv", row.names = FALSE)


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

main_WI %>%
  filter(group %in% c("Refugees South", "Refugees North", "Host Community North"),
         !is.na(wealth_quintile)) %>%
  mutate(wealth_quintile = fct_rev(wealth_quintile)) %>%
  group_by(group, wealth_quintile) %>%         # calculate counts
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(pct = n / sum(n)) %>%                 # compute percentages
  ggplot(aes(x = group, y = pct, fill = wealth_quintile)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    data = . %>% filter(wealth_quintile == "Poorest"),
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3.5
  ) +
  xlab("Group") +
  ylab("Percentage") +
  ggtitle("Wealth Distribution FDS South Sudan") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip()


main_WI %>%
  filter(group %in% c("Refugees South", "Refugees North", "Host Community North"),
         !is.na(wealth_quintile), 
         is.na(DH_11_years) | DH_11_years <= 2018) %>%
  mutate(wealth_quintile = fct_rev(wealth_quintile)) %>%
  group_by(group, wealth_quintile) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = group, y = pct, fill = wealth_quintile)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    data = . %>% filter(wealth_quintile == "Poorest"),
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3.5
  ) +
  xlab("Group") +
  ylab("Percentage") +
  ggtitle("Wealth Distribution FDS South Sudan") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip()



ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/quintiles_SSD.png", width = 6, height = 4, dpi = 300)


#Graph for only households that have been in SSD longer (proxy: year HH left place of origin, use before 2018)
main_WI %>%
  filter(DH_11_years >= 2000, DH_11_years <= 2025) %>%
  ggplot(aes(x = DH_11_years)) +
  geom_histogram(
    binwidth = 1,             # similar to breaks = 25 over 2000–2025
    fill = "lightblue",
    color = "white"
  ) +
  labs(
    x = "",
    y = "Households",
    title = "Refugee households in South Sudan FDS - Year of displacement" ,
    caption = "Note: Excludes 7 households with displacement before the year 2000."
  ) +
  theme_unhcr()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "white"))

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/displacementyear.png", width = 6, height = 4, dpi = 300)




#quintile distribution if excluding the most recently displaced households: DH_11_years <= 2018
#only excludes 69 HH
main_WI %>%
  filter(group %in% c("Refugees South", "Refugees North", "Host Community North"),
         !is.na(wealth_quintile), 
         is.na(DH_11_years) | DH_11_years <= 2018) %>%
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

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/quintiles_SSD_exclnewref.png", width = 6, height = 4, dpi = 300)



# filter out outliers
main_WI %>%
  #filter(comscore >= -2) %>%
  filter(comscore >= -0.2 & comscore < 0.4) %>%
  ggplot(aes(x = group, y = comscore, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA)+
  scale_fill_manual(
    values = c(
      "Refugees South" = "#0072BC", 
      "Refugees North" = "#8EBEFF", 
      "Host Community North" = "#18375F"
    )
  ) +
 scale_x_discrete(limits = rev) + 
  labs(
    x = "",
    y = "Wealth Index",
    title = "Wealth Index Scores Distribution"
  ) +
  theme_unhcr() +
  theme(
    legend.position = "none"
  )

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/wi_distribution.png", width = 6, height = 4, dpi = 300)










#Sensitivity testing - calculation with incamp and outofcamp separately: 
zero_sd_vars_incamp <- sapply(main_WI_incamp[ , 9:ncol(main_WI_incamp)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_incamp <- names(main_WI_incamp)[which(zero_sd_vars_incamp) + 8]  # +6 to account for starting at column 5
main_WI_incamp <- main_WI_incamp[ , !(names(main_WI_incamp) %in% removed_vars_incamp)]
cat("Removed variables from incamp:", removed_vars_incamp, "\n")


zero_sd_vars_outofcamp <- sapply(main_WI_outofcamp[ , 9:ncol(main_WI_outofcamp)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_outofcamp <- names(main_WI_outofcamp)[which(zero_sd_vars_outofcamp) + 8]  # +6 to account for starting at column 5
main_WI_outofcamp <- main_WI_outofcamp[ , !(names(main_WI_outofcamp) %in% removed_vars_outofcamp)]
cat("Removed variables from outofcamp:", removed_vars_outofcamp, "\n")



# Run PCA for incamp dataset
main_pca_incamp <- psych::principal(
  main_WI_incamp[, 9:ncol(main_WI_incamp)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,
  cor = "mixed",
  scores = TRUE                 # return scores
)

# Run PCA for outofcamp dataset
main_pca_outofcamp <- psych::principal(
  main_WI_outofcamp[, 9:ncol(main_WI_outofcamp)],        # select only binary variables
  rotate = "varimax",           # optional rotation
  nfactors = 1,                 # number of components
  covar = TRUE,                 # use covariance matrix (not correlation)
  cor = "mixed",
  scores = TRUE                 # return scores
)






#Merge pca scores into main dataset
main_WI$urbscore <- NA
main_WI$rurscore <- NA


main_WI$urbscore[main_WI$incamp_bin == 1] <- main_pca_incamp$scores[, 1]
main_WI$rurscore[main_WI$incamp_bin == 0] <- main_pca_outofcamp$scores[, 1]




#Run Separate regressions to link incamp/outofcamp PCA scores to common pca score
# incamp regression: comscore ~ urbscore
incamp_model <- lm(comscore ~ urbscore, data = main_WI, subset = incamp_bin == 1)

# outofcamp regression: comscore ~ rurscore
outofcamp_model <- lm(comscore ~ rurscore, data = main_WI, subset = incamp_bin == 0)

# Extract coefficients
urb_const <- coef(incamp_model)[1]
urb_coeff <- coef(incamp_model)[2]

rur_const <- coef(outofcamp_model)[1]
rur_coeff <- coef(outofcamp_model)[2]


#Construct Combined Wealth index (combscore)
main_WI$combscore <- NA
main_WI$combscore[main_WI$incamp_bin == 1] <- urb_const + urb_coeff * main_WI$urbscore[main_WI$incamp_bin == 1]
main_WI$combscore[main_WI$incamp_bin == 0] <- rur_const + rur_coeff * main_WI$rurscore[main_WI$incamp_bin == 0]



#Combined/national quintiles
main_WI <- main_WI %>%
  filter(!is.na(combscore)) %>%
  mutate(
    q_combscore = ntile(combscore, 5),
    q_combscore = factor(q_combscore, levels = 1:5,
                         labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  )




#Graph if running PCA together
main_WI %>%
  filter(Intro_07_1 %in% c(1, 3), !is.na(wealth_quintile)) %>%
  mutate(
    population_group = case_when(
      Intro_07_1 == 1 ~ "Refugees",
      Intro_07_1 == 3 ~ "Host Community"
    )
  ) %>%
  mutate(wealth_quintile = fct_rev(wealth_quintile)) %>%  # Reverse order
  ggplot(aes(x = population_group, fill = wealth_quintile)) +
  geom_bar(position = "fill") +
  ggtitle("Wealth Distribution FDS South Sudan") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +  # Reverse legend
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_flip()















#Graphs if running incamp and outofcamp separately
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
                                 
#Inspect loadings
print(main_pca$loadings, cutoff = 0, sort=T)
#print(main_pca_incamp$loadings, cutoff = 0, sort=F)
#print(main_pca_outofcamp$loadings, cutoff = 0, sort=F)


                                 


