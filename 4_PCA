main <- read.csv("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main.csv")

#select all variables for final dataset
main_WI <- main %>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    Improved_dw,             # Improved drinking water
#    Improved_dw_final,      # Improved drinking water, <30 min
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
#    starts_with("Assets")
    Assets01, #Radio
    Assets01aa,	#Satellite dish
    Assets01b,	#Television
    Assets01bb,	#Seeds for planting
    Assets01c,	#Table phone
    Assets01cc,	#Plough (oxen)
    Assets01d,	#Refrigerator
    Assets01dd,	#Diesel driven irrigation system
    Assets01e,	#CD/DVD player
    Assets01ee,	#Hoe
    Assets01f,	#Fan
    Assets01ff,	#Maloda
    Assets01g,	#Air conditioner
    Assets01gg,	#Spade
    Assets01h,	#Stove/kanun (traditional)
    Assets01hh,	#Axe
    Assets01i,	#Gas/kerosene cooker
    Assets01ii,	#Panga
    Assets01j,	#Grinding stone
    Assets01jj,	#Spraying cans
    Assets01k,	#Mixer/blender
    Assets01kk,	#Harrow
    Assets01l,	#Generator
    Assets01ll,	#Ridger
    Assets01m,	#Solar panel (any size)
    Assets01mm,	#Cultivators
    Assets01n,	#Wall clock
    Assets01nn,	#Spreaders
    Assets01o,	#Modem/router
    Assets01q,	#Water pump
    Assets01r,	#Chair
    Assets01s_a,#Mattress
    Assets01s_b,#Bed
    Assets01t,  #Sofa
    Assets01u,	#Room-cooler
    Assets01v,	#Washing machine
    Assets01w,	#Camera, other than phone
    Assets01x,	#Sewing machine
    Assets01y,	#Tools for lighting (solar lamp, torch etc.)
    Assets01z,	#Freezer

  #Working assets
    Assets02a,	#Watch
    Assets02b,	#Mobile phone
    Assets02c,	#Smart phone
    Assets02d,  #Bicycle
    Assets02e,  #Motorcycle or motor scooter
    Assets02f_a,#Car
    Assets02f_b,#Truck
    Assets02g,  #Boat without motor/canoe
    Assets02j,  #Tractor
    Assets02l,  #Laptop/computer
    Assets02n,  #Pair of shoes

  #Animals
     Assets04a_1, #Cows/bulls: 1-4
     Assets04a_2, #Cows/bulls: 5-9
     Assets04a_3, #Cows/bulls: 10+
     Assets04b_1, #Other cattle: 1-4
     Assets04b_2, #Other cattle: 5-9
     Assets04b_3, #Other cattle: 10+
     Assets04c_1, #Horses/donkeys/mules: 1-4
     Assets04c_2, #Horses/donkeys/mules: 5-9
     Assets04c_3, #Horses/donkeys/mules: 10+
     Assets04d_1, #Goats: 1-4
     Assets04d_2, #Goats: 5-9
     Assets04d_3, #Goats: 10+
     Assets04e_1, #Sheep: 1-4
     Assets04e_2, #Sheep: 5-9
     Assets04e_3, #Sheep: 10+
     Assets04f_1, #Barnyard animals: 1-4
     Assets04f_2, #Barnyard animals: 5-9
     Assets04f_3, #Barnyard animals: 10+
     Assets04g_1, #Pigs: 1-4
     Assets04g_2, #Pigs: 5-9
     Assets04g_3  #Pigs: 10+
  )



print(main_WI)
str(main_WI)


#remove variables with zero variation
zero_sd_vars_main <- sapply(main_WI[ , 6:ncol(main_WI)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_main <- names(main_WI)[which(zero_sd_vars_main) + 5]  # +5 to account for starting at column 5
main_WI <- main_WI[ , !(names(main_WI) %in% removed_vars_main)]
cat("Removed variables from main:", removed_vars_main, "\n")



#PCA
pca_main <- principal(main_WI[6:ncol(main_WI)], rotate = "varimax", cor = "mixed") 
print(pca_main$loadings, cutoff = 0, sort=F)
print(pca_main$scores)

kmo_result <- KMO(main_WI[6:ncol(main_WI)])
print(kmo_result)

main_WI$scores <- pca_main$scores


#Create quintiles based on wealth scores
main_WI <- main_WI %>%
  filter(!is.na(group), !is.na(scores)) %>%
  mutate(
    wealth_quintile = ntile(scores, 5),
    wealth_quintile = factor(wealth_quintile, levels = 1:5, 
                             labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  )

# Step 3: Summarize for plotting
wi_data <- main_WI %>%
  group_by(group, wealth_quintile) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# Ensure correct ordering
wi_data$wealth_quintile <- factor(
  table_data$wealth_quintile,
  levels = c( "Richest", "Fourth", "Middle", "Second", "Poorest")
)

# Step 4: Plot
ggplot(wi_data, aes(x = share, y = group, fill = wealth_quintile)) +
  geom_col(position = "stack", width = 0.5) +  
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Poorest" = "#666666",
      "Second" = "#00B398",
      "Middle" = "#8EBEFF",
      "Fourth" = "#0072BC",
      "Richest" = "#18375F"
    )
    ,   guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Share of Households",
    y = "",
    fill = "Wealth Quintile",
    title = "Wealth Quintile Distribution by Group"
  ) +
  theme_unhcr()

# Step 5: Save plot
ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/quintiles.png", width = 6, height = 4, dpi = 300)
