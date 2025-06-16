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

#Descriptive figures

#import main 

main <- read.csv("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/main.csv")


#Label variables
labels <- read_excel("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/var_labels.xlsx")
labels_vector <- setNames(labels$label, labels$variable)

for (var in names(labels_vector)) {
  if (var %in% names(main))
    var_label(main[[var]]) <- labels_vector[[var]]
}

main <- main %>%
  set_variable_labels(Improved_dw="Access to improved drinking water source",
                      Improved_san="Access to improved sanitation",
                      Clean_cookingfuel="Clean cooking fuel",
                      shared_san_facility="Sharing of sanitation facilities",
                      electricity = "Access to electricity",
                      crowding_cat = "Crowding")

#create dummies
main <- dummy_cols(main, select_columns = c("BD01","SAN01", "HC04", "HH02", "HH03", "HH06"), remove_selected_columns = T)
main <- main%>%
  select(-c(HH02_NA, HH03_NA, HH06_NA, HC04_NA))

#label dummy variables
dummy_labels <- read_excel("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/value_labels.xlsx")
dummy_vector <- setNames(dummy_labels$dummy_label, dummy_labels$dummy_var)

for (var in names(dummy_vector)) {
  if (var %in% names(main))
    var_label(main[[var]]) <- dummy_vector[[var]]
}


main_fig <- main%>%
  select(
    # Core service access variables
    ID,
    wgh_samp_resc_str,
    Intro_07_1,
    Region,
    group,
    Improved_dw,             # Improved drinking water
    #    Improved_dw_final,  # Improved drinking water, <30 min
    electricity,             # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    Land01,
    agricultural_land_ha,
    House01,
    HH_30a,
    #Dummies
    starts_with("BD01_"),
    starts_with("SAN01_"),
    starts_with("HC04_"),
    # Housing materials
    starts_with("HH02_"),
    starts_with("HH03_"),
    starts_with("HH06_"),
    #Assets
     starts_with("Assets")
    )%>%
  select(-c(HH02_finished,
            HH03_finished,
            HH06_finished,
            HH02_rudimentary,
            HH03_rudimentary,
            HH06_rudimentary,
            HH02_natural,
            HH03_natural,
            HH06_natural,
            HH02_WI,
            HH03_WI,
            HH06_WI))

#separate into groups for figures:
#create subsamples of population, keep only relevant variables
main_fig <- main_fig %>%
  mutate(Intro_07_1 = as.character(Intro_07_1))


refsouth_before <- main_fig %>%
  filter(Intro_07_1 == 1 & Region == "South")
refsouth <- refsouth_before %>% drop_na()

refnorth_before <- main_fig %>%
  filter(Intro_07_1 == 1 & Region == "North")
refnorth <- refnorth_before %>% drop_na()

hostnorth_before <- main_fig %>%
  filter(Intro_07_1 == 3 & Region == "North")
hostnorth <- hostnorth_before %>% drop_na()




#FIGURES
##1 number of observations and dropped observations per population group
# Create a list of population groups with before/after data frames
groups <- list(
  "Refugees South" = list(before = refsouth_before, after = refsouth),
  "Refugees North" = list(before = refnorth_before, after = refnorth),
  "Host community North" = list(before = hostnorth_before, after = hostnorth)
)

# Build the summary data frame
plot_data <- bind_rows(lapply(names(groups), function(name) {
  before <- groups[[name]]$before
  after <- groups[[name]]$after
  data.frame(
    Population_Group = name,
    Observations = nrow(after),
    Dropped = nrow(before) - nrow(after),
    Total = nrow(before)
  )
}))

# Calculate cumulative position for dropped label
plot_data <- plot_data %>%
  mutate(Cumulative = Observations + Dropped + 45)

# Reshape for plotting
plot_data_long <- plot_data %>%
  pivot_longer(cols = c("Observations", "Dropped"), names_to = "Type", values_to = "Count")

# Define colors
colors <- c("Observations" = "#0072BC", "Dropped" = "#8EBEFF")

# Plot
ggplot(plot_data_long, aes(x = Population_Group, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(data = filter(plot_data_long, Type == "Observations"),
            aes(label = Count), position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  geom_text(data = filter(plot_data_long, Type == "Dropped"),
            aes(label = Count, y = plot_data$Cumulative),
            color = "#666666", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = 0) +
  labs(title = "Remaining Observations by Population Group") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_unhcr() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_line(color = "grey", size = 0.5)
  )
ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/observations.png", width = 6, height = 4, dpi = 300)

##2 Single variable figures - access to electricity, access to bank account, Land ownership, House ownership
#function to prepare data and generate a standardized stacked bar plot
create_access_plot <- function(varname, df_list, weightname, labels, title, save_path, access_label_colors = c("Access" = "white", "No_Access" = "black")) {
  # Compute weighted means
  access_data <- data.frame(
    Population_Group = labels,
    Access = mapply(function(df) weighted.mean(df[[varname]], df[[weightname]], na.rm = TRUE), df_list)
  ) %>%
    mutate(No_Access = 1 - Access) %>%
    pivot_longer(cols = c("Access", "No_Access"), names_to = "Access_Type", values_to = "Percentage") %>%
    mutate(
      Access_Type = factor(Access_Type, levels = c("No_Access", "Access"))
    )
  
  # Define colors
  fill_colors <- c("Access" = "#0072BC", "No_Access" = "#8EBEFF")
  
  # Plot
  p <- ggplot(access_data, aes(x = Population_Group, y = Percentage * 100, fill = Access_Type)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(
      label = ifelse(Percentage * 100 > 1, paste0(round(Percentage * 100, 1)), ""),
      color = Access_Type
    ), position = position_stack(vjust = 0.5), size = 4, show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = fill_colors, labels = c("No", "Yes")) +
    scale_color_manual(values = access_label_colors) +
    scale_y_continuous(breaks = 0) +
    labs(title = title, fill = "Access Type") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_unhcr() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.major.x = element_line(color = "grey", size = 0.5)
    )
  
  # Save plot
  ggsave(save_path, plot = p, width = 5, height = 4, dpi = 300)
}

# Define inputs
df_list <- list(refsouth, refnorth, hostnorth)
labels <- c("Refugees South", "Refugees North", "Host community North")

# Generate plots
create_access_plot("electricity", df_list, "wgh_samp_resc_str", labels, "Access to Electricity (%)",
                   "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/electricity.png")

create_access_plot("HH_30a", df_list, "wgh_samp_resc_str", labels, "Access to Bank account (%)",
                   "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/bankaccount.png",
                   access_label_colors = c("Access" = "black", "No_Access" = "black"))

create_access_plot("Land01", df_list, "wgh_samp_resc_str", labels, "Land ownership (%)",
                   "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/landownership.png")

create_access_plot("House01", df_list, "wgh_samp_resc_str", labels, "House ownership (%)",
                   "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/houseownership.png")


##3 Categorical variables - Main source of drinking water, type of toilet facility, sharing of toilet facility, type of cooking fuel 
# Function to calculate weighted percentage for binary variables (1 = selected)
calculate_percentage <- function(df, variables) {
  sapply(variables, function(var) {
    if (var %in% names(df)) {
      weighted.mean(df[[var]] == 1, df$wgh_samp_resc_str, na.rm = TRUE) * 100
    } else {
      0
    }
  })
}

# Common color palette
colors <- c("Refugees South" = "#0072BC", "Refugees North" = "#8EBEFF", "Host community North" = "#18375F")

# Reusable plot function
plot_categorical_variable <- function(data_long, title, caption, filename) {
  ggplot(data_long, aes(x = Variable, y = Percentage, fill = Population_Group)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.8) +
    geom_text(aes(label = sprintf("%.1f", Percentage)), 
              vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
    scale_fill_manual(values = colors) +
    labs(title = title,
         x = NULL, y = NULL, fill = "Population Group", caption = caption) +
    theme_unhcr() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_line(color = "white", size = 0.5))
  
  ggsave(filename, width = 8, height = 6, dpi = 300)
}


#DRINKINGWATER
# Variables
vars_bd01 <- unique(c(grep("^BD01", names(refsouth), value = TRUE),
                      grep("^BD01", names(refnorth), value = TRUE),
                      grep("^BD01", names(hostnorth), value = TRUE)))

labels_bd01 <- sapply(vars_bd01, function(var) {
  label <- attr(refsouth[[var]], "label") %||%
    attr(refnorth[[var]], "label") %||%
    attr(hostnorth[[var]], "label")
  sub("Main source of drinking water: ", "", label)
})

# Data
df_bd01 <- data.frame(
  Variable = labels_bd01,
  Refugees_South = calculate_percentage(refsouth, vars_bd01),
  Refugees_North = calculate_percentage(refnorth, vars_bd01),
  Host_community_North = calculate_percentage(hostnorth, vars_bd01)
) %>% 
  filter(if_any(starts_with("Refugees_") | starts_with("Host_"), ~ . > 2)) %>%
  pivot_longer(-Variable, names_to = "Population_Group", values_to = "Percentage") %>%
  mutate(Population_Group = factor(Population_Group,
                                   levels = c("Refugees_South", "Refugees_North", "Host_community_North"),
                                   labels = c("Refugees South", "Refugees North", "Host community North")),
         Variable = fct_reorder(Variable, -Percentage, .fun = sum))

# Plot
plot_categorical_variable(df_bd01, 
                          title = "Main Source of Drinking Water (%)",
                          caption = "Note: Includes sources used by ≥2% in at least one group. Low-frequency categories omitted (Piped into dwelling, Piped to neighbour, Protected well, Protected spring, Rainwater, Tanker-truck, Cart with small tank, Bottled water, Sachet water).",
                          filename = "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/Drinkingwater.png"
)

#SAN01 - Type of toilet facility
vars_san01 <- unique(c(grep("^SAN01_[0-9]{2}$", names(refsouth), value = TRUE),
                       grep("^SAN01_[0-9]{2}$", names(refnorth), value = TRUE),
                       grep("^SAN01_[0-9]{2}$", names(hostnorth), value = TRUE)))

labels_san01 <- sapply(vars_san01, function(var) {
  label <- attr(refsouth[[var]], "label") %||%
    attr(refnorth[[var]], "label") %||%
    attr(hostnorth[[var]], "label")
  sub("Type of toilet facility: ", "", label)
})

df_san01 <- data.frame(
  Variable = labels_san01,
  Refugees_South = calculate_percentage(refsouth, vars_san01),
  Refugees_North = calculate_percentage(refnorth, vars_san01),
  Host_community_North = calculate_percentage(hostnorth, vars_san01)
) %>%
  filter(if_any(starts_with("Refugees_") | starts_with("Host_"), ~ . > 2)) %>%
  pivot_longer(-Variable, names_to = "Population_Group", values_to = "Percentage") %>%
  mutate(Population_Group = factor(Population_Group,
                                   levels = c("Refugees_South", "Refugees_North", "Host_community_North"),
                                   labels = c("Refugees South", "Refugees North", "Host community North")),
         Variable = fct_reorder(Variable, -Percentage, .fun = sum))

plot_categorical_variable(df_san01, 
                          title = "Type of Toilet Facility (%)",
                          caption = "Note: Facilities used by ≥2% of households in at least one group shown. Low frequency categories omitted (Flush to piped sewer system, Flush to septic tank, Flush to open drain, Flush to dk where, Composting toilet, Bucket, Hanging toilet/hanging latrine). Values are proportion of all households reporting to use these toilet facility type, no matter if shared or not.",
                          filename = "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/sanitation.png"
)


#SAN01_shared - Sharing of toilet facilities


#HC04 - Type of cooking fuel
vars_hc04 <- unique(c(grep("^HC04", names(refsouth), value = TRUE),
                      grep("^HC04", names(refnorth), value = TRUE),
                      grep("^HC04", names(hostnorth), value = TRUE)))

labels_hc04 <- sapply(vars_hc04, function(var) {
  label <- attr(refsouth[[var]], "label") %||%
    attr(refnorth[[var]], "label") %||%
    attr(hostnorth[[var]], "label")
  sub("Type of cooking fuel: ", "", label)
})

df_hc04 <- data.frame(
  Variable = labels_hc04,
  Refugees_South = calculate_percentage(refsouth, vars_hc04),
  Refugees_North = calculate_percentage(refnorth, vars_hc04),
  Host_community_North = calculate_percentage(hostnorth, vars_hc04)
) %>%
  filter(if_any(starts_with("Refugees_") | starts_with("Host_"), ~ . > 2)) %>%
  pivot_longer(-Variable, names_to = "Population_Group", values_to = "Percentage") %>%
  mutate(Population_Group = factor(Population_Group,
                                   levels = c("Refugees_South", "Refugees_North", "Host_community_North"),
                                   labels = c("Refugees South", "Refugees North", "Host community North")),
         Variable = fct_reorder(Variable, -Percentage, .fun = sum))

plot_categorical_variable(df_hc04, 
                          title = "Type of Cooking Fuel (%)",
                          caption = "Note: Fuels used by ≥2% of households in at least one group included.Low frequency categories omitted (Alcohol/ethanol,  Gasoline/diesel, Kerosene/paraffin, Coal/lignite briquettes/pellets, Animal waste/dung, Processed biomass pellets/briquettes, Garbage/plastic, Sawdust).",
                          filename = "C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/cookingfuel.png"
)

##4 Dwelling Materials

#HH02
variables_hh02 <- unique(c(grep("^HH02", names(refsouth), value = TRUE),
                           grep("^HH02", names(refnorth), value = TRUE),
                           grep("^HH02", names(hostnorth), value = TRUE)))

# Extract labels for the variables and remove the prefix
labels_hh02 <- sapply(variables_hh02, function(var) {
  label <- attr(refsouth[[var]], "label")
  if (is.null(label)) label <- attr(refnorth[[var]], "label")
  if (is.null(label)) label <- attr(hostnorth[[var]], "label")
  sub("Main wall material: ", "", label)
})

# Calculate percentages for each dataset
percentages_south_hh02 <- calculate_percentage(refsouth, variables_hh02)
percentages_north_hh02 <- calculate_percentage(refnorth, variables_hh02)
percentages_host_hh02 <- calculate_percentage(hostnorth, variables_hh02)

# Create a data frame for plotting
hh02_data <- data.frame(
  Variable = labels_hh02,
  Refugees_South = percentages_south_hh02,
  Refugees_North = percentages_north_hh02,
  Host_community_North = percentages_host_hh02
)


# Filter out variables with all values <= 2%
hh02_data_filtered <- hh02_data %>% filter(if_any(c(Refugees_South, Refugees_North, Host_community_North), ~ . > 2))

# Convert data to long format for ggplot
hh02_data_long_filtered <- hh02_data_filtered %>% pivot_longer(cols = -Variable, names_to = "Population_Group", values_to = "Percentage")


hh02_data_long_filtered <- hh02_data_long_filtered %>%
  group_by(Variable) %>%
  mutate(Total_Percentage = sum(Percentage)) %>%
  ungroup()


hh02_data_long_filtered$Variable <- factor(hh02_data_long_filtered$Variable, levels = hh02_data_long_filtered %>%
                                             group_by(Variable) %>%
                                             dplyr::summarize(Total_Percentage = sum(Percentage)) %>%
                                             arrange(desc(Total_Percentage)) %>%
                                             pull(Variable))


# Ensure the columns are ordered and renamed
hh02_data_long_filtered$Population_Group <- factor(hh02_data_long_filtered$Population_Group, levels = c("Refugees_South", "Refugees_North", "Host_community_North"), labels = c("Refugees South", "Refugees North", "Host community North"))


# Create the column chart with percentages above the columns, including 0% labels
ggplot(hh02_data_long_filtered, aes(x = Variable, y = Percentage, fill = Population_Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(values = colors) +
  labs(title = "Main wall material (%)", x = "Variable", y = "Percentage", fill = "Population Group", caption = "Note: Includes main wall materials used by at least 2% of households in at least one group. The following categories are below 2% and therefore not shown: Stone with mud, Uncovered adobe, Plywood, Cardboard, Reused wood, Stone with lime/cement, Cement blocks, Covered adobe, Wood planks/shingles.") +
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "white", size = 0.5))

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/wallmaterial.png", width = 8, height = 6, dpi = 300)

#HH03
variables_hh03 <- unique(c(grep("^HH03", names(refsouth), value = TRUE),
                           grep("^HH03", names(refnorth), value = TRUE),
                           grep("^HH03", names(hostnorth), value = TRUE)))

# Extract labels for the variables and remove the prefix
labels_hh03 <- sapply(variables_hh03, function(var) {
  label <- attr(refsouth[[var]], "label")
  if (is.null(label)) label <- attr(refnorth[[var]], "label")
  if (is.null(label)) label <- attr(hostnorth[[var]], "label")
  sub("Main roof material: ", "", label)
})

# Calculate percentages for each dataset
percentages_south_hh03 <- calculate_percentage(refsouth, variables_hh03)
percentages_north_hh03 <- calculate_percentage(refnorth, variables_hh03)
percentages_host_hh03 <- calculate_percentage(hostnorth, variables_hh03)

# Create a data frame for plotting
hh03_data <- data.frame(
  Variable = labels_hh03,
  Refugees_South = percentages_south_hh03,
  Refugees_North = percentages_north_hh03,
  Host_community_North = percentages_host_hh03
)

# Filter out variables with all values <= 2%
hh03_data_filtered <- hh03_data %>% filter(if_any(c(Refugees_South, Refugees_North, Host_community_North), ~ . > 2))

# Convert data to long format for ggplot
hh03_data_long_filtered <- hh03_data_filtered %>% pivot_longer(cols = -Variable, names_to = "Population_Group", values_to = "Percentage")

hh03_data_long_filtered <- hh03_data_long_filtered %>%
  group_by(Variable) %>%
  mutate(Total_Percentage = sum(Percentage)) %>%
  ungroup()


hh03_data_long_filtered$Variable <- factor(hh03_data_long_filtered$Variable, levels = hh03_data_long_filtered %>%
                                             group_by(Variable) %>%
                                             dplyr::summarize(Total_Percentage = sum(Percentage)) %>%
                                             arrange(desc(Total_Percentage)) %>%
                                             pull(Variable))

# Ensure the columns are ordered and renamed
hh03_data_long_filtered$Population_Group <- factor(hh03_data_long_filtered$Population_Group, levels = c("Refugees_South", "Refugees_North", "Host_community_North"), labels = c("Refugees South", "Refugees North", "Host community North"))


# Create the column chart with percentages above the columns, including 0% labels
ggplot(hh03_data_long_filtered, aes(x = Variable, y = Percentage, fill = Population_Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(values = colors) +
  labs(title = "Main roof material (%)", x = "Variable", y = "Percentage", fill = "Population Group", caption = "Note: Includes main roof materials used by at least 2% of households in at least one group. The following categories are below 2% and therefore not shown: Rustic mat, Palm/bamboo, Wood planks, Cardboard, Wood, Fiber, Ceramic tiles.") +
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "white", size = 0.5))


ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/roofmaterial.png", width = 8, height = 6, dpi = 300)

#HH06
variables_hh06 <- unique(c(grep("^HH06", names(refsouth), value = TRUE),
                           grep("^HH06", names(refnorth), value = TRUE),
                           grep("^HH06", names(hostnorth), value = TRUE)))

# Extract labels for the variables and remove the prefix
labels_hh06 <- sapply(variables_hh06, function(var) {
  label <- attr(refsouth[[var]], "label")
  if (is.null(label)) label <- attr(refnorth[[var]], "label")
  if (is.null(label)) label <- attr(hostnorth[[var]], "label")
  sub("Main floor material: ", "", label)
})

# Calculate percentages for each dataset
percentages_south_hh06 <- calculate_percentage(refsouth, variables_hh06)
percentages_north_hh06 <- calculate_percentage(refnorth, variables_hh06)
percentages_host_hh06 <- calculate_percentage(hostnorth, variables_hh06)

# Create a data frame for plotting
hh06_data <- data.frame(
  Variable = labels_hh06,
  Refugees_South = percentages_south_hh06,
  Refugees_North = percentages_north_hh06,
  Host_community_North = percentages_host_hh06
)

# Filter out variables with all values <= 1%
hh06_data_filtered <- hh06_data %>% filter(if_any(c(Refugees_South, Refugees_North, Host_community_North), ~ . > 0.9))

# Convert data to long format for ggplot
hh06_data_long_filtered <- hh06_data_filtered %>% pivot_longer(cols = -Variable, names_to = "Population_Group", values_to = "Percentage")

hh06_data_long_filtered <- hh06_data_long_filtered %>%
  group_by(Variable) %>%
  mutate(Total_Percentage = sum(Percentage)) %>%
  ungroup()


hh06_data_long_filtered$Variable <- factor(hh06_data_long_filtered$Variable, levels = hh06_data_long_filtered %>%
                                             group_by(Variable) %>%
                                             dplyr::summarize(Total_Percentage = sum(Percentage)) %>%
                                             arrange(desc(Total_Percentage)) %>%
                                             pull(Variable))



# Ensure the columns are ordered and renamed
hh06_data_long_filtered$Population_Group <- factor(hh06_data_long_filtered$Population_Group, levels = c("Refugees_South", "Refugees_North", "Host_community_North"), labels = c("Refugees South", "Refugees North", "Host community North"))


# Create the column chart with percentages above the columns, including 0% labels
ggplot(hh06_data_long_filtered, aes(x = Variable, y = Percentage, fill = Population_Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), vjust = -0.5, position = position_dodge(width = 0.8), size = 3) +
  scale_fill_manual(values = colors) +
  labs(title = "Main floor material (%)", x = "Variable", y = "Percentage", fill = "Population Group", caption = "Note: Includes main floor materials used by at least 1% of households in at least one group. The following categories are below 1% and therefore not shown: Wood planks, Palm/bamboo, Parquet or polished wood, Vinyl or asphalt strips, Ceramic tiles, Carpet, Plastic sheeting or canvas.") +
  theme_unhcr() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "white", size = 0.5))

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/floormaterial.png", width = 8, height = 6, dpi = 300)


##5 Heatmaps Asset & Livestock ownership
#Assets
variables <- unique(c(grep("^Assets01|^Assets02", names(refsouth), value = TRUE),
                      grep("^Assets01|^Assets02", names(refnorth), value = TRUE),
                      grep("^Assets01|^Assets02", names(hostnorth), value = TRUE),
                      grep("^Assets01|^Assets02", names(main), value = TRUE)))

# Extract labels for the variables
labels <- sapply(variables, function(var) {
  label <- attr(refsouth[[var]], "label")
  if (is.null(label)) label <- attr(refnorth[[var]], "label")
  if (is.null(label)) label <- attr(hostnorth[[var]], "label")
  if (is.null(label)) label <- attr(main[[var]], "label")
  label
})

# Calculate percentages for each dataset
percentages_south <- calculate_percentage(refsouth, variables)
percentages_north <- calculate_percentage(refnorth, variables)
percentages_host <- calculate_percentage(hostnorth, variables)
percentages_main <- calculate_percentage(main, variables)

# Create a data frame for plotting
heatmap_data <- data.frame(
  Variable = labels,
  Refugees_South = percentages_south,
  Refugees_North = percentages_north,
  Host_community_North = percentages_host,
  Total_population = percentages_main
)


heatmap_data$diff <- ((heatmap_data$Refugees_South - heatmap_data$Refugees_North)/heatmap_data$Refugees_North)^2 +
  ((heatmap_data$Refugees_North - heatmap_data$Host_community_North)/heatmap_data$Host_community_North)^2 +
  ((heatmap_data$Host_community_North - heatmap_data$Refugees_South)/heatmap_data$Refugees_South)^2
heatmap_data$sd <- apply(heatmap_data[, c("Refugees_South", "Refugees_North", "Host_community_North")], 1, sd)

# Reorder the Variable factor based on the standard deviation
heatmap_data$Variable <- factor(heatmap_data$Variable, levels = heatmap_data %>%
                                  arrange(sd) %>%
                                  pull(Variable))

# Separate the data into two tables based on the percentage in the main dataset & remove the column for main from the final tables
heatmap_data_lowvar <- heatmap_data %>% filter(sd < 1) %>% arrange(Variable) %>% select(-c(Total_population, diff, sd)) 
heatmap_data_highvar <- heatmap_data %>% filter(sd >= 1) %>% arrange(Variable) %>% select(-c(Total_population, diff, sd)) 


create_heatmap <- function(data, title) {
  data_long <- data %>% pivot_longer(cols = -Variable, names_to = "Population_Group", values_to = "Percentage")
  
  # Ensure the columns are ordered and renamed
  data_long$Population_Group <- factor(data_long$Population_Group, levels = c("Refugees_South", "Refugees_North", "Host_community_North"), labels = c("Refugees South", "Refugees North", "Host community North"))
  
  
  ggplot(data_long, aes(x = Population_Group, y = Variable, fill = Percentage)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", Percentage)), color = "black", size = 3) +
    #   scale_fill_gradientn(colors = c("white","#DCE9FF","#0072BC", "#044F85"),  breaks = c(0,1, 60,80)) +  
    scale_fill_gradientn(colors = c("#DCE9FF", "#044F85"), limits = c(0, 80), breaks = c(0,20,40,60, 80)) +
    #   scale_fill_gradient(low = "#DCE9FF" , high= "#044F85")+
    labs(title = title, x = NULL, y = NULL, fill = "Percentage (%)", caption = "Note: Assets ordered by variation, starting with the asset with the highest standard deviation between population groups. This table includes only assets with a standard deviation of 1% and higher. The following assets are low in variation and incidence and therefore excluded: Air conditioner, Camera, other than phone, Laptop/computer, Sofa, Ridger, Mixer/blender, Diesel driven irigation system, Modem/router, Wall clock, Cultivators, Generator, Freezer, CD/DVD player, Room-cooler, Satellite dish, Boat without motor/canoe, Water pump, Car, Fan, Washing machine, Tractor, Truck.") +
    theme_unhcr() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(size = 10))
}


create_heatmap(heatmap_data_highvar, "Asset ownership (%)")
ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/heatmap_assets.png", width = 8.5, height = 11, dpi = 300)

#Livestock ownership
variables <- unique(c(grep("^Assets04", names(refsouth), value = TRUE),
                      grep("^Assets04", names(refnorth), value = TRUE),
                      grep("^Assets04", names(hostnorth), value = TRUE),
                      grep("^Assets04", names(main), value = TRUE)))

# Extract labels for the variables
labels <- sapply(variables, function(var) {
  label <- attr(refsouth[[var]], "label")
  if (is.null(label)) label <- attr(refnorth[[var]], "label")
  if (is.null(label)) label <- attr(hostnorth[[var]], "label")
  if (is.null(label)) label <- attr(main[[var]], "label")
  label
})

# Calculate percentages for each dataset
percentages_south <- calculate_percentage(refsouth, variables)
percentages_north <- calculate_percentage(refnorth, variables)
percentages_host <- calculate_percentage(hostnorth, variables)
percentages_main <- calculate_percentage(main, variables)

# Create a data frame for plotting
heatmap_data <- data.frame(
  Variable = labels,
  Refugees_South = percentages_south,
  Refugees_North = percentages_north,
  Host_community_North = percentages_host,
  Total_population = percentages_main
)

# Remove the column for main from the final table
heatmap_data <- heatmap_data %>% select(-Total_population)
heatmap_data <- heatmap_data[!grepl("^(Other cattle|Pigs)", heatmap_data$Variable), ]

# Function to create heatmap
create_heatmap <- function(data) {
  data_long <- data %>% pivot_longer(cols = -Variable, names_to = "Population_Group", values_to = "Percentage")
  
  # Ensure the columns are ordered and renamed
  data_long$Population_Group <- factor(data_long$Population_Group, levels = c("Refugees_South", "Refugees_North", "Host_community_North"), labels = c("Refugees South", "Refugees North", "Host community North"))
  
  data_long$Variable <- factor(data_long$Variable, levels= c("Pigs: 10+", "Pigs: 5-9", "Pigs: 1-4", "Barnyard animals: 10+", "Barnyard animals: 5-9", "Barnyard animals: 1-4", "Sheep: 10+", "Sheep: 5-9", "Sheep: 1-4", "Goats: 10+", "Goats: 5-9", "Goats: 1-4", "Horses/donkeys/mules: 10+", "Horses/donkeys/mules: 5-9", "Horses/donkeys/mules: 1-4", "Other cattle: 10+", "Other cattle: 5-9", "Other cattle: 1-4", "Cows/bulls: 10+", "Cows/bulls: 5-9", "Cows/bulls: 1-4"))
  
  ggplot(data_long, aes(x = Population_Group, y = Variable, fill = Percentage)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", Percentage)), color = "black", size = 3) +
    scale_fill_gradient(low = "#DCE9FF", high = "#044F85") +
    labs(title = "Livestock Ownership (%)", x = NULL, y = NULL, fill = "Percentage", caption = "Note: Excludes Other cattle and Pigs due to low ownership (below 1% in all groups).") +
    theme_unhcr() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(size = 10))
}

create_heatmap(heatmap_data)
ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/DHS Wealth index/figures/heatmap_livestock.png", width = 8.5, height = 8, dpi = 300)

