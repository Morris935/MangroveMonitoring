# Load necessary libraries
library(tidyverse)
library(vegan)  # For biodiversity analysis

# Read the dataset
data <- readxl::read_excel("mangrove_seedling.xlsx")

# Convert necessary columns to appropriate data types
data$Site <- as.factor(data$Site)
data$Species <- as.factor(data$Species)

# 1. Summary statistics
summary(data)

# 2. Species diversity (Shannon and Simpson Index)
species_diversity <- data %>% 
  group_by(Site, Species) %>% 
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = Total_Count, values_fill = 0)

shannon_index <- diversity(species_diversity[-1], index = "shannon")
simpson_index <- diversity(species_diversity[-1], index = "simpson")

# 3. Forest Density and Basal Area (BA = π * (DBH/2)^2 per tree)
data <- data %>% mutate(Basal_Area = pi * (DBH/2)^2 * Count)

# Compute total basal area per site
basal_area_per_site <- data %>% 
  group_by(Site) %>% 
  summarise(Total_BA = sum(Basal_Area, na.rm = TRUE))

# 4. Mean DBH and Height Analysis
dbh_stats <- data %>% summarise(Mean_DBH = mean(DBH, na.rm = TRUE), SD_DBH = sd(DBH, na.rm = TRUE))
height_stats <- data %>% summarise(Mean_Height = mean(H, na.rm = TRUE), SD_Height = sd(H, na.rm = TRUE))

# 5. Seedling density per site (Seedling count per plot size)
density_per_site <- data %>% 
  group_by(Site) %>% 
  summarise(Mean_Density = sum(Count, na.rm = TRUE) / sum(Plot.Size, na.rm = TRUE))

# 6. Correlation between DBH and Height
correlation_dbh_height <- cor(data$DBH, data$H, use = "complete.obs")

# 7. Importance Value Index (IVI) calculation
total_trees <- sum(data$Count, na.rm = TRUE)
species_abundance <- data %>% group_by(Species) %>% summarise(Abundance = sum(Count, na.rm = TRUE))
relative_abundance <- species_abundance %>% mutate(Relative_Abundance = (Abundance / total_trees) * 100)

relative_basal_area <- data %>% 
  group_by(Species) %>% 
  summarise(Total_BA = sum(Basal_Area, na.rm = TRUE)) %>% 
  mutate(Relative_BA = (Total_BA / sum(Total_BA)) * 100)

species_freq <- data %>% 
  group_by(Species) %>% 
  summarise(Frequency = n()) %>% 
  mutate(Relative_Frequency = (Frequency / sum(Frequency)) * 100)

IVI <- relative_abundance %>% 
  left_join(relative_basal_area, by = "Species") %>% 
  left_join(species_freq, by = "Species") %>% 
  mutate(IVI = Relative_Abundance + Relative_BA + Relative_Frequency)

# 8. Visualization - DBH distribution
dbh_plot <- ggplot(data, aes(x = DBH)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  ggtitle("Distribution of DBH in Mangrove Forests")

# Save plots
ggsave("dbh_distribution.png", plot = dbh_plot)

# 9. Write analysis results to a file
report <- paste("Comprehensive Mangrove Ecology Analysis\\n",
                "----------------------------------------\\n",
                "1. Summary Statistics:\\n",
                "Mean DBH:", dbh_stats$Mean_DBH, "SD:", dbh_stats$SD_DBH, "\\n",
                "Mean Height:", height_stats$Mean_Height, "SD:", height_stats$SD_Height, "\\n\\n",
                "2. Forest Density & Basal Area:\\n", capture.output(print(basal_area_per_site)), "\\n",
                "3. Species Diversity (Shannon Index):", shannon_index, "\\n",
                "Species Diversity (Simpson Index):", simpson_index, "\\n\\n",
                "4. Seedling Density per Site:\\n", capture.output(print(density_per_site)), "\\n",
                "5. Correlation between DBH and Height:", correlation_dbh_height, "\\n\\n",
                "6. Importance Value Index (IVI) per Species:\\n", capture.output(print(IVI)))

writeLines(report, "mangrove_comprehensive_report.txt")
