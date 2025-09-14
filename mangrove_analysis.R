
# Load necessary libraries
library(dplyr)
library(xlsx)
library(vegan)

# Read the dataset
data <- read.xlsx("mangrove_seedling.xlsx", sheetName = "raw_data")
head(data)

# Summary of the dataset
summary(data)

# Convert necessary columns to appropriate data types
data$Site <- as.factor(data$Site)
data$Species <- as.factor(data$Species)

# Count of species per site
species_count <- data %>% 
  group_by(Site, Species) %>% 
  summarise(Total_Count = sum(Count, na.rm = TRUE))

# Summary statistics of DBH and Height
dbh_summary <- data %>% summarise(mean_DBH = mean(DBH, na.rm = TRUE), sd_DBH = sd(DBH, na.rm = TRUE))
height_summary <- data %>% summarise(mean_Height = mean(H, na.rm = TRUE), sd_Height = sd(H, na.rm = TRUE))

# Visualization: Distribution of Species
species_plot <- ggplot(data, aes(x = Species)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribution of Mangrove Species")

# Save plot
ggsave("/mnt/data/species_distribution.png", plot = species_plot)

# Write analysis results to a file
report <- paste("Mangrove Data Analysis Report\n",
                "Summary Statistics:\n",
                "DBH Mean:", dbh_summary$mean_DBH, "SD:", dbh_summary$sd_DBH, "\n",
                "Height Mean:", height_summary$mean_Height, "SD:", height_summary$sd_Height, "\n",
                "Species Distribution by Site:\n", capture.output(print(species_count)))

writeLines(report, "/mnt/data/mangrove_analysis_report.txt")
