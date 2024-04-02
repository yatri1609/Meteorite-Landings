library(dplyr)
library(tidyverse)

data_meterorite <- read.csv("./Data/Meteorite_Landings_20240401.csv", header = TRUE)

# Print head 
head(data_meterorite)

# Calculate number of missing values
sum(is.na(data_meterorite))

# Total number of rows
nrow(data_meterorite)

# Column wise sum of missing data 
colSums(is.na(data_meterorite))

# Calculate the mean mass for each recclass and impute missing values
data_meterorite <- data_meterorite %>%
  group_by(recclass) %>%
  mutate(
    mean_mass = mean(mass..g., na.rm = TRUE), # Calculate mean mass for each group, excluding NAs
    mass..g. = ifelse(is.na(mass..g.), mean_mass, mass..g.), # Impute missing mass values
    median_year = median(year, na.rm = TRUE), # Calculate median year for each group, excluding NAs
    year = ifelse(is.na(year), median_year, year) # Impute missing year values
  ) %>%
  ungroup() %>%
  select(-mean_mass, -median_year) # Remove the temporary columns

# Use the overall mean or median as a fallback for any remaining NAs
overall_mean_mass <- mean(data_meterorite$mass..g., na.rm = TRUE)
data_meterorite$mass..g.[is.na(data_meterorite$mass..g.)] <- overall_mean_mass

overall_median_year <- median(data_meterorite$year, na.rm = TRUE)
data_meterorite$year[is.na(data_meterorite$year)] <- overall_median_year

# Check for any remaining missing values
sum(is.na(data_meterorite$mass..g.))
sum(is.na(data_meterorite$year))

# Export data into processed csv fro visualization
write.csv(data_meterorite, "./Data/meteorite_landing_processed.csv")