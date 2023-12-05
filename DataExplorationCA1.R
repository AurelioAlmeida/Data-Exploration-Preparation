setwd("C:/Users/AurelioAlmeida/Documents/GitHub/Data-Exploration-Preparation")

library(readr)
library(visdat)

# Load the dataset
Global_Covid_Dataset_ <- read.csv("C:/Users/AurelioAlmeida/Documents/GitHub/Data-Exploration-Preparation/Global Covid Dataset .csv")

# Check dataset structure
str(data)

# Display dataset first few rows
head(data)

# Check number of rows
num_rows <- nrow(data)
print(num_rows)

# List of EU countries
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden"
)

# Filter the dataset for EU countries
eu_dataset <- subset(Global_Covid_Dataset_, location %in% eu_countries)

# Check data types
str(eu_dataset)

# Get summary statistics
summary(eu_dataset)

# Visualize missing data
vis_dat(eu_dataset)

# Calculate statistical parameters
summary_stats <- summary(eu_dataset[, c("new_cases", "new_deaths", "total_cases", "total_deaths", "weekly_cases", "weekly_deaths", "biweekly_cases", "biweekly_deaths")])


# Extract mean, median, min, max, and sd for new_cases
mean_new_cases <- mean(eu_dataset$new_cases, na.rm = TRUE)
median_new_cases <- median(eu_dataset$new_cases, na.rm = TRUE)
min_new_cases <- min(eu_dataset$new_cases, na.rm = TRUE)
max_new_cases <- max(eu_dataset$new_cases, na.rm = TRUE)
sd_new_cases <- sd(eu_dataset$new_cases, na.rm = TRUE)

# Print or use these values
print(mean_new_cases)
print(median_new_cases)
print(min_new_cases)
print(max_new_cases)
print(sd_new_cases)


# Calculate summary statistics for new_deaths
summary_new_deaths <- summary(eu_dataset$new_deaths)

# Extract values
mean_new_deaths <- mean(eu_dataset$new_deaths, na.rm = TRUE)
median_new_deaths <- median(eu_dataset$new_deaths, na.rm = TRUE)
min_new_deaths <- min(eu_dataset$new_deaths, na.rm = TRUE)
max_new_deaths <- max(eu_dataset$new_deaths, na.rm = TRUE)
sd_new_deaths <- sd(eu_dataset$new_deaths, na.rm = TRUE)

# Print values for new_deaths
print(mean_new_deaths)
print(median_new_deaths)
print(min_new_deaths)
print(max_new_deaths)
print(sd_new_deaths)

# Calculate summary statistics for total_cases
summary_total_cases <- summary(eu_dataset$total_cases)

# Extract values
mean_total_cases <- mean(eu_dataset$total_cases, na.rm = TRUE)
median_total_cases <- median(eu_dataset$total_cases, na.rm = TRUE)
min_total_cases <- min(eu_dataset$total_cases, na.rm = TRUE)
max_total_cases <- max(eu_dataset$total_cases, na.rm = TRUE)
sd_total_cases <- sd(eu_dataset$total_cases, na.rm = TRUE)

# Print values for total_cases
print(mean_total_cases)
print(median_total_cases)
print(min_total_cases)
print(max_total_cases)
print(sd_total_cases)

# Calculate summary statistics for total_deaths
summary_total_deaths <- summary(eu_dataset$total_deaths)

# Extract values
mean_total_deaths <- mean(eu_dataset$total_deaths, na.rm = TRUE)
median_total_deaths <- median(eu_dataset$total_deaths, na.rm = TRUE)
min_total_deaths <- min(eu_dataset$total_deaths, na.rm = TRUE)
max_total_deaths <- max(eu_dataset$total_deaths, na.rm = TRUE)
sd_total_deaths <- sd(eu_dataset$total_deaths, na.rm = TRUE)

# Print values for total_deaths
print(mean_total_deaths)
print(median_total_deaths)
print(min_total_deaths)
print(max_total_deaths)
print(sd_total_deaths)

# Calculate summary statistics for weekly_cases
summary_weekly_cases <- summary(eu_dataset$weekly_cases)

# Extract values
mean_weekly_cases <- mean(eu_dataset$weekly_cases, na.rm = TRUE)
median_weekly_cases <- median(eu_dataset$weekly_cases, na.rm = TRUE)
min_weekly_cases <- min(eu_dataset$weekly_cases, na.rm = TRUE)
max_weekly_cases <- max(eu_dataset$weekly_cases, na.rm = TRUE)
sd_weekly_cases <- sd(eu_dataset$weekly_cases, na.rm = TRUE)

# Print values for weekly_cases
print(mean_weekly_cases)
print(median_weekly_cases)
print(min_weekly_cases)
print(max_weekly_cases)
print(sd_weekly_cases)


# Calculate summary statistics for weekly_cases
summary_weekly_cases <- summary(eu_dataset$weekly_cases)

# Extract values
mean_weekly_cases <- mean(eu_dataset$weekly_cases, na.rm = TRUE)
median_weekly_cases <- median(eu_dataset$weekly_cases, na.rm = TRUE)
min_weekly_cases <- min(eu_dataset$weekly_cases, na.rm = TRUE)
max_weekly_cases <- max(eu_dataset$weekly_cases, na.rm = TRUE)
sd_weekly_cases <- sd(eu_dataset$weekly_cases, na.rm = TRUE)

# Print values for weekly_cases
print(mean_weekly_cases)
print(median_weekly_cases)
print(min_weekly_cases)
print(max_weekly_cases)
print(sd_weekly_cases)



# Calculate summary statistics for weekly_deaths
summary_weekly_deaths <- summary(eu_dataset$weekly_deaths)

# Extract values
mean_weekly_deaths <- mean(eu_dataset$weekly_deaths, na.rm = TRUE)
median_weekly_deaths <- median(eu_dataset$weekly_deaths, na.rm = TRUE)
min_weekly_deaths <- min(eu_dataset$weekly_deaths, na.rm = TRUE)
max_weekly_deaths <- max(eu_dataset$weekly_deaths, na.rm = TRUE)
sd_weekly_deaths <- sd(eu_dataset$weekly_deaths, na.rm = TRUE)

# Print values for weekly_deaths
print(mean_weekly_deaths)
print(median_weekly_deaths)
print(min_weekly_deaths)
print(max_weekly_deaths)
print(sd_weekly_deaths)

# Calculate summary statistics for biweekly_cases
summary_biweekly_cases <- summary(eu_dataset$biweekly_cases)

# Extract values
mean_biweekly_cases <- mean(eu_dataset$biweekly_cases, na.rm = TRUE)
median_biweekly_cases <- median(eu_dataset$biweekly_cases, na.rm = TRUE)
min_biweekly_cases <- min(eu_dataset$biweekly_cases, na.rm = TRUE)
max_biweekly_cases <- max(eu_dataset$biweekly_cases, na.rm = TRUE)
sd_biweekly_cases <- sd(eu_dataset$biweekly_cases, na.rm = TRUE)

# Print values for biweekly_cases
print(mean_biweekly_cases)
print(median_biweekly_cases)
print(min_biweekly_cases)
print(max_biweekly_cases)
print(sd_biweekly_cases)

# Calculate summary statistics for biweekly_deaths
summary_biweekly_deaths <- summary(eu_dataset$biweekly_deaths)

# Extract values
mean_biweekly_deaths <- mean(eu_dataset$biweekly_deaths, na.rm = TRUE)
median_biweekly_deaths <- median(eu_dataset$biweekly_deaths, na.rm = TRUE)
min_biweekly_deaths <- min(eu_dataset$biweekly_deaths, na.rm = TRUE)
max_biweekly_deaths <- max(eu_dataset$biweekly_deaths, na.rm = TRUE)
sd_biweekly_deaths <- sd(eu_dataset$biweekly_deaths, na.rm = TRUE)

# Print values for biweekly_deaths
print(mean_biweekly_deaths)
print(median_biweekly_deaths)
print(min_biweekly_deaths)
print(max_biweekly_deaths)
print(sd_biweekly_deaths)






