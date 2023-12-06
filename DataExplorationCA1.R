setwd("C:/Users/AurelioAlmeida/Documents/GitHub/Data-Exploration-Preparation")

library(readr)
library(visdat)
library(caret)
library(zoo)
library(imputeTS)
library(ggplot2)

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


###start of letter C

##Handling missing values before perform scaling methods

# Interpolate missing values of "new_cases" variable
eu_dataset$new_cases <- na.approx(eu_dataset$new_cases, na.rm = FALSE)

##Min-Max Normalization

# Extract the variable
new_cases <- eu_dataset$new_cases

# Function to apply Min-Max Normalization
min_max_normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply Min-Max Normalization
new_cases_min_max <- min_max_normalize(new_cases)

# Min-Max Normalization
summary_min_max_new_cases <- summary(new_cases_min_max)

# Print values
cat("\nSummary Statistics - Min-Max Normalization:\n")
cat(paste(names(summary_min_max_new_cases), ": ", summary_min_max_new_cases, sep="\n"), "\n")

## Z-score Standardization

# Interpolate missing values using linear interpolation
eu_dataset$new_cases <- na.interpolation(eu_dataset$new_cases, option = "linear")

# Function to apply Z-score Standardization
z_score_standardize_new_cases <- function(x) {
  return((x - mean(x)) / sd(x))
}

# Extract the variable
new_cases <- eu_dataset$new_cases

# Apply Z-score Standardization
new_cases_z_score <- z_score_standardize_new_cases(new_cases)

# Summary Statistics - Z-score Standardization
summary_z_score_new_cases <- summary(new_cases_z_score)

# Print values
cat("\nSummary Statistics - Z-score Standardization (new_cases):\n")
cat(paste(names(summary_z_score_new_cases), ": ", summary_z_score_new_cases, sep="\n"), "\n")

##Robust scalar

# Interpolate missing values of "new_cases" variable
eu_dataset$new_cases <- na.approx(eu_dataset$new_cases, na.rm = FALSE)

# Extract the variable
new_cases <- eu_dataset$new_cases

# Function to apply Robust Scaling
robust_scale <- function(x) {
  return((x - median(x)) / IQR(x))
}

# Apply Robust Scaling
new_cases_robust <- robust_scale(new_cases)

# Summary Statistics - Robust Scaling
summary_robust <- summary(new_cases_robust)

# Print values
cat("\nSummary Statistics - Robust Scaling (new_cases):\n")
cat(paste(names(summary_robust), ": ", summary_robust, sep="\n"), "\n")


#start letter D

##Plot line

# Make sure the 'date' column is in the correct date format
eu_dataset$date <- lubridate::mdy(eu_dataset$date)

# Remove rows with missing values in 'total_cases', 'total_deaths', 'location', and 'date'
eu_dataset <- na.omit(eu_dataset[c("date", "total_cases", "total_deaths", "location")])


# Select the top 5 countries with the highest total cases
top_countries <- eu_dataset %>%
  group_by(location) %>%
  summarize(max_total_cases = max(total_cases)) %>%
  arrange(desc(max_total_cases)) %>%
  head(5)

# Filter the dataset for the top 5 countries
top_countries_data <- eu_dataset %>%
  filter(location %in% top_countries$location)

# Create a line plot for total cases and total deaths by location
ggplot(top_countries_data, aes(x = date, y = total_cases, group = location, color = location)) +
  geom_line(aes(y = total_deaths)) +
  labs(title = "Line Plot of Total Cases Over Time of the 5 Countries with the Highest Total Cases", x = "Date", y = "Total of Cases") +
  theme_minimal()

##Heatmaps

# Make sure the 'date' column is in the correct date format
eu_dataset$date <- lubridate::mdy(eu_dataset$date)

# Remove rows with missing values in 'total_cases', 'total_deaths', 'location', and 'date'
eu_dataset <- na.omit(eu_dataset[c("date", "total_cases", "total_deaths", "location")])

# Select the top 5 countries with the highest total cases
top_countries <- eu_dataset %>%
  group_by(location) %>%
  summarize(max_total_cases = max(total_cases)) %>%
  arrange(desc(max_total_cases)) %>%
  head(5)

# Filter the dataset for the top 5 countries
top_countries_data <- eu_dataset %>%
  filter(location %in% top_countries$location)

# Create a heatmap for total cases of the top 5 countries over time
ggplot(top_countries_data, aes(x = as.factor(year(date)), y = location, fill = total_cases)) +
  geom_tile(color = "white", size = 0.2) +  # Add white borders for better separation
  scale_fill_viridis_c(option = "magma", na.value = "white") +  # Adjust color scale to magma
  labs(title = "Heatmap of Total Cases Over Time for top 5 countries with the highest total cases",
       x = "Year",
       y = "Country",
       fill = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels straight
        legend.position = "right") +  # Adjust legend position
  scale_fill_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))  # Use label_number to format as millions

##Scatter plot

# Install and load the necessary library if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Select the top 5 countries with the highest total cases
top_countries <- names(sort(tapply(eu_dataset$total_cases, eu_dataset$location, max), decreasing = TRUE)[1:5])

# Convert date to Date type if it's not already
eu_dataset$date <- as.Date(eu_dataset$date)

# Plot scatter plots for each of the top 5 countries with numeric format for total cases
ggplot(subset(eu_dataset, location %in% top_countries), aes(x = date, y = total_cases, color = location)) +
  geom_point() +
  labs(title = 'Scatter plot of Total Cases Over Time for top 5 countries with the highest total cases',
       x = 'Date',
       y = 'Total Cases (in Millions)') +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"))

