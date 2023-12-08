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
  labs(title = "Line Plot ofTotal Cases Over Time of the 5 Countries with the Highest Total Cases", x = "Date", y = "Total of Cases") +
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

# Install and load the necessary library
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Select the top 5 countries with the highest total cases
top_countries <- names(sort(tapply(eu_dataset$total_cases, eu_dataset$location, max), decreasing = TRUE)[1:5])

# Convert date to Date type
eu_dataset$date <- as.Date(eu_dataset$date)

# Plot scatter plots for each of the top 5 countries for total cases
ggplot(subset(eu_dataset, location %in% top_countries), aes(x = date, y = total_cases, color = location)) +
  geom_point() +
  labs(title = 'Scatter plot of Total Cases Over Time for top 5 countries with the highest total cases',
       x = 'Date',
       y = 'Total Cases') +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"))  # Format labels in millions


#start letter D - Line Plot

# Make sure the 'date' column is in the correct date format
eu_dataset$date <- lubridate::mdy(eu_dataset$date)

# Remove rows with missing values in 'total_cases', 'total_deaths', 'location', and 'date'
eu_dataset <- na.omit(eu_dataset[c("date", "total_cases", "total_deaths", "location")])

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

labs(title = "Line Plot of Total Cases Over Time for top 5 countries with the highest total cases", x = "Date", y = "Total of Cases") +
  theme_minimal()

#Letter D - Scatter plot

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

labs(title = 'Scater Plot of Total Cases Over Time for top 5 countries with the highest total cases',
     x = 'Date',
     y = 'Total Cases (in Millions)') +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"))  # Format labels in millions

#### Start of question E


# Load required libraries
library(ggplot2)
library(scales)

# Convert it to Date format in a suitable format
eu_dataset_cleaned$date <- as.Date(eu_dataset_cleaned$date)

# Convert 'new_cases' to numeric so the labels is displayed properly
eu_dataset_cleaned$new_cases <- as.numeric(eu_dataset_cleaned$new_cases)

# Filter the dataset to include only the last 8 months
end_date <- max(eu_dataset_cleaned$date)
start_date_8months <- end_date - months(8)
filtered_dataset_8months <- subset(eu_dataset_cleaned, date >= start_date_8months)

# Scatter plot with date as gradient color for the last 8 months
ggplot(filtered_dataset_8months, aes(x = new_cases, y = total_deaths, color = date)) +
  geom_point(alpha = 0.7) +
  labs(title = 'Comparison of New Cases and Total Deaths Over the Last 8 Months of 2020.',
       x = 'New Cases', y = 'Total Deaths') +
  scale_color_gradient(low = "blue", high = "red", labels = scales::date_format('%m-%y'),
                       breaks = seq(min(filtered_dataset_8months$date), max(filtered_dataset_8months$date), by = '1 month')) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma_format())  # Format x-axis labels as numerical values

#### Time series analysis Histogram plot

library(ggplot2)
library(scales)

# Convert it to Date format if not in a suitable format.
eu_dataset$date <- as.Date(eu_dataset$date)

# Convert 'total_cases' to numeric
eu_dataset$total_cases <- as.numeric(eu_dataset$total_cases)

# Extract month and year
eu_dataset$month_year <- format(eu_dataset$date, "%b %Y")

# Time series plot of total_cases
ggplot(eu_dataset, aes(x = date, y = total_cases, group = 1)) +
  geom_line(color = "blue") +
  labs(title = 'Total Cases Over Time', x = 'Date', y = 'Total Cases') +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis labels as numerical values with commas
  scale_x_date(breaks = seq(min(eu_dataset$date), max(eu_dataset$date), by = '2 months'), labels = scales::date_format('%b %Y')) +  # Format x-axis labels with abbreviated month and year
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees

#### numeric variables for correlation analysis

# Install and load the mice package if needed
#install.packages("mice")
library(mice)

# Impute missing values using mice
subset_data_imputed <- mice(subset_data, method = "pmm", m = 5)  # Adjust 'm' as needed

# Extract the completed data
subset_data_imputed <- complete(subset_data_imputed)

# Calculate correlation matrix
cor_matrix <- cor(subset_data_imputed)

# Visualize correlation matrix
corrplot::corrplot(cor_matrix, method = "color")


#### Feature subgroups

# Choosing relevant features
features_for_clustering <- eu_dataset[, c("total_cases", "new_cases","new_deaths")]


### Handling Missing Values

# we load the dataset for the 'cluster_data'
cluster_data <- read.csv("/Users/renner/Documents/GitHub/Data-Exploration-Preparation/Global Covid Dataset .csv")  # Replace 'your_dataset.csv' with your actual file path


# We adjust the columns 
columns_with_missing_values <- c("total_cases", "new_cases", "new_deaths")
imputed_data <- mice(cluster_data[, columns_with_missing_values], method = "pmm", m = 5)
completed_datasets <- complete(imputed_data)

# Standardize the data
standardized_features <- scale(completed_datasets[[1]])  # Using the first imputed dataset for simplicity

#### Categorical Variables - Question E

# Assuming 'total_deaths' is factor, convert it to numeric
eu_dataset$total_deaths <- as.numeric(as.character(eu_dataset$total_deaths))

# Assuming 'date' is in a suitable format, if not, convert it to Date format
eu_dataset$date <- as.Date(eu_dataset$date)

# Extract month-year from the 'date' column
eu_dataset$month_year <- format(eu_dataset$date, "%b %Y")

# Filter the data for the top 10 locations based on total deaths
top_10_locations <- eu_dataset %>%
  group_by(location) %>%
  summarize(total_deaths = sum(total_deaths, na.rm = TRUE)) %>%
  top_n(10, wt = total_deaths) %>%
  pull(location)

# Filter the dataset for the top 10 locations
filtered_dataset <- eu_dataset %>%
  filter(location %in% top_10_locations)

# Create a stacked bar plot for 'total_deaths' by 'month_year' and 'location'
library(ggplot2)
ggplot(filtered_dataset, aes(x = month_year, fill = location)) +
  geom_bar(position = "stack") +
  labs(title = "Total Deaths by Location Over 12 Months", x = "Month-Year", y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees

#### Time Series Decomposition - Question E

# Check for missing values in the total_cases column
sum(is.na(eu_dataset$total_cases))

# Remove the missing values 
eu_dataset_clean <- na.omit(eu_dataset$total_cases)
ts_data <- ts(eu_dataset_clean, frequency = 12)

# Impute missing values
# Using linear interpolation for imputation

library(zoo)

# Convert 'total_cases' to numeric
eu_dataset$total_cases <- as.numeric(as.character(eu_dataset$total_cases))

# Impute missing values using linear interpolation
eu_dataset$total_cases <- na.approx(eu_dataset$total_cases, na.rm = FALSE)

# Create a time series
ts_data <- ts(eu_dataset$total_cases, frequency = 12)

# Ploting time series decomposition
ts_data <- ts(eu_dataset$total_cases, frequency = 12)  # Assuming monthly data
decomposition <- decompose(ts_data)
plot(decomposition)

##start letter F

library(dplyr)
library(tidyr)

# Convert 'location' to factor
eu_dataset$location <- as.factor(eu_dataset$location)

# Apply dummy encoding
eu_dataset <- eu_dataset %>%
  mutate(across(location, as.factor)) %>%
  select(-date) %>%
  model.matrix(~ . - 1, data = .) %>%
  as.data.frame()

# View the modified dataset
head(eu_dataset)

##start letter G

##apply PCA


##setting 5 principal components
num_components <- 5

# Extract numerical variables for PCA
numerical_data <- eu_dataset[, c("new_cases", "new_deaths", "total_cases", "total_deaths", "weekly_cases", "weekly_deaths", "biweekly_cases", "biweekly_deaths")]

# Standardize the numerical data
scaled_data <- scale(numerical_data)

# Apply PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Extract the first few components
first_few_components <- pca_result$x[, 1:num_components]

# Write a profile of the first few components
for (i in 1:num_components) {
  cat("\nPrincipal Component", i, "Profile:\n")
  cat("Variance Explained:", round(pca_result$sdev[i]^2 / sum(pca_result$sdev^2) * 100, 2), "%\n")
  cat("Loadings:\n")
  cat(pca_result$rotation[, i], "\n")
}


