# Step 1: Load required libraries 
library(tidyverse)
library(lubridate)
library(ggplot2)
# Step 2: Explore General Trends and Patterns

# Overview of the Dataset
summary(data)
str(data)

#Explore Time Trends
data %>%
  mutate(JOUR = as.Date(JOUR)) %>%
  ggplot(aes(x = JOUR)) +
  geom_line(stat = "count", color = "blue") +
  labs(title = "Daily Validation Counts Over Time")


# Step 3: Identify Seasonality and Monthly Trends

data %>%
  mutate(month = month(JOUR)) %>%
  ggplot(aes(x = month, y = NB_VALD)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Monthly Validation Counts",
       x = "Month",
       y = "Validation Counts")


# convert NB_VALD to INT 

data <- data %>%
  mutate(NB_VALD = as.integer(NB_VALD))

# Step 4: Comparison with Norms
# 4.1 Define Normal Weeks
normal_week <- data %>%
  group_by(week = lubridate::week(JOUR)) %>%
  summarize(avg_validation = mean(NB_VALD))


# create a Weekday Variable
data$weekday <- weekdays(as.Date(data$JOUR))


selected_date <- as.Date("2022-07-07")

# Define the start and end dates for the week around the selected date
start_date <- selected_date - lubridate::days(3)  # Three days before the selected date
end_date <- selected_date + lubridate::days(3)    # Three days after the selected date

# Filter the data for the selected week
selected_week_data <- data %>%
  filter(JOUR >= start_date & JOUR <= end_date)

selected_week_data <- selected_week_data %>%
  mutate(NB_VALD = as.integer(NB_VALD))

# Calculate total validations per day
total_validations_per_day <- selected_week_data %>%
  group_by(JOUR) %>%
  summarise(total_validations = sum(NB_VALD))

