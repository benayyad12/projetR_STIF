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

# Step 4: Identify and Handle Outliers
data %>%
  filter(!is.na(NB_VALD)) %>%
  ggplot(aes(x = 1, y = NB_VALD)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplot of Validation Counts",
       x = "",
       y = "Validation Counts")

# convert NB_VALD to INT 

data <- data %>%
  mutate(NB_VALD = as.integer(NB_VALD))

# Step 5: Comparison with Norms
# 5.1 Define Normal Weeks
normal_week <- data %>%
  group_by(week = lubridate::week(JOUR)) %>%
  summarize(avg_validation = mean(NB_VALD))

# Assuming you have a week column in your dataset
data_with_week <- data %>%
  mutate(week = lubridate::week(JOUR))

