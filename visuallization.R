# Load the required libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(heatmaply)
library(tidyr)


# Visualization function
visualize_cleaned_data_extended <- function(df) {
  # Create a function to calculate the percentage of missing values in a column
  missing_percentage <- function(x) {
    sum(is.na(x)) / length(x) * 100
  }
  
  # Calculate the percentage of missing values for each column
  missing_values <- sapply(df, missing_percentage)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(column = names(missing_values), percentage = missing_values)
  
  # Create a bar plot for missing values
  plot_missing_values <- ggplot(plot_data, aes(x = column, y = percentage)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
    labs(title = "Percentage of Missing Values in Each Column",
         x = "Columns",
         y = "Percentage Missing") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 100)
  
  # Check for duplicates
  duplicates_exist <- any(duplicated(df))
  
  # Bar plot for the number of instances by Titre
  plot_titre_instances <- ggplot(df, aes(x = CATEGORIE_TITRE)) +
    geom_bar(fill = "skyblue") +
    labs(title = "Number of Instances by Titre",
         x = "Category",
         y = "Number of Instances") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Heatmap for correlations among numeric columns
  numeric_columns <- df %>% select_if(is.numeric)
  correlation_heatmap <- heatmaply(cor(numeric_columns), 
                                   main = "Correlation Heatmap",
                                   fontsize_row = 10, fontsize_col = 10)
  
  # Boxplots for numeric columns
  boxplots_numeric_columns <- numeric_columns %>%
    gather(key = "variable", value = "value") %>%
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Boxplots for Numeric Columns",
         x = "Columns",
         y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Bar plots for categorical columns
  categorical_columns <- df %>% select_if(is.factor)
  barplots_categorical_columns <- lapply(colnames(categorical_columns), function(col) {
    ggplot(df, aes(x = get(col))) +
      geom_bar(fill = "skyblue") +
      labs(title = paste("Bar Plot for", col),
           x = col,
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  return(list(missing_values = plot_missing_values,
              duplicates_exist = duplicates_exist,
              titre_instances = plot_titre_instances,
              correlation_heatmap = correlation_heatmap,
              boxplots_numeric_columns = boxplots_numeric_columns,
              barplots_categorical_columns = barplots_categorical_columns))
}
