library(dplyr)
library(stringr)
library(ggplot2)

clean_and_process_data <- function(df)
{
  # Display column names
  names(df)
  
  # Display dimensions of the dataframe
  dim(df)
  
  # Display number of columns
  ncol(df)
  
  # Display number of rows
  nrow(df)
  
  # Display structure of the dataframe
  str(df)
  
  # Display summary statistics of the dataframe
  summary(df)
  
  # Display structure of the dataframe again
  str(df)
  
  # Null values in every column
  null_values_per_column <- sapply(df, function(x) sum(is.na(x)))
  null_values_per_column
  
  # Create a function to calculate the percentage of missing values in a column
  missing_percentage <- function(x) {
    sum(is.na(x)) / length(x) * 100
  }
  
  # Calculate the percentage of missing values for each column
  missing_values <- sapply(df, missing_percentage)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(column = names(missing_values), percentage = missing_values)
  
  # Create a boxplot using ggplot2
  ggplot(plot_data, aes(x = column, y = percentage)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
    labs(title = "Percentage of Missing Values in Each Column",
         x = "Columns",
         y = "Percentage Missing") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 100)
  
  # Check for duplicates
  any(duplicated(df))
  
  # Check the total number of duplicate rows
  num_duplicates <- sum(duplicated(df))
  print(num_duplicates)
  
  # Columns that should be int not char: CODE_STIF_RES / CODE_STIF_ARRET / ID_REFA_LDA / NB_VALD
  
  # Types of TITRES:
  unique(df$CATEGORIE_TITRE)
  
  # Number of TITRES:
  df %>% group_by(CATEGORIE_TITRE) %>% summarise(n())
  
  # Plot the number of instances by Titre
  valeurs <- table(df$CATEGORIE_TITRE)
  categories <- names(valeurs)
  print(valeurs)
  print(categories)
  bar_colors <- rainbow(length(categories))
  barplot(valeurs, names.arg = categories, col = bar_colors, main = "Number of Instances by Titre", xlab = "Category", ylab = "Number of Instances")
  
  # Remove spaces from the NB_VALD column using str_trim
  df$NB_VALD <- str_trim(df$NB_VALD)
  print(df$NB_VALD)
  
  # Convert 'JOUR' to Date type
  df$JOUR <- as.Date(df$JOUR, format = "%d/%m/%Y")
  str(df$JOUR)
  
  # Identify outliers using IQR
  identify_outliers <- function(df, threshold = 1.5) {
    outliers <- numeric()
    for (col in colnames(df)) {
      if (is.numeric(df[[col]])) {
        q1 <- quantile(df[[col]], 0.25)
        q3 <- quantile(df[[col]], 0.75)
        iqr <- q3 - q1
        lower_bound <- q1 - threshold * iqr
        upper_bound <- q3 + threshold * iqr
        
        # Identify outliers
        outliers <- c(outliers, which(df[[col]] < lower_bound | df[[col]] > upper_bound))
      }
    }
    unique(outliers)
  }
  
  # Identify outliers using IQR
  outliers_indices <- identify_outliers(df)
  
  # Display rows with outliers
  outliers_data <- df[outliers_indices, ]
  
  # Print the indices of rows with outliers
  print(outliers_indices)
  
  # Print the rows with outliers
  print(outliers_data)
  
  # Remove outliers
  df_no_outliers <- df[-outliers_indices, ]
  
  # Update the dataframe
  df <- df_no_outliers
  
  # Display unique values in the NB_VALD column
  unique(df$NB_VALD)
  
  # Replace "Moins de 5" with the integer value 3 in the 'NB_VALD' column (3 is the median in this case)
  if ("Moins de 5" %in% df$NB_VALD) {
    df$NB_VALD <- ifelse(df$NB_VALD == "Moins de 5", 3, df$NB_VALD)
  }
  
  # Check if "?" exists in CATEGORIE_TITRE and replace with the mode value
  if ("?" %in% df$CATEGORIE_TITRE) {
    
    # Replace "?" with the mode value
    df$CATEGORIE_TITRE <- gsub("\\?", "NON DEFINI", df$CATEGORIE_TITRE)
  }
  
  if("?" %in% df$ID_REFA_LDA | "" %in% df$ID_REFA_LDA)
  {
    df$ID_REFA_LDA[df$ID_REFA_LDA %in% c("","?")] <- "UNKNOWN"
  }
  
  # Sort the dataframe based on the 'JOUR' column in descending order
  df <- arrange(df, desc(JOUR))
  
  return(df)
}


# # Example usage for each dataset
# df_2017s2 <- read.delim("Data/data-rf-2017/2017_S2_NB_FER.txt")
# df_2017s2 <- clean_and_process_data(df_2017s2)

