library(dplyr)
library(stringr)
library(ggplot2)
library(sf)





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
  
  if("lda" %in% names(df))
  {
    names(df)[names(df) == "lda"] <- "ID_REFA_LDA"
  }
  
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
  

  # Types of TITRES:
  unique(df$CATEGORIE_TITRE)
  
  # Number of TITRES:
  df %>% group_by(CATEGORIE_TITRE) %>% summarise(n())
  

  # Remove spaces from the NB_VALD column using str_trim
  df$NB_VALD <- str_trim(df$NB_VALD)
  print(df$NB_VALD)
  
  # Convert 'JOUR' to Date type
  df$JOUR <- as.Date(df$JOUR, format = "%d/%m/%Y")
  str(df$JOUR)
  
  # make ID_REFA_LDA same type : 

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
  
  df$ID_REFA_LDA <- as.character(df$ID_REFA_LDA)
  
  # Sort the dataframe based on the 'JOUR' column in descending order
  df <- arrange(df, desc(JOUR))
  
  return(df)
}



# function to check names of columns if they are identical in all dataframes : 

check_columns <- function(dataframes)
{
  columns_references <- names(dataframes[[1]])
  
  equal <- all(sapply(dataframes,function(dataframe) identical(names(dataframe),columns_references)))
  
  if(equal)
  {
    print("All dataframes have the same columns")
  }
  else{
    print("dataframes have not identical columns names")
  }
}


# read dataset as dataframes : 

df_2017s2 <- read.delim("Data/data-rf-2017/2017_S2_NB_FER.txt")

df_2017s1 <- read.delim("Data/data-rf-2017/2017S1_NB_FER.txt")

df_2018s1 <- read.delim("Data/data-rf-2018/2018_S1_NB_FER.txt")

df_2018s2 <- read.delim("Data/data-rf-2018/2018_S2_NB_Fer.txt")

df_2019s1 <- read.delim("Data/data-rf-2019/2019_S1_NB_FER.txt")

df_2019s2 <- read.delim("Data/data-rf-2019/2019_S2_NB_FER.txt")

df_2020s1 <- read.delim("Data/data-rf-2020/2020_S1_NB_FER.txt")

df_2020s2 <- read.delim("Data/data-rf-2020/2020_S2_NB_FER.txt")

df_2021s1 <- read.delim("Data/data-rf-2021/2021_S1_NB_FER.txt")

df_2021s2 <- read.delim("Data/data-rf-2021/2021_S2_NB_FER.txt")

df_2022s1 <- read.delim("Data/data-rf-2022/2022_S1_NB_FER.txt")

df_2022s2 <- read.delim("Data/data-rf-2022/2022_S2_NB_FER.txt",sep=";",header = TRUE)

df_2023s1 <- read.csv("data/validations-reseau-ferre-nombre-validations-par-jour-1er-semestre.csv", header = TRUE, sep = ";")


# read data arret :

arrets <- st_read("data/REF_ZdA/PL_ZDL_R_11_12_2023.shp")


# process dataframes using clean_and_process script : 

df_2017s1_new <- clean_and_process_data(df_2017s1)

df_2017s2_new <- clean_and_process_data(df_2017s2)

df_2018s1_new <- clean_and_process_data(df_2018s1)

df_2018s2_new <- clean_and_process_data(df_2018s2)


df_2019s1_new <- clean_and_process_data(df_2019s1)

df_2019s2_new <- clean_and_process_data(df_2019s2)


df_2020s1_new <- clean_and_process_data(df_2020s1)


df_2020s2_new <- clean_and_process_data(df_2020s2)


df_2021s1_new <- clean_and_process_data(df_2021s1)

df_2021s2_new <- clean_and_process_data(df_2021s2)


df_2022s1_new <- clean_and_process_data(df_2022s1)

df_2022s2_new <- clean_and_process_data(df_2022s2)

df_2023s1_new <- clean_and_process_data(df_2023s1)

names(arrets)[names(arrets) == "idrefa_lda"] <- "ID_REFA_LDA"

# change type of ID_REFA_LDA to string
arrets <- mutate(arrets, ID_REFA_LDA = as.character(ID_REFA_LDA))

# Combine dataframes
validations <- bind_rows(
      df_2017s1_new,
      df_2017s2_new,
      df_2018s1_new,
     df_2018s2_new,
     df_2019s1_new,
     df_2019s2_new,
     df_2020s1_new,
     df_2020s2_new,
    df_2021s1_new,
     df_2021s2_new,
     df_2022s1_new,
     df_2022s2_new,
     df_2023s1_new
)

# Perform a full join with arrets based on "ID_REFA_LDA"
data <- full_join(validations, arrets, by = "ID_REFA_LDA")