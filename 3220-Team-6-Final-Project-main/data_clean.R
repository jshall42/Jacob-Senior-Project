#Written by Jacob 
#Loading in all libraries needed for the code
library(tidyverse)
library(dplyr)
#Reading in the data
data <- read.csv("train.csv")
data_dict <- read.csv("data_dict.csv")

#Takes the dataframe data and removes all the columns refrencing seasons
#and storing it in the dataframe data_no_seas(data no seasons)
data_no_seas <- data %>%
  select(-Basic_Demos.Enroll_Season, -Physical.Season, -Fitness_Endurance.Season,
         -FGC.Season, -BIA.Season, -CGAS.Season, -PAQ_A.Season,
         -PAQ_C.Season, -PCIAT.Season)

# Function to remove outliers based on IQR, excluding specified columns
remove_outliers_iqr_exclude <- function(data, exclude_columns) {
  # Select only numeric columns that are not in the exclude_columns list
  data_no_outliers <- data %>%
    select(-all_of(exclude_columns)) %>%  # Exclude specified columns
    select(where(is.numeric)) %>%          # Select only numeric columns
    mutate(across(
      .fns = ~ {
        # Calculate the IQR, Q1, Q3
        Q1 <- quantile(., 0.25, na.rm = TRUE)
        Q3 <- quantile(., 0.75, na.rm = TRUE)
        IQR_value <- IQR(., na.rm = TRUE)
        
        # Define the lower and upper bounds for outliers
        lower_bound <- Q1 - 1.5 * IQR_value
        upper_bound <- Q3 + 1.5 * IQR_value
        
        # Replace outliers with NA
        ifelse(. < lower_bound | . > upper_bound, NA, .)
      }
    ))
  
  # Combine the modified numeric data with the excluded columns (Age, Sex, ID)
  data_no_outliers <- bind_cols(
    data %>% select(all_of(exclude_columns)),  # Add back excluded columns
    data_no_outliers  # Add the modified numeric columns
  )
  
  return(data_no_outliers)
}

# Example usage
exclude_columns <- c("Basic_Demos.Age", "Basic_Demos.Sex", "id")  # Specify columns to exclude
data_no_outliers <- remove_outliers_iqr_exclude(data, exclude_columns)

# Select only numeric columns that are not in the exclude_columns list
data_no_outliers <- data %>%
  select(-all_of(exclude_columns)) %>%  # Exclude specified columns
  select(where(is.numeric)) %>%          # Select only numeric columns
  mutate(across(
    .fns = ~ {
      # Calculate the IQR, Q1, Q3
      Q1 <- quantile(., 0.25, na.rm = TRUE)
      Q3 <- quantile(., 0.75, na.rm = TRUE)
      IQR_value <- IQR(., na.rm = TRUE)
      
      # Define the lower and upper bounds for outliers
      lower_bound <- Q1 - 1.5 * IQR_value
      upper_bound <- Q3 + 1.5 * IQR_value
      
      # Replace outliers with NA
      ifelse(. < lower_bound | . > upper_bound, NA, .)
    }
  ))

# Combine the modified numeric data with the excluded columns (Age, Sex, ID)
data_no_outliers <- bind_cols(
  data %>% select(all_of(exclude_columns)),  # Add back excluded columns
  data_no_outliers  # Add the modified numeric columns
)

#Takes the dataframe data_no_seas and takes the means of all columns
#grouped by age to produce a data set with no NA's data_no_na
data_no_na <- data_no_outliers %>%
  mutate(
    Age_Group = case_when(
      Basic_Demos.Age >= 13 & Basic_Demos.Age <= 22 ~ "13-22",
      Basic_Demos.Age >= 5 & Basic_Demos.Age <= 12 ~ "5-12",
      TRUE ~ "Other"  # This handles cases outside the defined age groups (e.g., NA or ages > 22)
    )
  ) %>%
  group_by(Age_Group) %>%
  mutate(across(
    .cols = all_of(data_dict$Name),  
    .fns = ~ if_else(is.na(.), round(mean(., na.rm = TRUE), ), .)
  )) %>%
  ungroup()

ggplot(data, aes(x = data_no_na$PreInt_EduHx.computerinternet_hoursday, y = data_no_na$PCIAT.PCIAT_Total)) +
  stat_summary(fun = "mean", geom = "bar", fill = "skyblue") +  # Mean bar plot
  theme_minimal() +
  labs(title = "Bar Plot of Mean PCIAT_TOTAL by InternetUsage",
       x = "InternetUsasage", y = "PCIAT_TOTAL")

ggplot(data, aes(x = data_no_na$PreInt_EduHx.computerinternet_hoursday, y = data_no_na$PCIAT.PCIAT_Total)) +
  geom_jitter(width = 0.2, alpha = 0.6) +  # Jittered points
  theme_minimal() +
  labs(title = "Dot Plot (Jitter) of PCIAT_TOTAL by InternetUsage",
       x = "InternetUsage", y = "PCIAT_TOTAL")
