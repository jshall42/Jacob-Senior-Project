# Authors: Jacob Hall, edited by Caleb Smith

# Loading in all libraries needed for the code
library(tidyverse)
library(dplyr)

# Reading in the data
data <- read.csv("train.csv")
data_dict <- read.csv("data_dict.csv")

# Takes the dataframe data and removes all the columns referencing seasons
# and storing it in the dataframe data_no_seas (data without seasons)
data_no_seas <- data %>%
  select(-Basic_Demos.Enroll_Season, -Physical.Season, -Fitness_Endurance.Season,
         -FGC.Season, -BIA.Season, -CGAS.Season, -PAQ_A.Season,
         -PAQ_C.Season, -PCIAT.Season)

# Example usage
exclude_columns <- c("Basic_Demos.Age", "Basic_Demos.Sex", "id")  # Specify columns to exclude

# Select only numeric columns that are not in the exclude_columns list
data_no_outliers <- data %>%
  mutate(
    Age_Group = case_when(
      Basic_Demos.Age >= 13 & Basic_Demos.Age <= 22 ~ "13-22",
      Basic_Demos.Age >= 5 & Basic_Demos.Age <= 12 ~ "5-12",
      TRUE ~ "Other"  # This handles cases outside the defined age groups (e.g., NA or ages > 22)
    )
  ) %>%
  group_by(Age_Group) %>%
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
  )) %>%
  ungroup()

# Combine the modified numeric data with the excluded columns (Age, Sex, ID)
data_no_outliers <- bind_cols(
  data %>% select(all_of(exclude_columns)),  # Add back excluded columns
  data_no_outliers  # Add the modified numeric columns
)

# Data transformation and imputation for missing values
data_no_na <- data_no_outliers %>%
  mutate(
    Age_Group = case_when(
      Basic_Demos.Age >= 5 & Basic_Demos.Age <= 12 ~ "5-12",
      Basic_Demos.Age >= 13 & Basic_Demos.Age <= 22 ~ "13-22",
      TRUE ~ NA_character_  # Any other age group will be NA
    )
  ) %>%
  group_by(Age_Group) %>%
  mutate(across(
    .cols = all_of(data_dict$Name),  # Select columns from data_dict
    .fns = ~ if_else(is.na(.), round(mean(., na.rm = TRUE), 0), .)  # Replace NA with rounded mean
  )) %>%
  ungroup()

# Apply SII categories directly within the mutate function without using a separate function
data_no_na <- data_no_na %>%
  mutate(
    # Assign SII based on PCIAT.PCIAT_Total using case_when
    sii = case_when(
      PCIAT.PCIAT_Total >= 0 & PCIAT.PCIAT_Total <= 30 ~ "0",
      PCIAT.PCIAT_Total >= 31 & PCIAT.PCIAT_Total <= 49 ~ "1",
      PCIAT.PCIAT_Total >= 50 & PCIAT.PCIAT_Total <= 79 ~ "2",
      PCIAT.PCIAT_Total >= 80 & PCIAT.PCIAT_Total <= 100 ~ "3",
      TRUE ~ NA_character_  # In case of any invalid or out-of-range values
    )
  )

# Write the cleaned data to a CSV file
write.csv(data_no_na, "Data_set.csv", row.names = FALSE)

# Updated ggplot with data_no_na as the source
ggplot(data_no_na, aes(x = PreInt_EduHx.computerinternet_hoursday, y = PCIAT.PCIAT_Total)) +
  stat_summary(fun = "mean", geom = "bar", fill = "skyblue") +  # Mean bar plot
  theme_minimal() +
  labs(title = "Bar Plot of Mean PCIAT_TOTAL by InternetUsage",
       x = "InternetUsage", y = "PCIAT_TOTAL")

ggplot(data_no_na, aes(x = PreInt_EduHx.computerinternet_hoursday, y = PCIAT.PCIAT_Total)) +
  geom_jitter(width = 0.2, alpha = 0.6) +  # Jittered points
  theme_minimal() +
  labs(title = "Dot Plot (Jitter) of PCIAT_TOTAL by InternetUsage",
       x = "InternetUsage", y = "PCIAT_TOTAL")

