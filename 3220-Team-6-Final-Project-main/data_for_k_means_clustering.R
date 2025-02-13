# File: data_for_k_means_clustering.R
# Author: Caleb Smith
# install.packages("tidyverse")
library(tidyverse)

colors <- c("#1f77b4","#ff7f0e", "#2ca02c", "#d62728", 
            "#9467bd","#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

data <- read.csv("train.csv")
data_dict <- read.csv("data_dict.csv")

data_dict_no_sii <- as.data.frame(data_dict[-68,])
colnames(data_dict_no_sii) <- c("Name")
data_dict_new <- as.data.frame(data_dict_no_sii[-c(44:64),])
colnames(data_dict_new) <- c("Name")


# Function to remove outliers based on IQR, excluding specified columns
remove_outliers_iqr_exclude <- function(data, exclude_columns) {
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
    )) %>% ungroup()
  
  # Combine the modified numeric data with the excluded columns (Age, Sex, ID)
  data_no_outliers <- bind_cols(
    data %>% select(all_of(exclude_columns)),  # Add back excluded columns
    data_no_outliers  # Add the modified numeric columns
  )
  
  return(data_no_outliers)
}

exclude_columns <- c("Basic_Demos.Age", "Basic_Demos.Sex", "id")  # Specify columns to exclude
data_no_outliers <- remove_outliers_iqr_exclude(data, exclude_columns)
# data_no_sii_no_outliers <- remove_outliers_iqr_exclude(data_no_sii, exclude_columns)

data_no_sii_no_outliers <- data_no_outliers %>%
  select(-sii, -PCIAT.PCIAT_01, -PCIAT.PCIAT_02, -PCIAT.PCIAT_03, 
         -PCIAT.PCIAT_04, -PCIAT.PCIAT_05, -PCIAT.PCIAT_06, -PCIAT.PCIAT_07, -PCIAT.PCIAT_08, 
         -PCIAT.PCIAT_09, -PCIAT.PCIAT_10, -PCIAT.PCIAT_11, -PCIAT.PCIAT_12, -PCIAT.PCIAT_13,
         -PCIAT.PCIAT_14, -PCIAT.PCIAT_15, -PCIAT.PCIAT_16, -PCIAT.PCIAT_17, -PCIAT.PCIAT_18,
         -PCIAT.PCIAT_19, -PCIAT.PCIAT_20, -PCIAT.PCIAT_Total)

data_no_na <- data_no_sii_no_outliers %>%
  mutate(
    Age_Group = case_when(
      Basic_Demos.Age >= 13 & Basic_Demos.Age <= 22 ~ "13-22",
      Basic_Demos.Age >= 5 & Basic_Demos.Age <= 12 ~ "5-12",
      TRUE ~ "Other"  # This handles cases outside the defined age groups (e.g., NA or ages > 22)
    )
  ) %>%
  group_by(Age_Group) %>%
  mutate(across(
    .cols = all_of(data_dict_new$Name),  
    .fns = ~ if_else(is.na(.), round(mean(., na.rm = TRUE), ), .)
  )) %>%
  ungroup()

data_no_na <- select(data_no_na, -Age_Group, -Fitness_Endurance.Max_Stage, -Fitness_Endurance.Time_Mins, -Fitness_Endurance.Time_Sec, -PAQ_A.PAQ_A_Total, -PAQ_C.PAQ_C_Total)

# data_no_na$cluster <-as.factor(kmModel$cluster)

for (x in colnames(data_no_na)) {
  if (!is.numeric(data_no_na[[x]])) {
    data_no_na <- data_no_na %>% select(-x)
  }
}

# is.nan(data_no_na_no_outliers) # May not need this

set.seed(123)
WCSS <- sapply(1:5, function(k) kmeans(data_no_na,k)$tot.withinss)
WCSS

# Plot the WCSS for each cluster
ggplot() +
  geom_point(aes(x = 1:5, y = WCSS), cex = 4) +
  geom_line(aes(x = 1:5, y = WCSS)) +
  labs(x = "Number of clusters (k)", 
       y = "Within-cluster sum of squares (WCSS)") +
  theme(text = element_text(size = 20))

set.seed(123)
# 4 clusters is ideal since there are 4 different sii values
kmModel <- kmeans(data_no_na, 4)
kmModel

data_no_na$sii <- data$sii

data_no_na$cluster <- as.factor(kmModel$cluster)

kmModel$tot.withinss

getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# class(data_no_na$sii)

# data_no_na_backup <- data_no_na

# getmode(data_no_na_backup[data_no_na_backup$cluster == 3, 44])

# data_no_na_backup[data_no_na_backup$cluster == 3, 44]

data_no_na <- data_no_na %>%
  group_by(cluster) %>%
  mutate(across(
    .cols = sii,  
    .fns = ~ if_else(is.na(.), getmode(.), .)
  )) %>%
  ungroup()

# class(data_no_na[["sii"]])

# data_no_na[data_no_na$sii == 3, 45]
# data_no_na[data_no_na$sii == 2, 45]

#Plot the clusters and centroids
data_no_na |> 
  ggplot(aes(x=BIA.BIA_LDM, y=BIA.BIA_LST)) +
  geom_point(aes(color = cluster, shape = cluster), cex=4) + 
  geom_point(data=kmCenters, aes(x=BIA.BIA_LDM, y=BIA.BIA_LST), 
             color="black", cex=10, shape = 18) +
  labs(x='BIA.BIA_LDM', y='BIA.BIA_LST') +
  theme(legend.position="none") +
  scale_color_manual(values = colors)

kmCenters <- kmModel$centers

data_no_na_backup3 <- data_no_na

summary(data_no_na)

data_no_na$sii <- as.factor(data_no_na$sii)

summary(data_no_na)

data <- read.csv("train.csv")
# Written by ChatGPT, modified by Caleb
# Function to add missing columns from df2 to df1
add_missing_columns <- function(df1, df2) {
  # Loop through each column name in df2
  for (col_name in colnames(df2)) {
    # Check if the column is missing in df1
    if (!(col_name %in% colnames(df1))) {
      # Add the missing column to df1, filled with NA
      df1[[col_name]] <- df2[[col_name]]
    }
  }
  return(df1)
}

# Apply the function
data_no_na <- add_missing_columns(data_no_na, data)

# Check the updated df1
print(data_no_na)

# #ChatGPT Helped build-Jacob
# # Load necessary libraries
# library(tidyverse)
# library(dplyr)
# library(tidymodels)
# library(ranger)
# library(themis)
# 
# # Reading in the data
# # data <- read.csv("updated_data.csv")
# 
# # Remove the columns that are used to derived sii 
# # data <- data |> select(-c("PCIAT.PCIAT_01", "PCIAT.PCIAT_02", "PCIAT.PCIAT_03", "PCIAT.PCIAT_04", 
# #                           "PCIAT.PCIAT_05", "PCIAT.PCIAT_06", "PCIAT.PCIAT_07", "PCIAT.PCIAT_08",    
# #                           "PCIAT.PCIAT_09", "PCIAT.PCIAT_10", "PCIAT.PCIAT_11", "PCIAT.PCIAT_12", 
# #                           "PCIAT.PCIAT_13", "PCIAT.PCIAT_14", "PCIAT.PCIAT_15", "PCIAT.PCIAT_16",    
# #                           "PCIAT.PCIAT_17", "PCIAT.PCIAT_18", "PCIAT.PCIAT_19", "PCIAT.PCIAT_20",    
# #                           "PCIAT.PCIAT_Total"))
# 
# # Balance classes by resampling using SMOTE (for oversampling)
# # data <- data %>%
# #   mutate(sii = factor(sii, levels = c(0, 1, 2, 3), labels = c("none", "mild", "moderate", "severe")))
# 
# # Create training and testing data using initial_split()
# set.seed(8675309)
# split <- initial_split(data_no_na, prop = 0.70)
# training_data <- training(split)
# testing_data <- testing(split)
# 
# summary(training_data)
# 
# # Balance the training set by oversampling the minority classes
# data_recipe <- recipe(sii ~ ., data = training_data) %>%
#   step_impute_mean(all_numeric_predictors()) %>%  # Impute missing values for numeric predictors
#   step_novel(all_nominal_predictors()) %>%  # Handle unseen factor levels in the test data
#   step_dummy(all_nominal_predictors()) %>%  # Convert categorical features to dummy variables
#   step_smote(sii)  # Oversample minority classes using SMOTE (Synthetic Minority Oversampling)
# 
# # Specify the Random Forest model
# random_forest_model <- rand_forest(mode = "classification", engine = "ranger", mtry = 9, min_n = 8)
# 
# # Define the workflow
# data_work_flow <- workflow() %>%
#   add_model(random_forest_model) %>%
#   add_recipe(data_recipe)
# 
# # Fit the model
# random_forest_model_fit <- data_work_flow %>%
#   fit(data = training_data)
# 
# # Use the augment() function to add predictions to the testing data
# testing_data_augmented <- augment(random_forest_model_fit, new_data = testing_data)
# 
# # View the augmented data with predictions added
# testing_data_augmented %>%
#   select(.pred_class, sii) %>%
#   head()
# 
# # Calculate the confusion matrix
# cm <- conf_mat(testing_data_augmented, truth = sii, estimate = .pred_class)
# cm
# 
# # Plot confusion matrix
# cm %>%
#   autoplot(type = "heatmap")
# 
# # Inspect misclassified predictions
# misclassified <- testing_data_augmented %>%
#   filter(.pred_class != sii)
# 
# nrow(testing_data_augmented)
# nrow(misclassified)
# 
# # Check the distribution of classes in the training data after resampling
# table(training_data$sii) 
# 
# data_no_na_copy <- data_no_na
# kmModelCopy <- kmModel
# random_forest_model_copy <- random_forest_model
# random_forest_model_fit_copy <- random_forest_model_fit
# 
