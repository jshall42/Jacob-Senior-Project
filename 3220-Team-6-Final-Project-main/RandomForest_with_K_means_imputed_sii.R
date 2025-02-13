#ChatGPT helped build this-Jacob
#It was mostly me trying things then checking with ChatGPT to see why
#I was getting errors
#Modified to work with the data that has sii imputed by k means clustering-Caleb

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(tidymodels)
library(ranger)
library(themis)

# Reading in the data
data <- read.csv("updated_data.csv")

# Balance classes by resampling using SMOTE (for oversampling)
data_no_na <- data_no_na %>%
  mutate(sii = factor(sii, levels = c(0, 1, 2, 3), labels = c("none", "mild", "moderate", "severe")))

# Create training and testing data using initial_split()
set.seed(8675309)
split <- initial_split(data_no_na, prop = 0.70)
training_data <- training(split)
testing_data <- testing(split)

# Balance the training set by oversampling the minority classes
data_recipe <- recipe(sii ~ ., data = training_data) %>%
  step_impute_mean(all_numeric_predictors()) %>%  # Impute missing values for numeric predictors
  step_novel(all_nominal_predictors()) %>%  # Handle unseen factor levels in the test data
  step_dummy(all_nominal_predictors()) %>%  # Convert categorical features to dummy variables
  step_smote(sii)  # Oversample minority classes using SMOTE (Synthetic Minority Oversampling)

# Specify the Random Forest model
random_forest_model <- rand_forest(mode = "classification", engine = "ranger", mtry = 9, min_n = 8)

# Define the workflow
data_work_flow <- workflow() %>%
  add_model(random_forest_model) %>%
  add_recipe(data_recipe)

# Fit the model
random_forest_model_fit <- data_work_flow %>%
  fit(data = training_data)

# Use the `augment()` function to add predictions to the testing data
testing_data_augmented <- augment(random_forest_model_fit, new_data = testing_data)

# View the augmented data with predictions added
testing_data_augmented %>%
  select(.pred_class, sii) %>%
  head()

# Calculate the confusion matrix
cm <- conf_mat(testing_data_augmented, truth = sii, estimate = .pred_class)
cm

# Plot confusion matrix
cm %>%
  autoplot(type = "heatmap")

# Inspect misclassified predictions
misclassified <- testing_data_augmented %>%
  filter(.pred_class != sii)

nrow(testing_data_augmented)
nrow(misclassified)

# Check the distribution of classes in the training data after resampling
table(training_data$sii)
