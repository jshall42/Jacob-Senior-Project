# Daniel Rodriguez - used zybooks chapter in random forest to help build the model
# Used ChatGPT to help with errors I was getting in recipe() and workflow() functions

#Loading in all libraries needed for the code
library(tidyverse)
library(dplyr)
library(tidymodels)

#Reading in the data
data <- read.csv("Data_set.csv")
data_dict <- read.csv("data_dictionary.csv")


# Remove the columns that are used to derived sii and remove Age_Group which does not provide meaning  
data <- data |> select(-c("PCIAT.PCIAT_01", "PCIAT.PCIAT_02", "PCIAT.PCIAT_03", "PCIAT.PCIAT_04", 
                          "PCIAT.PCIAT_05", "PCIAT.PCIAT_06", "PCIAT.PCIAT_07", "PCIAT.PCIAT_08",    
                          "PCIAT.PCIAT_09", "PCIAT.PCIAT_10", "PCIAT.PCIAT_11", "PCIAT.PCIAT_12", 
                          "PCIAT.PCIAT_13", "PCIAT.PCIAT_14", "PCIAT.PCIAT_15", "PCIAT.PCIAT_16",    
                          "PCIAT.PCIAT_17", "PCIAT.PCIAT_18", "PCIAT.PCIAT_19", "PCIAT.PCIAT_20",    
                          "PCIAT.PCIAT_Total", "Age_Group"))

# Converting numeric representations into descriptive categories
data$Basic_Demos.Sex <- factor(data$Basic_Demos.Sex, levels=c(0, 1), labels=c("male", "female"))
data$FGC.FGC_CU_Zone <- factor(data$FGC.FGC_CU_Zone, levels=c(0, 1), labels=c("Improvement", "Healthy Fitness Zone"))
data$FGC.FGC_GSND_Zone <- factor(data$FGC.FGC_GSND_Zone, levels=c(1,2,3), labels=c("Weak", "Normal", "Strong"))
data$FGC.FGC_GSD_Zone <- factor(data$FGC.FGC_GSD_Zone, levels=c(1,2,3), labels=c("Weak", "Normal", "Strong"))
data$FGC.FGC_PU_Zone <- factor(data$FGC.FGC_PU_Zone, levels=c(0, 1), labels=c("Improvement", "Healthy Fitness Zone"))
data$FGC.FGC_SRL_Zone <- factor(data$FGC.FGC_SRL_Zone, levels=c(0, 1), labels=c("Improvement", "Healthy Fitness Zone"))
data$FGC.FGC_SRR_Zone <- factor(data$FGC.FGC_SRR_Zone, levels=c(0, 1), labels=c("Improvement", "Healthy Fitness Zone"))
data$FGC.FGC_TL_Zone <- factor(data$FGC.FGC_TL_Zone, levels=c(0, 1), labels=c("Improvement", "Healthy Fitness Zone"))
data$BIA.BIA_Activity_Level_num <- factor(data$BIA.BIA_Activity_Level_num, levels = c(1, 2, 3, 4, 5)
                                          , labels = c("very light", "light", "moderate", "heavy", "exceptional"))
data$BIA.BIA_Frame_num <- factor(data$BIA.BIA_Frame_num, levels=c(1,2,3), labels=c("Small", "Medium", "Large"))
data$PreInt_EduHx.computerinternet_hoursday <- factor(data$PreInt_EduHx.computerinternet_hoursday, levels = c(0, 1, 2, 3),
                                                      labels = c("less than 1hr/day", "around 1hr/day", "around 2hr/day", "more than 3hr/day" ))
data$sii <- factor(data$sii, levels = c(0, 1, 2), labels=c("none", "mild", "moderate"))

# Set a seed so samples are the same each time the code runs
set.seed(8675309)

# Create training and testing data using initial_split()
# Select instances for a 70% training 30% testing split
split <- initial_split(data, prop=0.70)

# Assign training instances to training_data
training_data <- training(split)

# Assign testing instances to testing_data
testing_data <- testing(split)

# Specify input and output features using recipe()

# Species = output feature, all other features are inputs
data_recipe <- recipe(sii ~ ., data = training_data) |>
  step_impute_mean(all_numeric_predictors()) |>  # Step to impute missing values in numeric predictors with the mean
  step_novel(all_nominal_predictors()) |>  # Handle unseen factor levels in test data
  step_dummy(all_nominal_predictors()) # Convert categorical (nominal) features to dummy variables

# Initialize random forest model using rand_forest()
random_forest_Model <- rand_forest(mode = "classification",
                                   engine = "randomForest",
                                   mtry = 9,
                                   min_n = 8)

random_forest_Model

# Define the workflow (put model and recipe together)
data_work_flow <- workflow() |>
  add_model(random_forest_Model) |>
  add_recipe(data_recipe)

data_work_flow

# Fit the model
random_forest_Model_fit <- data_work_flow |>
  fit(data = training_data)

random_forest_Model_fit

# Use the `augment()` function to add predictions to testing data
testing_data_augmented <- augment(random_forest_Model_fit, new_data = testing_data)

# View the augmented data with predictions added
head(testing_data_augmented)

# Only view prediction class, sii, and id
testing_data_augmented |> select(.pred_class, .pred_none, .pred_mild, .pred_moderate, sii, id) |>
head()

# Calculate confusion matrix
cm <- conf_mat(testing_data_augmented, truth = sii, estimate = .pred_class)
cm

# Plot confusion matrix
cm |>
  autoplot(type = "heatmap")

