#ChatGPT helped building this - Jaden
#Used it for error checking and creating the ggplot for residuals
#Chose only top 7 features to prevent overfitting

install.packages("Metrics")
library(Metrics)
library(ggplot2)

data_no_na <- read.csv("Data_set.csv")#Reading in the data set

top_7_features <- c("Physical.Height", "Basic_Demos.Age", "BIA.BIA_FFM","BIA.BIA_BMR", "BIA.BIA_ICW", "BIA.BIA_LST", "BIA.BIA_LDM") # 7 highest correlating features that are not PCIAT

x_7 <- data_no_na[, top_7_features]
y_7 <- data_no_na$sii

formula <- as.formula(paste("y_7 ~", paste("poly(", colnames(x_7), ", 2)", collapse = " + "))) # Create the polynomial regression formula

model <- lm(formula, data = data_no_na) #polynomial regression model

summary(model) # show summary of model

y_pred <- predict(model, newdata = data_no_na)# Predict using the fitted model

mse <- mse(y_7, y_pred)      # Mean squared error
r2 <- cor(y_7, y_pred)^2    # R^2 Score
mse
r2
#show mse and R^2 score

data_no_na$residuals <- residuals(model) #get residuals of model

# Plot Residuals vs Predicted Values
ggplot(data_no_na, aes(x = y_pred, y = residuals)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Predicted Values",
    x = "Predicted sii",
    y = "Residuals"
  ) +
  theme_minimal()