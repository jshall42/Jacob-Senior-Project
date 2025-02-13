library(ggplot2)


data_no_na <- read.csv("Data_set.csv")#Reading in the clean data set


lm_multiple <- lm(PCIAT.PCIAT_Total ~ BIA.BIA_ICW, data = data_no_na)#Linear Regression model thae it checking


summary(lm_multiple)$r.squared

# Plot residuals
plot(lm_multiple$residuals)
