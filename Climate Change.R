data <- read.csv("climate_change.csv")
training_data <- subset(data, Year <= 2006)
test_data <- subset(data, Year > 2006)
training_model <- lm(Temp ~ MEI + CO2 + CH4 +N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training_data)

##Compute the correlations between all the variables in the training set. Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)
cor(training_data$N2O, training_data$MEI)
cor(training_data$N2O, training_data$CO2)
cor(training_data$N2O, training_data$CH4)
cor(training_data$N2O, training_data$CFC.11)
cor(training_data$N2O, training_data$CFC.12)
cor(training_data$N2O, training_data$Aerosols)
cor(training_data$N2O, training_data$TSI)

##reduce model
training_model1 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = training_data)

##step model
new_training_model <- step(training_model)

##prediction
PredictionData <- predict(new_training_model, newdata = test_data)
SSE <- sum((PredictionData-test_data$Temp)^2)
SST <- sum((test_data$Temp-mean(training_data$Temp))^2)
R <- 1 - SSE/SST
R
