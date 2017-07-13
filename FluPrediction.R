FluTrain <- read.csv("FluTrain.csv")
##Looking at the time period 2004-2011, which week corresponds to the highest percentage of ILI-related physician visits?
FluTrain$Week[which.max(FluTrain$ILI)]
##Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain$Week[which.max(FluTrain$Queries)]
##Plot the histogram of the dependent variable, ILI.
hist(FluTrain$ILI)
##Plot the natural logarithm of ILI versus Queries. 
plot(log(FluTrain$ILI), FluTrain$Queries)
##build model
FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
##prediction
FluTest <- read.csv("FluTest.csv")
PredTest1 <- exp(predict(FluTrend1, newdata=FluTest))
##What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
##What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?
(FluTest$ILI[which(FluTest$Week == "2012-03-11 - 2012-03-17")]-PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")])/FluTest$ILI[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
##What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?
RMSE_ILI <- sqrt(mean((PredTest1-FluTest$ILI)^2))
##Using past record to predict current week value
install.packages("zoo")
library(zoo)
ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

##Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable
FluTrend2 <- lm(log(FluTrain$ILI) ~ FluTrain$Queries + log(FluTrain$ILILag2), data = FluTrain)
##add ILILag2 to the FluTest data frame
ILILag2 <- lag(zoo(FluTest$ILI), -2 , na.pad =TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
FluTest$ILILag2[1] <- FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] <- FluTrain$ILI[nrow(FluTrain)]
##What is the test-set RMSE of the FluTrend2 model?
PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))
RMSE_ILI2 <- sqrt(mean((PredTest2 - FluTest$ILI)^2))
