pisaTrain <- read.csv("pisa2009train.csv") 
pisaTest <- read.csv("pisa2009test.csv")

##what is the average reading test score of males?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

##Which variables are missing data in at least one observation in the training set? 
summary(pisaTrain)

##change the reference level
pisaTrain$raceeth <- relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth, "White")

##building model
lmScore <- lm(readingScore ~., data = pisaTrain)
RMSE <- sqrt(mean(lmScore$residuals^2))

##prediction
predTest <- predict(lmScore, newdata = pisaTest)
SSE <- sum((predTest - pisaTest$readingScore)^2)
RMSE <- sqrt(SSE/nrow(pisaTest))

##test-set R-squared value of lmScore
SST <- sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
R <- 1 - SSE/SST