NBA <- read.csv("NBA_train.csv")
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg <- lm(W ~ PTSdiff, data = NBA)

#result: w = 41 + 0.0326*PTSdiff
#PTSdiff >= (42-41)/0.0326 
#PTSdiff >= 31 to win 42 games in season

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data =NBA)
SSE <- sum(PointsReg$residuals^2)
RMSE <- sqrt(SSE/nrow(NBA))
mean(NBA$PTS)
##NOT BAD
##Remodel
PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data =NBA)
PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data =NBA)
PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data =NBA)

SSE_4 <- sum(PointsReg4$residuals^2)
RMSE_4 <- sqrt(SSE_4/nrow(NBA))

##Prediction
NBA_test = read.csv("NBA_test.csv")
PointsPredictions <- predict(PointsReg4, newdata = NBA_test)
SEE <- sum((PointsPredictions - NBA_test$PTS)^2)
SST <- sum((mean(NBA$PTS)- NBA_test$PTS)^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(NBA_test))
