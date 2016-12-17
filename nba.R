
setwd("C:/Users/Chibot/Dropbox/edgedata")
setwd("C:/Users/Craig/Dropbox/edgedata")
NBA=read.csv("NBA_train.csv")

str(NBA)


table(NBA$W,NBA$Playoffs)


NBA$PTSDiff=NBA$PTS-NBA$oppPTS

plot(NBA$PTSDiff,NBA$W)


#strong correlation between points differential and wins
#create a model 

WinsReg=lm(NBA$W ~ NBA$PTSDiff,data=NBA)

summary(WinsReg)


#create a model to predict points scores
PointsReg=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB +TOV + STL + BLK, data=NBA)
summary(PointsReg)

PointsReg$residuals
#compute the Sum of Squared Errors (SSE)
SSE= sum(PointsReg$residuals^2)
SSE
#28,394,314 - not very interpretable quantity
#calculate the root mean squared error - more interpretable
# and is more like the average error

#equal to the squared root of the SSE divided by the total number of observations
RMSE=sqrt(SSE/nrow(NBA))
RMSE
#184.4
#on average we make an error of 184.4 points
#this is not that bad
#when compared to the average number of points in a season
#184.4 compared to 8,370.24 points in the entire season
mean(NBA$PTS)
#8370.24
summary(PointsReg)


#create a model WITHOUT the statistically insignificant variable turnovers (TOV)
PointsReg2=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
summary(PointsReg2)


#now take out next statistically significant variable DRB
PointsReg3=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=NBA)
summary(PointsReg3)

#take out blocks also
PointsReg4=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(PointsReg4)

#compute the Sum of Squared Errors (SSE) on new model
SSE_4= sum(PointsReg4$residuals^2)
SSE_4

#equal to the squared root of the SSE divided by the total number of observations
RMSE_4=sqrt(SSE_4/nrow(NBA))
RMSE_4
#root mean squared error difference between models are very small
#which means we kept the same amount of error
#original model RMSE:
RMSE


#making predictions:
# read in test data set
NBA_Test=read.csv("NBA_test.csv")
str(NBA_Test)
summary(NBA_Test)


#Let's predict how many points will be scored in the 2012-2013 season using our model
PointsPredictions=predict(PointsReg4,newdata=NBA_Test)
#sum of PREDICTED amount - sum of ACTUAL points squared and summed
SSE=sum((PointsPredictions-NBA_Test$PTS)^2)
# sum of the average number of points minus the test actual number of points
SST=sum((mean(NBA$PTS)-NBA_Test$PTS)^2)
SSE
SST
R2=1-(SSE/SST)
R2

RMSE=sqrt(SSE/nrow(NBA_Test))
# a little higher than the training data, but still reasonable
RMSE

