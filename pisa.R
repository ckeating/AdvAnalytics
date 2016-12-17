setwd("C:/Users/Chibot/Dropbox/edgedata")
pisaTest=read.csv("pisa2009test.csv")
pisaTrain=read.csv("pisa2009train.csv")

summary(pisaTest)
summary(pisaTrain)
str(pisaTest)
str(pisaTrain)

summary(pisaTrain$raceeth)

tapply(pisaTrain$readingScore,pisaTrain$male,mean)


pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)


pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

#build model 

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)


predTest=predict(lmScore,newdata=pisaTest)

summary(predTest)
summary(pisaTrain$readingScore)


SSE= sum(lmScore$residuals^2)
SSEs

#equal to the squared root of the SSE divided by the total number of observations
RMSE=sqrt(SSE/nrow(pisaTrain))
RMSE


##NBA predictions
# #create a model to predict points scores
# PointsReg=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB +TOV + STL + BLK, data=NBA)
# summary(PointsReg)
# 
# PointsReg$residuals
# #compute the Sum of Squared Errors (SSE)
# SSE= sum(PointsReg$residuals^2)
# SSE
# #28,394,314 - not very interpretable quantity
# #calculate the root mean squared error - more interpretable
# # and is more like the average error
# 
# #equal to the squared root of the SSE divided by the total number of observations
# RMSE=sqrt(SSE/nrow(NBA))
# RMSE
# #184.4
# #on average we make an error of 184.4 points
# #this is not that bad
# #when compared to the average number of points in a season
# #184.4 compared to 8,370.24 points in the entire season
# mean(NBA$PTS)
# #8370.24
# summary(PointsReg)




#Let's predict the reading score on the test population
predTest=predict(lmScore,newdata=pisaTest)

# sum of the average scores minus the test actual scores
#SST
SST=sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
SST


#sum of PREDICTED test score - sum of ACTUAL test scores squared and summed
#SSE the sum of squared errors
SSE=sum((predTest-pisaTest$readingScore)^2)


#baseline is just the mean score of the training set:
mean(pisaTrain$readingScore)

#compute R squared
R2=1-(SSE/SST)
R2





pisaTest$readingScore


RMSE=sqrt(SSE/nrow(pisaTest))
RMSE

SSE
SST


RMSE=sqrt(SSE/nrow(NBA_Test))
# a little higher than the training data, but still reasonable
RMSE


#Let's predict the test scores on the test dataset
predTest=predict(lmScore,newdata=pisaTest)
#sum of PREDICTED test score - sum of ACTUAL test scores squared and summed
#SSE
SSE=sum((predTest-pisaTest$readingScore)^2)


# R2 
SST
SSE

R2=1-(SSE/SST)
R2


