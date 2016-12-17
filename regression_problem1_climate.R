setwd("C:/Users/Chibot/Dropbox/edgedata")
Climate=read.csv("climate_change.csv")

summary(Climate )

Climate_train=subset(Climate,Year<=2006)
Climate_test=subset(Climate,Year>2006)

summary(Climate_train)
summary(Climate_test)

Tempreg=lm(Temp ~ MEI+ CO2+ CH4+ N2O+ CFC.11+ CFC.12+ TSI+ Aerosols,data=Climate_train)

summary(Tempreg)


#simple model
Tempreg_2=lm(Temp ~ MEI+ TSI+ Aerosols+N2O,data=Climate_train)
summary(Tempreg_2)

#using step function to build a simple model
Tempreg.simple=step(Tempreg)
summary(Tempreg.simple)

#predict temperature for the testing set based on our simple model
TempPredict = predict(Tempreg.simple,newdata=Climate_test)

##nba version SSE=sum((PointsPredictions-NBA_Test$PTS)^2)
SSE=sum((TempPredict-Climate_test$Temp)^2)

# sum of the average temp minus the test actual temp
SST=sum((mean(Climate_train$Temp)-Climate_test$Temp)^2)
SSE
SST
R2=1-(SSE/SST)
R2

summary(Tempreg.simple)

summary(Climate_train)
summary(Climate_test)



##NBA method
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






#Let's predict how many points will be scored in the 2012-2013 season using our model
TempPredictions=predict(PointsReg4,newdata=NBA_Test)
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
