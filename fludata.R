setwd("C:/Users/Chibot/Dropbox/edgedata")
FluTrain=read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")

str(FluTrain)
summary(FluTrain)
str(FluTest)
summary(FluTest)


FluTrain$Week[which.max(FluTrain$ILI)]

FluTrain$Week[which.max(FluTrain$Queries)]


hist(FluTrain$ILI)

#model to predict the flu related visits (ILI)
FluTrend1=lm(log(ILI)~Queries,data=FluTrain)

summary(FluTrend1)

cor(FluTrain$Queries,log(FluTrain$ILI))


PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

PredTest1

obs=2.2934216
est=2.187378
RE=(obs-est)/obs
RE


#calculated RMSE
#
SSE=sum((PredTest1-FluTest$ILI)^2)
SSE
RMSE=sqrt(SSE/nrow(FluTest))
RMSE



ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
#add variable to training data set
FluTrain$ILILag2 = coredata(ILILag2)

str(FluTrain)
summary(FluTrain)

#plot the new ILILag 2 variable
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))


#train a model using the lag variable
FluTrend2=lm(log(ILI) ~ Queries +log(ILILag2),data=FluTrain)

summary(FluTrend1)
summary(FluTrend2)

#add variable to training data set
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

summary(FluTest)
summary(FluTrain)
str(FluTest)




#set new values in FluTest
FluTest$ILILag2[2]=FluTrain$ILI[417]
FluTest$ILILag2[1]=FluTrain$ILI[416]

tail(FluTrain,5)
head(FluTest,5)



PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

PredTest2



#calculated RMSE
#
SSE_2=sum((PredTest2-FluTest$ILI)^2)
SSE_2
RMSE_2=sqrt(SSE_2/nrow(FluTest))
RMSE_2

SSE
RMSE
SSE_2
RMSE_2



Fluplot(log(FluTrain$ILI),FluTrain$Queries)
plot(FluTrain$Queries,log(FluTrain$ILI))


which.max(FluTrain$ILI)

