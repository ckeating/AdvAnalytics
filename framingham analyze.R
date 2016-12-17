##initialize session
#setwd("C:/Users/Chibot/Dropbox/edgedata")
setwd("C:/Users/Craig/Dropbox/edgedata")
Framingham=read.csv("framingham.csv")
library(caTools)
library(ROCR)
#randomly split data into training and test

str(Framingham)
summary(Framingham)



set.seed(1000)
#used to split dataset into training (75%)
#and testing (25%)
#outcome variable is first argument
#second variable is % of data in training set
split=sample.split(Framingham$TenYearCHD,SplitRatio=0.65)


train=subset(Framingham,split==TRUE)
test=subset(Framingham,split==FALSE)


framinghamLog=glm(TenYearCHD ~ .,data=train,family=binomial)

summary(framinghamLog)

predictTest=predict(framinghamLog,type="response",newdata=test)

#confusion matrix
table(test$TenYearCHD,predictTest>0.5)

#predict out of sample AUC
library(ROCR)
ROCRpred=(prediction(predictTest,test$TenYearCHD))
as.numeric(performance(ROCRpred,"auc")@y.values)

