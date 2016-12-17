##initialize session
setwd("C:/Users/Chibot/Dropbox/edgedata")
quality=read.csv("quality.csv")
library(caTools)
library(ROCR)
#randomly split data into training and test

set.seed(88)
#used to split dataset into training (75%)
#and testing (25%)
split=sample.split(quality$PoorCare,SplitRatio=0.75)


qualityTrain=subset(quality,split==TRUE)
qualityTest=subset(quality,split==FALSE)


QualityLog=glm(PoorCare ~ OfficeVisits + Narcotics,family=binomial,data=qualityTrain)
predictTrain=predict(QualityLog,type="response")

##

predictTest = predict(QualityLog, type="response", newdata=qualityTest)


ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


odds=-1.5+(3*1)+(-.5*5)
odds
log(odds)


setwd("C:/Users/Chibot/Dropbox/edgedata")

quality=read.csv("quality.csv")

str(quality)
table(quality$PoorCare)

#baseline model has an accuracy of .7480916
98/131


library(caTools)

#randomly split data into training and test

set.seed(88)
#used to split dataset into training (75%)
#and testing (25%)
split=sample.split(quality$PoorCare,SplitRatio=0.75)
split


#create training and testing sets
#create train
qualityTrain=subset(quality,split==TRUE)
qualityTest=subset(quality,split==FALSE)
nrow(qualityTrain)
nrow(qualityTest)

#creates model
QualityLog=glm(PoorCare ~ OfficeVisits + Narcotics,family=binomial,data=qualityTrain)
summary(QualityLog)

#predict values
predictTrain=predict(QualityLog,type="response")
summary(predictTrain)

tapply(predictTrain,qualityTrain$PoorCare,mean)

#creating a confusion matrix where threshold=0.5
#actual outcomes by predicted outcomes
table(qualityTrain$PoorCare,predictTrain>0.5)

#threshold= 0.5
#sensitivity (true positive rate)= true positives/(true positives/false negatives)
sensitivity= 10/(10+15)
sensitivity

#specificity (true negative rate)= true negatives/(true negatives/false positives)
specificity=70/(70+4)
specificity


#creating a confusion matrix where threshold=0.7
#by increasing threshold, sensitivity goes down, specificity goes up
#actual outcomes by predicted outcomes
table(qualityTrain$PoorCare,predictTrain>0.7)

#threshold= 0.7
#sensitivity (true positive rate)= true positives/(true positives/false negatives)
sensitivity= 8/(8+17)
sensitivity

#specificity (true negative rate)= true negatives/(true negatives/false positives)
specificity=73/(73+1)
specificity




#creating a confusion matrix where threshold=0.2
#by increasing threshold, sensitivity goes up, specificity goes down
#actual outcomes by predicted outcomes
table(qualityTrain$PoorCare,predictTrain>0.2)

#threshold= 0.7
#sensitivity (true positive rate)= true positives/(true positives/false negatives)
sensitivity= 16/(16+9)
sensitivity

#specificity (true negative rate)= true negatives/(true negatives/false positives)
specificity=54/(54+20)
specificity



#Roc curve
install.packages("ROCR")

library(ROCR)


ROCRpred=prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-.2,1.7))






  
#first quick question
library(caTools)
set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)


QualityLog = glm(PoorCare ~ StartedOnCombination+ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)

exp(3)
2.71828^3
