# KAGGLE COMPETITION - GETTING STARTED
# want to do:
# dataset
# 1- add a factor variable for age range
# 

# 1:
# impute the most important variables shown by logisitcal regression
# rerun logisitic regression on this data set
# measure accuracy
# 2:
#run a CART model - most important variables - look up function to show most predicted
# impute the tree most important variables then run a CART model on this
# 3:
# run a random forest model - most important variables - look up function to show most predicted
# impute the tree most important variables then run a CART model on this
# 4: run a random forest model


#1. impute most important variables rerun logistic regression


#relevant questions:
Q98197
Q100562
Q108754
Q109244
Q109367
Q113181
Q115611
Q120379

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.

library(gmodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(caret)
library(caTools)
library(ROCR)
library(mice)


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer
setwd("C:/Users/Chibot/Dropbox/edgedata")

test = read.csv("test2016.csv")
train = read.csv("train2016.csv")

setwd("C:/Users/Chibot/Dropbox/edgedata")
train = read.csv("train2016.csv",na.string=c("","NA"))

train2 = read.csv("train2016.csv")
test = read.csv("test2016.csv",na.string=c("","NA"))


#basic data analysis
table(train$Income,train$EducationLevel)

summary(train)

require(Amelia)
missmap(train, main="Voting training data", 
        col=c("yellow", "black"), legend=FALSE)


barplot(table(train$Party),
          main="Party", col="black")

library(vcd)

mosaicplot(train$HouseholdStatus ~ train$Party,
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

#impute entire train dataset
# 
# #create data frame consisting only of the variables with missing values
 simple.train=train[c("YOB","Gender","Income","HouseholdStatus","Q124742","Q121699","Q121700","Q120194","Q118232","Q116197","Q115611","Q114517","Q114386","Q113181","Q112478","Q112270","Q108950","Q109244","Q108342","Q101596","Q100689","Q99716","Q99581","Q98869","Q98578","Q98059","Q98197")]
 simple.test=test[c("YOB","Gender","Income","HouseholdStatus","Q124742","Q121699","Q121700","Q120194","Q118232","Q116197","Q115611","Q114517","Q114386","Q113181","Q112478","Q112270","Q108950","Q109244","Q108342","Q101596","Q100689","Q99716","Q99581","Q98869","Q98578","Q98059","Q98197")]
# 
# summary(simple)
# 
 set.seed(144)
 imputed.train=complete(mice(train))
 imputed.test=complete(mice(test))
 
 # save(imputed.train,file="imputed.train.rda")
 # save(imputed.test,file="imputed.test.rda")
 # 
 
 load(imputed.train,file="imputed.train.rda")
 load (imputed.test,file="imputed.test.rda")
 #simple logistic regression
 simplelog=glm(Party ~ .,data=vote.train,family=binomial) 
   
  summary(simplelog)

  
  # And then make predictions on the training set:
  pred.simplelog= predict(simplelog, newdata=vote.test , type="response")
  
  
  threshold = 0.5
  pred.simplelog
  
  confusionMatrix(pred.simplelog,vote.test$Party)
  
  #pred.test.logsig
  
  PredTestLabels = as.factor(ifelse(pred.simplelog<threshold, "Democrat", "Republican"))

  
  
    
  PredTestLabels
  # However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.
  
  # Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):
  
  MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
  
  write.csv(MySubmission, "imputesimpleLog.csv", row.names=FALSE)
  
  
  
   
 
 train$YOB=imputed.train$YOB
 train$Gender=imputed.train$Gender
 train$Income=imputed.train$Income
 train$HouseholdStatus=imputed.train$HouseholdStatus
 train$Q124742=imputed.train$Q124742
 train$Q121699=imputed.train$Q121699
 train$Q121700=imputed.train$Q121700
 train$Q120194=imputed.train$Q120194
 train$Q118232=imputed.train$Q118232
 train$Q115611=imputed.train$Q115611
 train$Q114517=imputed.train$Q114517
 train$Q114386=imputed.train$Q114386
 train$Q113181=imputed.train$Q113181
 train$Q112478=imputed.train$Q112478
 train$Q112270=imputed.train$Q112270
 train$Q108950=imputed.train$Q108950
 train$Q109244=imputed.train$Q109244
 train$Q108342=imputed.train$Q108342
 train$Q101596=imputed.train$Q101596
 train$Q100689=imputed.train$Q100689
 train$Q99716=imputed.train$Q99716
 train$Q99581=imputed.train$Q99581
 train$Q98869=imputed.train$Q98869
 train$Q98578=imputed.train$Q98578
 train$Q98059=imputed.train$Q98059
 train$Q98197=imputed.train$Q98197
 
 
 
 test$YOB=imputed.test$YOB
 test$Gender=imputed.test$Gender
 test$Income=imputed.test$Income
 test$HouseholdStatus=imputed.test$HouseholdStatus
 test$Q124742=imputed.test$Q124742
 test$Q121699=imputed.test$Q121699
 test$Q121700=imputed.test$Q121700
 test$Q120194=imputed.test$Q120194
 test$Q118232=imputed.test$Q118232
 test$Q115611=imputed.test$Q115611
 test$Q114517=imputed.test$Q114517
 test$Q114386=imputed.test$Q114386
 test$Q113181=imputed.test$Q113181
 test$Q112478=imputed.test$Q112478
 test$Q112270=imputed.test$Q112270
 test$Q108950=imputed.test$Q108950
 test$Q109244=imputed.test$Q109244
 test$Q108342=imputed.test$Q108342
 test$Q101596=imputed.test$Q101596
 test$Q100689=imputed.test$Q100689
 test$Q99716=imputed.test$Q99716
 test$Q99581=imputed.test$Q99581
 test$Q98869=imputed.test$Q98869
 test$Q98578=imputed.test$Q98578
 test$Q98059=imputed.test$Q98059
 test$Q98197=imputed.test$Q98197
 
 
# summary(imputed)
# 
# 
# #"Q116197","Q115611","Q114517","Q114386","Q113181","Q112478","Q112270","Q108950",
# #"Q109244","Q108342","Q101596","Q100689","Q99716","Q99581","Q98869","Q98578","Q98059","Q98197")]
# 
#copy variables back into polling dataframe
#polling$Rasmussen=imputed$Rasmussen
#polling$SurveyUSA=imputed$SurveyUSA
train$YOB=imputed$YOB
train$Gender=imputed$Gender
train$Income=imputed$Income
train$HouseholdStatus=imputed$HouseholdStatus
train$Q124742=imputed$Q124742
train$Q121699=imputed$Q121699
train$Q121700=imputed$Q121700
train$Q120194=imputed$Q120194
train$Q118232=imputed$Q118232
train$Q115611=imputed$Q115611
train$Q114517=imputed$Q114517
train$Q114386=imputed$Q114386
train$Q113181=imputed$Q113181
train$Q112478=imputed$Q112478
train$Q112270=imputed$Q112270
train$Q108950=imputed$Q108950
train$Q109244=imputed$Q109244
train$Q108342=imputed$Q108342
train$Q101596=imputed$Q101596
train$Q100689=imputed$Q100689
train$Q99716=imputed$Q99716
train$Q99581=imputed$Q99581
train$Q98869=imputed$Q98869
train$Q98578=imputed$Q98578
train$Q98059=imputed$Q98059
train$Q98197=imputed$Q98197




test
# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:


#split into train and test
spl=sample.split(imputed.train$Party,SplitRatio = .70)

vote.train=subset(imputed.train,spl==TRUE)
vote.test=subset(imputed.train,spl==FALSE)  

str(vote.train)
summary(vote.train)
# 
# #create data frame consisting only of the variables with missing values
# simple=train[c("YOB","Gender","Income","HouseholdStatus","Q124742","Q121699","Q121700","Q120194","Q118232","Q116197","Q115611","Q114517","Q114386","Q113181","Q112478","Q112270","Q108950","Q109244","Q108342","Q101596","Q100689","Q99716","Q99581","Q98869","Q98578","Q98059","Q98197")]
# 
# summary(simple)
# 
# set.seed(144)
# imputed=complete(mice(simple))
# summary(imputed)
# 
# 
# #"Q116197","Q115611","Q114517","Q114386","Q113181","Q112478","Q112270","Q108950",
# #"Q109244","Q108342","Q101596","Q100689","Q99716","Q99581","Q98869","Q98578","Q98059","Q98197")]
# 
# #copy variables back into polling dataframe
# #polling$Rasmussen=imputed$Rasmussen
# #polling$SurveyUSA=imputed$SurveyUSA
# vote.train$YOB=imputed$YOB
# vote.train$Gender=imputed$Gender
# vote.train$Income=imputed$Income
# vote.train$HouseholdStatus=imputed$HouseholdStatus
# vote.train$Q124742=imputed$Q124742
# vote.train$Q121699=imputed$Q121699
# vote.train$Q121700=imputed$Q121700
# vote.train$Q120194=imputed$Q120194
# vote.train$Q118232=imputed$Q118232
# vote.train$Q115611=imputed$Q115611
# vote.train$Q114517=imputed$Q114517
# vote.train$Q114386=imputed$Q114386
# vote.train$Q113181=imputed$Q113181
# vote.train$Q112478=imputed$Q112478
# vote.train$Q112270=imputed$Q112270
# vote.train$Q108950=imputed$Q108950
# vote.train$Q109244=imputed$Q109244
# vote.train$Q108342=imputed$Q108342
# vote.train$Q101596=imputed$Q101596
# vote.train$Q100689=imputed$Q100689
# vote.train$Q99716=imputed$Q99716
# vote.train$Q99581=imputed$Q99581
# vote.train$Q98869=imputed$Q98869
# vote.train$Q98578=imputed$Q98578
# vote.train$Q98059=imputed$Q98059
# vote.train$Q98197=imputed$Q98197


vote.logsig=glm(Party ~ Gender+Q109244+Q98869+Q98197+Q113181+Q115611,data=imputed.train,family=binomial)

vote.train

str(vote.test)
str(vote.train)
str(test)
# And then make predictions on the training set:
pred.test.logsig = predict(vote.logsig, newdata=imputed.test , type="response")


threshold = 0.5

pred.test.logsig

PredTestLabels = as.factor(ifelse(pred.test.logsig<threshold, "Democrat", "Republican"))

PredTestLabels
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "sig2Logimputed.csv", row.names=FALSE)

#with CART
vote.CART= rpart(Party~., data = train, method="class")

prp(vote.CART)

numFolds=trainControl(method="cv", number=10)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train(Party ~ .,data=vote.train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

vote.CARTCV= rpart(Party ~ ., data = vote.train, method="class",cp=.01)
prp(vote.CARTCV)

printcp(vote.CARTCV)




train

vote.CARTCV.pred = predict(vote.CARTCV, newdata=vote.test)
vote.CARTCV.pred


table(vote.test$Party,vote.CARTCV.pred>0.5)



summary(train)

threshold=.5
PredTestLabels = as.factor(ifelse(vote.CARTCV.pred[,2]<threshold, "Democrat", "Republican"))
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "sig2CART_impute.csv", row.names=FALSE)


#other question list - any difference?


# (Q98059) Do/did you have any siblings?
# (Q98197) Do you pray or meditate on a regular basis?
# (Q99480) Did your parents spank you as a form of discipline/punishment?
# (Q106272) Do you own any power tools? (power saws, drills, etc.)
# (Q107869) Do you feel like you're "normal"?
# (Q108343) Do you feel like you have too much personal financial debt?
# (Q108617) Do you live in a single-parent household?
# (Q108754) Do both of your parents have college degrees?
# (Q109244) Are you a feminist?
# (Q109367) Have you ever been poor (however you personally defined it at the time)?
# (Q112270) Are you better looking than your best friend?
# (Q113181) Do you meditate or pray on a regular basis?
# (Q114386) Are you more likely to over-share or under-share?
# (Q115195) Do you live within 20 miles of a major metropolitan area?
# (Q115602) Were you an obedient child?
# (Q115611) Do you personally own a gun?
# (Q116441) Do you have a car payment?
# (Q116881) Would you rather be happy or right?
# (Q116953) Do you like rules?
# (Q120650) Were your parents married when you were born?
# (Q122771) Do/did you get most of your K-12 education in public school, or private school?
# (Q123621) Are you currently employed in a full-time job?
# (Q124122) Did your parents fight in front of you?


numFolds=trainControl(method="cv", number=5)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122 ,data=vote.train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

vote.CARTCV= rpart(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122 , data = vote.train, method="class",cp=.02)

prp(vote.CARTCV)

printcp(vote.CARTCV)




train

vote.CARTCV.pred = predict(vote.CARTCV, newdata=test)
vote.CARTCV.pred


table(vote.test$Party,vote.CARTCV.pred>0.5)



summary(train)

threshold=.5
PredTestLabels = as.factor(ifelse(vote.CARTCV.pred[,2]<threshold, "Democrat", "Republican"))
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "sig3CART_impute.csv", row.names=FALSE)

vote.CARTCV.pred

#let's try SVM:

svmmod=svm(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122 , data = imputed.train)

pred.svm=predict(svmmod,newdata=imputed.test)



str(svmmod)

str(pred.svm)

pred.svm
points(vote.train$Party,pred.svm , col = "red", pch=4)
plot(vote.train$Party,pred.svm)




tuneResult <- tune(svm, Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122,  data = vote.train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# best performance: MSE = 8.371412, RMSE = 2.89 epsilon 1e-04 cost 4
# Draw the tuning graph
plot(tuneResult)


pred.svm

threshold=.5
PredTestLabels = as.factor(pred.svm)
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "sig3SVM_impute.csv", row.names=FALSE)



#split into train and test
spl=sample.split(imputed.train$Party,SplitRatio = .70)

vote.train=subset(imputed.train,spl==TRUE)
vote.test=subset(imputed.train,spl==FALSE)  


library(randomForest)
#random forest
vote.forest=randomForest(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122,data=vote.train,ntree=2000)


pred.forest=predict(vote.forest,vote.test,type="response")

summary(imputed.train)

varImpPlot(vote.forest)


boxplot(imputed.train$YOB)

imputed.train$Kid=i

pred.forest

threshold=.5
PredTestLabels = as.factor(pred.forest)
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "forest_impute.csv", row.names=FALSE)
  
library(caret)
confusionMatrix(pred.forest,vote.test$Party)

confusionMatrix(vote.CARTCV.pred,vote.test$Party)


nrow(imputed.train)
length(pred.forest)
