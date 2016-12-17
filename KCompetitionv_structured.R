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



#1 - simple model
# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.

library(gmodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(caret)
library(caTools)

# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer
setwd("C:/Users/Chibot/Dropbox/edgedata")

train = read.csv("train2016.csv")
test = read.csv("test2016.csv")


str(train,list.len=300)

table(train$Party)


#split into train and test
spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)   


str(train,list.len=300)
# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:

summary(train)

SimpleMod = glm(Party ~ . -USER_ID, data=vote.train, family=binomial)


summary(SimpleMod)
str(train)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=vote.test, type="response")

threshold = 0.5

confusionMatrix(table(vote.test$Party,PredTest>0.5))

table(vote.test$Party,PredTest>0.5)

nrow(vote.test)
length(PredTest)

str(PredTest)


summary(PredTest)


simp.cm=table(vote.test$Party,PredTest>0.5)

sum(diag(simp.cm))/nrow(vote.test)

#simple log model accuracy= .5964072

#model 2 - randomly chosen independent variables
vote.logsig=glm(Party ~ Gender+Q109244+Q98869+Q98197+Q113181+Q115611,data=vote.train,family=binomial)

summary(vote.logsig)

str(vote.test)
str(vote.train)
str(test)
# And then make predictions on the training set:
pred.test.logsig = predict(vote.logsig, newdata=vote.test , type="response")


simplog.cm=table(vote.test$Party,pred.test.logsig>0.5)
simplog.cm
sum(diag(simplog.cm))/nrow(vote.test)
#accuracy .6221557


#model 3 - logisical regression on independent variables from forum user

#split into train and test
spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)   


vote.logsigforum=glm(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122,data=vote.train,family=binomial)

summary(vote.logsigforum)


# And then make predictions on the training set:
pred.test.logsigforum = predict(vote.logsigforum, newdata=vote.test , type="response")


logsigforum.cm=table(vote.test$Party,pred.test.logsigforum>0.5)
logsigforum.cm
sum(diag(logsigforum.cm))/nrow(vote.test)

#accuracy 0.5814371 - LOWER?!? how could that be


#model 4 CART model on all variables
#baseline
table(vote.train$Party)
#
#Democrat Republican 
#2066       1832 
# 2066/nrow(vote.train)
# =0.5300154


vote.CART= rpart(Party~., data = vote.train, method="class")

prp(vote.CART)


vote.CART.pred = predict(vote.CART, newdata=vote.test)
vote.CART.pred


CART.cm=table(vote.test$Party,vote.CART.pred[,2]>0.5)

sum(diag(CART.cm))/nrow(vote.test)

# accuracy= 0.6065868

#model/approach 5 impute yOB and try logisic regression

setwd("C:/Users/Chibot/Dropbox/edgedata")

train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

simple.train=train[c("YOB","Gender")]
imputed.train=complete(mice(simple.train))
head(imputed.train)

#do same for test
simple.test=test[c("YOB","Gender")]
imputed.test=complete(mice(simple.test))

test$YOB=imputed.test$YOB
test$Gender=imputed.test$Gender
test$Age=2013-test$YOB



train$YOB=imputed.train$YOB
train$Gender=imputed.train$Gender

train$Age=2013-train$YOB


f_agegroup<-function(Age){
   if (Age>0 && Age<=2) {"Infant"}
  else if(Age>=3 && Age<=12){"Child"}
  else if (Age>=13 && Age<=18){"Teens"}
  else if (Age>=19 && Age<25) {"YoungAdult"}
  else if (Age>=25 && Age<=34) {"Adult"}
  else if (Age>=35 && Age<=60){"MiddleAdult"}
  else if (Age>60){"WiseAdult"}
  else {"NA"}
}


train$Agegroup<-mapply(f_agegroup,train$Age)
test$Agegroup <-mapply(f_agegroup,test$Age)

table(train$Agegroup)

#split it
#split into train and test
spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)   

vote.logsigage=glm(Party ~  YOB+ Agegroup +Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122,data=vote.train,family=binomial)

summary(vote.logsigage)

pred.logage = predict(vote.logsigage, newdata=vote.test, type="response")

threshold = 0.5

pred.logage.cm=table(vote.test$Party,pred.logage>0.5)

#accuracy
sum(diag(pred.logage.cm))/nrow(vote.test)

#accuracy 0.6227545
#kaggle score .62069

#prepare for submission
pred.logage = predict(vote.logsigage, newdata=test, type="response")

PredTestLabels = as.factor(ifelse(pred.logage<threshold, "Democrat", "Republican"))

str(PredTestLabels)

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "logwithage.csv", row.names=FALSE)




vote.CART= rpart(Party ~  YOB+ Agegroup +Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122, data = vote.train, method="class")

prp(vote.CART)

str(vote.train)
vote.CART.pred = predict(vote.CART, newdata=vote.test)
vote.CART.pred


CART.cm=table(vote.test$Party,vote.CART.pred[,2]>0.5)

sum(diag(CART.cm))/nrow(vote.test)
#accuracy: .6281437


#cart model with my selected questions

#98059 - siblings - concern about them
#98197 - pray or meditate - relighous?
#100562 do you think your life will be better? - will candidate help?
#102089 rent or own primary residence? - economic
#102671 credit card debt - financial
#106042 - medications - health care concerns
#106388 50 hours per week - financial
#108343 = too much debt - financial
#108754 - both parents have college degrees?
#109367 - ever been poor - financial
#113181 mediate or pray ? religous
#114961 - money buy happiness - financial
#115195 - near major metro area 
#115611 - own a gun - MAJOR political issue
#116441 - car payment finance
#118233 - threatened by violence - terrorst link?
#120379 - college bent - plans to persue advanced degree
#122771 get education in public or private?
#123464 - job that pays minimum wage? finance



table(train$Q113181,train$Q98197)

train$pray=ifelse(train$Q113181!=train$Q98197,"Pray","DonotPray")
test$pray=ifelse(test$Q113181!=test$Q98197,"Pray","DonotPray")




table(train$pray)

summary(train$EducationLevel,train$Q122771)

table(train$EducationLevel,train$Q122771)
plot(train$Party,train$EducationLevel)


spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)   


#how does a logisitcal model work here?
modlog.myvars=glm(Party ~ YOB+ Agegroup +Gender+ HouseholdStatus+Q98059 +  Q98197 +  Q100562 +  Q102089 +   Q106042+  Q106388+  Q108343+  Q108754+  Q109367+  Q113181+  Q114961+  Q115195+  Q115611+  Q116441+  Q118233+  Q120379+  Q122771+  Q123464,data=vote.train, family=binomial)
#try with only significant variables?
modlog.myvars=glm(Party ~ Gender+ HouseholdStatus+Q98059 +  Q98197 +  Q113181+  Q115611+  Q120379,data=vote.train, family=binomial)

summary(modlog.myvars)


pred.logmyvar = predict(modlog.myvars, newdata=vote.test, type="response")

threshold = 0.5

pred.logmyvar.cm=table(vote.test$Party,pred.logmyvar>0.5)

#accuracy
sum(diag(pred.logmyvar.cm))/nrow(vote.test)  



#make new variable - college plus 108754 yes?
#check if different answers for medidate or pray set to NA if different
#new category if college educated AND private?

#cart with validation
#

numFolds=trainControl(method="cv", number=5)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train( Party ~ YOB+ Agegroup +Gender+ HouseholdStatus+Q98059 +  Q98197 +  Q100562 +  Q102089 +   Q106042+  Q106388+  Q108343+  Q108754+  Q109367+  Q113181+  Q114961+  Q115195+  Q115611+  Q116441+  Q118233+  Q120379+  Q122771+  Q123464,data=vote.train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

vote.CARTCV= rpart(Party ~ YOB+ Agegroup +Gender+ HouseholdStatus+Q98059 +  Q98197 +  Q100562 +  Q102089 +   Q106042+  Q106388+  Q108343+  Q108754+  Q109367+  Q113181+  Q114961+  Q115195+  Q115611+  Q116441+  Q118233+  Q120379+  Q122771+  Q123464, data = vote.train, method="class",cp=.02)
prp(vote.CARTCV)

printcp(vote.CARTCV)


vote.CARTCV.pred = predict(vote.CARTCV, newdata=vote.test)


CARTCV.cm=table(vote.test$Party,vote.CARTCV.pred[,2]>0.5)

sum(diag(CART.cm))/nrow(vote.test)

#.0.6281437

#now try fully imputed dataset

load("imputed.test.rda")
load("imputed.train.rda")

summary(d)
load (imputed.test,file="imputed.test.rda")
attach(vars)
summary(imputed.train)

train=imputed.train
test=imputed.test

spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)   

vote.logsig=glm(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122,data=vote.train,family=binomial)

summary(vote.logsig)

pred.logsig = predict(vote.logsig, newdata=vote.test, type="response")

threshold = 0.5

pred.logsig.cm=table(vote.test$Party,pred.logsig>0.5)

#accuracy
sum(diag(pred.logsig.cm))/nrow(vote.test)


#prepare for submission
pred.logsig = predict(vote.logsig, newdata=test, type="response")

PredTestLabels = as.factor(ifelse(pred.logsig<threshold, "Democrat", "Republican"))

str(PredTestLabels)

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "logsigimpute_0613.csv", row.names=FALSE)



##much higher accuracy!
##0.6898204
##but only a kaggle score of 0.61063


#cart with validation on imputed dataset
#

numFolds=trainControl(method="cv", number=5)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train( Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122,data=vote.train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

vote.CARTCV= rpart(Party ~  YOB+ Gender+ HouseholdStatus+ Q98059  +	Q98197  +	Q99480  +	Q106272 +	Q107869 +	Q108343 +	Q108617 +	Q108754 +	Q109244 +	Q109367 +	Q112270 +	Q113181 +	Q114386 +	Q115195 +	Q115602 +	Q115611 +	Q116441 +	Q116881 +	Q116953 +	Q120650 +	Q122771 +	Q123621 +	Q124122, data = vote.train, method="class",cp=.02)
prp(vote.CARTCV)

printcp(vote.CARTCV)


vote.CARTCV.pred = predict(vote.CARTCV, newdata=vote.test)


CARTCV.cm=table(vote.test$Party,vote.CARTCV.pred[,2]>0.5)

sum(diag(CARTCV.cm))/nrow(vote.test)

#.6712575
# .625 Kaggle

#prepare for submission imputed CART
vote.CARTCV.pred = predict(vote.CARTCV, newdata=test)

PredTestLabels = as.factor(ifelse(vote.CARTCV.pred[,2]<threshold, "Democrat", "Republican"))

str(PredTestLabels)

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "CARTsigimpute_0613.csv", row.names=FALSE)








#Joonho vars - compare models
vote.logsig=glm(Party ~ Gender+Q109244+Q98869+Q98197+Q113181+Q115611,data=vote.train,family=binomial)

summary(vote.logsig)

str(vote.test)
str(vote.train)
str(test)
# And then make predictions on the training set:
pred.test.logsig = predict(vote.logsig, newdata=vote.test , type="response")


simplog.cm=table(vote.test$Party,pred.test.logsig>0.5)
simplog.cm
sum(diag(simplog.cm))/nrow(vote.test)
#accuracy .6826347



#Joonho CART model
vote.CART= rpart(Party ~ Gender+Q109244+Q98869+Q98197+Q113181+Q115611, data = vote.train, method="class")

prp(vote.CART)


vote.CART.pred = predict(vote.CART, newdata=vote.test)
vote.CART.pred


CART.cm=table(vote.test$Party,vote.CART.pred[,2]>0.5)

sum(diag(CART.cm))/nrow(vote.test)

# accuracy= 0.6808383



#Joonho Forest 
# Build random forest model

load("imputed.test.rda")
load("imputed.train.rda")



train=imputed.train
test=imputed.test

spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)   

summary(train)
summary(test)

set.seed(1)
vote.Forest = randomForest(Party ~ Gender+Q109244+Q98869+Q98197+Q113181+Q115611, data=vote.train)

vote.Forest.pred=predict(vote.Forest,newdata=vote.test)

forest.cm=table(vote.test$Party,vote.Forest.pred)

sum(diag(forest.cm))/sum(forest.cm)

#number of times a variable is selected for a split

vu = varUsed(vote.Forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(vote.Forest$forest$xlevels[vusorted$ix]))

varImpPlot(vote.Forest)

#prepare for submission imputed CART
vote.Forest.pred=predict(vote.Forest,newdata=test)

as.factor(vote.Forest.pred)

PredTestLabels = vote.Forest.pred

str(PredTestLabels)

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "Forestimpute_0613.csv", row.names=FALSE)

