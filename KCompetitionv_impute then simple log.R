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
train = read.csv("train2016.csv",na.string=c("","NA"))

train2 = read.csv("train2016.csv")
test = read.csv("test2016.csv",na.string=c("","NA"))

#impute entire train dataset
# 
# #create data frame consisting only of the variables with missing values
 simple=train[c("YOB","Gender","Income","HouseholdStatus","Q124742","Q121699","Q121700","Q120194","Q118232","Q116197","Q115611","Q114517","Q114386","Q113181","Q112478","Q112270","Q108950","Q109244","Q108342","Q101596","Q100689","Q99716","Q99581","Q98869","Q98578","Q98059","Q98197")]
# 
# summary(simple)
# 
 set.seed(144)
 imputed=complete(mice(simple))
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
spl=sample.split(train$Party,SplitRatio = .70)

vote.train=subset(train,spl==TRUE)
vote.test=subset(train,spl==FALSE)  

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


vote.logsig=glm(Party ~ YOB+Gender +Income +HouseholdStatus+Q124742+Q121699+Q121700+Q120194+Q118232+Q116197+Q115611+Q114517+Q114386+Q113181+Q112478+Q112270+Q108950+Q109244+Q108342+Q101596+Q100689+Q99716+Q99581+Q98869+Q98578+Q98059+Q98197,data=train,family=binomial)

vote.train

str(vote.test)
str(vote.train)
str(test)
# And then make predictions on the training set:
pred.test.logsig = predict(vote.logsig, newdata=test , type="response")


threshold = 0.5

pred.test.logsig

PredTestLabels = as.factor(ifelse(pred.test.logsig<threshold, "Democrat", "Republican"))

PredTestLabels
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "imputed_sigLog.csv", row.names=FALSE)



















#create confusion matrix
table(na.omit(vote.train$Party),pred.train.logsimple>0.5)


str(vote.train)


vote.train=complete.cases(vote.train)
pred.train.logsimple=complete.cases(pred.train.logsimple)

table(vote.train$Party,pred.train.logsimple>0.5)


length(pred.train.logsimple)

vote.train$Party


table(vote.train$Party)
table(pred.train.logsimple>0.5)

length(vote.train$Party)


table(vote.train$Party)
table(pred.logsimple>0.5)

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

acctbl=table(train$)



SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)
vote.logsig=glm(Party ~ YOB+Gender +Income +HouseholdStatus+Q124742+Q121699+Q121700+Q120194+Q118232+Q116197+Q115611+Q114517+Q114386+Q113181+Q112478+Q112270+Q108950+Q109244+Q108342+Q101596+Q100689+Q99716+Q99581+Q98869+Q98578+Q98059+Q98197,data=vote.train,family=binomial)
summary(vote.logsig)
vote.logsig.pred=predict(vote.logsig, newdata=vote.test, type="response")

table(vote.train$Party,vote.logsig.pred>.5)


YOB+Gender +Income +HouseholdStatus+Q124742+Q121699+Q121700+Q120194+Q118232+Q116197+Q115611+Q114517+Q114386+Q113181+Q112478+Q112270+Q108950+Q109244+Q108342+Q101596+Q100689+Q99716+Q99581+Q98869+Q98578+Q98059+Q98197
simple=()


PredTestLabels = as.factor(ifelse(vote.logsig.pred<threshold, "Democrat", "Republican"))




# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog1.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!






vote.logsig=glm(Party ~ YOB+Gender +Income +HouseholdStatus+Q120379+Q98197+Q113181+Q115611+Q100562+Q108754+Q109244+Q109367,data=train,family=binomial)
summary(vote.logsig)


#baseline accuracy
CrossTable(train$Party)

#predict with "simple" log model
predict.


#CART
vote.CART= rpart(Party~., data = vote.train, method="class",minbucket=20)
prp(vote.CART)




#random forest



Train=subset(stevens,spl==TRUE)
Test=subset(stevens,spl==FALSE)


summary(train)


plot(train$YOB)
boxplot(train$YOB)
table(train$EducationLevel)
table(train$Income,train$EducationLevel)
CrossTable(train$Income,train$EducationLevel)







vote.CART= rpart(Party~., data = train, method="class")

prp(vote.CART)

numFolds=trainControl(method="cv", number=5)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train(Party ~ .,data=train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

vote.CARTCV= rpart(Party~., data = train, method="class",cp=.04,minbucket=25)
prp(vote.CARTCV)


vote.CARTCV.pred = predict(vote.CARTCV, newdata=test)
vote.CARTCV.pred

summary(train)

threshold=.5
PredTestLabels = as.factor(ifelse(vote.CARTCV.pred[,2]<threshold, "Democrat", "Republican"))
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog3.csv", row.names=FALSE)

str(test)
str(train)

vote.forest=randomForest(Party ~ . , data=train)

set.seed(1000)
letter.Forest = randomForest(letter ~ . - isB, data=train)
letter.Forest.pred=predict(letter.Forest,newdata=test,type="class")
acctbl.letter.for=table(test$letter,letter.Forest.pred)
acctbl.letter.for


#let's try CART on most significant

numFolds=trainControl(method="cv", number=5)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train(Party ~ YOB+Gender +Income +HouseholdStatus+Q124742+Q121699+Q121700+Q120194+Q118232+Q116197+Q115611+Q114517+Q114386+Q113181+Q112478+Q112270+Q108950+Q109244+Q108342+Q101596+Q100689+Q99716+Q99581+Q98869+Q98578+Q98059+Q98197,data=train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)




vote.CARTCV= rpart(Party ~ YOB+Gender +Income +HouseholdStatus+Q124742+Q121699+Q121700+Q120194+Q118232+Q116197+Q115611+Q114517+Q114386+Q113181+Q112478+Q112270+Q108950+Q109244+Q108342+Q101596+Q100689+Q99716+Q99581+Q98869+Q98578+Q98059+Q98197, data = train, method="class",cp=.04,minbucket=25)
prp(vote.CARTCV)


vote.CARTCV.pred = predict(vote.CARTCV, newdata=test)
vote.CARTCV.pred

summary(train)

threshold=.5
PredTestLabels = as.factor(ifelse(vote.CARTCV.pred[,2]<threshold, "Democrat", "Republican"))
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionTree_sig", row.names=FALSE)



#clustering
# caret need for normalization
library(caret)

preproc = preProcess(train)
trainNorm = predict(preproc, train)

summary(airlinesNorm)

# Compute distances
distances = dist(trainNorm, method = "euclidean")


# Hierarchical clustering
clustervote = hclust(distances, method = "ward.D") 



clusterGroups.vote = cutree(clustervote, k = 5)

str(clusterGroups.kos)
head(clusterGroups.kos,5)
clusterGroups.kos

#how many observations in each cluster?
table(clusterGroups.vote)
table(train$Gender)

clust1=subset(trainNorm,clusterGroups.vote==1)
clust2=subset(trainNorm,clusterGroups.vote==2)
clust3=subset(trainNorm,clusterGroups.vote==3)
clust4=subset(trainNorm,clusterGroups.vote==4)
clust5=subset(trainNorm,clusterGroups.vote==5)


str(clust1)
#CART on each cluster
set.seed(200)
cart1=rpart(Party ~ .,data=clust1, method="class",cp=.04,minbucket=25)
cart1.pred = predict(cart1, newdata=test)

cart2=rpart(Party ~ .,data=clust2, method="class",cp=.04,minbucket=25)
cart2.pred = predict(cart2, newdata=test)

cart3=rpart(Party ~ .,data=clust3, method="class",cp=.04,minbucket=25)
cart3.pred = predict(cart3, newdata=test)

cart4=rpart(Party ~ .,data=clust4, method="class",cp=.04,minbucket=25)
cart4.pred = predict(cart4, newdata=test)

cart5=rpart(Party ~ .,data=clust5, method="class",cp=.04,minbucket=25)
cart5.pred = predict(cart5, newdata=test)


cardpredall=c(cart1.pred,cart2.pred,cart3.pred,cart4.pred,cart5.pred)


threshold=.5
PredTestLabels = as.factor(ifelse(cart5.pred<threshold, "Democrat", "Republican"))
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionTree_clust", row.names=FALSE)



#let's try forest on self picked questions
Q98197
Q100562
Q108754
Q109244
Q109367
Q113181
Q115611
Q120379



set.seed(100)
StevensForest=randomForest(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,nodesize=25,ntree=200)
PredictForest=predict(StevensForest,newdata=Test)
table(Test$Reverse,PredictForest)



#run with k-means
#rerun
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5,iter.max = 1000)

KMC.Cluster=KMC$cluster

kmc.clust1=subset(airlinesNorm,KMC.Cluster==1)
kmc.clust2=subset(kos,KMC.Cluster==2)
kmc.clust3=subset(kos,KMC.Cluster==3)
kmc.clust4=subset(kos,KMC.Cluster==4)
kmc.clust5=subset(kos,KMC.Cluster==5)

table(KMC.Cluster)

tail(sort(colMeans(clust2)))

tail(sort(colMeans(kmc.clust1)))
tail(sort(colMeans(kmc.clust2)))
tail(sort(colMeans(kmc.clust3)))
tail(sort(colMeans(kmc.clust4)))
tail(sort(colMeans(kmc.clust5)))
tail(sort(colMeans(kmc.clust6)))
tail(sort(colMeans(kmc.clust7)))


tail(sort(colMeans(clust1)))
tail(sort(colMeans(clust2)))
tail(sort(colMeans(clust3)))
tail(sort(colMeans(clust4)))
tail(sort(colMeans(clust5)))
tail(sort(colMeans(clust6)))
tail(sort(colMeans(clust7)))


table(clusterGroups.kos, KMC$cluster)
str(kmc.clust3)


##impute missing values then run CART model
summary(train)
str(train)

library(mice)
simple=train[c("USER_ID","YOB")]
imputed=complete(mice(simple))


train$USER_ID=imputed$USER_ID
train$YOB=imputed$YOB

summary(imputed)

numFolds=trainControl(method="cv", number=5)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train(Party ~ .,data=train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

vote.CARTCV= rpart(Party~., data = train, method="class",cp=.04,minbucket=10)
prp(vote.CARTCV)

vote.CARTCV.pred = predict(vote.CARTCV, newdata=test)
vote.CARTCV.pred

summary(train)

threshold=.5
PredTestLabels = as.factor(ifelse(vote.CARTCV.pred[,2]<threshold, "Democrat", "Republican"))
PredTestLabels

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "impute3.csv", row.names=FALSE)


str(train)
summary(test)
