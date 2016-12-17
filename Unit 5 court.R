#laptop
#setwd("C:/Users/Craig/Dropbox/edgedata")
#desktop
setwd("C:/Users/Chibot/Dropbox/edgedata")
stevens=read.csv("stevens.csv")
source("myfun.r")

str(stevens)
summary(stevens)

library(caTools)

set.seed(3000)
spl=sample.split(stevens$Reverse,SplitRatio = .70)

Train=subset(stevens,spl==TRUE)
Test=subset(stevens,spl==FALSE)

library(rpart)
library(rpart.plot)


StevensTree=rpart(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,
                  method = "class",minbucket=25)

prp(StevensTree)
PredictCart=predict(StevensTree,newdata=Test,type="class")

table(Test$Reverse,PredictCart)
##accuracy
(41+71)/(41+36+22+71)
library(ROCR)
PredictROC=predict(StevensTree,newdata=Test)
PredictROC

pred=prediction(PredictROC[,2],Test$Reverse)
perf=performance(pred,"tpr","fpr")

#auc
as.numeric(performance(pred, "auc")@y.values)


#build a CART model with 5 minbucket

StevensTree=rpart(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,
                  method = "class",minbucket=5)


prp(StevensTree)


#build a CART model with 100 minbucket

StevensTree=rpart(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,
                  method = "class",minbucket=100)

#only 1 split
prp(StevensTree)



PredictCart=predict(StevensTree,newdata=Test,type="class")

table(Test$Reverse,PredictCart)

library(randomForest)

StevensForest=randomForest(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,
                         ,nodesize=25,ntree=200)

Train$Reverse=as.factor(Train$Reverse)
Test$Reverse=as.factor(Test$Reverse)

set.seed(200)
StevensForest=randomForest(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,nodesize=25,ntree=200)
PredictForest=predict(StevensForest,newdata=Test)
table(Test$Reverse,PredictForest)
#accuracy
(44+76)/(44+76+17+33)

set.seed(100)
StevensForest=randomForest(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,nodesize=25,ntree=200)
PredictForest=predict(StevensForest,newdata=Test)
table(Test$Reverse,PredictForest)
#accuracy
(43+74)/(43+74+19+34)


accur(Test$Reverse,PredictForest)


library(caret)
library(e1071)
numFolds=trainControl(method="cv", number=10)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

StevensTreeCV=rpart(Reverse ~ Circuit+ Issue + Petitioner + Respondent+ LowerCourt + Unconst,data=Train,method="class",cp=0.19)

PredictCV=predict(StevensTreeCV,newdata=Test,type="class")
table(Test$Reverse,PredictCV)

accur(Test$Reverse,PredictCV)
#accuracy 
(59+64)/(59+64+18+29)



prp(StevensTreeCV)
