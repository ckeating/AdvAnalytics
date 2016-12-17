library(gmodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(caret)

setwd("C:/Users/Chibot/Dropbox/edgedata")
census=read.csv("census.csv")
str(census)
summary(census)

tbl=table(census$race)
barplot(tbl)

hist(tbl)
set.seed(2000)
spl=sample.split(census$over50k,SplitRatio = .60)


train=subset(census,spl==TRUE)
test=subset(census,spl==FALSE)




#logisitic model to predict if earnings are above 50k
census.log=glm(over50k ~ .,family="binomial",data=train)

summary(census.log)

#accuracy on testing set
census.log.pred=predict(census.log,newdata=test,type="response")

log.tbl=table(test$over50k,census.log.pred>.50)
sum(diag(log.tbl))/sum(log.tbl)

CrossTable(test$over50k)


#calculate AUC
pred.ROC = prediction(census.log.pred, test$over50k)
perf = performance(pred.ROC, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred.ROC,"auc")@y.values)


#build classification tree to predict whether earnings are over 50K


# CART model
censustree = rpart(over50k~., data = train, method="class")

prp(censustree)

# Make predictions
PredictCART = predict(censustree, newdata = test,type="class")
CART.tbl=table(test$over50k, PredictCART)
sum(diag(CART.tbl))/sum(CART.tbl)

PredictCART.forROC

#calculate AUC for CART tree
PredictCART.forROC = predict(censustree, newdata = test)
pred.tree.ROC = prediction(PredictCART.forROC[,2], test$over50k)
perf = performance(pred.tree.ROC, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred.tree.ROC,"auc")@y.values)


#downsample training set
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]


# Build random forest model
set.seed(1)
census.Forest = randomForest(over50k ~ ., data=trainSmall)
census.Forest.pred=predict(census.Forest,newdata=test)
acctbl.for=table(test$over50k,census.Forest.pred)
acctbl.for

sum(diag(acctbl.for))/sum(acctbl.for)

#number of times a variable is selected for a split

vu = varUsed(census.Forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(census.Forest$forest$xlevels[vusorted$ix]))

varImpPlot(census.Forest)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
cartGrid

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Perform the cross validation
train(over50k ~ ., method = "rpart", trControl = numFolds, tuneGrid = cartGrid ,data=train)

#create a new cart model with cp parameter
censustreeCV = rpart(over50k ~ ., data = train, method="class", cp = 0.002)

censustreeCV.pred= predict(censustreeCV, newdata = test,type="class")
CART.tbl=table(test$over50k, censustreeCV.pred)
sum(diag(CART.tbl))/sum(CART.tbl)

PredictCART.forROC
prp(censustreeCV)
nrow(censustreeCV$splits)

censustreeCV$cptable
