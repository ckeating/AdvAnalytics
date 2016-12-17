install.packages("gmodels")
library(gmodels)
library(rpart)
library(rpart.plot)
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

setwd("C:/Users/Chibot/Dropbox/edgedata")
letters=read.csv("letters_ABPR.csv")

str(letters)
letters$isB = as.factor(letters$letter == "B")


library(caTools)

set.seed(1000)
spl=sample.split(letters$isB,SplitRatio = .50)

train=subset(letters,spl==TRUE)
test=subset(letters,spl==FALSE)

table(test$isB)
383/(383+1175)

CrossTable(test$isB)

#build classification tree to predict the letter B
CARTb = rpart(isB ~ . - letter, data=train, method="class")

prp(CARTb)

str(train)
str(CARTb.pred)
CARTb.pred=predict(CARTb,newdata=test,type="class")

CARTb.pred
table()

#accuracy
#what you're predicting, and predictions
acctbl=table(test$isB,CARTb.pred)
sum(diag(acctbl))/sum(acctbl)

set.seed(1000)

# Build random forest model
CART.Forest = randomForest(isB ~ . - letter, data=train)
CART.Forest.pred=predict(CART.Forest,newdata=test,type="class")
acctbl.for=table(test$isB,CART.Forest.pred)
acctbl.for

sum(diag(acctbl.for))/sum(acctbl.for)

letters$letter = as.factor( letters$letter )

#resplit data on letter variable
set.seed(1000)
spl=sample.split(letters$letter,SplitRatio = .50)

train=subset(letters,spl==TRUE)
test=subset(letters,spl==FALSE)

#baseline on testing - percentage of MAJORITY value
CrossTable(test$letter)

#build a classification tree to predict "letter"
CART.letter=rpart(letter ~ . - isB, data=train, method="class")
CART.letter.pred=predict(CART.letter,newdata=test,type="class")

#accuracy
aactbl.CARTletter=table(test$letter,CART.letter.pred)
aactbl.CARTletter
sum(diag(aactbl.CARTletter))/sum(aactbl.CARTletter)


#build a random forest model to predict letter
set.seed(1000)
letter.Forest = randomForest(letter ~ . - isB, data=train)
letter.Forest.pred=predict(letter.Forest,newdata=test,type="class")
acctbl.letter.for=table(test$letter,letter.Forest.pred)
acctbl.letter.for

sum(diag(acctbl.letter.for))/sum(acctbl.letter.for)
