#Unit 6 assignment 1 



#clear workspace
rm(list = ls())
setwd("C:/Users/Chibot/Dropbox/edgedata")

stocks=read.csv("StocksCluster.csv", stringsAsFactors=FALSE)
summary(stocks)
str(stocks)
nrow(stocks)

head(stocks,54)

#proportion of stocks with positive returns
table(stocks$PositiveDec>0)

6324/(6324+5256)


stocks.cor=cor(stocks)
stocks.cor
#max correlation
results <- data.frame(v1=character(0), v2=character(0), cor=numeric(0), stringsAsFactors=FALSE)
d=stocks.cor
diag(d) <- 0
while (sum(d>0)>1) {
  maxval <- max(d)
  max <- which(d==maxval, arr.ind=TRUE)[1,]
  results <- rbind(results, data.frame(v1=rownames(d)[max[1]], v2=colnames(d)[max[2]], cor=maxval))
  d[max[1],] <- 0
  d[,max[1]] <- 0
  d[max[2],] <- 0
  d[,max[2]] <- 0
}
results

#max correlation .19167279
# results
# v1        v2        cor
# 1   ReturnNov ReturnOct 0.19167279
# 2  ReturnJune ReturnFeb 0.16999448
# 3 PositiveDec ReturnApr 0.09435353
# 4  ReturnJuly ReturnMay 0.09085026
# 5   ReturnSep ReturnMar 0.07651833


#split then build logistic regression model
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

str(stocksTrain)

StocksModel=glm(PositiveDec ~.,family=binomial,data=stocksTrain)
summary(StocksModel)

#predict values
predictTrain=predict(StocksModel,type="response",data=stocksTrain)

#creating a confusion matrix where threshold=0.5
#actual outcomes by predicted outcomes
table(stocksTrain$PositiveDec,predictTrain>0.5)
(3640+990)/(3640+990+787+2689)

#now test set predictions
#predict values
predictTest=predict(StocksModel,type="response",newdata=stocksTest)

#creating a confusion matrix where threshold=0.5
#test set accuracy
table(stocksTest$PositiveDec,predictTest>0.5)
(1553+417)/(1553+417+1160+344)

#test set baseline accuracy
table(stocksTest$PositiveDec)
1897/(1897+1577)


#cluster the stocks
#remove dependent variable, normalize data


#step 1 - remove dependent variable 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#step 2 - normalize the data
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

#mean of ReturnJan in NormTrain
mean(normTrain$ReturnJan)

#mean of ReturnJan in NormTest
mean(normTest$ReturnJan)


#3.3 clustering stocks
#Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
# EXPLANATION
# 
# From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), 
# we see that the average return in January is slightly higher in 
# the training set than in the testing set. 
# Since normTest was constructed by subtracting by the mean ReturnJan value from
# the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.


#run k-means clustering on normtrain

set.seed(144)
km = kmeans(normTrain, centers = 3)

table(km$cluster)
KMC.Cluster=KMC$cluster

#obtain training and testing set cluster assignments
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

#how many test set obs in cluster 2? -2080
table(clusterTest)
# 
# > table(clusterTest)
# clusterTest
# 1    2    3 
# 1298 2080   96 


stocksTrain1=subset(stocksTrain,clusterTrain==1)
stocksTrain2=subset(stocksTrain,clusterTrain==2)
stocksTrain3=subset(stocksTrain,clusterTrain==3)


stocksTest1=subset(stocksTest,clusterTest==1)
stocksTest2=subset(stocksTest,clusterTest==2)
stocksTest3=subset(stocksTest,clusterTest==3)


mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)


#train logistic models for each cluster

StocksModel1=glm(PositiveDec ~.,family=binomial,data=stocksTrain1)
StocksModel2=glm(PositiveDec ~.,family=binomial,data=stocksTrain2)
StocksModel3=glm(PositiveDec ~.,family=binomial,data=stocksTrain3)


summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)


#predict values
predictTest1=predict(StocksModel1,type="response",newdata=stocksTest1)
predictTest2=predict(StocksModel2,type="response",newdata=stocksTest2)
predictTest3=predict(StocksModel3,type="response",newdata=stocksTest3)

#creating a confusion matrix where threshold=0.5
#actual outcomes by predicted outcomes
#accuracy stockstest1
table(stocksTest1$PositiveDec,predictTest1>0.5)
(30+774)/(30+774+471+23)
table(stocksTest2$PositiveDec,predictTest2>0.5)
(757+388)/(757+388+626+309)
table(stocksTest3$PositiveDec,predictTest3>0.5)
(49+13)/(49+13+21+13)


AllPredictions = c(predictTest1, predictTest2, predictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)


table(AllOutcomes,AllPredictions>0.5)

(1544+467)/(1544+467+1110+353)


#now test set predictions
#predict values
predictTest=predict(StocksModel,type="response",newdata=stocksTest)

#creating a confusion matrix where threshold=0.5
#test set accuracy
table(stocksTest$PositiveDec,predictTest>0.5)
(1553+417)/(1553+417+1160+344)

#test set baseline accuracy
table(stocksTest$PositiveDec)
1897/(1897+1577)




 