#clear workspace
rm(list = ls())
setwd("C:/Users/Chibot/Dropbox/edgedata")

##do everything at once
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
library(tm)
library(randomForest)
library(caTools)

#create corpus
corpus = Corpus(VectorSource(emails$text))

# 2) Convert to lowercase. 
corpus = tm_map(corpus, content_transformer(tolower))

#2a - convert to PlainTextDocument
corpus = tm_map(corpus, PlainTextDocument)

#   3) Remove the punctuation in
corpus = tm_map(corpus, removePunctuation)

# 4) Remove the English language stop words 
corpus = tm_map(corpus, removeWords, stopwords("english"))

# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpus = tm_map(corpus, stemDocument)

# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtm = DocumentTermMatrix(corpus)

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
spdtm = removeSparseTerms(dtm, 0.95)

# 8) Convert  to data frames
emailsSparse   = as.data.frame(as.matrix(spdtm))

#make column names valid
colnames(emailsSparse) = make.names(colnames(emailsSparse))

#add variable spam from original dataset
emailsSparse$spam=emails$spam

#set spam variable to factor

emailsSparse$spam = as.factor(emailsSparse$spam)

#split into training and testing sets
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

### ---------------------- end everything at once







emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)






emails$text[which.max(nchar(emails$text))]

#longest email
max(nchar(emails$text))

#shortest email
which.min(nchar(emails$text))

library(tm)

#create corpus
corpus = Corpus(VectorSource(emails$text))


# 2) Convert to lowercase. 

corpus = tm_map(corpus, content_transformer(tolower))


#2a - convert to PlainTextDocument
corpus = tm_map(corpus, PlainTextDocument)


#   3) Remove the punctuation in

corpus = tm_map(corpus, removePunctuation)


# 4) Remove the English language stop words 

corpus = tm_map(corpus, removeWords, stopwords("english"))


# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpus = tm_map(corpus, stemDocument)


# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.

dtm = DocumentTermMatrix(corpus)
dtm

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
spdtm = removeSparseTerms(dtm, 0.95)

spdtm



# 8) Convert  to data frames
emailsSparse   = as.data.frame(as.matrix(spdtm))

#make column names valid
colnames(emailsSparse) = make.names(colnames(emailsSparse))

#most frequent word stem?

abtotal<-colSums(emailsSparse)
abtotal[which.max(abtotal)]

#add variable spam from original dataset

emailsSparse$spam=emails$spam
colSums(emailsSparse)

#how many word stems appear at least 5000 times in the ham emails
#my approach
#1. subset into ham only
#2. create variable is length is >=5000
#3. sum this variable
ham=subset(emailsSparse,spam==0)
spam=subset(emailsSparse,spam==1)

table(ham$spam)
table(spam$spam)

hamct=colSums(ham)
str(hamct)
hamct

subset(hamct,hamct>=5000)
subset(spam,hamct>=5000)

table(colSums(ham) >= 5000)

table(colSums(spam) >= 1000)


colSums(spam)

sort(colSums(subset(emailsSparse, spam == 1)))

#building machine learning models

emailsSparse$spam = as.factor(emailsSparse$spam)


#split into training and testing sets
library(caTools)

set.seed(123)

spl = sample.split(emailsSparse$spam, 0.7)

str(spl)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

table(train$spam)
str(train)

##build 3 different models
# 1) A logistic regression model called spamLog. You may see a warning message here - we'll discuss this more later.
# 
# 2) A CART model called spamCART, using the default parameters to train the model 
# (don't worry about adding minbucket or cp).
# Remember to add the argument method="class" since this is a binary classification problem.
# 
# 3) A random forest model called spamRF, using the default parameters to train the model 
# (don't worry about specifying ntree or nodesize). 
# Directly before training the random forest model, 
# set the random seed to 123 (even though we've already done this earlier in the problem, 
# it's important to set the seed right before training the model so we all obtain the same results. 
# Keep in mind though that on certain operating systems, your results might still be slightly different).


#logistic regression model

spamLog=glm(spam ~ .,family=binomial,data=train)
summary(spamLog)

#build CART model
library(rpart)
library(rpart.plot)

spamCART = rpart(spam~., data=train, method="class")

#display CART tree
prp(spamCART)


#build random forest
library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data = train )


#predict on logistic model
predictLog=predict(spamLog,type="response")
summary(predictLog)
str(predictLog)

table(predictLog<0.00001)
table(predictLog> 0.99999)
table(predictLog>=0.00001 & predictLog<=0.99999)




#predict on CART model
pred.CART.train = predict(spamCART, data=train)
#first 10 rows, all the columns
pred.CART.train[1:10,]
#select right-most or 2nd column
pred.CART.train.prob = pred.CART.train[,2]
pred.CART.train.prob



##----------- predict CART model end


table(pred.CART.train.prob)

summary(spamLog)


#predict on RF model
predictForest=predict(spamRF,data=train,class="prob")
table(train$spam, predictForest>0.5)

predTrainRF = predict(spamRF, type="prob")[,2]

str(predictForest)

predictForest.prob=predictForest[,2]

(3013+914)/(3013+914+39+44)


#logisic model accuracy
#creating a confusion matrix where threshold=0.5
#actual outcomes by predicted outcomes
table(train$spam,predictLog>0.5)
(3052+954)/(3052+954+4+0)

#training set AUC
ROCRpredtrain = prediction(predictLog, train$spam)

auc = as.numeric(performance(ROCRpredtrain, "auc")@y.values)
auc


#training set accuracy of CART model
table(train$spam,pred.CART.train.prob>.5)
(2885+894)/(2885+894+167+64)


#training set AUC of CART model
#training set AUC
ROCPred_CART = prediction(pred.CART.train.prob, train$spam) 

auc = as.numeric(performance(ROCPred_CART, "auc")@y.values)
auc

#training set AUC of forest model
#training set AUC
ROCPred_Forest = prediction(predTrainRF, train$spam) 

auc = as.numeric(performance(ROCPred_Forest, "auc")@y.values)
auc



##predict logistic model on test and check accuracy and auc
#predict on logistic model
predictLog.test=predict(spamLog,type="response",newdata=test)

ROCRpredlog.test = prediction(predictLog.test, test$spam)


#accuracy
table(test$spam,predictLog.test>0.5)
(1257+376)/(1257+376+51+34)


auc = as.numeric(performance(ROCRpredlog.test, "auc")@y.values)
auc


##predict CARt model on test and check accuracy and auc
pred.CART.test = predict(spamCART, newdata=test)
#select right-most or 2nd column
pred.CART.test.prob = pred.CART.test[,2]


#test set accuracy of CART model
table(test$spam,pred.CART.test.prob>.5)
(1228+386)/(1228+386+24+80)
(1228+386)/nrow(test)


#training set AUC of CART model
#training set AUC
ROCPred_CART.test = prediction(pred.CART.test.prob, test$spam) 

auc = as.numeric(performance(ROCPred_CART.test, "auc")@y.values)
auc



#predict on RF model on test set
predictForest.test=predict(spamRF,newdata=test,type="prob")[,2]


table(test$spam,predictForest.test>0.5)
(1292+388)/nrow(test)

#ttest set AUC of forest model
ROCPred_Forest.test = prediction(predictForest.test, test$spam) 

auc = as.numeric(performance(ROCPred_Forest.test, "auc")@y.values)
auc



