setwd("C:/Users/Chibot/Dropbox/edgedata")
  
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)


#Problem 1.1 find abstract with longest character count
summary(nchar(trials$abstract))
#or
max((nchar(trials$abstract)))

#this finds the OBSERVATION with the longest abstract character count
which.max(nchar(trials$abstract))


#Problem 1.2 - find # of search results with no abstract character count
#my solutions
nrow(subset(trials,nchar(trials$abstract)==0))

#other approaches:
table(nchar(trials$abstract) == 0) 
sum(nchar(trials$abstract) == 0)


# Problem 1.3 - Loading the Data
# Find the observation with the minimum number of characters in the title (the variable "title") out of all of the observations in this dataset. 
# What is the text of the title of this article? Include capitalization and punctuation in your response, but don't include the quotes.

trials$title[which.min(nchar(trials$title))]

trials[2]

#problem 2.1 preparing the corpus Load tm package

library(tm)

# Create two corpera
# corpusTitle
# corpusExtract 

# 1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
# 

names(trials)[names(trials)=="title"] <- "corpusTitle"
names(trials)[names(trials)=="abstract"] <- "corpusAbstract"

names(trials)

#create corpusTitle
corpusTitle = Corpus(VectorSource(trials$corpusTitle))

#create corpusAbstact
corpusAbstract = Corpus(VectorSource(trials$corpusAbstract))

# 2) Convert corpusTitle and corpusAbstract to lowercase. 

corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract= tm_map(corpusAbstract, content_transformer(tolower))

str(corpusTitle[1]$content)


#2a - convert to PlainTextDocument
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

   
#   3) Remove the punctuation in corpusTitle and corpusAbstract.


corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)


# 4) Remove the English language stop words from corpusTitle and corpusAbstract.

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))


# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)



# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract) 

dtmTitle.dtm = DocumentTermMatrix(corpusTitle)
inspect(dtmTitle.dtm[1:10,1:100])
#remove sparsity and reinspect
dtmTitle.sparse=removeSparseTerms(dtmTitle.dtm, 0.95)

inspect(dtmTitle.sparse[1:10,1:10])



dtmTitle.sparse


# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)




# 8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle   = as.data.frame(as.matrix(dtmTitle))
dtmAbstract   = as.data.frame(as.matrix(dtmAbstract))

# returns rows, columns
# rows are # of documents, columns are number of variables
dim(dtmTitle)
dim(dtmAbstract)
#how many terms in each?
# 
# > dim(dtmTitle)
# [1] 1860   31
# > dim(dtmAbstract)
# [1] 1860  335

#could also use ncol
ncol(dtmTitle)
ncol(dtmAbstract)

# > ncol(dtmTitle)
# [1] 31
# > ncol(dtmAbstract)
# [1] 335


#problem 2.3 most frequent word stem?

colSums(dtmTitle)
abtotal<-colSums(dtmAbstract)
abtotal[which.max(abtotal)]

#3.1 building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

str(dtmTitle)

rm(dtm)
#combine the two data frames
dtm = cbind(dtmTitle, dtmAbstract)

#add the variable "trial" from the trials dataset
dtm$trial=trials$trial
dtm


#3.3 build a model

#split into training and testing sets
library(caTools)

set.seed(144)

spl = sample.split(dtm$trial, 0.7)

str(spl)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trial)

# Just as in any binary classification problem, 
# the naive baseline always PREDICTS THE MOST COMMON CLASS. From table(train$trial), 
# we see 730 training set results were not trials, and 572 were trials. 
# Therefore, the naive baseline always predicts a result is not a trial, yielding accuracy of 730/(730+572).

# > table(train$trial)
# 
# 0   1 
# 730 572 

730/(730+572)
# > 730/(730+572)
# [1] 0.5606759


# Build a CART model regression tree

library(rpart)
library(rpart.plot)


#method - class = classification
trialCART.train = rpart(trial~., data=train, method="class")

library(rattle)

prp(rxAddInheritance(trialCART))

fancyRpartPlot(trialCART.train)

prp(trialCART.train)

# Make predictions on the training set

pred.train = predict(trialCART.train, data=train)
#first 10 rows, all the columns
pred[1:10,]
#select right-most or 2nd column
pred.prob.train = pred.train[,2]


#what is the maximum predicted probability?
max(pred.prob)


pred.train = predict(trialCART, data=train)

#select right-most or 2nd column
pred.train.prob = pred.train[,2]


#what is the maximum predicted probability?
max(pred.train.prob)


##accuracy, sensitivity, and specificity of training set
table(train$trial,pred.train.prob>=0.5)


#accuracy
accuracy=(631+441)/(631+441+131+99)
accuracy

#sensitivity
631/(631+99)

#specificity
441/(441+131)


#make predictions on testing set

pred.test = predict(trialCART.train, newdata=test)
#select right-most or 2nd column
pred.prob.test = pred.test[,2]


#select right-most or 2nd column
pred.test.prob = pred.test[,2]



##accuracy, sensitivity, and specificity of training set
table(test$trial,pred.test.prob>=0.5)


#accuracy
accuracy=(261+161)/(261+162+83+52)
accuracy

#sensitivity
631/(631+99)

#specificity
441/(441+131)


#------------- determine testing set AUC
library(ROCR)

# ROC curve
pred.test.ROCR = prediction(pred.test.prob, test$trial)

perfROCR = performance(pred.test.ROCR, "tnr", "fnr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(pred.test.ROCR, "auc")@y.values









#sensitivity
12/(12+11)
#specificity
167/(167+12)



# Make predictions on the testing set

pred.test = predict(trialCART, newdata=test)
#select right-most or 2nd column
pred.test.prob = pred.test[,2]


#what is the maximum predicted probability of train?
max(pred.test.prob)
#what is the maximum predicted probability of test?
max(pred.train.prob)


# EXPLANATION
# Because the CART tree assigns the same predicted probability
# to each leaf node and there are a small number of leaf nodes compared to data points, 
# we expect exactly the same maximum predicted probability.


pred.train = predict(trialCART, data=train)

#select right-most or 2nd column
pred.train.prob = pred.train[,2]


#what is the maximum predicted probability?
max(pred.train.prob)








# Compute accuracy

table(test$Vandal, pred.prob >= 0.5)

(618+12)/(618+12+0+533)

# Baseline model accuracy

table(test$responsive)
215/(215+42)





str(dtm,list.len=999)

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded[[1]]

corpusAdded = tm_map(corpusAdded, PlainTextDocument)

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded = tm_map(corpusAdded, stemDocument)

strwrap(corpusAdded[1])


# Create matrix

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

length(stopwords("english")) 

# Remove sparse terms
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Create data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))


#prepend all words with the letter A
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

str(wordsAdded)


#create a Removed terms corpus

# Create corpus

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument)



# Create matrix

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved  


# Remove sparse terms
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# Create data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved  ))

#prepend all words with the letter R
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))


#nwords

ncol(wordsRemoved)
str(wordsRemoved)
str(wordsAdded)


#combine dataframes
wikiWords = cbind(wordsAdded, wordsRemoved)
str(wikiWords)
wikiWords$Vandal=wiki$Vandal



library(caTools)

set.seed(123)

spl = sample.split(wikiWords$Vandal, 0.7)

str(spl)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

table(test$Vandal)
618/1163



# Build a CART model

library(rpart)
library(rpart.plot)

summary(train)

#method - class = classification
CART.Vandal = rpart(Vandal~., data=train, method="class")

prp(CART.Vandal)



# Make predictions on the test set

pred = predict(CART.Vandal, newdata=test)
#first 10 rows, all the columns
pred[1:10,]
#select right-most or 2nd column
pred.prob = pred[,2]

# Compute accuracy

table(test$Vandal, pred.prob >= 0.5)

(618+12)/(618+12+0+533)

# Baseline model accuracy

table(test$responsive)
215/(215+42)


wikiWords2 = wikiWords

#grepl - find word in another word
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
summary(wikiWords2)

table(wikiWords2$HTTP)



wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)


# new cART model based on wikiwords2

#method - class = classification
CART.HTTP = rpart(Vandal~., data=wikiTrain2, method="class")
prp(CART.HTTP)


#make prediction
pred.HTTP = predict(CART.HTTP, newdata=wikiTest2)
#first 10 rows, all the columns
pred.HTTP[1:20,]
#select right-most or 2nd column
pred.HTTPprob = pred.HTTP[,2]

# Compute accuracy

table(wikiTest2$Vandal, pred.HTTPprob >= 0.5)



(609+57)/(609+57+488+9)

# Baseline model accuracy

table(test$responsive)
215/(215+42)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

summary(wikiWords2)
wikiWords2

sum(wikiWords2$NumWordsAdded)/nrow(wikiWords2)
mean(wikiWords2$NumWordsAdded)



#create split of wikiwords2 dataset and rerun CART model
wikiWordsTrain = subset(wikiWords2, spl==TRUE)
wikiWordsTest = subset(wikiWords2, spl==FALSE)



# new cART model based on wikiwords2

#method - class = classification
CART.wiki = rpart(Vandal~., data=wikiWordsTrain, method="class")
prp(CART.wiki)


#make prediction
pred.wiki = predict(CART.wiki, newdata=wikiWordsTest)
#first 10 rows, all the columns
pred.wiki[1:20,]
#select right-most or 2nd column
pred.wikiprob = pred.wiki[,2]

# Compute accuracy

table(wikiWordsTest$Vandal, pred.wikiprob >= 0.5)

(514+248)/(514+248+297+104)


#using non-Textual data to predict
wikiWords3=wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin





# build another newcART model based on wikiwords3

#create split of wikiwords2 dataset and rerun CART model
wikiWords3Train = subset(wikiWords3, spl==TRUE)
wikiWords3Test = subset(wikiWords3, spl==FALSE)



#method - class = classification
CART.wiki3 = rpart(Vandal~., data=wikiWords3Train, method="class")
prp(CART.wiki3)


#make prediction
pred.wiki3 = predict(CART.wiki3, newdata=wikiWords3Test)
#first 10 rows, all the columns
pred.wiki3[1:20,]
#select right-most or 2nd column
pred.wiki3prob = pred.wiki3[,2]

# Compute accuracy

table(wikiWords3Test$Vandal, pred.wiki3prob >= 0.5)

(595+241)/nrow(wikiWords3Test)



corpusAdded = tm_map(corpusAdded, PlainTextDocument)

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded = tm_map(corpusAdded, stemDocument)

strwrap(corpusAdded[1])

