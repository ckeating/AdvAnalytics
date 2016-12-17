# Unit 5 - Twitter

setwd("C:/Users/Chibot/Dropbox/edgedata")
# VIDEO 5

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

str(tweets)


# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages

#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)


# Create corpus
 
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus

corpus[[1]]


# Convert to lower-case

corpus = tm_map(corpus, tolower)

corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]




# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity
# find words that appear at least 20 times
findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

#argument 1 - matrix frequencies
#argument 2 - only keep terms that appear in .
sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame
# need this for predictive models

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly
# make all words appropriate variable names
# do this after each time you make a data frame
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))


# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

#method = "class" - classification model
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
# by making predictions on the test set
# tweetCART is model
# type="class" class predictions
predictCART = predict(tweetCART, newdata=testSparse, type="class")

#first argument -actual values, seconrd argument our predictions
table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)



# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

forest.table=table(testSparse$Negative, predictRF)

# Accuracy:
forest.accuracy=sum(diag(forest.table))/nrow(testSparse)
forest.accuracy

#use logistical regression:

tweetLog=glm(Negative ~ .,family=binomial,data=trainSparse)
summary(tweetLog)

predictions = predict(tweetLog, newdata=testSparse, type="response")

#accuracy:
#format: table(<training data$dependent variable>, <prediction dataset> > <threshold>)

log.table=table(testSparse$Negative,predictions>0.5)

log.accuracy=sum(diag(log.table))/nrow(testSparse)




