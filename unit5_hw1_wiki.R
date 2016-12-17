setwd("C:/Users/Chibot/Dropbox/edgedata")
  
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)

wiki$Vandal = as.factor(wiki$Vandal)



# Load tm package

library(tm)

rm(corpusAdded)
# Create corpus

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


