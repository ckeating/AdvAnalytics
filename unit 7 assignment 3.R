setwd("C:/Users/Chibot/Dropbox/edgedata")


tweets=read.csv("tweets.csv",stringsAsFactors = FALSE)


library(tm)
library(randomForest)
library(caTools)

#create corpus
corpus = Corpus(VectorSource(tweets$Tweet))


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
allTweets   = as.data.frame(as.matrix(dtm))



dtm


#install.packages("wordcloud")
library(wordcloud)
inspect

colnames(allTweets)

colSums(allTweets)

sum(allTweets)

wordcloud(colnames(allTweets),colSums(allTweets))


#remove stopwords and apple

# Remove stopwords and apple


#create corpus
corpus = Corpus(VectorSource(tweets$Tweet))


# 2) Convert to lowercase. 
corpus = tm_map(corpus, content_transformer(tolower))

#2a - convert to PlainTextDocument
corpus = tm_map(corpus, PlainTextDocument)

#   3) Remove the punctuation in
corpus = tm_map(corpus, removePunctuation)

# 4) Remove the English language stop words 
#corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
#corpus = tm_map(corpus, stemDocument)

# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtm = DocumentTermMatrix(corpus)

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
#spdtm = removeSparseTerms(dtm, 0.95)

# 8) Convert  to data frames
allTweets   = as.data.frame(as.matrix(dtm))



str(allTweets)


pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]


pal <- brewer.pal(9, "Purples")

wordcloud(colnames(allTweets),colSums(allTweets),min.freq=20,random.order = FALSE,rot.per=.15)
wordcloud(colnames(allTweets),colSums(allTweets),min.freq=30,random.order = FALSE)
wordcloud(colnames(allTweets),colSums(allTweets),min.freq=30,random.order = FALSE,random.color = TRUE)


wordcloud(colnames(allTweets),colSums(allTweets),min.freq=30,random.order = FALSE,random.color = TRUE,colors=pal)
wordcloud(colnames(allTweets),colSums(allTweets),min.freq=30,random.order = FALSE,random.color = FALSE,colors=pal)



pal=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]
pal=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]

wordcloud(colnames(allTweets),colSums(allTweets),min.freq=30,random.order = FALSE,random.color = FALSE,colors=pal)

display.brewer.all()

display.brewer.all()
