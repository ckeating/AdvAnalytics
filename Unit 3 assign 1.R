##initialize session
#setwd("C:/Users/Chibot/Dropbox/edgedata")
setwd("C:/Users/Craig/Dropbox/edgedata")
tunes=read.csv("songs.csv")
library(caTools)
library(ROCR)
library(mice)
#randomly split data into training and test


str(tunes)
summary(tunes)

#songs by year
table(tunes$year)
#songs by artist
View(table(tunes$artistname,tunes$Top10))

subset(tunes,artistname=="Michael Jackson" & Top10==1)
summary(tunes)
table(tunes$timesignature)

#highest tempo
tunes$songtitle[which.max(tunes$tempo)]


#split into test and train
SongsTrain=subset(tunes,tunes$year<=2009)
SongsTest=subset(tunes,tunes$year==2010)

table(SongsTrain$year)
table(SongsTest$year)

str(SongsTrain)


#create a vector of variables NOT to include in testing and training sets
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

#remove these variables from test and train
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

#test
str(SongsTrain)
str(SongsTest)

#build model
SongsLog1=glm(Top10 ~ .,data=SongsTrain,family=binomial)
summary(SongsLog1)

#check correlation
cor(SongsTrain)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

summary(SongsLog1)
summary(SongsLog2)

#now remove energy
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

summary(SongsLog1)
summary(SongsLog2)
summary(SongsLog3)
cor(SongsTrain$energy,SongsTrain$loudness)

#make prediction on test set with a threshold of .45
pred1=predict(SongsLog3,newdata=SongsTest,type="response")
table(SongsTest$Top10,pred1>=0.45)
accuracy=(309+19)/(309+5+40+19)
accuracy

#baseline model - simply frequency of dependent variable???
table(SongsTest$Top10)
314/(314+59)
#sensitivity
19/(19+40)
#specificity
309/(309+5)
