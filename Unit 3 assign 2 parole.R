##initialize session
setwd("C:/Users/Chibot/Dropbox/edgedata")
#setwd("C:/Users/Craig/Dropbox/edgedata")
parole=read.csv("parole.csv")
library(caTools)
library(ROCR)
#install.packages("mice")
library(mice)
#randomly split data into training and test


str(parole)
summary(parole)

#songs by year
table(parole$violator)

#convert variables state and crime to factors
parole$state=as.factor(parole$state)
parole$crime=as.factor(parole$crime)

str(parole)
parole

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

mod1=glm(violator ~ .,data=train,family = "binomial")
summary(mod1)

exp(1.61119919)

str(train)
str(test)
str(parole)

#predict 
pred1=predict(mod1,newdata=test,type="response")
summary(pred1)


#make prediction on test set with a threshold of .45
table(test$violator,pred1>=0.5)

#accuracy
accuracy=(167+12)/(167+12+11+12)
accuracy


#sensitivity
12/(12+11)
#specificity
167/(167+12)

#simple model
table(parole$violator)

597/(597+78)




library(ROCR)


ROCRpred=prediction(pred1,test$violator)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-.2,1.7))

as.numeric(performance(ROCRpred,"auc")@y.values)


summary(ROCRperf)

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
