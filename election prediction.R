##initialize session
#setwd("C:/Users/Chibot/Dropbox/edgedata")
setwd("C:/Users/Craig/Dropbox/edgedata")
polling=read.csv("pollingdata.csv")
library(caTools)
library(ROCR)
library(mice)
#randomly split data into training and test

pollingorig=polling;
str(polling)
summary(polling)

table(polling$Year)

#create data frame consisting only of the variables with missing values
simple=polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]

summary(simple)

set.seed(144)
imputed=complete(mice(simple))
summary(imputed)

#copy variables back into polling dataframe
polling$Rasmussen=imputed$Rasmussen
polling$SurveyUSA=imputed$SurveyUSA

summary(polling)
summary(pollingorig)


#train on data from the 2004 and 2008 elections
#test on 2012 election
# break into testing and training sets
Train=subset(polling,Year==2004 | Year==2008)
Test=subset(polling,Year==2012)

#first step is to understand our BASELINE model
#to do that we will look at the breakdown of the
#DEPENDENT variable in the training set
#this shows:
#in 47 of 100, the Democrat won the state, while 53 of 100 the Republican
#won the state
table(Train$Republican)
View(Train)


#this baseline model will have an accuracy of 53%
#on the 

#smart baseline
#rows are TRUE outcome
#columns are smart baseline predictions
#42 observations where smart baseline predicted a 
#democrat win, and a democrat one, 2 (zero column) where 
#the results were inconclusive, and 3 where the prediction was
#Republican, but a Democrat won
# the baseline is much better as it only made 3 mistakes
#and 3 inconclusives
table(Train$Republican,sign(Train$Rasmussen))


#testing for multicollinearity
#run a correlation between ONLY the numeric independent
#variables
polling=read.csv("PollingData_Imputed.csv")
str(polling)
str(Train)
#choosing dependent variable
#should be the one that is most HIGHLY correlated to the dependent variable
#Republican, which is PropR at 94.84%
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

#build model
#high coefficent for PropR
#the AIC measuring strength of the mode is 19.78
mod1=glm(Republican~PropR,data=Train,family = "binomial")
summary(mod1)

#compute predicted probabilites that Republican is going to win 
#on the training set
pred1=predict(mod1,type="response")
table(Train$Republican,pred1>=0.5)

#see if we can improve this model by selecting 
#another variable, choose the LEAST correlated independent
#variables - the choices would be Rasmussen and DiffCount
#OR SurveyUSA and DiffCount
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

mod2=mod1=glm(Republican~SurveyUSA+DiffCount,data=Train,family = "binomial")
pred2=predict(mod2,type="response")
table(Train$Republican,pred2>=0.5)

#evaluate model on testing set
table(Test$Republican,sign(Test$Rasmussen))
TestPrediction=predict(mod2,newdata=Test,type="response")

table(Test$Republican,TestPrediction>=0.5)
