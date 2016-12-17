# Unit 4 - "Judge, Jury, and Classifier" Lecture

setwd("C:/Users/Chibot/Dropbox/edgedata")
gerber=read.csv("gerber.csv")
str(gerber)
summary(gerber)
table(gerber$voting)
108696/(235388+108696)

table()

head(gerber,10)

table(gerber$voting,)

table(gerber$hawthorne==1,gerber$voting)
table(gerber$civicduty==1,gerber$voting)
table(gerber$neighbors==1,gerber$voting)
table(gerber$self==1,gerber$voting)

14438/(14438+23763)
12/37
12/38
14/37
13/38

#mean value of voting by the group
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

str(gerber)
#build a logistic regression model
vote.log=glm(voting ~ civicduty + hawthorne + self+neighbors, data=gerber,family=binomial)
summary(vote.log)


#predict then measure accuracy
predict.vote.log=predict(vote.log,type="response")

acctbl=table(gerber$voting,predict.vote.log>.3)
sum(diag(acctbl))/sum(acctbl)

134513+100875+56730+51966
134513+51966
sum(acctbl)

sum(diag(acctbl))

acctbl=table(gerber$voting,predict.vote.log>.5)
sum(diag(acctbl))/sum(acctbl)

#baseline % of people who did not vote
table(gerber$voting)
235388/(235388+108696)

library(ROCR)
#auc of the model
ROCRpred = prediction(predict.vote.log, gerber$voting)

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#try out a tree for 
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3=rpart(voting ~ civicduty + hawthorne + self + neighbors +sex, data=gerber, cp=0.0)
prp(CARTmodel3)
library(rattle)
fancyRpartPlot(CARTmodel2)


str(gerber)
#create 1 tree only with control variable
#create another tree onlly with control and sex

CART.controlonly=rpart(voting ~ control, data=gerber, cp=0.0)
CART.controlsex=rpart(voting ~ control+sex, data=gerber, cp=0.0)

prp(CART.controlonly,digits = 6)
prp(CART.controlsex,digits = 6)

abs(.296638-.34)
.345818-.334176

mendiff=abs(.302795-.345818)
womdiff=abs(.290456-.334176)
mendiff-womdiff


log.controlsex=glm(voting ~ control+sex,family=binomial,data=gerber)
summary(log.controlsex)


Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(log.controlsex, newdata=Possibilities, type="response")

#tree- woman and control -        .302795
#logistic woman and control -     .2908065
abs(.290456-.2908065)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
.290456-.2904558




