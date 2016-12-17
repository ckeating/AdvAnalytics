setwd("C:/Users/Craig/Dropbox/edgedata")
loans=read.csv("loans_imputed.csv")
library(caTools)
library(ROCR)
library(mice)
#randomly split data into training and test
str(loans)
summary(loans)

library(gmodels)
CrossTable(loans$not.fully.paid)


set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loantrain = subset(loans, split == TRUE)
loantest = subset(loans, split == FALSE)


loanreg1=glm(not.fully.paid ~ .,data=loantrain,family = "binomial")
summary(loanreg1)

#predict 
predloan1=predict(loanreg1,newdata=loantest,type="response")
summary(predloan1)
str(predloan1)

summary(loantest)

loantest$predicted.risk=predloan1


#make prediction on test set with a threshold of .45
table(loantest$not.fully.paid,predloan1>=0.5)

#accuracy of model:
(2400+3)/(2400+13+457+3)

#accuracy of baseline model:
table(loantest$not.fully.paid)
CrossTable(loantest$not.fully.paid)


library(ROCR)


ROCRpred=prediction(predloan1,loantest$not.fully.paid)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-.2,1.7))

auc=as.numeric(performance(ROCRpred,"auc")@y.values)
auc


#int rate model

loanreg2=glm(not.fully.paid ~ int.rate,data=loantrain,family = "binomial")
summary(loanreg1)

#predict 
predloan1=predict(loanreg1,newdata=loantest,type="response")
summary(predloan1)
str(predloan1)

summary(loantest)

#remove non-numerics from frame

#create a vector of variables NOT to include in testing and training sets
nonloanvars = c("purpose")

#remove these variables from test and train
loancor = loans[ , !(names(loans) %in% nonloanvars) ]

cor(loancor)

str(loans)

#make test set predictions using the bivariate model

#predict values with bivariate model
predloanbi=predict(loanreg2,newdata=loantest,type="response")
summary(predloanbi)
str(predloanbi)

summary(loantest)

#make prediction on test set with bivariable model with a threshold of .5
table(loantest$not.fully.paid,predloanbi>=0.8)


loantest$predicted.risk=predloan1


#make prediction on test set with a threshold of .45
table(loantest$not.fully.paid,predloan1>=0.5)

#accuracy of model:
(2400+3)/(2400+13+457+3)

#accuracy of baseline model:
table(loantest$not.fully.paid)
CrossTable(loantest$not.fully.paid)


library(ROCR)


ROCRpred=prediction(predloanbi,loantest$not.fully.paid)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-.2,1.7))

auc=as.numeric(performance(ROCRpred,"auc")@y.values)
auc

#computing return
# To compute interest revenue, consider a $c investment in a loan that has an annual 
# interest rate r over a period of t years. 
# Using continuous compounding of interest, this investment pays 
# back c * exp(rt) dollars by the end of the t years, where exp(rt) is e raised to the r*t power.
# 
# How much does a $10 investment with an annual interest rate of 6% pay back 
# after 3 years, using continuous compounding of interest? 
# Hint: remember to convert the percentage to a proportion before 
# doing the math. Enter the number of dollars, without the $ sign.

c=10
r=.06
t=3
payback=c*(exp(r*t))
payback
profitcheck=payback-c
profitcheck
#c * exp(rt) - c
c * exp(r*t) - c
c * (exp(r*t)) + c

loantest$profit = exp(loantest$int.rate*3) - 1

loantest$profit[loantest$not.fully.paid == 1] = -1
summary(loantest$profit)
summary(loantest)

View(loantest)


highInterest=subset(loantest,loantest$int.rate>=.15)
summary(highInterest)
CrossTable(highInterest$not.fully.paid)

