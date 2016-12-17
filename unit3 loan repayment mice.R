library(gmodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(caret)

setwd("C:/Users/Chibot/Dropbox/edgedata")
census=read.csv("census.csv")

loans=read.csv("loans.csv")
str(loans)

CrossTable(loans$not.fully.paid)



library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed



