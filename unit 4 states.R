data(state)
str(state)
statedata = data.frame(state.x77)
str(statedata)

state.lm=lm(Life.Exp ~ .,data=statedata)
summary(state.lm)

state.lm$residuals
#compute the Sum of Squared Errors (SSE)
SSE= sum(state.lm$residuals^2)
SSE
#28,394,314 - not very interpretable quantity
#calculate the root mean squared error - more interpretable
# and is more like the average error

#equal to the squared root of the SSE divided by the total number of observations
RMSE=sqrt(SSE/nrow(statedata))
RMSE

state.lm2=lm(Life.Exp ~ Population+ Murder+ Frost+HS.Grad ,data=statedata)
summary(state.lm2)
SSE2= sum(state.lm2$residuals^2)
SSE2

cor(statedata)

#CART model
state.CART=rpart(Life.Exp ~. , data=statedata)
prp(state.CART)

state.CART.pred=predict(state.CART,data=statedata)
SSE.CART=sum((state.CART.pred-statedata$Life.Exp)^2)
SSE.CART  
  
#remake tree, but make bigger  
state.CART5=rpart(Life.Exp ~. , data=statedata,minbucket=5)
prp(state.CART5)

state.CART5.pred=predict(state.CART5,data=statedata)
SSE.CART5=sum((state.CART5.pred-statedata$Life.Exp)^2)
SSE.CART5

#remake tree with only area as independent variable
state.CARTArea=rpart(Life.Exp ~ Area , data=statedata,minbucket=1)
prp(state.CARTArea)

state.CARTArea.pred=predict(state.CARTArea,data=statedata)
SSE.CARTArea=sum((state.CARTArea.pred-statedata$Life.Exp)^2)

#cross-fold validation
library(caret)
library(e1071)
numFolds=trainControl(method="cv", number=10)
#pick possible values for cp value
#seq from 0.01 to 0.5 in increments of 0.01
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
set.seed(111)
train(Life.Exp ~ .,data=statedata,method="rpart",trControl=numFolds,tuneGrid=cpGrid)


#create tree with cp value
statetreeCV=rpart(Life.Exp ~ .,data=statedata,cp=0.12)
prp(statetreeCV)

##SSE
statetreeCV.pred = predict(statetreeCV, data=statedata)
tree.sse = sum((statetreeCV.pred - statedata$Life.Exp)^2)
tree.sse



#retrain tree with only 
set.seed(111)
train(Life.Exp ~ Area,data=statedata,method="rpart",trControl=numFolds,tuneGrid=cpGrid)


#create tree with cp value
statetreeCV.area=rpart(Life.Exp ~ Area,data=statedata,cp=0.02)
prp(statetreeCV.area)

##SSE
statetreeCV.area.pred = predict(statetreeCV.area, data=statedata)
tree.CV.sse = sum((statetreeCV.area.pred - statedata$Life.Exp)^2)
tree.CV.sse


