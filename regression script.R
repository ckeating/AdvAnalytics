setwd("C:/Users/Chibot/Dropbox/edgedata")
wine=read.csv("wine.csv")

str(wine)
summary(wine)

model1=lm(Price ~AGST, data=wine)
summary(model1)


#compute SSE
model1$residuals

SSE=sum(model1$residuals^2)
SSE

#add another variable - 
model2=lm(Price ~AGST + HarvestRain, data=wine)
summary(model2)

#add all variables
model3=lm(Price ~AGST + HarvestRain + WinterRain + Age+FrancePop, data=wine)
summary(model3)

SSE=sum(model3$residuals^2)




SSE=sum(model2$residuals^2)
SSE



#predict using harvest rain and winter rain
#add another variable - 
modelrain=lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelrain)

#add all variables
model3=lm(Price ~AGST + HarvestRain + WinterRain + Age+FrancePop, data=wine)
summary(model3)

SSE=sum(model3$residuals^2)

#use only significant variables
model4=lm(Price ~AGST + HarvestRain + WinterRain  + Age, data=wine)
summary(model4)



##correlation
cor(wine$WinterRain,wine$Price)
cor(wine$Age,wine$FrancePop)

#correlation of all pairs of variables
cor(wine)

model5=lm(Price ~AGST + HarvestRain + WinterRain  , data=wine)
summary(model5)


#predicting with new data
wineTest=read.csv("wine_test.csv")
str(wineTest)

predictTest=predict(model4, newdata=wineTest)
predictTest

SSE=sum((wineTest$Price-predictTest)^2)
SST = sum((wineTest$Price-mean(wine$Price))^2)
1-SSE/SST
