rm(list=ls())

library(caret)
library(caTools)
library(dplyr)


setwd("C:/Users/Chibot/Dropbox/edgedata")
energy = read.csv("energy.csv")

View(energy)

summary(energy)

#lets plot
# Create the ggplot object with the data and the aesthetic mapping:
energyplot = ggplot(energy, aes(x = energy$YEAR, y = energy$GenTotal))


# Add the geom_point geometry
energyplot + geom_bar(stat="identity")


# Add fill for energy types
energyplot + geom_bar(stat="identity")


# Make a line graph instead:
energyplot + geom_hist()


#state with highest total energy
result=tapply(energy$GenTotalRenewable,energy$STATE,sum)
sort(result,decreasing=TRUE)


install.packages("sqldf")
library(sqldf)


str(energy)


energy %>% group_by(STATE)%>%
  summarise(totalrenewable=sum(GenTotalRenewable))%>%
  arrange(desc(totalrenewable))


#what year did the top state produce the most energy?
View(energy %>% group_by(STATE,YEAR)%>%
       filter(STATE=="ID")%>%
       summarise(totalrenewable=sum(GenTotalRenewable))%>%
       arrange(desc(totalrenewable)))


#states that voted republican
repub=subset(energy,energy$presidential.results==0)
View(repub)

mean(repub$AllSourcesCO2,na.rm=TRUE)

#dplyr version
energy %>% group_by(presidential.results)%>%
  summarise(AvgCO2=mean(AllSourcesCO2,na.rm=TRUE))


#add year to grouping
#dplyr version
View(energy %>% group_by(presidential.results,YEAR)%>%
       summarise(AvgNOx=mean(AllSourcesNOx,na.rm=TRUE)))

#how do CO2 emissions and energy Industrial sales
cor(energy$AllSourcesCO2,energy$EsalesIndustrial,use="complete")


#SO2 Industrial
cor(energy$AllSourcesSO2,energy$EsalesIndustrial,use="complete")
#[1] 0.4812317

#NOx residential
cor(energy$AllSourcesNOx,energy$EsalesResidential,use="complete")
# > cor(energy$AllSourcesNOx,energy$EsalesResidential,use="complete")
# [1] -0.5038829

#CO2 commercial
cor(energy$AllSourcesCO2,energy$EsalesCommercial,use="complete")
#[1] -0.373383


#boxplot of total energy price by state
boxplot(energy$EPriceTotal~energy$STATE)
tapply(energy$EPriceTotal,energy$STATE,mean)


#what year did the top state produce the most energy?
avgenergy=energy %>% group_by(STATE)%>%
       summarise(AvgEnergyPrice=mean(EPriceTotal))%>%
       arrange(desc(AvgEnergyPrice))

View(avgenergy)

#Let's look at highest Generated energy (GenTotal)
energy %>% group_by(STATE)%>%
  summarise(AvgGenerated=mean(GenTotal))%>%
  arrange(desc(AvgGenerated))

#We are interested in predicting whether states are going to increase their solar energy 
#generation over the next year. 
#Let's subset our dataset into a training and a testing set by using the following commands:

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]

#Let's build now a logistic regression model "mod" using
#the train set to predict the binary variable GenSolarBinary. 
#To do so, we consider the following as potential predictive variables: 
#GenHydro, GenSolar, CumlFinancial, CumlRegulatory, Total.salary, Import.

mod=glm(GenSolarBinary ~ GenHydro+GenSolar+CumlFinancial+CumlRegulatory + Total.salary+Import,family="binomial",data=train)

summary(mod)

#predict
#make prediction on test set with a threshold of .50
pred1=predict(mod,newdata=test,type="response")
table(test$GenSolarBinary,pred1>=0.50)
(154+18)/nrow(test)

str(test)
str(pred1)

#accuracy for states voting republican
repub=subset(test,test$presidential.results==0)
predrepub=predict(mod,newdata=repub,type="response")
table(repub$GenSolarBinary,predrepub>=0.50)
(90+2)/nrow(repub)


#accuracy for states voting republican
dem=subset(test,test$presidential.results==1)
preddem=predict(mod,newdata=dem,type="response")
table(dem$GenSolarBinary,preddem>=0.50)
(64+16)/nrow(dem)

# Let us create a train.limited and test.limited datasets, 
# where we only keep the variables 
# CumlRegulatory, CumlFinancial, presidential.results, Total.salary, and Import.
#train.limited=data.frame(train$CumlRegulatory, train$CumlFinancial, train$presidential.results, train$Total.salary, train$Import)


train.limited=select(train,CumlRegulatory, CumlFinancial, presidential.results, Total.salary, Import)
test.limited=select(test,CumlRegulatory, CumlFinancial, presidential.results, Total.salary, Import)

str(train.limited)
str(test.limited)


library(caret)

#normalize the datea
preproc = preProcess(train.limited)
train.limitednorm = predict(preproc, train.limited)
test.limitednorm= predict(preproc, test.limited)




#run kmeans clustering on normtrain
set.seed(100)
km = kmeans(train.limitednorm, centers = 2,iter.max = 1000)

table(km$cluster)


#obtain training and testing set cluster assignments
library(flexclust)
km.kcca = as.kcca(km, train.limitednorm )
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=test.limitednorm)



Train1=subset(train,clusterTrain==1)
Train2=subset(train,clusterTrain==2)

Test1=subset(test,clusterTest==1)
Test2=subset(test,clusterTest==2)

summary(Test1)

#which training sets have the most that voted republican?
table(Train1$presidential.results)
table(Train2$presidential.results)


mean(Train1$presidential.results)
mean(Train2$presidential.results)


#which sets enacted the most regulatory and financial incentives?
mean(Train1$CumlRegulatory)
mean(Train2$CumlRegulatory)

sum(Train1$CumlRegulatory)
sum(Train2$CumlRegulatory)



mean(Train1$CumlFinancial)
mean(Train2$CumlFinancial)

mean(Train1$AllSourcesCO2,na.rm=TRUE)
mean(Train2$AllSourcesCO2,na.rm = TRUE)

mean(Train1$AllSourcesSO2,na.rm=TRUE)
mean(Train2$AllSourcesSO2,na.rm = TRUE)

mean(Train1$AllSourcesNOx,na.rm=TRUE)
mean(Train2$AllSourcesNOx,na.rm = TRUE)

str(Train1)
str(Train2)

#let's create a logistic model on the first cluster
str(Train1)
mod1=glm(GenSolarBinary ~GenHydro+GenSolar +CumlFinancial+CumlRegulatory + Total.salary +Import, family="binomial",data=Train1) 
summary(mod1)


#let's create a logistic model on the second cluster
mod2=glm(GenSolarBinary ~GenHydro+GenSolar +CumlFinancial+CumlRegulatory + Total.salary +Import, family="binomial",data=Train2) 
summary(mod2)


#predict values
predictTest1=predict(mod1,type="response",newdata=Test1)
predictTest2=predict(mod2,type="response",newdata=Test2)

#what is the accuracy of test 1?
table(Test1$GenSolarBinary,predictTest1>0.5)
#accuracy
sum(diag(table(Test1$GenSolarBinary,predictTest1>0.5)))/nrow(Test1)
(115+4)/(115+4+11+1)

#predict using mod, the first model on Test1
predictTest1.mod=predict(mod,type="response",newdata=Test1)
table(Test1$GenSolarBinary,predictTest1.mod>0.5)
#accuracy
# table(Test1$GenSolarBinary,predictTest1.mod>0.5)
# 
# FALSE
# 0   116
# 1    15
116/(116+15)
#.8854962

#what is the accuracy of test 2?
table(Test2$GenSolarBinary,predictTest2>0.5)
#accuracy
sum(diag(table(Test2$GenSolarBinary,predictTest2>0.5)))/nrow(Test2)
(39+20)/(39+20+14+6)



#predict using mod, the first model on Test2
predictTest2.mod=predict(mod,type="response",newdata=Test2)
table(Test2$GenSolarBinary,predictTest2.mod>0.5)
#accuracy:
(38+18)/(38+18+16+7)



#evaluate performance of the cluster-then-predict algorithm
AllPredictions = c(predictTest1, predictTest2)

AllOutcomes = c(Test1$GenSolarBinary,Test2$GenSolarBinary)


table(AllOutcomes,AllPredictions>0.5)
#accuracy:
(154+24)/(154+24+25+7)

