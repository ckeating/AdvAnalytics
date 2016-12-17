
#examine the variables in relation to the predictor




setwd("C:/Users/Chibot/Dropbox/edgedata")


train <- read.csv("tittrain.csv")
test <- read.csv("tittest.csv")

str(train)

prop.table(table(train$Survived))

test$Survived<-rep(0,418)

summary(train$Sex)

summary(train)

#per cell %
prop.table(table(train$Sex, train$Survived))

# row-wise %
prop.table(table(train$Sex, train$Survived),1)

summary(train$Age)

train$Child<-0


train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)


aggregate(Survived ~ Child + Sex, data=train, FUN=sum)


aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})


library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

plot(fit)
text(fit)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


fancyRpartPlot(fit)


Prediction <- predict(fit, test, type = "class")
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
        control=rpart.control(minsplit=2, cp=0)
)


fancyRpartPlot(fit)
