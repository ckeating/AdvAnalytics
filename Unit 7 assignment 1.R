setwd("C:/Users/Chibot/Dropbox/edgedata")



statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

nrow(table(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")


polling=read.csv("PollingData_Imputed.csv")

summary(polling)

Train=subset(polling,Year>=2004 & Year<=2008)
Test=subset(polling,Year==2012 )

table(Test$Year)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

predictionDataFrame
summary(predictionDataFrame)

table(predictionDataFrame$TestPredictionBinary,predictionDataFrame$Test.State)

repub=subset(predictionDataFrame,TestPredictionBinary=1)
nrow(table(repub$Test.State))

mean(TestPrediction)
mean()

View(predictionDataFrame)
View(table(predictionDataFrame$Test.State,predictionDataFrame$TestPredictionBinary))

df=data.frame(table(predictionDataFrame$Test.State,predictionDataFrame$TestPredictionBinary))
df

table(TestPredictionBinary)



predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionDataFrame


predictionMap = merge(statesMap, predictionDataFrame, by = "region")

nrow

predictionMap = predictionMap[order(predictionMap$order),]

summary(predictionDataFrame)
nrow(statesMap)
nrow(predictionDataFrame)
nrow(predictionMap)

#color each state with a prediction
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")


#color 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ 
    geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#color by test prediction
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "white", high = "gray", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

predictionDataFrame


table(predictionDataFrame$Test.State,predictionDataFrame$TestPrediction)



    ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ 
      geom_polygon(color = "black",alpha=0.3) + 
      scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

