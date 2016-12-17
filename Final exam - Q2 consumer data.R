
library(gmodels)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(caret)
library(caTools)
library(ROCR)
library(mice)


ls()


setwd("C:/Users/Chibot/Dropbox/edgedata")
households = read.csv("Households_final.csv")

str(households)
summary(households)

View(households)

CrossTable(households$MorningPct==100)
CrossTable(households$AfternoonPct==100)

CrossTable(households$NumVisits>300)

h1=subset(households,households$AvgSalesValue>150)
disc25=subset(households,households$AvgDiscount>25)

summary(disc25)
min(h1$AvgDiscount)


library(caret)
preproc = preProcess(households)
HouseholdsNorm = predict(preproc, households)

summary(HouseholdsNorm)


#kmeans clustering
set.seed(200)
HouseholdsNorm.KMC = kmeans(HouseholdsNorm, centers = 10,iter.max = 1000)


str(HouseholdsNorm.KMC)

HouseholdsNorm.KMC$centers

table(HouseholdsNorm.KMC$cluster)



#kmeans clustering - this time with 5
set.seed(5000)
HouseholdsNorm.KMC5 = kmeans(HouseholdsNorm, centers = 5,iter.max = 1000)


table(HouseholdsNorm.KMC5$cluster)


HouseholdsNorm.KMC5$centers


kmc.clust1=subset(airlinesNorm,KMC.Cluster==1)
kmc.clust2=subset(kos,KMC.Cluster==2)
kmc.clust3=subset(kos,KMC.Cluster==3)
kmc.clust4=subset(kos,KMC.Cluster==4)
kmc.clust5=subset(kos,KMC.Cluster==5)



set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)


#visualization
plotit=ggplot()


