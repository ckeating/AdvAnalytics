#Unit 6 assignment 1 



#clear workspace
rm(list = ls())
setwd("C:/Users/Chibot/Dropbox/edgedata")

airlines=read.csv("AirlinesCluster.csv", stringsAsFactors=FALSE)
summary(airlines)
head(kos,5)


# caret need for normalization
install.packages("caret")
library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

# Compute distances
distances = dist(airlinesNorm, method = "euclidean")


# Hierarchical clustering
clusterair = hclust(distances, method = "ward.D") 

plot(clusterair)


clusterGroups.air = cutree(clusterair, k = 5)

str(clusterGroups.kos)
head(clusterGroups.kos,5)
clusterGroups.kos

#how many observations in each cluster?
table(clusterGroups.air)

clust1=subset(airlinesNorm,clusterGroups.air==1)
clust2=subset(airlinesNorm,clusterGroups.air==2)
clust3=subset(airlinesNorm,clusterGroups.air==3)
clust4=subset(airlinesNorm,clusterGroups.air==4)
clust5=subset(airlinesNorm,clusterGroups.air==5)


#compare average values for each cluster
tapply(airlines$Balance, clusterGroups.air, mean)
tapply(airlines$QualMiles, clusterGroups.air, mean)
tapply(airlines$BonusMiles, clusterGroups.air, mean)
tapply(airlines$BonusTrans, clusterGroups.air, mean)
tapply(airlines$FlightMiles, clusterGroups.air, mean)
tapply(airlines$FlightTrans, clusterGroups.air, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups.air, mean)




#run with k-means
#rerun
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5,iter.max = 1000)


KMC.Cluster=KMC$cluster

kmc.clust1=subset(airlinesNorm,KMC.Cluster==1)
kmc.clust2=subset(kos,KMC.Cluster==2)
kmc.clust3=subset(kos,KMC.Cluster==3)
kmc.clust4=subset(kos,KMC.Cluster==4)
kmc.clust5=subset(kos,KMC.Cluster==5)

table(KMC.Cluster)

tail(sort(colMeans(clust2)))

tail(sort(colMeans(kmc.clust1)))
tail(sort(colMeans(kmc.clust2)))
tail(sort(colMeans(kmc.clust3)))
tail(sort(colMeans(kmc.clust4)))
tail(sort(colMeans(kmc.clust5)))
tail(sort(colMeans(kmc.clust6)))
tail(sort(colMeans(kmc.clust7)))


tail(sort(colMeans(clust1)))
tail(sort(colMeans(clust2)))
tail(sort(colMeans(clust3)))
tail(sort(colMeans(clust4)))
tail(sort(colMeans(clust5)))
tail(sort(colMeans(clust6)))
tail(sort(colMeans(clust7)))


table(clusterGroups.kos, KMC$cluster)
str(kmc.clust3)
