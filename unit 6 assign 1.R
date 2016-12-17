#Unit 6 assignment 1 



#clear workspace
rm(list = ls())
setwd("C:/Users/Chibot/Dropbox/edgedata")

kos=read.csv("dailykos.csv", stringsAsFactors=FALSE)
str(kos)
head(kos,5)


# Compute distances
distances = dist(kos, method = "euclidean")


# Hierarchical clustering
clusterkos = hclust(distances, method = "ward.D") 

plot(clusterkos)


clusterGroups.kos = cutree(clusterkos, k = 7)

table(clusterGroups.kos)

str(clusterGroups.kos)
head(clusterGroups.kos,5)
clusterGroups.kos

clust1=subset(kos,clusterGroups.kos==1)
clust2=subset(kos,clusterGroups.kos==2)
clust3=subset(kos,clusterGroups.kos==3)
clust4=subset(kos,clusterGroups.kos==4)
clust5=subset(kos,clusterGroups.kos==5)
clust6=subset(kos,clusterGroups.kos==6)
clust7=subset(kos,clusterGroups.kos==7)

str(clust3)

HierCluster = split(kos, clusterGroups.kos)

str(HierCluster)



tail(sort(colMeans(clust5)))


#run with k-means
#rerun
set.seed(1000)
KMC = kmeans(kos, centers = 7)

KMC.Cluster=KMC$cluster

kmc.clust1=subset(kos,KMC.Cluster==1)
kmc.clust2=subset(kos,KMC.Cluster==2)
kmc.clust3=subset(kos,KMC.Cluster==3)
kmc.clust4=subset(kos,KMC.Cluster==4)
kmc.clust5=subset(kos,KMC.Cluster==5)
kmc.clust6=subset(kos,KMC.Cluster==6)
kmc.clust7=subset(kos,KMC.Cluster==7)


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
