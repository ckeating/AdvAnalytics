setwd("C:/Users/Chibot/Dropbox/edgedata")



edges=read.csv("edges.csv")

users=  read.csv("users.csv")

users
edges

subset(edges,V1==4019)
subset(edges,V2==4019)

table(users$school)
#install.packages("igraph")
library(igraph)
g = graph.data.frame(edges, FALSE, users)
# g = graph.data.frame(users, FALSE, edges) #error
# g = graph.data.frame(edges, TRUE, users)
# g = graph.data.frame(users, TRUE, edges)

g = graph.data.frame(edges, FALSE, users) 

plot(g, vertex.size=5, vertex.label=NA)
mean(degree(g))

userframe=as.data.frame(degree(g))

str(userframe)
userframe$friends=userframe$`degree(g)`
nrow(subset(userframe,userframe$friends>=10)     )

table(degree(g)>=10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

min(V(g)$size)
max(V(g)$size)


summary(degree(g))



V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
degree(g)

#color based on school

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "B"] = "gray"

V(g)$color[V(g)$school == "AB"] = "blue"




plot(g, vertex.label=NA)



#color based on locale
V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA,edge.width=.3)

rglplot(g)

coords=layout_with_fr(g,dim=3)

rglplot(g,layout=coords)

V(g)$locale
degree(g)

