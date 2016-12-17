USDA=read.csv("USDA.csv")
summary(USDA)

names(USDA)
str(USDA)

summary(USDA)
#max sodium is 38758 mg - which food is this?

USDA$Sodium
which.max(USDA$Sodium)
#list the description of the food with the highest amount of 
#sodium
USDA$Description[which.max(USDA$Sodium)]

#create a data frame of foods with Sodium above 10,000 mg
HighSodium=subset(USDA,Sodium>10000)
#how many rows in this data frame?
nrow(HighSodium)
HighSodium$Description


#ask R if caviar is in the description vector
match("CAVIAR",USDA$Description)

USDA$Sodium[match("CAVIAR",USDA$Description)]
#[1] 1500

#how does 1500 compare to the sodium values in the 
#entire data set?

summary(USDA$Sodium)
sd(USDA$Sodium,na.rm=TRUE)

#scatterplot
plot(USDA$Protein,USDA$TotalFat,xlab="Protein",ylab="Total Fat", main="Protein vs Fat",col="red")

#histogram
hist(USDA$VitaminC, main='Histogram of vitamin C levels',xlim=c(0,2000),breaks=100)
hist(USDA$VitaminC, main='Histogram of vitamin C levels',xlim=c(0,100),breaks=2000)

boxplot(USDA$Sugar, main='Boxplot of sugar levels',ylab="Sugar in grams")

USDA$HighSodium=as.numeric(USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE))
USDA$HighProtein=as.numeric(USDA$Protein>mean(USDA$Protein,na.rm=TRUE))
USDA$HighFat=as.numeric(USDA$TotalFat>mean(USDA$TotalFat,na.rm=TRUE))
USDA$HighCarbs=as.numeric(USDA$Carbohydrate>mean(USDA$Carbohydrate,na.rm=TRUE))


str(USDA)

