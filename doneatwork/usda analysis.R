setwd("C:/edgedata")

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

#plot using histogram
hist(USDA$VitaminC,xlab="Vitamin C (mg)", main="Histogram of vitamin C levels")

#limit x axis to 0 to 100 mg
hist(USDA$VitaminC,xlab="Vitamin C (mg)", main="Histogram of vitamin C levels"
     ,xlim=c(0,100))

#break up cells into smaller pieces
hist(USDA$VitaminC,xlab="Vitamin C (mg)", main="Histogram of vitamin C levels"
     ,xlim=c(0,100),breaks=100)
#only see 5 cells - 20 milligrams long
#divided the spectrum of 2000 into 20 milligram buckets
hist(USDA$VitaminC,xlab="Vitamin C (mg)", main="Histogram of vitamin C levels"
     ,xlim=c(0,100),breaks=2000)


#more than 5000 foods have less than 1 mg of vitamin c

boxplot(USDA$Sugar,main = "Boxplot of sugar levels",ylab="sugar in grams")

mean(USDA$Sodium,na.rm=TRUE)

HighSodium=USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE)

#create a numeric vector
HighSodium=as.numeric(USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE))

str(HighSodium)

#add a variable to a dataframe
#use $ notation

USDA$HighSodium=as.numeric(USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE))
USDA$HighProtein=as.numeric(USDA$Protein>mean(USDA$Protein,na.rm=TRUE))
USDA$HighFat=as.numeric(USDA$TotalFat>mean(USDA$TotalFat,na.rm=TRUE))
USDA$HighCarbs=as.numeric(USDA$Carbohydrate>mean(USDA$Carbohydrate,na.rm=TRUE))

table(USDA$HighSodium)
#how many have high sodium and high fat
table(USDA$HighSodium ,USDA$HighFat,xlab="shhi")

#average amount of iron 
tapply(USDA$Iron,USDA$HighProtein,mean,na.rm=TRUE)

tapply(USDA$VitaminC,USDA$HighCarbs,max,na.rm=TRUE)

tapply(USDA$VitaminC,USDA$HighCarbs,summary,na.rm=TRUE)


#assignment 1
mvt=read.csv("mvtWeek1.csv")
str(mvt)

max(mvt$ID)

summary(mvt$Beat)
boxplot(mvt$Beat)

min(mvt$Beat)

mvt$Arrest==TRUE
table(mvt$Arrest)

table(mvt$LocationDescription)

tst<-subset(mvt,LocationDescription=="ALLEY")
str(tst)

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
DateConvert

as.Date(strptime(median(mvt$Date), "%m/%d/%y %H:%M"))

median(DateConvert)


mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

#replace old date variable

mvt$Date=DateConvert
str(mvt)

table(mvt$Arrest)

table(mvt$Month)

table(mvt$Weekday)
head(mvt)

Arrests=subset(mvt,mvt$Arrest==TRUE)
Arrests
table(Arrests$Arrest)
table(Arrests$Month)

png('hist_crime_bydate.png')
hist(mvt$Date, breaks=100)
dev.off()

boxplot(mvt$Arrest ~ mvt$Date,main = "Boxplot of arrest")
boxplot(mvt$Date ~ mvt$Arrest,main = "Boxplot of arrest")

table(mvt$Year,mvt$Arrest)

sort(table(mvt$LocationDescription))

Top5=subset(mvt,LocationDescription=="STREET"| LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"|
                LocationDescription=="GAS STATION"|LocationDescription=="DRIVEWAY - RESIDENTIAL" |LocationDescription=="ALLEY")

sort(table(Top5$LocationDescription))

str(Top5)

Top5$LocationDescription=factor(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription,Top5$Weekday)


