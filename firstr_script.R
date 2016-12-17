# variable naming
# case sensitive

# functions
# square root
SquareRoot3<-sqrt(9)

#set working directory
setwd("C:/Users/Chibot/Dropbox/edgedata")

Country<-c("Brazil","China","India","Switzerland","USA")
LifeExpectancy<-c(74,76,65,83,79)

#elements of a vector - 1st value in Countries
Countries[1]

#0 to 100, by increments of 2
seq(0,100,2)

#all data files are loaded in data frames
#most algorithms require

CountryData=data.frame(Country,LifeExpectancy)
CountryData

str(Country)

#now add a column to the dataframe
#add population in 1000's to each country
CountryData$Population=c(199000,1390000,1240000,7997,318000)
CountryData


#add two new observations
Country=c("Australia","Greece")
LifeExpectancy=c(82,81)
Population=c(23050,11125)

NewCountryData=data.frame(Country,LifeExpectancy,Population)
NewCountryData

#combine the two data frames - stacks the rows
AllCountryDAta=rbind(CountryData,NewCountryData)
AllCountryDAta

getwd()

WHO=read.csv("WHO.csv")
#structure of the data:
str(WHO)

#factor variables
#several different categories or levels
summary(WHO)

#subsetting data
# a new data frame with only countries in Europe
WHO_Europe=subset(WHO,Region=="Europe")
summary(WHO_Europe)

#let's write this out to csv
write.csv(WHO_Europe,"WHO_Europe.csv")

ls()
#
rm(WHO_Europe)


##basic data analysis
WHO$Under15
#average population 
mean(WHO$Under15)
#standard deviation
sd(WHO$Under15)

#summary of one variable
summary(WHO$Under15)
#1st quartile - value for which 25% of the values in the dataset lie below the 1st quartile

which.min(WHO$Under15) #row number of the observation

which.max(WHO$Under15) #124 th observation has the maximum value
WHO$Country[124]

plot(WHO$GNI,WHO$FertilityRate)

Outliers=subset(WHO,GNI>10000 & FertilityRate>2.5)
#number of rows in 
nrow(Outliers)
#output just a few variables from a dataset
Outliers[c("Country","GNI","FertilityRate")]

mean(WHO$Over60)

summary(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
#make which.min an argument
WHO$Country[which.min(WHO$Over60)]


#largest literacy rate
summary(WHO$LiteracyRate)
which.max(WHO$LiteracyRate)
WHO$Country[44]

#useful for understanding the distribution 
#of a variable
#histogram of cellular subscribers
hist(WHO$CellularSubscribers)


boxplot(WHO$LifeExpectancy ~WHO$Region)

boxplot(WHO$LifeExpectancy)


#adding labels
boxplot(WHO$LifeExpectancy ~WHO$Region, xlab="",ylab="Life Expectancy of Countries by Region")

tapply(WHO$Over60,WHO$Region,mean)
#removes values with missing literacy rate with na.rm argument
tapply(WHO$LiteracyRate,WHO$Region,mean,na.rm=TRUE)

tapply(WHO$ChildMortality,WHO$Region,mean,na.rm=TRUE)


