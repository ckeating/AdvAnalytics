##load everything......

setwd("C:/Users/Chibot/Dropbox/edgedata")
CPS=read.csv("CPSData.csv")

MetroAreaMap=read.csv("MetroAreaCodes.csv")
CountryMap=read.csv("CountryCodes.csv")
#merge in the metro area codes
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
#merge in the country map data
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

str(MetroAreaMap)
str(CountryMap)

summary(CPS)
str(CPS)



###--------------- done loading




table(CPS$Industry)

sort(table(CPS$State)) 
table(CPS$Citizenship)
prop.table(table(CPS$Citizenship))

table(CPS$Race,CPS$Hispanic)
table(CPS$PeopleInHousehold)
summary(CPS$State)


table(CPS$Citizenship, is.na(CPS$Married))


table(CPS$State, is.na(CPS$MetroAreaCode))

dc=subset(CPS, State="Disctrict of Columbia" & is.na(MetroAreaCode))

View(dc)
table(dc$MetroAreaCode,is.na(CPS$MetroAreaCode))

prop.table(table(dc$Region,is.na(CPS$MetroAreaCode)),2)


table(dc$Region,is.na(CPS$MetroAreaCode))


prop.table(table(dc$Region,is.na(CPS$MetroAreaCode)))
prop.table(table(dc$Region,is.na(CPS$MetroAreaCode)),1)
library(gmodels)
CrossTable(dc$Region,is.na(CPS$MetroAreaCode))

# what state Which state has a proportion of interviewees
#living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean),decreasing=TRUE)

MetroAreaMap=read.csv("MetroAreaCodes.csv")
CountryMap=read.csv("CountryCodes.csv")

str(MetroAreaMap)
str(CountryMap)
summary(MetroAreaMap)
View(MetroAreaMap)
View(CountryMap)


#merge the CPS dataset with the MetroAreaMap


CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)

summary(CPS$MetroArea)
fucko=sort(table(CPS$MetroArea),decreasing=TRUE)

str(fucko)

View(fucko)
#highest proportion of interviewees of Hispanic ethnicity?
table(CPS$MetroArea,CPS$Hispanic)


sort(tapply(CPS$Hispanic,CPS$MetroArea,mean),decreasing=TRUE)

sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean),decreasing=TRUE)

sort(tapply())


tbl<-sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE),decreasing = FALSE)


#country of birth NOT the United States
tbl<-sort(tapply(CPS$Country != "United States", CPS$MetroArea, mean,na.rm=TRUE),decreasing = FALSE)

tapply(CPS$Country != "United States", CPS$MetroArea, mean,na.rm=TRUE)


View(sort(tapply(CPS$Country == "India", CPS$MetroArea, sum,na.rm=TRUE),decreasing=TRUE))
View(sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum,na.rm=TRUE),decreasing=TRUE))
View(sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum,na.rm=TRUE),decreasing=TRUE))


View(tbl)
spanish=subset(CPS,Hispanic==1)


summary(spanish)
table(spanish$Hispanic)


CPS$Hispanic

summary(CPS$Country)
sort(table(CPS$Country))


