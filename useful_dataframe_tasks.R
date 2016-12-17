#useful dataframe 

str(mtcars)

#dimensions of a data frame
dim(mtcars) #observations/variable

#change column name
#copy mtcars
mtcars2=mtcars

names(mtcars2)
#rename mpg to miles.per.gallon:
names(mtcars2)[names(mtcars2)=="mpg"] <- "miles.per.gallon"
names(mtcars2)


