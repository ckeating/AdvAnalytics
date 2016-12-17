
setwd("C:/Users/Chibot/Dropbox/edgedata")

#"IBM", "GE", "ProcterGamble", "CocaCola", and "Boeing"

IBM=read.csv("IBMStock.csv")
GE=read.csv("GEStock.csv")
ProcterGamble=read.csv("ProcterGambleStock.csv")
CocaCola=read.csv("CocaColaStock.csv")
Boeing=read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(GE)
summary(GE)
summary(IBM)
summary(CocaCola)
summary(ProcterGamble)
summary(Boeing)
sd(ProcterGamble$StockPrice)


IBM$Stock="IBM"
GE$Stock="GE"
CocaCola$Stock="CocaCola"
Boeing$Stock="Boeing"
ProcterGamble$Stock="ProcterGamble"

stocks=rbind(IBM,GE,CocaCola,Boeing,ProcterGamble)

stocks97=subset(stocks,Date>="2007-01-01" & Date <"2008-01-01")

stocks97
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="Red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="Blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(stocks97$Date,stocks97$StockPrice,type="l", col=stocks97$Stock)





plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="Blue")
lines(IBM$Date, IBM$StockPrice,col="Green")
lines(Boeing$Date, Boeing$StockPrice,col="Black")
lines(GE$Date, GE$StockPrice,col="Orange")
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="Blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="Green")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="Black")
lines(GE$Date[301:432], GE$StockPrice[301:432],col="Orange")

CocaCola
ProcterGamble
IBM
Boeing
GE


GE97=subset(GE,Date>="2007-09-01" & Date <"2008-01-01")
coke97=subset(CocaCola,Date>="2007-09-01" & Date <"2008-01-01")
ibm97=subset(IBM,Date>="2007-09-01" & Date <"2008-01-01")
boeing97=subset(Boeing,Date>="2007-09-01" & Date <"2008-01-01")
pg97=subset(ProcterGamble,Date>="2007-09-01" & Date <"2008-01-01")


plot(coke97$Date, coke97$StockPrice, type="l", col="red", ylim=c(0,210))
lines(pg97$Date, pg97$StockPrice,col="Blue")
lines(ibm97$Date, ibm97$StockPrice,col="Green")
lines(boeing97$Date, boeing97$StockPrice,col="Black")
lines(GE97$Date, GE97$StockPrice,col="Orange")



tapply(IBM$StockPrice,months(IBM$Date),mean)
tapply(GE$StockPrice,months(GE$Date),mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)
tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date),mean)
tapply(Boeing$StockPrice,months(Boeing$Date),mean)




