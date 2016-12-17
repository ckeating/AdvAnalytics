setwd("C:/Users/Chibot/Dropbox/edgedata")
baseball=read.csv("baseball.csv")



str(baseball)
summary(baseball)


moneyball=subset(baseball,Year<2002)

moneyball$RD=moneyball$RS-moneyball$RA
str(moneyball)

plot(moneyball$RD,moneyball$W)

winsreg=lm(W~RD,moneyball)
summary(winsreg)

#predict 

RunsReg=lm(RS~OBP + SLG  , data=moneyball)
summary(RunsReg)

RunAllowed=lm(RA ~ OOBP + OSLG , data=moneyball)

summary(RunAllowed)

#Predicting Runs and Wins

