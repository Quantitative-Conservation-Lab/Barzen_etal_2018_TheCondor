setwd("C:\\Users\\sconver\\Documents\\Cranes\\Eastern Migratory Population\\Nesting Research\\Data and Analyses\\Final Nest Initiation Analyses")

library ("date")
library("lme4")

data.set <- read.csv("CraneNestData_Final.csv")

#Eliminate renests
data.set <- data.set[which(data.set$Nest.Attempt==1),]

exper <- data.set$CumAtt.F
exper.bin <- data.set$Exper.Female

Ind <- data.set$Female
Ind <- as.numeric(as.factor(Ind))
nind = length(unique(Ind))

year <- data.set$Year-2004

n=length(year)

date <- data.set$Date.Found
JDate <- as.date(as.character(date))
JDatecorr <- as.numeric(JDate-16436)

for(i in 1:length(JDatecorr)){
  if(year[i] == 1){JDatecorr[i] <- JDatecorr[i]}
  if(year[i] == 2){JDatecorr[i] <- JDatecorr[i]-365}
  if(year[i] == 3){JDatecorr[i] <- JDatecorr[i]-(365*2)}
  if(year[i] == 4){JDatecorr[i] <- JDatecorr[i]-(365*3)}
  if(year[i] == 5){JDatecorr[i] <- JDatecorr[i]-(365*3+366)}
  if(year[i] == 6){JDatecorr[i] <- JDatecorr[i]-(365*4+366)}
  if(year[i] == 7){JDatecorr[i] <- JDatecorr[i]-(365*5+366)}
  if(year[i] == 8){JDatecorr[i] <- JDatecorr[i]-(365*6+366)}
  if(year[i] == 9){JDatecorr[i] <- JDatecorr[i]-(365*6+366*2)}
}
year <- year+2004

one <- rep(1,length(JDatecorr))

par(mar=c(5,5,4,2))
boxplot(JDatecorr~year,xlab="Year",ylab="Nest Initiation Date",cex.lab=2,cex.axis=2)


par(mar=c(5,5,4,2))
boxplot(JDatecorr~exper.bin,xlab="Experience",ylab="Nest Initiation Date",cex.lab=2,cex.axis=2)

par(mar=c(5,5,4,2))
boxplot(JDatecorr~one,xlab="All First Nests",ylab="Nest Initiation Date",cex.lab=2,cex.axis=2)
