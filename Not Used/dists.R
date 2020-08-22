setwd("C:\\Users\\sconverse\\Documents\\Cranes\\Eastern Migratory Population\\Nesting Research\\Data and Analyses\\Final Nest Initiation Analyses")

library ("date")
library("lme4")

##########################################################################
#                                                                        #
#           Bring in Basic Nest Initiation Data                          #
#                                                                        #  
##########################################################################

data.set <- read.csv("CraneNestData_Final.csv")

#Eliminate renests
#data.set <- data.set[which(data.set$Nest.Attempt==1),]

#Pairs
pair <- as.numeric(as.factor(as.numeric(data.set$Pair)))
n.pair <- length(unique(pair))

#2005 is year 1
year <- data.set$Year-2004

n=length(year)

#Initation Date
date <- data.set$Date.Found
JDate <- as.date(as.character(date))
JDatecorr <- as.numeric(JDate-16436)

#March 1 is Day 1 in every year 
for(i in 1:length(JDatecorr)){
  if(year[i] == 1){JDatecorr[i] <- JDatecorr[i]-59}
  if(year[i] == 2){JDatecorr[i] <- JDatecorr[i]-365-59}
  if(year[i] == 3){JDatecorr[i] <- JDatecorr[i]-(365*2)-59}
  if(year[i] == 4){JDatecorr[i] <- JDatecorr[i]-(365*3)-60}
  if(year[i] == 5){JDatecorr[i] <- JDatecorr[i]-(365*3+366)-59}
  if(year[i] == 6){JDatecorr[i] <- JDatecorr[i]-(365*4+366)-59}
  if(year[i] == 7){JDatecorr[i] <- JDatecorr[i]-(365*5+366)-59}
  if(year[i] == 8){JDatecorr[i] <- JDatecorr[i]-(365*6+366)-60}
  if(year[i] == 9){JDatecorr[i] <- JDatecorr[i]-(365*6+366*2)-59}
}

first <- data.frame(date = JDatecorr[which(data.set$Nest.Attempt==1)])
re <- data.frame(date = JDatecorr[which(data.set$Nest.Attempt>1)])

#Now, combine your two dataframes into one.  First make a new column in each.
first$seq <- 'first'
re$seq <- 're'

#and combine into your new data frame vegLengths
dist.nst <- rbind(first,re)

#now make your lovely plot
ggplot(dist.nst, aes(date, fill = seq)) + geom_density(alpha = 0.2)




