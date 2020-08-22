setwd("C:\\Users\\sconver\\Documents\\Projects - Old\\Cranes\\Eastern Migratory Population\\Nesting Research\\Data and Analyses\\Final Nest Initiation Analyses")

library ("date")
library("lme4")

##########################################################################
#                                                                        #
#           Bring in Basic Nest Initiation Data                          #
#                                                                        #  
##########################################################################

data.set <- read.csv("CraneNestData_Final.csv")

#Eliminate renests
data.set <- data.set[which(data.set$Nest.Attempt==1),]

#Pairs
pair <- as.numeric(as.factor(as.numeric(data.set$Pair)))
n.pair <- length(unique(pair))

#2005 is year 1
year <- data.set$Year-2004
n.year <- length(unique(year))

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

##########################################################################
#                                                                        #
#           Bring in Weather Data                                        #
#                                                                        #  
##########################################################################

temp <- read.csv("Weather.csv")

#Temp data date 
junk <- unlist(strsplit(as.character(temp$DateTime), " "))
junk  <- junk[seq.int(1L, length(junk), 2L)]
date <- strptime(junk,"%m/%e/%Y")

#Temperature converted to C
tempC <- (temp$Temp - 32)*(5/9)

#Set baseline for calculation of GDD
baseline <- 0

#Calculate GDD as the mean of the daily min and max, minus the baseline
#Calculated for each date
GDD <- array(NA,dim=c(9,4,31))
for(y in 2005:2013){
  for(m in 3:4){
    for(d in 1:31){
      junk1 <- max(tempC[intersect(which(as.numeric(format(date,'%Y'))==y),intersect(which(as.numeric(format(date,'%m'))==m),which(as.numeric(format(date,'%e'))==d)))])
      junk2 <- min(tempC[intersect(which(as.numeric(format(date,'%Y'))==y),intersect(which(as.numeric(format(date,'%m'))==m),which(as.numeric(format(date,'%e'))==d)))])
      GDD[y-2004,m-2,d] <- (junk1+junk2)/2 - baseline  
    }
  }
}
#Make GDD 0 when below baseline
GDD[which(GDD<baseline)] <- 0

#Figure out where weather data are missing 
count <- matrix(NA,nrow=9,ncol=4)
for(i in 1:9){
  for(j in 1:4){
    count[i,j] <- length(which(is.na(GDD[i,j,])==TRUE))
  }
}
#Fill in means from the day on either side for missing days - only 7 of these 
GDD[1,1,13] <- mean(c(GDD[1,1,12],GDD[1,1,14]))
GDD[1,2,24] <- mean(c(GDD[1,2,23],GDD[1,2,25]))
GDD[2,1,7:9] <- mean(c(GDD[2,1,6],GDD[2,1,10]))
GDD[7,3,19] <- mean(c(GDD[7,3,18],GDD[7,3,20]))
GDD[9,3,16] <- mean(c(GDD[9,3,15],GDD[9,3,17]))

#Put in matrix form with one column for each year 
GDD.mat <- matrix(NA,nrow=82,ncol=9)
for(y in 1:9){
  GDD.mat[1:21,y] <- GDD[y,1,11:31]
  GDD.mat[22:51,y] <- GDD[y,2,1:30]
  GDD.mat[52:82,y] <- GDD[y,3,]
}

#Get GDD for each nest 
#Sum over particular days depending on what range of days are of interest 
#Start Dates: Day 1 is March 1, Day 15 is March 15
#End Dates: Day 26 is the earliest nest date, Day 31 is March 31, Day 46 is the mean nest date
GDD.pred.1.26 <- GDD.pred.15.26 <- GDD.pred.1.31 <- GDD.pred.15.31 <- GDD.pred.1.46 <- GDD.pred.15.46 <- rep(NA,109)
for(i in 1:length(JDatecorr)){
  GDD.pred.1.26[i] <- sum(GDD.mat[1:26,year[i]])  
}  
for(i in 1:length(JDatecorr)){
  GDD.pred.15.26[i] <- sum(GDD.mat[15:26,year[i]])  
}  
for(i in 1:length(JDatecorr)){
  GDD.pred.1.31[i] <- sum(GDD.mat[1:31,year[i]])  
}  
for(i in 1:length(JDatecorr)){
  GDD.pred.15.31[i] <- sum(GDD.mat[15:31,year[i]])  
}  
for(i in 1:length(JDatecorr)){
  GDD.pred.1.46[i] <- sum(GDD.mat[1:46,year[i]])  
}  
for(i in 1:length(JDatecorr)){
  GDD.pred.15.46[i] <- sum(GDD.mat[15:46,year[i]])  
}  

#Set which one you want to look at correlations for 
GDD.pred <- GDD.pred.1.26 

#Get a single measure of GDD by year
GDD.year <- rep(NA,9)
GDD.year[1] <- GDD.pred[1]
GDD.year[2] <- GDD.pred[3]
GDD.year[3] <- GDD.pred[8]
GDD.year[4] <- GDD.pred[12]
GDD.year[5] <- GDD.pred[23]
GDD.year[6] <- GDD.pred[34]
GDD.year[7] <- GDD.pred[43]
GDD.year[8] <- GDD.pred[57]
GDD.year[9] <- GDD.pred[73]

#Look at correlation between weather and year 
year.cor <- c(1:9)

cor(GDD.year,year.cor)

##########################################################################
#                                                                        #
#           Bring in Other Predictor Data                                #
#                                                                        #  
##########################################################################

exper.M <- data.set$Exper.Male
exper.F <- data.set$Exper.Female
exper.Avg <- data.set$Exper.Avg
CumAtt.M <- data.set$CumAtt.Male
CumAtt.F <- data.set$CumAtt.Female
CumAtt.Avg <- data.set$CumAtt.Avg 

#Create a Figure of Experience Level by Year  
exper.yr <- rep(NA,9)
cumAtt.yr <- rep(NA,9)
count.exper.yr <- count.inexper.yr <- count.midexper.yr <- rep(NA,9)
for(i in 1:9){
  count.exper.yr[i] <- length(which(exper.Avg[which(year==i)]==1))
  count.inexper.yr[i] <- length(which(exper.Avg[which(year==i)]==0))
  count.midexper.yr[i] <- length(which(exper.Avg[which(year==i)]==0.5))
  exper.yr[i] <- mean(exper.Avg[which(year==i)])
  cumAtt.yr[i] <- mean(CumAtt.Avg[which(year==i)])
}
par(mar=c(5,5,4,2))
plot(x=c(2004.9:2012.9),y=count.inexper.yr,type='h',col='green',xlim=c(2005,2013.1),ylim=c(0,20),xlab="Year",ylab="Frequency",lwd=2,cex.axis=2,cex.lab=2)
lines(x=c(2005.1:2013.1),y=count.exper.yr,type='h',col='black',lwd=2)
lines(x=c(2005:2013),y= count.midexper.yr,type='h',col='blue',lwd=2)

##########################################################################
#                                                                        #
#           Standardize Everything                                       #
#                                                                        #  
##########################################################################

year.norm <- scale(year)
exper.M.norm <- scale(exper.M)
exper.F.norm <- scale(exper.F)
exper.Avg.norm <- scale(exper.Avg)
CumAtt.M.norm <- scale(CumAtt.M)
CumAtt.F.norm <- scale(CumAtt.F)
CumAtt.Avg.norm <- scale(CumAtt.Avg)
GDD.pred.1.26.norm <- scale(GDD.pred.1.26)
GDD.pred.15.26.norm <- scale(GDD.pred.15.26)
GDD.pred.1.31.norm <- scale(GDD.pred.1.31)
GDD.pred.15.31.norm <- scale(GDD.pred.15.31)
GDD.pred.1.46.norm <- scale(GDD.pred.1.46)
GDD.pred.15.46.norm <- scale(GDD.pred.15.46)

year.org <- year
year <- year.norm
exper.M <- exper.M.norm
exper.F <- exper.F.norm
exper.Avg <- exper.Avg.norm
CumAtt.M <- CumAtt.M.norm
CumAtt.F <- CumAtt.F.norm
CumAtt.Avg <- CumAtt.Avg.norm
GDD.1.26 <- GDD.pred.1.26.norm
GDD.15.26 <- GDD.pred.15.26.norm
GDD.1.31 <- GDD.pred.1.31.norm
GDD.15.31 <- GDD.pred.15.31.norm
GDD.1.46 <- GDD.pred.1.46.norm
GDD.15.46 <- GDD.pred.15.46.norm

##########################################################################
#                                                                        #
#           BUGS Model                                                   #
#                                                                        #  
##########################################################################

#data
dataset.1 <- list(JDatecorr=JDatecorr,GDD.1.26=GDD.1.26,GDD.15.26=GDD.15.26,GDD.1.31=GDD.1.31,GDD.15.31=GDD.15.31,GDD.1.46=GDD.1.46,GDD.15.46=GDD.15.46,pair=pair,n=n,n.pair=n.pair,n.year=n.year,year=year.org)

#initial values
inits.1 <- function(){
  list (int.pair=runif(1),sigma.pair=runif(1),w=rbinom(6,1,0.5))
}

#parameters
parameters.1 <- c("tau","int.pair","sigma.pair","sigma.year","b.GDD.1.26","b.GDD.15.26","b.GDD.1.31","b.GDD.15.31","b.GDD.1.46","b.GDD.15.46","w")

cat("
    model {
    
    for(i in 1:n){  
    JDatecorr[i] ~ dnorm(mu[i],tau)   
    mu[i] <- b1[pair[i]] + b2[year[i]] + w[1]*b.GDD.1.26*GDD.1.26[i] + w[2]*b.GDD.15.26*GDD.15.26[i] + w[3]*b.GDD.1.31*GDD.1.31[i] + w[4]*b.GDD.15.31*GDD.15.31[i] + w[5]*b.GDD.1.46*GDD.1.46[i] + w[6]*b.GDD.15.46*GDD.15.46[i]
    }
    
    for(c in 1:n.pair){
    b1[c] ~ dnorm(int.pair,tau.pair)
    }
    
    int.pair ~ dnorm(0,0.001)
    tau.pair <- pow(sigma.pair,-2)
    sigma.pair ~ dunif(0,25)
    
    for(c in 1:n.year){
    b2[c] ~ dnorm(0,tau.year)
    }
    
    tau.year <- pow(sigma.year,-2)
    sigma.year ~ dunif(0,25)
    
    tau <- pow(sigma,-2)
    sigma ~ dunif(0,25) 
    
    for(v in 1:6){
    w[v] ~ dbern(0.5)
    }
    
    tau.total <- pow(sigma.total,-2) 
    sigma.total ~ dunif(0,25)
    
    K <- w[1] + w[2] + w[3] + w[4] + w[5] + w[6]
    tau.model <- K*tau.total
    
    b.GDD.1.26 ~ dnorm(0,tau.model)
    b.GDD.15.26 ~ dnorm(0,tau.model)
    b.GDD.1.31 ~ dnorm(0,tau.model)
    b.GDD.15.31 ~ dnorm(0,tau.model)
    b.GDD.1.46 ~ dnorm(0,tau.model)
    b.GDD.15.46 ~ dnorm(0,tau.model)
    
    }
    ",file="model.1.txt")

#run MCMC
library("jagsUI")
jagsfit.1 <- jags(data=dataset.1, inits=inits.1, parameters.to.save=parameters.1, n.chains=3, n.burnin = 50000, n.iter = 100000, n.thin=1, model.file="model.1.txt",parallel=TRUE)

######PROCESSING OUTPUT######

#summary
jagsfit.1$summary
#Get values for weights
for(i in 1:6){
  print( round(mean(jagsfit.1$sims.list$w[,i]), dig=3)  )
  print( round(mean(jagsfit.1$sims.list$w[,i])/(1-mean(jagsfit.1$sims.list$w[,i])), dig=3)   )
}
