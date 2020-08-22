setwd("C:\\Users\\sconver\\Documents\\Projects - Old\\Cranes\\Eastern Migratory Population\\Nesting Research\\Data and Analyses\\Final Nest Survival Analyses")

#######NEST WITH TRT: 9/10 PRE, 11/12 TRT, 13 POST 

##General Date Info
#2009 first nest day is   Day 1 = April 2
#2009 last nest day is    Last Day = June 14 
#2010 first nest day is   Day 1 = April 1 
#2010 last nest day is    Last Day = June 11 
#2011 first nest day is   Day 1 = April 9 
#2011 last nest day is    Last Day = June 4 
#2012 first nest day is   Day 1 = March 26 
#2012 last nest day is    Last Day = June 13 
#2013 first nest day is   Day 1 = April 15 
#2013 last nest day is    Last Day = June 30 

##Encounter Histories
data.set <- read.csv("CraneNestData_Final.csv")
data.set[,1] <- as.character(data.set[,1])
data.set <- data.set[c(which(data.set$Include_YN=="Y")),]

enc.hist <- matrix(nrow=nrow(data.set),ncol=max(as.numeric(data.set$First.Day.Dead)))
enc.hist[] <- NA
First <- Last <- rep(NA,nrow(data.set))
Fate <- data.set$Fate
for(i in 1:nrow(enc.hist)){
  enc.hist[i,((data.set$Day.Found[i]):(data.set$Last.Day.Alive[i]))] <- 1    
  if(Fate[i] == 1){enc.hist[i,(data.set$First.Day.Dead[i])] <- 0}
  First[i] <- as.integer(data.set$Day.Found[i])
  Last[i] <- as.integer(data.set$First.Day.Dead[i])
}
n.ind <- nrow(data.set)

#Count of nests by year
n.09 <- length(which(data.set$Year==2009)) 
n.10 <- length(which(data.set$Year==2010))
n.11 <- length(which(data.set$Year==2011))
n.12 <- length(which(data.set$Year==2012))
n.13 <- length(which(data.set$Year==2013))
 
##Set up predictor variables

year <- data.set$Year-2008
n.year <- length((unique(year)))

trt <- rep(1,length(year))
for(i in 1:length(trt)){
if(year[i] == 2){trt[i] <- 2}
  if(year[i] == 3){trt[i] <- 3}
  if(year[i] == 4){trt[i] <- 3}
}
  
attempt <- data.set$Nest.Attempt
attempt[which(attempt == 3)] <- 2
attempt <- attempt-1

pair <- as.numeric(as.factor(as.numeric(data.set$Pair)))
n.pair <- length(unique(pair))

Age.M <- data.set$Age.Male
junk <- (Age.M-mean(Age.M))/sd(Age.M)
Age.M <- junk 

Age.F <- data.set$Age.Female
junk <- (Age.F-mean(Age.F))/sd(Age.F)
Age.F <- junk 

Age.Min <- data.set$Age.Min
junk <- (Age.Min-mean(Age.Min))/sd(Age.Min)
Age.Min <- junk 

Age.Max <- data.set$Age.Max
junk <- (Age.Max-mean(Age.Max))/sd(Age.Max)
Age.Max <- junk

Age.Avg <- data.set$Age.Avg
junk <- (Age.Avg-mean(Age.Avg))/sd(Age.Avg)
Age.Avg <- junk 

Exp.M <- data.set$Exper.Male
junk <- (Exp.M-mean(Exp.M))/sd(Exp.M)
Exp.M <- junk 

Exp.F <- data.set$Exper.Female
junk <- (Exp.F-mean(Exp.F))/sd(Exp.F)
Exp.F <- junk 

Exp.Min <- data.set$Exper.Min
junk <- (Exp.Min-mean(Exp.Min))/sd(Exp.Min)
Exp.Min <- junk 

Exp.Max <- data.set$Exper.Max
junk <- (Exp.Max-mean(Exp.Max))/sd(Exp.Max)
Exp.Max <- junk 

Exp.Avg <- data.set$Exper.Avg
junk <- (Exp.Avg-mean(Exp.Avg))/sd(Exp.Avg)
Exp.Avg <- junk 

CumAtt.M <- data.set$CumAtt.Male
junk <- (CumAtt.M-mean(CumAtt.M))/sd(CumAtt.M)
CumAtt.M <- junk 

CumAtt.F <- data.set$CumAtt.Female
junk <- (CumAtt.F-mean(CumAtt.F))/sd(CumAtt.F)
CumAtt.F <- junk 

CumAtt.Min <- data.set$CumAtt.Min
junk <- (CumAtt.Min-mean(CumAtt.Min))/sd(CumAtt.Min)
CumAtt.Min <- junk 

CumAtt.Max <- data.set$CumAtt.Max
junk <- (CumAtt.Max-mean(CumAtt.Max))/sd(CumAtt.Max)
CumAtt.Max <- junk 

CumAtt.Avg <- data.set$CumAtt.Avg
junk <- (CumAtt.Avg-mean(CumAtt.Avg))/sd(CumAtt.Avg)
CumAtt.Avg <- junk 

MGenAny <- data.set$MGenAny
junk <- (MGenAny-mean(MGenAny))/sd(MGenAny)
MGenAny <- junk 

MGenAll <- data.set$MGenAll
junk <- (MGenAll-mean(MGenAll))/sd(MGenAll)
MGenAll <- junk 

FGenAny <- data.set$FGenAny
junk <- (FGenAny-mean(FGenAny))/sd(FGenAny)
FGenAny <- junk 

FGenAll <- data.set$FGenAll
junk <- (FGenAll-mean(FGenAll))/sd(FGenAll)
FGenAll <- junk 

CGenAny <- data.set$CGenAny
junk <- (CGenAny-mean(CGenAny))/sd(CGenAny)
CGenAny <- junk 

CGenAll <- data.set$CGenAll
junk <- (CGenAll-mean(CGenAll))/sd(CGenAll)
CGenAll <- junk 

Pair.Type <- data.set$Pair.Type
junk <- (Pair.Type-mean(Pair.Type))/sd(Pair.Type)
Pair.Type <- junk

enc.hist.all <- enc.hist
First.all <- First
Last.all <- Last
attempt.all <- attempt
n.ind.all <- n.ind
year.all <- year
n.year.all <- n.year
pair.all <- pair
n.pair.all <- n.pair

##########################################INSECT DATA##########################################
source("Insects.r")
Insect.Predictors <- Insect.Predictors()
Ann.ln <- as.matrix(Insect.Predictors$Ann.ln)
Joh.ln <- as.matrix(Insect.Predictors$Joh.ln)
Mer.ln <- as.matrix(Insect.Predictors$Mer.ln)
AnnJoh.ln <- as.matrix(Insect.Predictors$AnnJoh.ln)
AnnMer.ln <- as.matrix(Insect.Predictors$AnnMer.ln)
JohMer.ln <- as.matrix(Insect.Predictors$JohMer.ln)
Comb.ln <- as.matrix(Insect.Predictors$Comb.ln)

St.Ann.ln <- as.matrix(Insect.Predictors$St.Ann.ln)
St.Joh.ln <- as.matrix(Insect.Predictors$St.Joh.ln)
St.Mer.ln <- as.matrix(Insect.Predictors$St.Mer.ln)
St.AnnJoh.ln <- as.matrix(Insect.Predictors$St.AnnJoh.ln)
St.AnnMer.ln <- as.matrix(Insect.Predictors$St.AnnMer.ln)
St.JohMer.ln <- as.matrix(Insect.Predictors$St.JohMer.ln)
St.Comb.ln <- as.matrix(Insect.Predictors$St.Comb.ln)


######################### BUGS INPUT #########################


#data
dataset <- list(enc.hist=enc.hist.all,First=First.all,Last=Last.all,n=n.ind.all,attempt=attempt.all,pair=pair.all,n.pair=n.pair.all,year=year.all,n.year=n.year.all,trt=trt)

#initial values
inits <- function(){
  list (beta.P=runif(n.pair.all),int.pair=runif(1),sigma.pair=runif(1),beta.S.attempt=runif(1))
}

#parameters
parameters <- c("int.pair","sigma.pair","sigma.year","beta.S.attempt","b.trt")

######################### CREATE MODEL FILE #########################

cat("
model {

#####LIKELIHOOD
for(i in 1:n){
  for(t in First[i]:(Last[i]-1)){
    enc.hist[i,(t+1)] ~ dbern(S[i,t]*enc.hist[i,t])   
    logit(S[i,t]) <- beta.P[pair[i]] + beta.Y[year[i]] + beta.S.attempt*attempt[i] + b.trt[trt[i]]
  }
}


#####PARAMETER PRIORS

#PARAMETER PRIORS
b.trt[1] <- 0
for(y in 2:3){
 b.trt[y] ~ dnorm(0,0.001)
}

#RANDOM EFFECTS AND ATTEMPT
for(i in 1:n.pair){
  beta.P[i] ~ dnorm(int.pair,tau.pair)
}
int.pair ~ dnorm(0,0.001)
tau.pair <- pow(sigma.pair,-2)
sigma.pair ~ dunif(0,25)

for(y in 1:n.year){
  beta.Y[y] ~ dnorm(0,tau.year)
}
tau.year <- pow(sigma.year,-2)
sigma.year ~ dunif(0,25)

beta.S.attempt ~ dnorm(0,0.001)

}
",file="nestmodel.txt")

######################### RUN BUGS #########################

#run MCMC
library("jagsUI")
jagsfit.1 <- jags(data=dataset, inits=inits, parameters.to.save=parameters, n.chains=3, n.burnin = 150000, n.iter=350000, n.thin=1, model.file="nestmodel.txt",parallel=TRUE)



S.trt <- 1/(1+exp(-(jagsfit.1$sims.list$int.pair + jagsfit.1$sims.list$b.trt[,3])))
S.control <- 1/(1+exp(-(jagsfit.1$sims.list$int.pair + jagsfit.1$sims.list$b.trt[,1])))

Success.trt <- S.trt^30
Success.control <- S.control^30

mean(S.trt)
quantile(S.trt,probs=c(0.025,0.975))
mean(S.control)
quantile(S.control,probs=c(0.025,0.975))
mean(Success.trt)
quantile(Success.trt,probs=c(0.025,0.975))
mean(Success.control)
quantile(Success.control,probs=c(0.025,0.975))
