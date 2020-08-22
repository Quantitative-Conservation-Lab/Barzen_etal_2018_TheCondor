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
n.year.all <- length(unique(year.all))
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
dataset <- list(enc.hist=enc.hist.all,First=First.all,Last=Last.all,n=n.ind.all,attempt=attempt.all,year=year.all,n.year=n.year.all,pair=pair.all,n.pair=n.pair.all,AnnJoh.ln=AnnJoh.ln,CumAtt.F=CumAtt.F,CGenAny=CGenAny)

#initial values
inits <- function(){
  list (beta.P=runif(n.pair.all),int.pair=runif(1),sigma.pair=runif(1),beta.S.attempt=runif(1),AnnJoh.ln=St.AnnJoh.ln,w.a=rbinom(1,1,0.5),w.b=rbinom(1,1,0.5),w.c=rbinom(1,1,0.5))
}

#parameters
parameters <- c("int.pair","sigma.pair","sigma.year","beta.S.attempt","b.AnnJoh.ln","mu.AnnJoh.ln","tau.AnnJoh.ln","rho.AnnJoh.ln","b.CumAtt.F","b.CGenAny","w.a","w.b","w.c")

######################### CREATE MODEL FILE #########################

cat("
model {

#####LIKELIHOOD
for(i in 1:n){
  AnnJoh.ln[i,First[i]] ~ dnorm(mu.AnnJoh.ln[year[i]],tau.AnnJoh.ln[year[i]])
  
  for(t in First[i]:(Last[i]-1)){
    enc.hist[i,(t+1)] ~ dbern(S[i,t]*enc.hist[i,t])   
    logit(S[i,t]) <- beta.P[pair[i]] + beta.Y[year[i]] + beta.S.attempt*attempt[i] + w.a*b.AnnJoh.ln*AnnJoh.ln[i,(t+1)] + w.b*b.CumAtt.F*CumAtt.F[i] + w.c*b.CGenAny*CGenAny[i] 

    AnnJoh.ln[i,(t+1)] ~ dnorm(mn.AnnJoh.ln[i,(t+1)],tau.AnnJoh.ln[year[i]])
    mn.AnnJoh.ln[i,(t+1)] <- mu.AnnJoh.ln[year[i]] + rho.AnnJoh.ln[year[i]]*(AnnJoh.ln[i,t]-mu.AnnJoh.ln[year[i]])
  }
}

#MODEL PRIORS 
tau.total ~ dgamma(3.29,7.8)
K <- w.a + w.b + w.c
tau.model <- K*tau.total

w.a ~ dbern(0.5)
w.b ~ dbern(0.5)
w.c ~ dbern(0.5)

#####PARAMETER PRIORS

#PARAMETER PRIORS - INSECT EFFECTS
b.AnnJoh.ln ~ dnorm(0,tau.model)
b.CumAtt.F ~ dnorm(0,tau.model) 
b.CGenAny ~ dnorm(0,tau.model)

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

#INSECT MODEL PARAMETERS 
 
for(y in 1:n.year){mu.AnnJoh.ln[y] ~ dnorm(0,0.001)}
for(y in 1:n.year){tau.AnnJoh.ln[y] ~ dgamma(0.1,0.1)}
for(y in 1:n.year){rho.AnnJoh.ln[y] ~ dnorm(0,0.001)}  

}
",file="nestmodel.txt")

######################### RUN BUGS #########################

start <- Sys.time()

library("jagsUI")
jagsfit.1 <- jags(data=dataset, inits=inits, parameters.to.save=parameters, n.chains=3, n.burnin = 10000, n.iter=50000, n.thin=1, model.file="nestmodel.txt", parallel=TRUE)

end <- Sys.time() - start
end

#Weights and BF 
round(mean(jagsfit.1$sims.list$w.a), dig=3)
round(mean(jagsfit.1$sims.list$w.a)/(1-mean(jagsfit.1$sims.list$w.a)), dig=3)

round(mean(jagsfit.1$sims.list$w.b), dig=3)
round(mean(jagsfit.1$sims.list$w.b)/(1-mean(jagsfit.1$sims.list$w.b)), dig=3)

round(mean(jagsfit.1$sims.list$w.c), dig=3)
round(mean(jagsfit.1$sims.list$w.c)/(1-mean(jagsfit.1$sims.list$w.c)), dig=3)



#Get Model Ranks
models <- paste(jagsfit.1$sims.list$w.a,jagsfit.1$sims.list$w.b,jagsfit.1$sims.list$w.c)
model.rank <- as.matrix(table(models))
model.rank.matrix <- matrix(nrow=length(model.rank),ncol=2)
model.rank.matrix[,1] <- row.names(model.rank)
model.rank.matrix[,2] <- model.rank[,1]
model.rank.sort <- as.matrix(model.rank[order(-as.numeric(model.rank.matrix[,2])),])
model.rank.fin <- matrix(nrow=length(model.rank),ncol=2)
model.rank.fin[,1] <- row.names(model.rank.sort)
model.rank.fin[,2] <- model.rank.sort[,1]
model.table <- matrix(nrow=nrow(model.rank.fin),ncol=3)
model.table[,1] <- model.rank.fin[,1]
model.table[,2] <- model.rank.fin[,2]
model.table[,3] <- round(as.numeric(model.rank.fin[,2])/(jagsfit.1$mcmc.info$n.samples),dig=3)

model.1 <- c(which(jagsfit.1$sims.list$w.a==1 & jagsfit.1$sims.list$w.b==0 & jagsfit.1$sims.list$w.c==1))


insect.model1 <- jagsfit.1$sims.list$b.AnnJoh.ln[model.1]
mn.insect.model1 <- mean(insect.model1)
LCI.insect.model1 <- quantile(insect.model1,probs=c(0.025))
UCI.insect.model1 <- quantile(insect.model1,probs=c(0.975))
est.insect.model1 <- rbind(mn.insect.model1,LCI.insect.model1,UCI.insect.model1)

Cgen.model1 <- jagsfit.1$sims.list$b.CGenAny[model.1]
mn.Cgen.model1 <- mean(Cgen.model1)
LCI.Cgen.model1 <- quantile(Cgen.model1,probs=c(0.025))
UCI.Cgen.model1 <- quantile(Cgen.model1,probs=c(0.975))
est.Cgen.model1 <- rbind(mn.Cgen.model1,LCI.Cgen.model1,UCI.Cgen.model1)





