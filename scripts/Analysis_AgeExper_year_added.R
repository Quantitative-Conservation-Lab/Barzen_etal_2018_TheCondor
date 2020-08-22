###########
#LIBRARIES
library("here")
library("jagsUI")
###########

#######NEST WITH TRT: 9/10 PRE, 11/12 TRT, 13 POST 

##General Date Info
#2009 first nest day is   Day 1 = April 2
#2009 last nest day is    Day 73 = June 14 
#2010 first nest day is   Day 1 = April 1 
#2010 last nest day is    Day 73 = June 11 
#2011 first nest day is   Day 1 = April 9 
#2011 last nest day is    Day 73 = June 4 
#2012 first nest day is   Day 1 = March 26 
#2012 last nest day is    Day 73 = June 10 
#2013 first nest day is   Day 1 = April 15 
#2013 last nest day is    Day 73 = June 30 

##Encounter Histories
data.set <- read.csv(here("data","CraneNestData_Final.csv"))
data.set[,1] <- as.character(data.set[,1])
data.set <- data.set[c(which(data.set$Include_YN=="Y")),]

enc.hist <- matrix(nrow=nrow(data.set),ncol=max(as.numeric(data.set$First.Day.Dead)))
enc.hist[] <- NA
First <- Last <- rep(NA,nrow(data.set))
for(i in 1:nrow(enc.hist)){
  enc.hist[i,((data.set$Day.Found[i]):(data.set$Last.Day.Alive[i]))] <- 1    
  if(data.set$Fate[i] == 1){enc.hist[i,(data.set$First.Day.Dead[i])] <- 0}
  First[i] <- as.integer(data.set$Day.Found[i])
  Last[i] <- as.integer(data.set$First.Day.Dead[i])
}
n.ind <- nrow(data.set)
 
##Set up predictor variables

year <- data.set$Year-2008
n.year <- length((unique(year)))

attempt <- data.set$Nest.Attempt
attempt[which(attempt == 3)] <- 2
attempt <- attempt-1

pair <- as.numeric(as.factor(data.set$Pair))
n.pair <- length(unique(pair))

Age.M <- data.set$Age.Male
junk <- (Age.M-mean(Age.M))/sd(Age.M)
Age.M <- junk 

Age.F <- data.set$Age.Female
junk <- (Age.F-mean(Age.F))/sd(Age.F)
Age.F <- junk 

Age.Mn <- data.set$Age.Min
junk <- (Age.Mn-mean(Age.Mn))/sd(Age.Mn)
Age.Mn <- junk 

Age.Mx <- data.set$Age.Max
junk <- (Age.Mx-mean(Age.Mx))/sd(Age.Mx)
Age.Mx <- junk

Age.Av <- data.set$Age.Avg
junk <- (Age.Av-mean(Age.Av))/sd(Age.Av)
Age.Av <- junk 

Exp.M <- data.set$Exper.Male
junk <- (Exp.M-mean(Exp.M))/sd(Exp.M)
Exp.M <- junk 

Exp.F <- data.set$Exper.Female
junk <- (Exp.F-mean(Exp.F))/sd(Exp.F)
Exp.F <- junk 

Exp.Mn <- data.set$Exper.Min
junk <- (Exp.Mn-mean(Exp.Mn))/sd(Exp.Mn)
Exp.Mn <- junk 

Exp.Mx <- data.set$Exper.Max
junk <- (Exp.Mx-mean(Exp.Mx))/sd(Exp.Mx)
Exp.Mx <- junk 

Exp.Av <- data.set$Exper.Avg
junk <- (Exp.Av-mean(Exp.Av))/sd(Exp.Av)
Exp.Av <- junk 

CuAt.M <- data.set$CumAtt.Male
junk <- (CuAt.M-mean(CuAt.M))/sd(CuAt.M)
CuAt.M <- junk 

CuAt.F <- data.set$CumAtt.Female
junk <- (CuAt.F-mean(CuAt.F))/sd(CuAt.F)
CuAt.F <- junk 

CuAt.Mn <- data.set$CumAtt.Min
junk <- (CuAt.Mn-mean(CuAt.Mn))/sd(CuAt.Mn)
CuAt.Mn <- junk 

CuAt.Mx <- data.set$CumAtt.Max
junk <- (CuAt.Mx-mean(CuAt.Mx))/sd(CuAt.Mx)
CuAt.Mx <- junk 

CuAt.Av <- data.set$CumAtt.Av
junk <- (CuAt.Av-mean(CuAt.Av))/sd(CuAt.Av)
CuAt.Av <- junk 

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

enc.hist.all <- enc.hist
First.all <- First
Last.all <- Last
attempt.all <- attempt
n.ind.all <- n.ind
year.all <- year
n.year.all <- n.year
pair.all <- pair
n.pair.all <- n.pair


######################### BUGS INPUT #########################

#data
dataset <- list(enc.hist=enc.hist.all,First=First.all,Last=Last.all,n=n.ind.all,attempt=attempt.all,pair=pair.all,n.pair=n.pair.all,year=year.all,n.year=n.year.all,Age.M=Age.M,Age.F=Age.F,Age.Av=Age.Av,Exp.M=Exp.M,Exp.F=Exp.F,Exp.Av=Exp.Av,CuAt.M=CuAt.M,CuAt.F=CuAt.F,CuAt.Av=CuAt.Av)

#initial values
inits <- function(){
  list (beta.P=runif(n.pair.all),int.pair=runif(1),sigma.pair=runif(1),beta.S.attempt=runif(1),w.a=rbinom(1,1,0.5),w.b=rbinom(1,1,0.5),w.c=rbinom(1,1,0.5),w.d=rbinom(1,1,0.5),w.e=rbinom(1,1,0.5),w.f=rbinom(1,1,0.5),w.g=rbinom(1,1,0.5),w.h=rbinom(1,1,0.5),w.i=rbinom(1,1,0.5))
}

#parameters
parameters <- c("int.pair","sigma.pair","sigma.year","beta.S.attempt","b.Age.M","b.Age.F","b.Age.Av","b.Exp.M","b.Exp.F","b.Exp.Av","b.CuAt.M","b.CuAt.F","b.CuAt.Av","w.a","w.b","w.c","w.d","w.e","w.f","w.g","w.h","w.i")

######################### CREATE MODEL FILE #########################

cat("
model {

#####LIKELIHOOD
for(i in 1:n){  
  for(t in First[i]:(Last[i]-1)){
    enc.hist[i,(t+1)] ~ dbern(S[i,t]*enc.hist[i,t])   
    logit(S[i,t]) <- beta.P[pair[i]] + beta.Y[year[i]] + beta.S.attempt*attempt[i] + w.a*b.Age.M*Age.M[i] + w.b*b.Age.F*Age.F[i] + w.c*b.Age.Av*Age.Av[i] + w.d*b.Exp.M*Exp.M[i] + w.e*b.Exp.F*Exp.F[i] + w.f*b.Exp.Av*Exp.Av[i] + w.g*b.CuAt.M*CuAt.M[i] + w.h*b.CuAt.F*CuAt.F[i] + w.i*b.CuAt.Av*CuAt.Av[i] 
  }
}

#MODEL PRIORS 
tau.total ~ dgamma(3.29,7.8)
K <- w.a + w.b + w.c + w.d + w.e + w.f + w.g + w.h + w.i
tau.model <- K*tau.total

w.a ~ dbern(0.5)
w.b ~ dbern(0.5)
w.c ~ dbern(0.5)
w.d ~ dbern(0.5)
w.e ~ dbern(0.5)
w.f ~ dbern(0.5)
w.g ~ dbern(0.5)
w.h ~ dbern(0.5)
w.i ~ dbern(0.5)
#####PARAMETER PRIORS

#PARAMETER PRIORS - INSECT EFFECTS
b.Age.M ~ dnorm(0,tau.model)
b.Age.F ~ dnorm(0,tau.model)
b.Age.Av ~ dnorm(0,tau.model)
b.Exp.M ~ dnorm(0,tau.model)
b.Exp.F ~ dnorm(0,tau.model)
b.Exp.Av ~ dnorm(0,tau.model)
b.CuAt.M ~ dnorm(0,tau.model)
b.CuAt.F ~ dnorm(0,tau.model)
b.CuAt.Av ~ dnorm(0,tau.model)

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

start <- Sys.time()

jagsfit.1 <- jags(data=dataset, inits=inits, parameters.to.save=parameters, n.chains=3, n.burnin = 5000, n.iter=20000, n.thin=1, model.file="nestmodel.txt", parallel=TRUE)

end <- Sys.time() - start
end

traceplot(jagsfit.1)
#Get values for weights
round(mean(jagsfit.1$sims.list$w.a), dig=3)
round(mean(jagsfit.1$sims.list$w.a)/(1-mean(jagsfit.1$sims.list$w.a)), dig=3)

round(mean(jagsfit.1$sims.list$w.b), dig=3)
round(mean(jagsfit.1$sims.list$w.b)/(1-mean(jagsfit.1$sims.list$w.b)), dig=3)

round(mean(jagsfit.1$sims.list$w.c), dig=3)
round(mean(jagsfit.1$sims.list$w.c)/(1-mean(jagsfit.1$sims.list$w.c)), dig=3)

round(mean(jagsfit.1$sims.list$w.d), dig=3)
round(mean(jagsfit.1$sims.list$w.d)/(1-mean(jagsfit.1$sims.list$w.d)), dig=3)

round(mean(jagsfit.1$sims.list$w.e), dig=3)
round(mean(jagsfit.1$sims.list$w.e)/(1-mean(jagsfit.1$sims.list$w.e)), dig=3)

round(mean(jagsfit.1$sims.list$w.f), dig=3)
round(mean(jagsfit.1$sims.list$w.f)/(1-mean(jagsfit.1$sims.list$w.f)), dig=3)

round(mean(jagsfit.1$sims.list$w.g), dig=3)
round(mean(jagsfit.1$sims.list$w.g)/(1-mean(jagsfit.1$sims.list$w.g)), dig=3)

round(mean(jagsfit.1$sims.list$w.h), dig=3)
round(mean(jagsfit.1$sims.list$w.h)/(1-mean(jagsfit.1$sims.list$w.h)), dig=3)

round(mean(jagsfit.1$sims.list$w.i), dig=3)
round(mean(jagsfit.1$sims.list$w.i)/(1-mean(jagsfit.1$sims.list$w.i)), dig=3)
