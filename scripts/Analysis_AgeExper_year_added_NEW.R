
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

library("jagsUI")
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
