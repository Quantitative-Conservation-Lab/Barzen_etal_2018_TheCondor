library("R2jags")
library("coda")

attach.jags(jagsfit.1)
j.mcmc <-  as.mcmc(jagsfit.1)

gelman.diag(j.mcmc)

jagsfit.1$BUGSoutput$summary

jmc.mat <- as.matrix(j.mcmc)

#Get Model Ranks
models <- paste(jmc.mat[,"w.a"],jmc.mat[,"w.b"],jmc.mat[,"w.c"])
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
model.table[,3] <- round(as.numeric(model.rank.fin[,2])/(nrow(jmc.mat)),dig=3)

model.1 <- c(which(jmc.mat[,"w.a"] == 1 & jmc.mat[,"w.b"] == 0 & jmc.mat[,"w.c"] == 1))

annjoh.ct.model1 <- jmc.mat[model.1,"b.AnnJoh.ln"]
mn.annjoh.ct.model1 <- mean(annjoh.ct.model1)
LCI.annjoh.ct.model1 <- quantile(annjoh.ct.model1,probs=c(0.025))
UCI.annjoh.ct.model1 <- quantile(annjoh.ct.model1,probs=c(0.975))
est.annjoh.ct.model1 <- rbind(mn.annjoh.ct.model1,LCI.annjoh.ct.model1,UCI.annjoh.ct.model1)


cgenany.ct.model1 <- jmc.mat[model.1,"b.CGenAny"]
mn.cgenany.ct.model1 <- mean(cgenany.ct.model1)
LCI.cgenany.ct.model1 <- quantile(cgenany.ct.model1,probs=c(0.025))
UCI.cgenany.ct.model1 <- quantile(cgenany.ct.model1,probs=c(0.975))
est.cgenany.ct.model1 <- rbind(mn.cgenany.ct.model1,LCI.cgenany.ct.model1,UCI.cgenany.ct.model1)


xlab.name = expression(paste(italic("S.annulus"), " count effect on daily nest survival", sep="  "))
dens <- density(ann.ct.model1)
plot(x=rep(0,5),y=c(0:4),type='l',xlim=c(-1.5,1.5),main='',xlab=xlab.name,ylab='Density',yaxt='n',cex.lab=1.5,cex.axis=1.5)
lines(dens)


S.1.1.mean <- 1/(1+exp(-(jmc.mat[,"int.pair"] + jmc.mat[,"b.AnnJoh.ln"]*0)))
S.1.1.lo <- 1/(1+exp(-(jmc.mat[,"int.pair"] + jmc.mat[,"b.AnnJoh.ln"]*min(AnnJoh.ln,na.rm=T))))

S.pred.first <- matrix(NA,nrow=3,ncol=31)
for(i in 1:31){
 S.hold <- S.1.1.lo^(30-i+1)*S.1.1.mean^(i-1)
 S.pred.first[1,i] <- mean(S.hold)
 S.pred.first[2,i] <- quantile(S.hold,probs=0.025)
 S.pred.first[3,i] <- quantile(S.hold,probs=0.975)
}

S.1.2.lo <- 1/(1+exp(-(jmc.mat[,46] + jmc.mat[,45]*1 + jmc.mat[,34]*0)))
S.1.2.hi <- 1/(1+exp(-(jmc.mat[,46] + jmc.mat[,45]*1 + jmc.mat[,34]*1.5)))

S.pred.second <- matrix(NA,nrow=3,ncol=31)
for(i in 1:31){
 S.hold <- S.1.2.lo^(30-i+1)*S.1.2.hi^(i-1)
 S.pred.second[1,i] <- mean(S.hold)
 S.pred.second[2,i] <- quantile(S.hold,probs=0.025)
 S.pred.second[3,i] <- quantile(S.hold,probs=0.975)
}

library(Hmisc)

errbar(x=0:30, y=S.pred.first[1,], S.pred.first[2,], S.pred.first[3,], ylim=c(0,1), xlab="Days of Exposure", ylab="Nest Success",pch=1,cex.lab=1.5,cex.axis=1.5)
errbar(x=0.2:30.2, y=S.pred.second[1,], S.pred.second[2,], S.pred.second[3,],add=T,pch=19)


#Daily Survival and Incubation Survival - for treatment effect 

no.trt <- 1/(1+exp(-(jmc.mat[,"int.pair"])))
trt <- 1/(1+exp(-(jmc.mat[,"int.pair"]+jmc.mat[,"b.trt"])))

mon.no.trt <- no.trt^30
mon.trt <- trt^30 

no.trt.est <- c(mean(mon.no.trt),quantile(mon.no.trt,probs=c(0.025,0.975)))
trt.est <- c(mean(mon.trt),quantile(mon.trt,probs=c(0.025,0.975)))

day.no.trt.est <- c(mean(no.trt),quantile(no.trt,probs=c(0.025,0.975)))
day.trt.est <- c(mean(trt),quantile(trt,probs=c(0.025,0.975)))




