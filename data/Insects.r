Insect.Predictors<-function(){

n.09 <- length(which(data.set$Year==2009)) 
n.10 <- length(which(data.set$Year==2010))
n.11 <- length(which(data.set$Year==2011))
n.12 <- length(which(data.set$Year==2012))
n.13 <- length(which(data.set$Year==2013))

flies.09 <- read.csv(here("data","All_INSECTS_Working2009.csv"))
flies.10 <- read.csv(here("data","All_INSECTS_Working2010.csv"))
flies.11 <- read.csv(here("data","All_INSECTS_Working2011.csv"))
flies.12 <- read.csv(here("data","All_INSECTS_Working2012.csv"))
flies.13 <- read.csv(here("data","All_INSECTS_Working2013.csv"))

##Set up trap locations in 2009
Trap.Locs.09 <- matrix(nrow=7,ncol=2)
colnames(Trap.Locs.09) <- c("lat","long")
Trap.Locs.09[1,(1:2)] <- c(44.069139653,-90.154327916)
Trap.Locs.09[2,(1:2)] <- c(44.154138054,-90.216879631)
Trap.Locs.09[3,(1:2)] <- c(44.195926104,-90.186617010)
Trap.Locs.09[4,(1:2)] <- c(44.190526781,-90.212996751)
Trap.Locs.09[5,(1:2)] <- c(44.099138163,-90.191936538)
Trap.Locs.09[6,(1:2)] <- c(44.151451264,-90.149261691)
Trap.Locs.09[7,(1:2)] <- c(44.085281771,-90.168858935)

#In 2010, only traps 3,6,7 were used
Trap.Locs.10 <- matrix(nrow=3,ncol=2)
colnames(Trap.Locs.10) <- c("lat","long")
Trap.Locs.10[1,(1:2)] <- Trap.Locs.09[3,(1:2)]
Trap.Locs.10[2,(1:2)] <- Trap.Locs.09[6,(1:2)]
Trap.Locs.10[3,(1:2)] <- Trap.Locs.09[7,(1:2)]

#All other years were like 2009
Trap.Locs.11 <- Trap.Locs.09
Trap.Locs.12 <- Trap.Locs.09
Trap.Locs.13 <- Trap.Locs.09

#Distance between traps and nests for each year
data.set.09 <-  data.set[which(data.set$Year == 2009),]
dist.09 <- matrix(nrow=nrow(data.set.09),ncol=7)
for(h in 1:nrow(data.set.09)){
  for(i in 1:7){
    dist.09[h,i] <- sqrt((data.set.09$Lat[h]-Trap.Locs.09[i,1])^2+(data.set.09$Long[h]-Trap.Locs.09[i,2])^2)
  }
}
data.set.10 <-  data.set[which(data.set$Year == 2010),]
dist.10 <- matrix(nrow=nrow(data.set.10),ncol=3)
for(h in 1:nrow(data.set.10)){
  for(i in 1:3){
    dist.10[h,i] <- sqrt((data.set.10$Lat[h]-Trap.Locs.10[i,1])^2+(data.set.10$Long[h]-Trap.Locs.10[i,2])^2)
  }
}
data.set.11 <-  data.set[which(data.set$Year == 2011),]
dist.11 <- matrix(nrow=nrow(data.set.11),ncol=7)
for(h in 1:nrow(data.set.11)){
  for(i in 1:7){
    dist.11[h,i] <- sqrt((data.set.11$Lat[h]-Trap.Locs.11[i,1])^2+(data.set.11$Long[h]-Trap.Locs.11[i,2])^2)
  }
}
data.set.12 <-  data.set[which(data.set$Year == 2012),]
dist.12 <- matrix(nrow=nrow(data.set.12),ncol=7)
for(h in 1:nrow(data.set.12)){
  for(i in 1:7){
    dist.12[h,i] <- sqrt((data.set.12$Lat[h]-Trap.Locs.12[i,1])^2+(data.set.12$Long[h]-Trap.Locs.12[i,2])^2)
  }
}
data.set.13 <-  data.set[which(data.set$Year == 2013),]
dist.13 <- matrix(nrow=nrow(data.set.13),ncol=7)
for(h in 1:nrow(data.set.13)){
  for(i in 1:7){
    dist.13[h,i] <- sqrt((data.set.13$Lat[h]-Trap.Locs.13[i,1])^2+(data.set.13$Long[h]-Trap.Locs.13[i,2])^2)
  }
}

#Resize 2010 to be same number of columns as other years 
dist.10.exp <- matrix(NA,nrow=nrow(dist.10),ncol=7)
dist.10.exp[,3] <- dist.10[,1]
dist.10.exp[,6] <- dist.10[,2]
dist.10.exp[,7] <- dist.10[,3]

#Combine all distance measurements into 1 object
dist.all <- rbind(dist.09,dist.10.exp,dist.11,dist.12,dist.13)

##########################################INSECTS WEIGHTED MEAN ACROSS TRAPS##########################################

#Set up S.annulus data for weighted mean across traps
Ann.wtmn.09 <- matrix(NA,nrow=n.09,ncol=(ncol(flies.09)-2)); junk <- flies.09[which(flies.09[,1] == "S.annulus"),-c(1,2)]
for(i in 1:n.09){
  for(j in 1:(ncol(flies.09)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.09[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Ann.wtmn.09[i,j] <- sum(hold)
      }
    }
  }
}

Ann.wtmn.10 <- matrix(NA,nrow=n.10,ncol=(ncol(flies.10)-2)); junk <- flies.10[which(flies.10[,1] == "S.annulus"),-c(1,2)]
for(i in 1:n.10){
  for(j in 1:(ncol(flies.10)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.10[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Ann.wtmn.10[i,j] <- sum(hold)
      }
    }
  }
}

Ann.wtmn.11 <- matrix(NA,nrow=n.11,ncol=(ncol(flies.11)-2)); junk <- flies.11[which(flies.11[,1] == "S.annulus"),-c(1,2)]
for(i in 1:n.11){
  for(j in 1:(ncol(flies.11)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.11[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Ann.wtmn.11[i,j] <- sum(hold)
      }
    }
  }
}

Ann.wtmn.12 <- matrix(NA,nrow=n.12,ncol=(ncol(flies.12)-2)); junk <- flies.12[which(flies.12[,1] == "S.annulus"),-c(1,2)]
for(i in 1:n.12){
  for(j in 1:(ncol(flies.12)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.12[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Ann.wtmn.12[i,j] <- sum(hold)
      }
    }
  }
}

Ann.wtmn.13 <- matrix(NA,nrow=n.13,ncol=(ncol(flies.13)-2)); junk <- flies.13[which(flies.13[,1] == "S.annulus"),-c(1,2)]
for(i in 1:n.13){
  for(j in 1:(ncol(flies.13)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.13[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Ann.wtmn.13[i,j] <- sum(hold)
      }
    }
  }
}

Ann.wtmn <- rbind(Ann.wtmn.09,Ann.wtmn.10,Ann.wtmn.11,Ann.wtmn.12,Ann.wtmn.13)

#Set up S.johannseni data for weighted mean across traps
Joh.wtmn.09 <- matrix(NA,nrow=n.09,ncol=(ncol(flies.09)-2)); junk <- flies.09[which(flies.09[,1] == "S.johannseni"),-c(1,2)]
for(i in 1:n.09){
  for(j in 1:(ncol(flies.09)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.09[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Joh.wtmn.09[i,j] <- sum(hold)
      }
    }
  }
}

Joh.wtmn.10 <- matrix(NA,nrow=n.10,ncol=(ncol(flies.10)-2)); junk <- flies.10[which(flies.10[,1] == "S.johannseni"),-c(1,2)]
for(i in 1:n.10){
  for(j in 1:(ncol(flies.10)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.10[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Joh.wtmn.10[i,j] <- sum(hold)
      }
    }
  }
}

Joh.wtmn.11 <- matrix(NA,nrow=n.11,ncol=(ncol(flies.11)-2)); junk <- flies.11[which(flies.11[,1] == "S.johannseni"),-c(1,2)]
for(i in 1:n.11){
  for(j in 1:(ncol(flies.11)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.11[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Joh.wtmn.11[i,j] <- sum(hold)
      }
    }
  }
}

Joh.wtmn.12 <- matrix(NA,nrow=n.12,ncol=(ncol(flies.12)-2)); junk <- flies.12[which(flies.12[,1] == "S.johannseni"),-c(1,2)]
for(i in 1:n.12){
  for(j in 1:(ncol(flies.12)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.12[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Joh.wtmn.12[i,j] <- sum(hold)
      }
    }
  }
}

Joh.wtmn.13 <- matrix(NA,nrow=n.13,ncol=(ncol(flies.13)-2)); junk <- flies.13[which(flies.13[,1] == "S.johannseni"),-c(1,2)]
for(i in 1:n.13){
  for(j in 1:(ncol(flies.13)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.13[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Joh.wtmn.13[i,j] <- sum(hold)
      }
    }
  }
}

Joh.wtmn <- rbind(Joh.wtmn.09,Joh.wtmn.10,Joh.wtmn.11,Joh.wtmn.12,Joh.wtmn.13)

#Set up S.meridionale data for weighted mean across traps
Mer.wtmn.09 <- matrix(NA,nrow=n.09,ncol=(ncol(flies.09)-2)); junk <- flies.09[which(flies.09[,1] == "S.meridionale"),-c(1,2)]
for(i in 1:n.09){
  for(j in 1:(ncol(flies.09)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.09[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Mer.wtmn.09[i,j] <- sum(hold)
      }
    }
  }
}

Mer.wtmn.10 <- matrix(NA,nrow=n.10,ncol=(ncol(flies.10)-2)); junk <- flies.10[which(flies.10[,1] == "S.meridionale"),-c(1,2)]
for(i in 1:n.10){
  for(j in 1:(ncol(flies.10)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.10[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Mer.wtmn.10[i,j] <- sum(hold)
      }
    }
  }
}

Mer.wtmn.11 <- matrix(NA,nrow=n.11,ncol=(ncol(flies.11)-2)); junk <- flies.11[which(flies.11[,1] == "S.meridionale"),-c(1,2)]
for(i in 1:n.11){
  for(j in 1:(ncol(flies.11)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.11[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Mer.wtmn.11[i,j] <- sum(hold)
      }
    }
  }
}

Mer.wtmn.12 <- matrix(NA,nrow=n.12,ncol=(ncol(flies.12)-2)); junk <- flies.12[which(flies.12[,1] == "S.meridionale"),-c(1,2)]
for(i in 1:n.12){
  for(j in 1:(ncol(flies.12)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.12[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Mer.wtmn.12[i,j] <- sum(hold)
      }
    }
  }
}

Mer.wtmn.13 <- matrix(NA,nrow=n.13,ncol=(ncol(flies.13)-2)); junk <- flies.13[which(flies.13[,1] == "S.meridionale"),-c(1,2)]
for(i in 1:n.13){
  for(j in 1:(ncol(flies.13)-2)){
    a <- junk[which(is.na(junk[,j])==FALSE),j];
    b <- dist.13[i,which(is.na(junk[,j])==FALSE)];
    hold <- rep(NA,length(a))
    for(k in 1:length(a)){
      if(length(a)>0){
        hold[k] <- (a[k]*(1/b[k])^2)/sum((1/b)^2)
        Mer.wtmn.13[i,j] <- sum(hold)
      }
    }
  }
}

Mer.wtmn <- rbind(Mer.wtmn.09,Mer.wtmn.10,Mer.wtmn.11,Mer.wtmn.12,Mer.wtmn.13)

##########################################DERIVED INSECT DATASETS##########################################
Ann.ln <- log(Ann.wtmn+1)
Joh.ln <- log(Joh.wtmn+1)
Mer.ln <- log(Mer.wtmn+1)

AnnJoh.ln <- log(Ann.wtmn + Joh.wtmn + 1)
AnnMer.ln <- log(Ann.wtmn + Mer.wtmn + 1)
JohMer.ln <- log(Joh.wtmn + Mer.wtmn + 1)
Comb.ln <- log(Ann.wtmn + Joh.wtmn + Mer.wtmn + 1)

##########################################STARTING VALUES##########################################
Ann.ln <- (Ann.ln-mean(Ann.ln,na.rm=T))/sd(Ann.ln,na.rm=T)
Joh.ln <- (Joh.ln-mean(Joh.ln,na.rm=T))/sd(Joh.ln,na.rm=T)
Mer.ln <- (Mer.ln-mean(Mer.ln,na.rm=T))/sd(Mer.ln,na.rm=T)
AnnJoh.ln <- (AnnJoh.ln-mean(AnnJoh.ln,na.rm=T))/sd(AnnJoh.ln,na.rm=T)
AnnMer.ln <- (AnnMer.ln-mean(AnnMer.ln,na.rm=T))/sd(AnnMer.ln,na.rm=T)
JohMer.ln <- (JohMer.ln-mean(JohMer.ln,na.rm=T))/sd(JohMer.ln,na.rm=T)
Comb.ln <- (Comb.ln-mean(Comb.ln,na.rm=T))/sd(Comb.ln,na.rm=T)


St.Ann.ln <- Ann.ln 
for(i in 1:nrow(Ann.ln)){
  St.Ann.ln[i,1] <- mean(Ann.ln[i,],na.rm=T)
}
for(i in 1:nrow(Ann.ln)){
  for(j in 2:ncol(Ann.ln)){
    if(is.na(St.Ann.ln[i,j])){ 
      St.Ann.ln[i,j] <- St.Ann.ln[i,j-1]
    }
  }
}
St.Ann.ln[!is.na(Ann.ln)] <- NA
for(i in 1:nrow(Ann.ln)){
  St.Ann.ln[i,1:(First.all[i]-1)] <- NA
  St.Ann.ln[i,(Last.all[i]+1):ncol(Ann.ln)] <- NA
}


St.Joh.ln <- Joh.ln 
for(i in 1:nrow(Joh.ln)){
  St.Joh.ln[i,1] <- mean(Joh.ln[i,],na.rm=T)
}
for(i in 1:nrow(Joh.ln)){
  for(j in 2:ncol(Joh.ln)){
    if(is.na(St.Joh.ln[i,j])){ 
      St.Joh.ln[i,j] <- St.Joh.ln[i,j-1]
    }
  }
}
St.Joh.ln[!is.na(Joh.ln)] <- NA
for(i in 1:nrow(Joh.ln)){
  St.Joh.ln[i,1:(First.all[i]-1)] <- NA
  St.Joh.ln[i,(Last.all[i]+1):ncol(Joh.ln)] <- NA
}


St.Mer.ln <- Mer.ln 
for(i in 1:nrow(Mer.ln)){
  St.Mer.ln[i,1] <- mean(Mer.ln[i,],na.rm=T)
}
for(i in 1:nrow(Mer.ln)){
  for(j in 2:ncol(Mer.ln)){
    if(is.na(St.Mer.ln[i,j])){ 
      St.Mer.ln[i,j] <- St.Mer.ln[i,j-1]
    }
  }
}
St.Mer.ln[!is.na(Mer.ln)] <- NA
for(i in 1:nrow(Mer.ln)){
  St.Mer.ln[i,1:(First.all[i]-1)] <- NA
  St.Mer.ln[i,(Last.all[i]+1):ncol(Mer.ln)] <- NA
}


St.AnnJoh.ln <- AnnJoh.ln 
for(i in 1:nrow(AnnJoh.ln)){
  St.AnnJoh.ln[i,1] <- mean(AnnJoh.ln[i,],na.rm=T)
}
for(i in 1:nrow(AnnJoh.ln)){
  for(j in 2:ncol(AnnJoh.ln)){
    if(is.na(St.AnnJoh.ln[i,j])){ 
      St.AnnJoh.ln[i,j] <- St.AnnJoh.ln[i,j-1]
    }
  }
}
St.AnnJoh.ln[!is.na(AnnJoh.ln)] <- NA
for(i in 1:nrow(AnnJoh.ln)){
  St.AnnJoh.ln[i,1:(First.all[i]-1)] <- NA
  St.AnnJoh.ln[i,(Last.all[i]+1):ncol(AnnJoh.ln)] <- NA
}


St.AnnMer.ln <- AnnMer.ln 
for(i in 1:nrow(AnnMer.ln)){
  St.AnnMer.ln[i,1] <- mean(AnnMer.ln[i,],na.rm=T)
}
for(i in 1:nrow(AnnMer.ln)){
  for(j in 2:ncol(AnnMer.ln)){
    if(is.na(St.AnnMer.ln[i,j])){ 
      St.AnnMer.ln[i,j] <- St.AnnMer.ln[i,j-1]
    }
  }
}
St.AnnMer.ln[!is.na(AnnMer.ln)] <- NA
for(i in 1:nrow(AnnMer.ln)){
  St.AnnMer.ln[i,1:(First.all[i]-1)] <- NA
  St.AnnMer.ln[i,(Last.all[i]+1):ncol(AnnMer.ln)] <- NA
}


St.JohMer.ln <- JohMer.ln 
for(i in 1:nrow(JohMer.ln)){
  St.JohMer.ln[i,1] <- mean(JohMer.ln[i,],na.rm=T)
}
for(i in 1:nrow(JohMer.ln)){
  for(j in 2:ncol(JohMer.ln)){
    if(is.na(St.JohMer.ln[i,j])){ 
      St.JohMer.ln[i,j] <- St.JohMer.ln[i,j-1]
    }
  }
}
St.JohMer.ln[!is.na(JohMer.ln)] <- NA
for(i in 1:nrow(JohMer.ln)){
  St.JohMer.ln[i,1:(First.all[i]-1)] <- NA
  St.JohMer.ln[i,(Last.all[i]+1):ncol(JohMer.ln)] <- NA
}


St.Comb.ln <- Comb.ln 
for(i in 1:nrow(Comb.ln)){
  St.Comb.ln[i,1] <- mean(Comb.ln[i,],na.rm=T)
}
for(i in 1:nrow(Comb.ln)){
  for(j in 2:ncol(Comb.ln)){
    if(is.na(St.Comb.ln[i,j])){ 
      St.Comb.ln[i,j] <- St.Comb.ln[i,j-1]
    }
  }
}
St.Comb.ln[!is.na(Comb.ln)] <- NA
for(i in 1:nrow(Comb.ln)){
  St.Comb.ln[i,1:(First.all[i]-1)] <- NA
  St.Comb.ln[i,(Last.all[i]+1):ncol(Comb.ln)] <- NA
}



list(Ann.ln=Ann.ln,Joh.ln=Joh.ln,Mer.ln=Mer.ln,AnnJoh.ln=AnnJoh.ln,AnnMer.ln=AnnMer.ln,JohMer.ln=JohMer.ln,Comb.ln=Comb.ln,St.Ann.ln=St.Ann.ln,St.Joh.ln=St.Joh.ln,St.Mer.ln=St.Mer.ln,St.AnnJoh.ln=St.AnnJoh.ln,St.AnnMer.ln=St.AnnMer.ln,St.JohMer.ln=St.JohMer.ln,St.Comb.ln=St.Comb.ln)

}

