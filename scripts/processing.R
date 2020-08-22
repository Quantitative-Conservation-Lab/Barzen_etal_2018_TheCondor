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
