###########
#LIBRARIES
library("here")
library("jagsUI")
###########

#This code performs the regression analysis for artificial nests in Barzen et al. 2018 
#The purpose is to evaluate whether CO2 traps at artificial nests provided a useful index of black fly abundance as measured at nearby traps

#data come from 4 pairs of CO2 traps and artificial nests
#counts of the number of each species in the traps 
#counts of the number of each species in the nests (on the artificial egg and on the artificial bird)

#read in data for 3 species (S. annulus, S. johannseni, and S. meridionale) 
sann <- read.csv(here("data","artificial_nests_S.ann.csv"))
sjoh <- read.csv(here("data","artificial_nests_S.joh.csv"))
smer <- read.csv(here("data","artificial_nests_S.mer.csv"))

##################################S. annulus##############################
#see how many of the triplet have at last one data point missing
#triplet is, for a given sample, the trap, the artificial bird, and the artificial egg
rm <- which(is.na(apply(all <- cbind(sann$CO2.Trap,sann$Bird,sann$Egg),1,function(x){sum(x)}))==TRUE)
length(rm)
#reduce dataset by removing NAs  
sann.rd <- sann[-c(rm),]

#create input data set - use log(count + 1) transformation on counts 
#the total for the artificial nest is the total number on the bird and on the egg 
total <- log(sann.rd$Bird + sann.rd$Egg +1) 
CO2 <- log(sann.rd$CO2.Trap+1)
trap <- as.factor(sann.rd$Trap)
year <- as.factor(sann.rd$Year)
trans.sann <- data.frame(cbind(total,CO2,trap,year))

#Regression including interaction with trap 
fit1.ann <- lm(total ~ CO2*trap, data = trans.sann)
# show results
summary(fit1.ann)
#interaction between CO2 trap count and trap not important 
confint(fit1.ann, level = 0.95)

#Regression including interaction with year 
fit2.ann <- lm(total ~ CO2*year, data = trans.sann)
# show results
summary(fit2.ann) 
#interaction between CO2 trap count and year not important 
confint(fit2.ann, level = 0.95)

#Final Regression with main effect only 
fit3.ann <- lm(total ~ CO2, data = trans.sann)
#show results
summary(fit3.ann)
confint(fit3.ann, level = 0.95)

##################################S. johannseni##############################

#see how many of the triplet have at last one data point missing
#triplet is, for a given sample, the trap, the artificial bird, and the artificial egg
rm <- which(is.na(apply(all <- cbind(sjoh$CO2.Trap,sjoh$Bird,sjoh$Egg),1,function(x){sum(x)}))==TRUE)
length(rm)
#reduce dataset by removing NAs
sjoh.rd <- sjoh[-c(rm),]

#create input data set - use log(count + 1) transformation on counts 
#the total for the artificial nest is the number on the bird and on the egg 
total <- log(sjoh.rd$Bird + sjoh.rd$Egg +1) 
CO2 <- log(sjoh.rd$CO2.Trap+1)
trap <- as.factor(sjoh.rd$Trap)
year <- as.factor(sjoh.rd$Year)
trans.sjoh <- data.frame(cbind(total,CO2,trap,year))

#Regression including interaction with trap 
fit1.joh <- lm(total ~ CO2*trap, data = trans.sjoh)
# show results
summary(fit1.joh)
#interaction between CO2 trap count and trap not important 
confint(fit1.joh, level = 0.95)

#Final Regression with main effect only 
fit2.joh <- lm(total ~ CO2, data = trans.sjoh)
#show results
summary(fit2.joh)
confint(fit2.joh, level = 0.95)

##################################S. meridionale##############################

#only one S. meridionale was ever counted at a trap
print(smer$Bird)
print(smer$Egg)
