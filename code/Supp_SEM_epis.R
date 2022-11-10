##Simplified SEM script#
rm(list = ls())
library(tidyverse)
library(car)
library(piecewiseSEM)
library(nlme)
library(cowplot)
library("GGally")
library(ggplot2)
library(MuMIn)
library(ggthemes)


sem.imputed <- read.csv("input/SEM_imputed.csv",
                        stringsAsFactors = FALSE, sep = ",")

sem_complete <- sem.imputed
#Keeping parameters of interest
names(sem_complete)
sem_complete<-sem_complete[c("Island","Reef","Transect","Distance","Shoot.growth","C.production","SLA","Seagrass.P",
                             "Seagrass.N","LAI"
                             , "Bites.area" , "Seagrass.C13", "Epiphytes.area", "Seagrass.C", "Shoot.density"
)]

#As factors
sem_complete$Reef = factor(sem_complete$Reef)
sem_complete$Transect = factor(sem_complete$Transect)
sem_complete$Island = factor(sem_complete$Island)

#Data transformation
par(mfrow = c(4,4))
for(i in 5:length(sem_complete)){
  hist(sem_complete[,i], main = names(sem_complete[i]))
  qqPlot(sem_complete[,i], main = names(sem_complete[i]), pch=19, cex=1)
  mtext(round(shapiro.test(sem_complete[,i])$p.value,3), side=3)
} #

sem_complete$C.production = log(sem_complete$C.production)
sem_complete$LAI = sqrt(sem_complete$LAI)
sem_complete$Seagrass.N = log(sem_complete$Seagrass.N)
sem_complete$Seagrass.P = log(sem_complete$Seagrass.P)
sem_complete$Shoot.growth = log(sem_complete$Shoot.growth)
sem_complete$Epiphytes.area = sqrt(sem_complete$Epiphytes.area)

#Plotting variables against one another
names(sem_complete)
myColors<-c("#6A51A3","#238B45")
pairs(sem_complete[, c("Shoot.growth","C.production","SLA","Seagrass.P","Seagrass.N","LAI")], 
      pch = 19,  cex = 2,cex.labels=2,
      col = myColors[as.factor(sem_complete$Island)],
      lower.panel=NULL)

###SEMs###
# Haiti inside ------------------------------------------------------------
Haiti_near = subset(sem_complete, sem_complete$Island == "Haiti" & as.numeric(sem_complete$Distance) <=3)
Haiti_near$SLA[47] <- NA #Outlier
Haiti_near <- Haiti_near[complete.cases(Haiti_near),]

Haiti.SEM_near = psem(
  
  lme(SLA ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  lme(LAI ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  lme(Shoot.growth ~ Seagrass.P + Seagrass.N + SLA + Seagrass.C13 + Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  lme(C.production ~ Seagrass.P + Seagrass.N + SLA + LAI + Seagrass.C13 + Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Haiti_near)

summary(Haiti.SEM_near)
AIC(Haiti.SEM_near, aicc = T)
plot(Haiti.SEM_near)

# Graphical evaluation of the best model
lapply(Haiti.SEM_near[1:4], plot) #OK 
lapply(Haiti.SEM_near[1:4], qqnorm) #These are OK

#Exporatory - Removed non-significant paths
Haiti.SEM_near2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  lme(Shoot.growth ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  lme(C.production ~ Seagrass.P + LAI + Seagrass.C13, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near),
  
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  SLA %~~% Seagrass.C13,
  
  Haiti_near)

summary(Haiti.SEM_near2) #Simplest model is also best model
AIC(Haiti.SEM_near2, aicc = T)
plot(Haiti.SEM_near2)

# Graphical evaluation of the best model
lapply(Haiti.SEM_near2[1:3], plot) #OK 
lapply(Haiti.SEM_near2[1:3], qqnorm) #These are OK

#Variance inflation factors check
vif(lme(C.production ~ Seagrass.P + LAI + Seagrass.C13, 
        random=~1|Reef/Distance, na.action = na.omit, data=Haiti_near))


# Bahamas inside ----------------------------------------------------------
Bahamas_near = subset(sem_complete, sem_complete$Island == "Bahamas"& as.numeric(sem_complete$Distance) <=3)
Bahamas_near <- Bahamas_near[complete.cases(Bahamas_near),]

Bahamas.SEM_near = psem(
  
  lme(SLA ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(LAI ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(Shoot.growth ~ Seagrass.P + Seagrass.N + SLA + Seagrass.C13+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(C.production ~ Seagrass.P + Seagrass.N + SLA + LAI + Seagrass.C13+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Bahamas_near)

summary(Bahamas.SEM_near)
AIC(Bahamas.SEM_near, aicc = T)
plot(Bahamas.SEM_near)

# Graphical evaluation of the best model
lapply(Bahamas.SEM_near[1:4], plot) #OK 
lapply(Bahamas.SEM_near[1:4], qqnorm) #These are OK


##Exploratory - deleted non-significant paths
Bahamas.SEM_near2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(LAI ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(Shoot.growth ~ Seagrass.P + Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(C.production ~ Seagrass.N + SLA + LAI, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Bahamas_near)

summary(Bahamas.SEM_near2) #simplest model the best model
AIC(Bahamas.SEM_near2, aicc = T)
plot(Bahamas.SEM_near2)

# Graphical evaluation of the best model
lapply(Bahamas.SEM_near2[1:4], plot) #OK 
lapply(Bahamas.SEM_near2[1:4], qqnorm) #These are OK

#Variance inflation factors check
vif(lme(C.production ~ Seagrass.N + SLA + LAI, 
        random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near))
vif(lme(Shoot.growth ~ Seagrass.P + Epiphytes.area, 
        random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near))

# Haiti outside -----------------------------------------------------------
Haiti_far = subset(sem_complete, sem_complete$Island == "Haiti"& as.numeric(sem_complete$Distance) > 3)
Haiti_far$SLA[72] <- NA#Remove outlier
Haiti_far <- Haiti_far[complete.cases(Haiti_far),]

Haiti.SEM_far = psem(
  
  lme(SLA ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(LAI ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(Shoot.growth ~ Seagrass.P + Seagrass.N + SLA + Seagrass.C13+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(C.production ~ Seagrass.P + Seagrass.N + SLA + LAI + Seagrass.C13+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  SLA %~~% Seagrass.C13,
  
  Haiti_far)

summary(Haiti.SEM_far)
plot(Haiti.SEM_far)


# Graphical evaluation of the best model
lapply(Haiti.SEM_far[1:4], plot) #OK 
lapply(Haiti.SEM_far[1:4], qqnorm) #These are OK

#Exploratory - deleted non-significant paths
Haiti.SEM_far2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(Shoot.growth ~ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(C.production ~ Seagrass.N + LAI+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Haiti_far)

summary(Haiti.SEM_far2) #Simplest model is best
AIC(Haiti.SEM_far2, aicc = T)
plot(Haiti.SEM_far2)

# Graphical evaluation of the best model
lapply(Haiti.SEM_far2[1:3], plot) #OK 
lapply(Haiti.SEM_far2[1:3], qqnorm) #These are OK

#Variance inflation factors check
vif(lme(C.production ~ Seagrass.N + LAI+ Epiphytes.area,
        random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far))

# Bahamas outside ---------------------------------------------------------
Bahamas_far = subset(sem_complete, sem_complete$Island == "Bahamas"& as.numeric(sem_complete$Distance) > 3)
Bahamas_far$SLA[13] <- NA #Remove outlier, this one is actually quite off
Bahamas_far <- Bahamas_far[complete.cases(Bahamas_far),]

Bahamas.SEM_far = psem(
  
  lme(SLA ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  lme(LAI ~ Seagrass.P + Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  lme(Shoot.growth ~ Seagrass.P + Seagrass.N + SLA + Seagrass.C13+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  lme(C.production ~ Seagrass.P + Seagrass.N + SLA + LAI + Seagrass.C13+ Epiphytes.area, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Bahamas_far)

summary(Bahamas.SEM_far)

# Graphical evaluation of the best model
lapply(Bahamas.SEM_far[1:4], plot) #OK 
lapply(Bahamas.SEM_far[1:4], qqnorm) #These are OK

#Exploratory - deleted non-significant paths
Bahamas.SEM_far2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),

  lme(C.production ~ Seagrass.P + LAI, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  # C.production %~~% Shoot.growth,
  # Shoot.growth %~~% LAI,
  
  Bahamas_far)

summary(Bahamas.SEM_far2)
AIC(Bahamas.SEM_far2, aicc = T)

# Graphical evaluation of the best model
lapply(Bahamas.SEM_far2[1:2], plot) #OK 
lapply(Bahamas.SEM_far2[1:2], qqnorm) #These are OK

vif(lme(C.production ~ Seagrass.P + LAI, 
        random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far))

