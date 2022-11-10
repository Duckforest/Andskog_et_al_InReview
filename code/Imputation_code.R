#Imputation code that I don't need for every run
#Calculate SD for growth for each distance
library(tidyverse)

sem <- read.csv("input/Simulation_dataset_Jan2022.csv",
                stringsAsFactors = FALSE, sep = ",")

#As factors
sem$Reef = factor(sem$Reef)
sem$Transect = factor(sem$Transect)
sem$Island = factor(sem$Island)

#Keeping parameters of interest
names(sem)
sem<-sem[c("Island","Reef","Transect","Distance","Shoot.growth","C.production","SLA","Seagrass.P",
           "Seagrass.N","LAI"
           , "Bites.area" , "Seagrass.C13", "Epiphytes.area", "Seagrass.C", "Shoot.density", "Shoot.area"
)]

#Make rownames so that I can enter imputated data in the correct cells
rownames(sem) <- apply(sem[,c("Reef","Transect","Distance")], 1, paste, collapse="_")


sdsem = as.data.frame(sem %>% 
                        group_by(Reef, Distance) %>% 
                        summarise(Island = first(Island),
                                  Shoot.growth.sd = sd(Shoot.growth, na.rm=T),
                                  Shoot.growth.mean = mean(Shoot.growth, na.rm=T),
                                  Seagrass.P.sd = sd(Seagrass.P),
                                  Seagrass.P.mean = mean(Seagrass.P, na.rm=T),
                                  Seagrass.N.sd = sd(Seagrass.N),
                                  Seagrass.N.mean = mean(Seagrass.N, na.rm=T),
                                  Seagrass.C.sd = sd(Seagrass.C),
                                  Seagrass.C.mean = mean(Seagrass.C, na.rm=T),
                                  Seagrass.C13.sd = sd(Seagrass.C13),
                                  Seagrass.C13.mean = mean(Seagrass.C13, na.rm=T),
                                  Shoot.density.sd = sd(Shoot.density),
                                  Shoot.density.mean = mean(Shoot.density, na.rm=T)
                        ))

row.names(sdsem) = do.call(paste, c(sdsem[c("Reef", "Distance")], sep="_"))

sdmean = as.data.frame(sdsem %>% 
                         group_by(Reef) %>% 
                         summarise(Island = first(Island),
                                   Shoot.growth.mean.sd = mean(Shoot.growth.sd, na.rm = TRUE),
                                   Seagrass.P.mean.sd = mean(Seagrass.P.sd, na.rm=TRUE),
                                   Seagrass.N.mean.sd = mean(Seagrass.N.sd, na.rm=TRUE),
                                   Seagrass.C.mean.sd = mean(Seagrass.C.sd, na.rm=TRUE),
                                   Seagrass.C13.mean.sd = mean(Seagrass.C13.sd, na.rm=TRUE),
                                   Shoot.density.mean.sd = mean(Shoot.density.sd, na.rm=TRUE)
                                   
                         ))

row.names(sdmean) = sdmean$Reef

###Create random numbers for missing values
names(sdsem)
names(sdmean)

#

# H1_1_2 Growth
H1_x_2 = subset(sem, Reef == "H1" & Distance == 2)
H1_1_3 = subset(sem, Reef == "H1"& Distance == 3 & Transect == 1)
H1_1_1 = subset(sem, Reef == "H1"& Distance == 1 & Transect == 1)
H1_1_2_growth=rbind(H1_x_2, H1_1_3,H1_1_1)
H1_1_2_growthmean = mean(H1_1_2_growth$Shoot.growth, na.rm = T)

sdH1growth = as.numeric(sdmean ["H1", "Shoot.growth.mean.sd"]) ##Mead SD of growth at all other distances
# set.seed(4)
H1_1_2_growthnew <- as.numeric(rnorm(1,mean=H1_1_2_growthmean,sd=sdH1growth))#Create random number for H1 growth
#Before adding value to dataset, make the other growth that is missing

# H1_1_3 Growth
H1_x_3 = subset(sem, Reef == "H1" & Distance == 3)
H1_1_4 = subset(sem, Reef == "H1"& Distance == 4 & Transect == 1)
H1_1_2 = subset(sem, Reef == "H1"& Distance == 2 & Transect == 1)
H1_1_3_growth=rbind(H1_x_3, H1_1_4,H1_1_2)
H1_1_3_growthmean = mean(H1_1_3_growth$Shoot.growth, na.rm = T)

# set.seed(4)
H1_1_3_growthnew <- as.numeric(rnorm(1,mean=H1_1_3_growthmean,sd=sdH1growth))#Create random number for H1 growth

###Adding imputed values to dataset
sem["H1_1_ 2.0", "Shoot.growth"] <-  H1_1_2_growthnew ##Inserted value
sem["H1_1_ 3.0", "Shoot.growth"] <-  H1_1_3_growthnew ##Inserted value


# H5_2_1 Growth
H5_x_1 = subset(sem, Reef == "H5" & Distance == 1)
H5_2_2 = subset(sem, Reef == "H5"& Distance == 2 & Transect == 2)
H5_2_0.5 = subset(sem, Reef == "H5"& Distance == 0.5 & Transect == 2)
H5_2_1_growth=rbind(H5_x_1, H5_2_2,H5_2_0.5)
H5_2_1_growthmean = mean(H5_2_1_growth$Shoot.growth, na.rm = T)

sdH5growth = as.numeric(sdmean ["H5", "Shoot.growth.mean.sd"]) ##Mead SD of growth at all other distances
# set.seed(8)
H5_2_1_growthnew <- as.numeric(rnorm(1,mean=H5_2_1_growthmean,sd=sdH5growth))#Create random number for H5 growth

# H5_3_1 Growth
H5_3_2 = subset(sem, Reef == "H5"& Distance == 2 & Transect == 3)
H5_3_0.5 = subset(sem, Reef == "H5"& Distance == 0.5 & Transect == 3)
H5_3_1_growth=rbind(H5_x_1, H5_3_2,H5_3_0.5)
H5_3_1_growthmean = mean(H5_3_1_growth$Shoot.growth, na.rm = T)

# set.seed(8)
H5_3_1_growthnew <- as.numeric(rnorm(1,mean=H5_3_1_growthmean,sd=sdH5growth))#Create random number for H5 growth

sem["H5_2_ 1.0", "Shoot.growth"] <-  H5_2_1_growthnew ##Inserted value
sem["H5_3_ 1.0", "Shoot.growth"] <-  H5_3_1_growthnew ##Inserted value

# H4_3_12 Shoot density
#Average Shoot density of H4_2_12 is 400, but since we didn't have any seagrass samples from this plot,
#it's not included in the dataset. I will use it here to calculate the shoot density of H4_3_12
H4_x_12 = subset(sem, Reef == "H4" & Distance == 12)
H4_3_6 = subset(sem, Reef == "H4"& Distance == 6 & Transect == 3)
# H4densityone = subset(sem, Reef == "H4"& Distance == 20 & Transect == 3) #Doesn't exist
H4_3_12_density=rbind(H4_x_12, H4_3_6)
H4_3_12_densitymean = mean(c(H4_3_12_density$Shoot.density), na.rm = T)

sdH4density = as.numeric(sdmean ["H4", "Shoot.density.mean.sd"]) ##Mead SD of density at all other distances
# set.seed(4)
H4densitynew <- as.numeric(rnorm(1,mean=H4_3_12_densitymean,sd=sdH4density))#Create random number for H4 density

sem["H4_3_12.0", "Shoot.density"] <-  H4densitynew ##Inserted value


# MOW2_1_2 Seagrass C
MOW2_x_2 = subset(sem, Reef == "MOW2" & Distance == 2)
MOW2_1_3 = subset(sem, Reef == "MOW2"& Distance == 3 & Transect == 1)
MOW2_1_1 = subset(sem, Reef == "MOW2"& Distance == 1 & Transect == 1)
MOW2_1_2_C=rbind(MOW2_x_2, MOW2_1_3,MOW2_1_1)
MOW2_1_2_Cmean = mean(MOW2_1_2_C$Seagrass.C, na.rm = T)

sdMOW2C = as.numeric(sdmean ["MOW2", "Seagrass.C.mean.sd"]) ##Mead SD of C at all other distances
# set.seed(5)
MOW2_1_2_Cnew <- as.numeric(rnorm(1,mean=MOW2_1_2_Cmean,sd=sdMOW2C))#Create random number for MOW2 C

sem["MOW2_1_ 2.0", "Seagrass.C"] <-  MOW2_1_2_Cnew ##Inserted value

#Calculate the new C.prod from the imputed growth, density, and %C values
#Area growth to biomass growth, coef = 0.03951503 
coef = 0.03951503
sem$Shoot.growth.biomass = sem$Shoot.growth*coef
#Compare with original datasheet that has biomass growth that the new column in sem-dataset is good
compare <- merge(all[c("Reef", "Transect", "Distance","Sum.growth.biomass")],
                 sem[c("Reef", "Transect", "Distance","Shoot.growth.biomass")], by = c("Reef", "Transect", "Distance"))
sem$C.production = sem$Shoot.growth.biomass*(sem$Seagrass.C/100)*sem$Shoot.density/1000
# Compare with original datasheet that has C prod that the new column in sem-dataset is good
compare2 <- merge(all[c("Reef", "Transect", "Distance","C.production")],
                 sem[c("Reef", "Transect", "Distance","C.production")], by = c("Reef", "Transect", "Distance"))
sem$LAI = (sem$Shoot.area*sem$Shoot.density)/100 #cm2 per m2
# Compare with original datasheet that has LAI that the new column in sem-dataset is good
compare3 <- merge(all[c("Reef", "Transect", "Distance","LAI")],
                  sem[c("Reef", "Transect", "Distance","LAI")], by = c("Reef", "Transect", "Distance"))

#Making dataset for SEM
#Removing 0.5 m distance
sem$Distance[sem$Distance == 0.5 ] <- NA
sem=sem[!is.na(sem$Distance),]

#Removing rows with NA
sem_complete = sem[complete.cases(sem), ] #Remove rows with NA

#Make rownames normal again
rownames(sem_complete) <- c()
#Write csv
# write.csv(sem_complete, "input/SEM_imputed.csv")

###SEM CODE to test whether imputated data matter to results####
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
sem_complete$Epiphytes.area = log(sem_complete$Epiphytes.area)

#Bahamas Ambient
Bahamas_far = subset(sem_complete, sem_complete$Island == "Bahamas"& as.numeric(sem_complete$Distance) > 3)
# Bahamas_far = subset(sem_complete, sem_complete$Island == "Bahamas"& as.numeric(sem_complete$Distance) > 4)
Bahamas_far$SLA[13] <- NA #Remove outlier, this one is actually quite off
# Bahamas_far$SLA[10] <- NA #Remove outlier, if not using the 4 meter distance
Bahamas_far <- Bahamas_far[complete.cases(Bahamas_far),]

#Exploratory - deleted non-significant paths
Bahamas.SEM_far2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  lme(C.production ~ Seagrass.P + LAI, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_far),
  
  # C.production %~~% Shoot.growth,
  # Shoot.growth %~~% LAI,
  
  Bahamas_far)
summary(Bahamas_far) #simplest model the best model


###Bahamas AR
Bahamas_near = subset(sem_complete, sem_complete$Island == "Bahamas"& as.numeric(sem_complete$Distance) <=3)
Bahamas_near <- Bahamas_near[complete.cases(Bahamas_near),]

##Exploratory - deleted non-significant paths
Bahamas.SEM_near2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(LAI ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(Shoot.growth ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  lme(C.production ~ Seagrass.N + SLA + LAI, 
      random=~1|Reef/Distance, na.action = na.omit, data=Bahamas_near),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Bahamas_near)

summary(Bahamas.SEM_near2) #simplest model the best model

#Haiti Ambient
Haiti_far = subset(sem_complete, sem_complete$Island == "Haiti"& as.numeric(sem_complete$Distance) > 3)
# Haiti_far = subset(sem_complete, sem_complete$Island == "Haiti"& as.numeric(sem_complete$Distance) > 4)
Haiti_far$SLA[72] <- NA#Remove outlier #There's hardly any difference
Haiti_far <- Haiti_far[complete.cases(Haiti_far),]

#Exploratory - deleted non-significant paths
Haiti.SEM_far2 = psem(
  
  lme(SLA ~ Seagrass.P, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(Shoot.growth ~ Seagrass.N, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  lme(C.production ~ Seagrass.P + Seagrass.N +  LAI, 
      random=~1|Reef/Distance, na.action = na.omit, data=Haiti_far),
  
  C.production %~~% Shoot.growth,
  Shoot.growth %~~% LAI,
  
  Haiti_far)

summary(Haiti.SEM_far2) #Simplest model is best

#Haiti AR
Haiti_near = subset(sem_complete, sem_complete$Island == "Haiti" & as.numeric(sem_complete$Distance) <=3)
Haiti_near$SLA[47] <- NA #Outlier
Haiti_near <- Haiti_near[complete.cases(Haiti_near),]

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

summary(Bahamas.SEM_far2) #simplest model the best model
summary(Bahamas.SEM_near2) #simplest model the best model
summary(Haiti.SEM_far2) #Simplest model is best
summary(Haiti.SEM_near2) #Simplest model is also best model
