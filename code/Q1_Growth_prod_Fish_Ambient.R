rm(list = ls())
library(lmerTest)
library(tidyverse)
library(lme4)
library(car)
library(nortest)
library(MuMIn)
library(lattice)

#Q1 Which environmental factors influence the size and magnitude of biogeochemical hotspots in seagrass beds?

data <- read.csv("input/Simulation_dataset_Jan2022.csv",
                 stringsAsFactors = FALSE, sep = ",")
reef_data <- read.csv("input/Reef_dataset_Sep2021.csv",
                  stringsAsFactors = FALSE, sep = ",")

#Removing ES1
data$Reef[data$Reef == "ES1" ] <- NA
data=data[!is.na(data$Reef),]
reef_data$Reef[reef_data$Reef == "ES1" ] <- NA
reef_data=reef_data[!is.na(reef_data$Reef),]


#Data transformation
par(mfrow = c(4,4))
for(i in 5:length(data)){
  hist(data[,i], main = names(data[i]))
  qqPlot(data[,i], main = names(data[i]), pch=19, cex=1)
  mtext(round(shapiro.test(data[,i])$p.value,3), side=3)
} #

data$C.production = log(data$C.production)
data$Shoot.growth = log(data$Shoot.growth)

#Merging responses and predictors
seagrass_reef_data <- merge(data, reef_data, by = c("Island","Reef", "Transect"), all = T)
seagrass_reef_data$Island <- as.factor(seagrass_reef_data$Island)
seagrass_reef_data$Reef <- as.factor(seagrass_reef_data$Reef)

#Calculating NP
seagrass_reef_data$Fish.NP = seagrass_reef_data$Fish.N/seagrass_reef_data$Fish.P*31/14
seagrass_reef_data$Seagrass.NP20 = seagrass_reef_data$Seagrass.N20/seagrass_reef_data$Seagrass.P20*31/14
seagrass_reef_data$Sediment.NP = seagrass_reef_data$Sediment.N/seagrass_reef_data$Sediment.P*31/14

#Adding centered variables to the seagrass_reef_dataset
seagrass_reef_data$Fish.P.cent =        scale(seagrass_reef_data$Fish.P)
seagrass_reef_data$Fish.N.cent =        scale(seagrass_reef_data$Fish.N)
seagrass_reef_data$Fish.NP.cent =       scale(seagrass_reef_data$Fish.NP)
seagrass_reef_data$Seagrass.P20.cent =  scale(seagrass_reef_data$Seagrass.P20)
seagrass_reef_data$Seagrass.N20.cent =  scale(seagrass_reef_data$Seagrass.N20)
seagrass_reef_data$Seagrass.NP20.cent = scale(seagrass_reef_data$Seagrass.NP20)
seagrass_reef_data$Sediment.P.cent =    scale(seagrass_reef_data$Sediment.P)
seagrass_reef_data$Sediment.N.cent =    scale(seagrass_reef_data$Sediment.N)
seagrass_reef_data$Sediment.NP.cent =   scale(seagrass_reef_data$Sediment.NP)


cor.test(seagrass_reef_data$Seagrass.P20.cent,seagrass_reef_data$Sediment.P.cent, method = "pearson") #Correlation 0.67
cor.test(seagrass_reef_data$Seagrass.P20.cent,seagrass_reef_data$Fish.P.cent, method = "pearson")
cor.test(seagrass_reef_data$Sediment.P.cent,seagrass_reef_data$Fish.P.cent, method = "pearson")
cor.test(seagrass_reef_data$Seagrass.N20.cent,seagrass_reef_data$Sediment.N.cent, method = "pearson")
cor.test(seagrass_reef_data$Seagrass.N20.cent,seagrass_reef_data$Fish.N.cent, method = "pearson")
cor.test(seagrass_reef_data$Sediment.N.cent,seagrass_reef_data$Fish.N.cent, method = "pearson")

# Growth ~ Fish and Ambient ----------------------------------------------------------
# Coefs table
coefs.growth = matrix(ncol =8, nrow = 3)
colnames(coefs.growth) = c("Intercept","log(Distance)","log(Distance):Fish","log(Distance):Ambient","log(Distance):Fish:Ambient",
                           "R2m","R2c", "AICc")
rownames(coefs.growth) = c("P", "N", "N:P")

# p-value table
p.growth = matrix(ncol =5, nrow = 3)
colnames(p.growth) = c("Intercept","log(Distance)","log(Distance):Fish","log(Distance):Ambient","log(Distance):Fish:Ambient")
rownames(p.growth) = c("P", "N", "N:P")

#Creating dataset list
Q1.growth.P = data.frame(na.omit(seagrass_reef_data[c("Island","Reef","Transect","Distance","Shoot.growth",
                                                        "Fish.P.cent", "Seagrass.P20.cent")]))
Q1.growth.N = data.frame(na.omit(seagrass_reef_data[c("Island","Reef","Transect","Distance","Shoot.growth",
                                                        "Fish.N.cent", "Seagrass.N20.cent")]))
Q1.growth.NP = data.frame(na.omit(seagrass_reef_data[c("Island","Reef","Transect","Distance","Shoot.growth",
                                                         "Fish.NP.cent", "Seagrass.NP20.cent")]))
Q1.growth.complete = list(Q1.growth.P,Q1.growth.N,Q1.growth.NP)

null.model <-lmer(Shoot.growth ~ log(Distance) + (1|Island/Reef),data = data, na.action = na.omit)
summary(null.model)
r.squaredGLMM(null.model)
AICc(null.model)

for (i in 1:3){
  names(Q1.growth.complete[[i]]) <- c("Island","Reef","Transect","Distance","Shoot.growth","Fish", "Ambient")
  data = Q1.growth.complete[[i]]
  
  growth.mod <- lmer(Shoot.growth ~ log(Distance) + 
                       log(Distance):Fish +
                       log(Distance):Ambient + log(Distance):Fish:Ambient + 
                       (1|Island/Reef),
                     data = data, na.action = na.omit)
  # print(summary(growth.mod))
  # print(vif(growth.mod))
  print(summary(growth.mod))

  #Plotting model residuals
  rm=resid(growth.mod)
  fm=fitted(growth.mod)
  mixel2=lm(rm~fm)
  par(mfrow=c(3,2))
  plot(mixel2)
  hist(rm)
  plot(c(0,5),c(0,5))
  text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
  text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)
  #550x730
  
  #Entering results in table
  coefs.growth[i,1:5] = round(summary(growth.mod)$coefficients[1:5,1],2)
  coefs.growth[i,6:8] =  c(round(r.squaredGLMM(growth.mod)[1],2),round(r.squaredGLMM(growth.mod)[2],2),
                            round(AICc(growth.mod), 1))
  
  p.growth[i,1:5] = round(summary(growth.mod)$coefficients[1:5,5],3)
}

# write.csv(coefs.growth, "output/Q1.coefs.growth.csv")
# write.csv(p.growth, "output/Q1.p.growth.csv")

# Production ~ Fish and Ambient --------------------------------------------------------------

# Coef table
coefs.prod = matrix(ncol =8, nrow = 3)
colnames(coefs.prod) = c("Intercept","log(Distance)","log(Distance):Fish","log(Distance):Ambient","log(Distance):Fish:Ambient", 
                         "R2m","R2c", "AICc")
rownames(coefs.prod) = c("P", "N", "N:P")

# p-value table
p.prod = matrix(ncol =5, nrow = 3)
colnames(p.prod) = c("Intercept","log(Distance)","log(Distance):Fish","log(Distance):Ambient","log(Distance):Fish:Ambient")
rownames(p.prod) = c("P", "N", "N:P")


Q1.prod.P = data.frame(na.omit(seagrass_reef_data[c("Island","Reef","Transect","Distance","C.production",
                                                        "Fish.P.cent", "Seagrass.P20.cent", "Sediment.P.cent")]))
Q1.prod.N = data.frame(na.omit(seagrass_reef_data[c("Island","Reef","Transect","Distance","C.production",
                                                        "Fish.N.cent", "Seagrass.N20.cent", "Sediment.N.cent")]))
Q1.prod.NP = data.frame(na.omit(seagrass_reef_data[c("Island","Reef","Transect","Distance","C.production",
                                                         "Fish.NP.cent", "Seagrass.NP20.cent", "Sediment.NP.cent")]))

Q1.prod.complete = list(Q1.prod.P,Q1.prod.N,Q1.prod.NP)

null.model2 <-lmer(C.production ~ log(Distance) + (1|Island/Reef),data = data, na.action = na.omit)
summary(null.model2)
r.squaredGLMM(null.model2)
AICc(null.model2)

for (i in 1:3){
  names(Q1.prod.complete[[i]]) <- c("Island","Reef","Transect","Distance","C.production","Fish", "Ambient", "Sediment")
  data = Q1.prod.complete[[i]]
  
  prod.mod <- lmer(C.production ~ log(Distance) + 
                     log(Distance):Fish +
                     log(Distance):Ambient + log(Distance):Fish:Ambient + 
                     (1|Island/Reef),
                   data = data, na.action = na.omit)
  
  # print(summary(prod.mod))
  print(vif(growth.mod))
  
  rm=resid(prod.mod)
  fm=fitted(prod.mod)
  mixel2=lm(rm~fm)
  par(mfrow=c(3,2))
  plot(mixel2)
  hist(rm)
  plot(c(0,5),c(0,5))
  text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
  text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)
  #550x730
  
  coefs.prod[i,1:5] = round(summary(prod.mod)$coefficients[1:5,1],2)
  coefs.prod[i,6:8] =  c(round(r.squaredGLMM(prod.mod)[1],2),round(r.squaredGLMM(prod.mod)[2],2),
                           round(AICc(prod.mod), 1))
  
  p.prod[i,1:5] = round(summary(prod.mod)$coefficients[1:5,5],3)
  
}

# write.csv(coefs.prod, "output/Q1.coefs.prod.csv")
# write.csv(p.prod, "output/Q1.p.prod.csv")

# Plot Prod ~ Fish and Ambient -------------------------------------------
#Fish
for (i in 1:3){
  datax = Q1.prod.complete[[i]]
  ey <-xyplot(as.numeric(C.production) ~ as.numeric(log(Distance)) | as.factor(round(Fish,2)), 
              data = datax,
              panel = function(x,y, ...) {
                panel.xyplot(x, y, ...)
                panel.lmline(x, y, ..., col.line = "black", lwd = 2)
              }, pch = 19, layout = c(13,1), col = color[i],
              xlab="Distance",
              ylab="C production",
              scales=list(alternating=FALSE,tck=c(1,0),x=list(tick.number=4)))
  print(ey)
}
#Ambient
for (i in 2:2){
  datax = Q1.prod.complete[[i]]
  sed <- data.frame(datax %>%
                      group_by(Reef) %>%
                      summarise(Ambient = mean(Ambient, na.rm = T)))
  sed[8,2] = sed[8,2]+0.004 #Because LH1 and LH3 are identical
                            #just for figure purposes they need to be separated
                            #Manually correct in figure after!
  datax = merge(datax, sed, by = "Reef")
  ey <-xyplot(as.numeric(C.production) ~ as.numeric(log(Distance)) | as.factor(round(Ambient.y,2)), 
              data = datax,
              panel = function(x,y, ...) {
                panel.xyplot(x, y, ...)
                panel.lmline(x, y, ..., col.line = "black", lwd = 2)
              }, pch = 19, layout = c(13,1), col = color[i],
              xlab="Distance",
              ylab="C production",
              scales=list(alternating=FALSE,tck=c(1,0),x=list(tick.number=4)))
  print(ey)
}

