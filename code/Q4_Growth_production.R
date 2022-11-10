rm(list = ls())
library(lmerTest)
library(tidyverse)
library(lme4)
library(car)
library(nortest)
library(MuMIn)
library(lattice)

#Q4 What explains the C production ~ Shoot growth relationship

data <- read.csv("input/Simulation_dataset_Jan2022.csv",
                 stringsAsFactors = FALSE, sep = ",")
#Removing ES1
data$Reef[data$Reef == "ES1" ] <- NA
data=data[!is.na(data$Reef),]

#Data transformation
par(mfrow = c(4,4))
for(i in 5:length(data)){
  hist(data[,i], main = names(data[i]))
  qqPlot(data[,i], main = names(data[i]), pch=19, cex=1)
  mtext(round(shapiro.test(data[,i])$p.value,3), side=3)
} #

data$C.production = log(data$C.production)
data$Shoot.growth = log(data$Shoot.growth)
data$Shoot.density = log(data$Shoot.density)
data$Seagrass.N = log(data$Seagrass.N)
data$Seagrass.P = log(data$Seagrass.P)
# data$Bites.area = log(data$Bites.area+0.01)

data$Seagrass.P.scaled =scale(data$Seagrass.P)
data$Seagrass.N.scaled =scale(data$Seagrass.N)
data$Seagrass.C13.scaled =scale(data$Seagrass.C13)
data$Shoot.growth.scaled =scale(data$Shoot.growth)
data$Shoot.density.scaled =scale(data$Shoot.density)
data$Epiphytes.scaled =scale(data$Epiphytes.area)
data$Bites.scaled =scale(data$Bites.area)
data$Seagrass.C.scaled =scale(data$Seagrass.C)


#AR dataset
LH = data %>% filter(grepl('LH', Reef))
Rest = data %>% filter(!grepl('LH', Reef))
# Rest = Rest %>% filter(!grepl('H5', Reef))

par(mfrow = c(2,2))
plot(LH$Shoot.growth.scaled, LH$C.production, pch = 19,
     col = factor(LH$Reef), xlim = c(-2,2), ylim = c(-3,0.5))
plot(Rest$Shoot.growth.scaled, Rest$C.production, pch = 19,
     col = factor(Rest$Reef), xlim = c(-2,2), ylim = c(-3,0.5))
legend("topleft",
       legend = levels(factor(Rest$Reef)),
       pch = 19,
       col = factor(levels(factor(Rest$Reef))))

plot(LH$Shoot.density.scaled, LH$C.production, pch = 19,
     col = factor(LH$Reef), xlim = c(-2,2), ylim = c(-3,0.5))
plot(Rest$Shoot.density.scaled, Rest$C.production, pch = 19,
     col = factor(Rest$Reef), xlim = c(-2,2), ylim = c(-3,0.5))

plot(LH$Shoot.density.scaled, LH$Shoot.growth.scaled, pch = 19,
     col = factor(LH$Reef), xlim = c(-2,2), ylim = c(-3,2))
plot(Rest$Shoot.density.scaled, Rest$Shoot.growth.scaled, pch = 19,
     col = factor(Rest$Reef), xlim = c(-2,2), ylim = c(-3,2))

plot(LH$Seagrass.C.scaled, LH$C.production, pch = 19,
     col = factor(LH$Reef), xlim = c(-2,2), ylim = c(-3,0.5))
plot(Rest$Seagrass.C.scaled, Rest$C.production, pch = 19,
     col = factor(Rest$Reef), xlim = c(-2,2), ylim = c(-3,0.5))


#Get variables of interest
LH <- LH[c("Island","Reef","Transect","Distance","C.production","Shoot.growth.scaled","Shoot.density.scaled",
           "Seagrass.P.scaled", "Seagrass.N.scaled", "Seagrass.C13.scaled","Epiphytes.scaled","Bites.scaled",
           "Seagrass.C.scaled")]
Rest <- Rest[c("Island","Reef","Transect","Distance","C.production","Shoot.growth.scaled","Shoot.density.scaled",
               "Seagrass.P.scaled", "Seagrass.N.scaled", "Seagrass.C13.scaled","Epiphytes.scaled","Bites.scaled",
               "Seagrass.C.scaled")]

#LH distributions
par(mfrow = c(4,4))
for(i in 5:length(LH)){
  hist(LH[,i], main = names(LH[i]))
  qqPlot(LH[,i], main = names(LH[i]), pch=19, cex=1)
  mtext(round(shapiro.test(LH[,i])$p.value,3), side=3)
} #

#Rest distributions
par(mfrow = c(4,4))
for(i in 5:length(Rest)){
  hist(Rest[,i], main = names(Rest[i]))
  qqPlot(Rest[,i], main = names(Rest[i]), pch=19, cex=1)
  mtext(round(shapiro.test(Rest[,i])$p.value,3), side=3)
} #


Rest = Rest[c(-100,-48),]
# Rest = Rest[c(-196,-48),]
# Rest = Rest[c(-48),]

data.list <- list(LH, Rest)

#Modeling
preds <- c("Seagrass.P.scaled", "Seagrass.N.scaled", "Seagrass.C13.scaled",
           "Epiphytes.scaled","Bites.scaled", "")

coefs = matrix(ncol = 7, nrow = 6)
p = matrix(ncol = 4, nrow = 6)

coefs.growth = coefs
coefs.density = coefs
p.growth = p
p.density = p

colnames(coefs.growth) = c("Intercept","Shoot growth","Predictor","Shoot growth : Predictor",
                           "R2m","R2c", "AICc")
rownames(coefs.growth) = c("P", "N", "13C","Epiphytes","Herbivory","null")
colnames(p.growth) = c("Intercept","Shoot growth","Predictor","Shoot growth : Predictor")
rownames(p.growth) = c("P", "N", "13C","Epiphytes","Herbivory","null")

colnames(coefs.density) = c("Intercept","Shoot density","Predictor","Shoot density : Predictor",
                           "R2m","R2c", "AICc")
rownames(coefs.density) = c("P", "N", "13C","Epiphytes","Herbivory","null")
colnames(p.density) = c("Intercept","Shoot density","Predictor","Shoot density : Predictor")
rownames(p.density) = c("P", "N", "13C","Epiphytes","Herbivory","null")

#Result list
coef.growth.list <- list(coefs.growth,coefs.growth)
names(coef.growth.list) <- c("LH", "Rest")
p.growth.list <- list(p.growth,p.growth)
names(p.growth.list) <- c("LH", "Rest")

coef.density.list <- list(coefs.density,coefs.density)
names(coef.density.list) <- c("LH", "Rest")
p.density.list <- list(p.density,p.density)
names(p.density.list) <- c("LH", "Rest")

#Growth
for (i in 1:length(preds)){
  response <- "C.production"
  for (j in 1:2){
    loop.data = data.list[[j]]
    if (j == 1){
      if(i == 6) {
        predictors <- "Shoot.growth.scaled"
        random <- "(1|Reef)"
      } else { 
        predictors <- paste( "Shoot.growth.scaled", preds[i], sep = "*" ) 
        random <- "(1|Reef)"
      }
    } else {
      random <- "(1|Island/Reef)"
    }
    
  # print(vif(mod))
    fixed = paste(response, predictors, sep = " ~ ")
    formula <- as.formula(paste(fixed, random, sep = "+"))
    mod <- lmer(formula, data = loop.data, na.action = na.omit)

  #Plotting model residuals
  rm=resid(mod)
  fm=fitted(mod)
  mixel2=lm(rm~fm)
  par(mfrow=c(3,2))
  plot(mixel2)
  hist(rm)
  plot(c(0,5),c(0,5))
  text(3,4,paste("model = ",fixed),cex=2)
  text(3,3,paste("Data = ",names(coef.growth.list[j])),cex=2)
  text(3,2,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
  text(3,1,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)
  #550x730
  print(summary(mod))
  # #Entering results in table
  if(i==6) { 
    g.to.add <-  c(round(summary(mod)$coefficients[1:2,1],3), NA, NA) 
    p.to.add <- c(round(summary(mod)$coefficients[1:2,5],3), NA, NA)
  } else { 
      g.to.add <- round(summary(mod)$coefficients[1:4,1],3)  
      p.to.add <- round(summary(mod)$coefficients[1:4,5],3)
      print(vif(mod))
      
      }
                     
  coef.growth.list[[j]][i,1:4] = g.to.add
  coef.growth.list[[j]][i,5:7] =  c(round(r.squaredGLMM(mod)[1],3),
                    round(r.squaredGLMM(mod)[2],3),
                    round(AICc(mod), 2))

  p.growth.list[[j]][i,1:4] = p.to.add
 
  }
}
# write.csv(p.growth.list[["LH"]], "output/LH_growth_p.csv")
# write.csv(p.growth.list[["Rest"]], "output/Rest_growth_p.csv")
# write.csv(coef.growth.list[["LH"]], "output/LH_growth_coef.csv")
# write.csv(coef.growth.list[["Rest"]], "output/Rest_growth_coef.csv")
#density
for (i in 1:length(preds)){
  response <- "C.production"
  for (j in 1:2){
    loop.data = data.list[[j]]
    if (j == 1){
      if(i == 6) {
        predictors <- "Shoot.density.scaled"
        random <- "(1|Reef)"
      } else { 
        predictors <- paste( "Shoot.density.scaled", preds[i], sep = "*" ) 
        random <- "(1|Reef)"
      }
    } else {
      random <- "(1|Island/Reef)"
    }
    
    # print(vif(mod))
    fixed = paste(response, predictors, sep = " ~ ")
    formula <- as.formula(paste(fixed, random, sep = "+"))
    mod <- lmer(formula, data = loop.data, na.action = na.omit)
    
    #Plotting model residuals
    rm=resid(mod)
    fm=fitted(mod)
    mixel2=lm(rm~fm)
    par(mfrow=c(3,2))
    plot(mixel2)
    hist(rm)
    plot(c(0,5),c(0,5))
    text(3,4,paste("model = ",fixed),cex=2)
    text(3,3,paste("Data = ",names(coef.density.list[j])),cex=2)
    text(3,2,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
    text(3,1,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)
    #550x730
    
    # #Entering results in table
    if(i==6) { 
      g.to.add <-  c(round(summary(mod)$coefficients[1:2,1],3), NA, NA) 
      p.to.add <- c(round(summary(mod)$coefficients[1:2,5],3), NA, NA)
    } else { 
      g.to.add <- round(summary(mod)$coefficients[1:4,1],3)  
      p.to.add <- round(summary(mod)$coefficients[1:4,5],3)
      print(vif(mod))
      
    }
    
    coef.density.list[[j]][i,1:4] = g.to.add
    coef.density.list[[j]][i,5:7] =  c(round(r.squaredGLMM(mod)[1],3),
                                      round(r.squaredGLMM(mod)[2],3),
                                      round(AICc(mod), 2))
    
    p.density.list[[j]][i,1:4] = p.to.add
    
  }
}
# write.csv(p.density.list[["LH"]], "output/LH_density_p.csv")
# write.csv(p.density.list[["Rest"]], "output/Rest_density_p.csv")
# write.csv(coef.density.list[["LH"]], "output/LH_density_coef.csv")
# write.csv(coef.density.list[["Rest"]], "output/Rest_density_coef.csv")



