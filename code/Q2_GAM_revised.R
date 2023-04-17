rm(list = ls())
library(gamm4)
library(mgcv)
library(MuMIn)
library(chngpt)
library("MASS") ## for mvrnorm
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(vioplot)
library(segmented)
library(Rmisc)
library(nortest)

#
# Load data ---------------------------------------------------------------
data <- read.csv("input/Simulation_dataset_Jan2022.csv",
                 stringsAsFactors = FALSE, sep = ",")
reef <- read.csv("input/Reef_dataset_Sep2021.csv",
                 stringsAsFactors = FALSE, sep = ",")
colnames(data)[colnames(data) == 'Sum.growth.biomass'] <- 'Shoot.growth'

#Removing 20m distance
data = subset(data, Distance < 13)

# Remove ES1
data$Reef[data$Reef == "ES1" ] <- NA
data=data[!is.na(data$Reef),]
reef$Reef[reef$Reef == "ES1" ] <- NA
reef=reef[!is.na(reef$Reef),]


# GAM simulations ---------------------------------------------------------

params.of.interest <- c("Shoot.growth","C.production","SLA","Seagrass.P","Seagrass.N","LAI","Seagrass.C13"
                        # ,"Shoot.density"
)
uniq.reefs <- unique(data$Reef)

# Getting the parameters for the GAM simulations
boot <-100
ns <-4 #Standard is 4

# all.reefs.thres <- list()
# all.reefs.slope <- list()
# all.reefs.std.slope <- list()
# 
# all.reefs.int <- list()
# all.reef.gampredic <- list()
# all.reef.gamr2pval <- list()
# 
# # all.reefs.seg.thres <- list()
# # all.reefs.seg.slope <- list()
# # all.reef.seg.gampredic <- list()
# # all.reef.seg.gamr2pval <- list()
# 
# colores <- c( brewer.pal(7,"Greens")[2], brewer.pal(7,"Purples")[2] )
# 
# 
# for(ar in 1 : length(uniq.reefs) ) {
#   thresh.mat <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   thresh.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
# 
#   slope.mat <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   slope.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
# 
#   int.mat <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   int.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
# 
#   std.slope.mat <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   std.slope.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
# 
# 
#   # #Segmented approach
#   # thresh.mat.seg <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   # thresh.mat.seg[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
#   #
#   # slope.mat.seg <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   # slope.mat.seg[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
#   #
#   # int.mat.seg <- matrix(nrow = boot, ncol = length(params.of.interest)+1, dimnames = list(NULL, c("Reef", params.of.interest)))
#   # int.mat.seg[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), boot)
# 
#   par(mfrow = c(1,length(params.of.interest)), mar = c(2,2,2,2))
# 
#   #gam.predicts for reef-level data - for MS figure
#   length.x.predict <- 100 # this sets the length of the vector to predit from the gams
#   gam.predict <- matrix( ncol = length(params.of.interest), nrow = length.x.predict, dimnames = list(NULL, params.of.interest))
#   r2_pval.gam <- matrix( nrow = length(params.of.interest), ncol = 2, dimnames = list(params.of.interest, c("r2", "p-val")) )
# 
# 
#   for (i in 1: length(params.of.interest)) {
#     #match the color with the island
#     if( data[ data$Reef == uniq.reefs[ar], 1][1] == "Bahamas" ) { co.snacks <-  colores[1] } else { co.snacks <- colores[2] }
# 
#     now <- na.omit (data[data$Reef == uniq.reefs[ar],c( "Distance", params.of.interest[i])])
#     x = now[,1]
#     y = now[,2]
# 
#     mod <-  gam(y ~ s(x, k=ns, bs = "cr"), na.action = "na.omit")
#     mod.lm <- lm(y ~ x, na.action = "na.omit")
# 
# 
#     if(summary(mod)$s.table[4] < 0.05 | AIC(mod) < AIC(mod.lm) ) {
#       par(mfrow=c(4,2))
#       plot(c(0,5),c(0,5))
#       text(3,3.5,paste(params.of.interest[i]),cex=2)
#       text(3,2,paste(uniq.reefs[ar]),cex=2)
# 
#     } else {
#       print("no gam")
#     }
# 
# 
#     val.for.pred <- data.frame(x=seq(min(x), max(x), length.out=length.x.predict ))
# 
#     #Coefs and covariance matrix for gam
#     Xp <- predict(mod, val.for.pred, type="lpmatrix") ## map coefs to fitted curves
#     beta <- coef(mod)
#     Vb   <- vcov(mod) ## posterior mean and cov of coefs
#     set.seed(10)
#     mrand <- mvrnorm(boot, beta, Vb) ## simulate n rep coef vectors from posterior
#     ilink <- family(mod)$linkinv
# 
#     for(k in 1:boot){
# 
#       #I added in a clause that the GAM had to be also better than the LM model
#       if(summary(mod)$s.table[4] > 0.05 | AIC(mod) > AIC(mod.lm) ) {
#         thresh.mat[k,i+1] <- 0
#         slope.mat[k,i+1] <- 0 } else {
# 
#           val.for.pred <- data.frame(x=seq(min(x), max(x), length.out=length.x.predict ))
#           pred   <- ilink(Xp %*% mrand[k, ])
#           # print(pred)
#           gamzy = data.frame(new.y = pred,
#                              new.x = val.for.pred$x)
#           # plot(gamzy$new.x, gamzy$new.y)
#           # text(max(gamzy$new.x),max(gamzy$new.y), params.of.interest[i])
# 
#           #Changepoint model, one breakpoint
#           # cpt.mod <- chngptm(formula.1=new.y~1, formula.2=~new.x, family="gaussian", data = gamzy,
#           #                              type="segmented")
#           # test=chngpt.test(formula.null=new.y~1, formula.chngpt=~new.x, gamzy,
#           #                  type="segmented", family="gaussian")
# 
#           #Changepoint model, two breakpoint
#           cpt.mod=chngptm(formula.1=new.y~1, formula.2=~new.x, gamzy, type="M111",
#                           family="gaussian", est.method="fastgrid", var.type="bootstrap")
#           pscore1 <- summary(cpt.mod)$coefficients[3,5]
#           pscore2 <- summary(cpt.mod)$coefficients[4,5]
# 
#           plot(gamzy$new.y ~ gamzy$new.x)
#           # legend(x= "topright", legend = c("threshold 1", round(pscore1, 3),
#           #                                  "threshold 2", round(pscore2, 3)))
#           abline(v = summary(cpt.mod)$chngpt[1,1], col = "red")
#           # abline(v = summary(cpt.mod)$chngpt[2,1], col = "red")
# 
# 
#           if(pscore1 < 0.05){
#             threshold <- summary(cpt.mod)$chngpt[1,1]
#             # threshold2 <- summary(cpt.mod)$chngpt[2,1]
#           } else {
#             threshold <- 0
#           }
# 
#           #estimate slope up to threshold
#           if(threshold == 0){
#             thresh.mat[k,i+1] <- 0
# 
#           }
# 
#           else {
#             linzy <- gamzy[which(gamzy$new.x < threshold),]
# 
#             thresh.mat[k,i+1] <- threshold
#             slope.mat[k,i+1] <- lm(new.y ~ new.x, data = linzy)$coefficients[2] #the slope
#             int.mat[k,i+1] <- lm(new.y ~ new.x, data = linzy)$coefficients[1] #the intercept
#             std.slope.mat[k,i+1] <- lm(scale(new.y) ~ new.x, data = linzy)$coefficients[2]
#             
#             #Plotting model residuals
#             abline(lm(new.y ~ new.x, data = linzy), col = "blue")
#             rm=resid(lm(new.y ~ new.x, data = linzy))
#             fm=fitted(lm(new.y ~ new.x, data = linzy))
#             mixel2=lm(rm~fm)
#             plot(mixel2)
#             hist(rm)
#             plot(c(0,5),c(0,5))
#             text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
#             text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)
#             #550x730
#             
#           }
# 
#           # #Segmented model two breakpoints
#           # seg.mod <-segmented(lm(new.y ~ new.x, na.action = "na.omit", data = gamzy), npsi=2)
#           #
#           # pscore1.seg <- pscore.test(seg.mod, more.break = T)$p.value
#           # # pscore2.seg <- pscore.test(seg.mod, more.break = T, n.break=2)$p.value
#           #
#           # if(pscore1.seg < 0.05){
#           #   threshold.seg <- seg.mod$psi[1,2]
#           #   # threshold2.seg <- seg.mod$psi[2,2]
#           # }
#           # else{
#           #   threshold.seg <- 0
#           # }
#           #
#           # #estimate slope up to threshold
#           # linzy.seg <- gamzy[which(gamzy$new.x < threshold.seg),]
#           #
#           #
#           # #To fix that the segmented model sometimes doesn't find a threshold
#           # if(threshold.seg > 0){
#           # thresh.mat.seg[k,i+1] <- threshold.seg
#           # slope.mat.seg[k,i+1] <- lm(new.y ~ new.x, data = linzy.seg)$coefficients[2] #the slope
#           # int.mat.seg[k,i+1] <- lm(new.y ~ new.x, data = linzy.seg)$coefficients[1] #the intercept
#           # }
#           # else{
#           #   thresh.mat.seg[k,i+1] <- 0
#           #   slope.mat.seg[k,i+1] <- 0
#           #   int.mat.seg[k,i+1] <- 0
#           # }
# 
# 
#         } # end if statement
# 
#     } # end 'k' bootstrapping loop
# 
# 
#     r2_pval.gam[i,] <- c(round(summary(mod)$r.sq, 2), round(summary(mod)$s.pv, 3))
# 
#   } # end params loop "i"
# 
#   # dev.off()
# 
#   all.reefs.thres[[ar]] <- thresh.mat
#   all.reefs.slope[[ar]] <- slope.mat
#   all.reefs.int[[ar]] <- int.mat
#   all.reef.gamr2pval[[ar]] <- r2_pval.gam
#   all.reefs.std.slope[[ar]] <- std.slope.mat
# 
# 
#   # all.reefs.seg.thres[[ar]] <- thresh.mat.seg
#   # all.reefs.seg.slope[[ar]] <- slope.mat.seg
#   # all.reef.seg.gamr2pval[[ar]] <- r2_pval.gam
# 
# 
# }#end "ar" loop

# write.csv(do.call(rbind, all.reefs.thres),file= paste("output/AllReef_Thresholds_Boot=", boot, ".csv", sep = ""),
#           row.names=F)
# write.csv(do.call(rbind, all.reefs.slope),file= paste("output/AllReef_Slope_Boot=", boot, ".csv", sep = ""),
#           row.names=F)
# write.csv(do.call(rbind, all.reefs.int),file= paste("output/AllReef_Intercept_Boot=", boot, ".csv", sep = ""),
#           row.names=F)
# write.csv(do.call(rbind, all.reefs.std.slope),file= paste("output/AllReef_Std_Slope_Boot=", boot, ".csv", sep = ""),
#           row.names=F)

# 
# names(all.reef.gamr2pval) <- uniq.reefs
# 
# saveRDS(all.reef.gamr2pval, file = "output/AllReefs_GamR2+PvalFullData",
#         ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL) #save is as a list
# 
# #Writing sheets for segmented approach
# write.csv(do.call(rbind, all.reefs.seg.thres),file= paste("output/AllReef_seg_Thresholds_Boot=", boot, ".csv", sep = ""),
#           row.names=F)
# write.csv(do.call(rbind, all.reefs.seg.slope),file= paste("output/AllReef_seg_Slope_Boot=", boot, ".csv", sep = ""),
#           row.names=F)
# write.csv(do.call(rbind, all.reefs.seg.slope),file= paste("output/AllReef_seg_Intercept_Boot=", boot, ".csv", sep = ""),
#           row.names=F)
# 
# names(all.reef.seg.gamr2pval) <- uniq.reefs
# 
# saveRDS(all.reef.seg.gamr2pval, file = "output/AllReefs_seg_GamR2+PvalFullData",
#         ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL) #save is as a list

# Figure 3 data prep----------------------------------------------------------------
#Loading and make 0s NA

islandfunc <- function(x) {
  if(x == "H1") return(1) 
  if (x == "H2") return(1) 
  if (x == "H3") return(1)
  if (x == "H4") return(1)
  if (x == "H5") return(1)
  if (x == "H6") return(1)
  if (x == "H8") return(1)
  if(x == "LH1") return(2) 
  if (x == "LH2") return(2) 
  if (x == "LH3") return(2)
  if (x == "MOW1") return(2)
  if (x == "MOW2") return(2)
  if (x == "MOW3") return(2)
}

colorfunc <- function(x) {
  if(x == "H5") return("#001709") 
  if (x == "H8") return("#00441B") 
  if (x == "H3") return("#006D2C")
  if (x == "H4") return("#238B45")
  if (x == "H2") return("#41AB5D")
  if (x == "H6") return("#74C476")
  if (x == "H1") return("#A1D99B")
  if(x == "MOW1") return("#230045") 
  if (x == "MOW2") return("#3F007D") 
  if (x == "MOW3") return("#54278F")
  if (x == "LH1") return("#6A51A3")
  if (x == "LH3") return("#807DBA")
  if (x == "LH2") return("#9E9AC8")
}


#Separating islands and filter out 20 m
figvars = c(params.of.interest, "Reef")
figvars2 = c("Transect", "Distance")

Haitiall=subset(data, Island=="Haiti")
Haitiall = subset(Haitiall, Distance < 13)
Haitiall = Haitiall[c(figvars,figvars2)]
Haitiall$colour <- sapply(Haitiall$Reef,colorfunc)

Bahamasall=subset(data, Island=="Bahamas")
Bahamasall = subset(Bahamasall, Distance < 13)
Bahamasall = Bahamasall[c(figvars,figvars2)]
Bahamasall$colour <- sapply(Bahamasall$Reef,colorfunc)

#Defining figure names
colnames(Haitiall) <- c("Shoot growth", "C production", "SLA", "Seagrass %P", "Seagrass %N","LAI", "Seagrass.C13",
                        # "Shoot density",
                        "Reef","Transect","Distance","colour")
colnames(Bahamasall) <- c("Shoot growth", "C production", "SLA", "Seagrass %P", "Seagrass %N","LAI", "Seagrass.C13",
                          # "Shoot density",
                          "Reef","Transect","Distance","colour")

# Threshold and Slope data -----------------------------------------------------------
thresholds <- read.csv("output/AllReef_Thresholds_Boot=100.csv",
                       stringsAsFactors = FALSE, sep = ",")
thresholds[thresholds == 0 ] <- NA

slopes <- read.csv("output/AllReef_Slope_Boot=100.csv",
                   stringsAsFactors = FALSE, sep = ",")
# slopes <- read.csv("output/AllReef_Std_Slope_Boot=100.csv",
#                    stringsAsFactors = FALSE, sep = ",") #Standardised slopes

slopes[slopes == 0 ] <- NA

#Remove ES1
thresholds$Reef[thresholds$Reef == "ES1" ] <- NA
thresholds=thresholds[!is.na(thresholds$Reef),]
slopes$Reef[slopes$Reef == "ES1" ] <- NA
slopes=slopes[!is.na(slopes$Reef),]

thresholds$colour <- sapply(thresholds$Reef,colorfunc)
thresholds$Island <- sapply(thresholds$Reef,islandfunc)

slopes$colour <- sapply(slopes$Reef,colorfunc)
slopes$Island <- sapply(slopes$Reef,islandfunc)

#Mean and sd threshold
meanthresh <- data.frame(thresholds %>% 
                           group_by(Island, Reef, colour) %>% 
                           summarise_all(.funs = c(mean = "mean"), na.rm = T))
sdthresh <- data.frame(thresholds %>% 
                         group_by(Island, Reef, colour) %>% 
                         summarise_all(.funs = c(sd = "sd"), na.rm = T))

#Mean and sd slopes
meanslope <- data.frame(slopes %>% 
                          group_by(Island, Reef, colour) %>% 
                          summarise_all(.funs = c(mean = "mean"), na.rm = T))
sdslope <- data.frame(slopes %>% 
                        group_by(Island, Reef, colour) %>% 
                        summarise_all(.funs = c(sd = "sd"), na.rm = T))

#Getting reefs in right order
meanthresh$Reef = ordered(meanthresh$Reef, c("MOW3","MOW2","MOW1", "LH3", "LH2", "LH1","H8", "H6", "H5", "H4", "H3","H2", "H1"))
meanthresh <- meanthresh[order(meanthresh$Reef),]
sdthresh$Reef = ordered(sdthresh$Reef, c("MOW3","MOW2","MOW1", "LH3", "LH2", "LH1","H8", "H6", "H5", "H4", "H3","H2", "H1"))
sdthresh <- sdthresh[order(sdthresh$Reef),]

Haiti.thresh= subset(meanthresh, meanthresh$Island == "1")
Bahamas.thresh= subset(meanthresh, meanthresh$Island == "2")

#getting alpha for non-significant GAMs
alfalfa <- function(x) {
  if (x == "NaN") return(0.2) 
  if(x > 0.2) return(1) 
}



# Threshold and slope differences between Islands --------------------------------------
islandfunction <- function(x) {
  if(x == "1") return("Haiti") 
  if (x == "2") return("Bahamas") 
}

thresholds$Island <- as.factor(sapply(thresholds$Island,islandfunction))
slopes$Island <- as.factor(sapply(slopes$Island,islandfunction))
slopes$Reef <- as.factor(slopes$Reef)

#Calculating means as well
thresh.mean = data.frame(thresholds%>% 
                           group_by(Island,Reef, colour) %>% 
                           summarise_all(mean, na.rm = T))
head(thresh.mean)
head(reef)
#Remove transect column
reef <- reef[-3]
reef_mean <- data.frame(reef %>%
                          group_by(Island,Reef) %>% 
                          summarise_all(mean, na.rm = T))

thresh.preds.mean = data.frame(unique(merge(reef_mean[c("Island","Reef","Fish.P","Fish.N",
                                                        "Seagrass.P20", "Seagrass.N20",
                                                        "Sediment.P", "Sediment.N")], thresh.mean, by=c("Island","Reef"))))
thresh.preds.mean$Island <- as.factor(thresh.preds.mean$Island)

slope.mean = data.frame(slopes %>% 
                          group_by(Island,Reef, colour) %>% 
                          summarise_all(mean, na.rm = T))

slope.preds.mean = data.frame(unique(merge(reef_mean[c("Island","Reef","Fish.P","Fish.N",
                                                       "Seagrass.P20", "Seagrass.N20",
                                                       "Sediment.P", "Sediment.N")], slope.mean, by=c("Island","Reef"))))
slope.preds.mean$Island <- as.factor(slope.preds.mean$Island)

#####Separating by island to check for normality
Haiti_thresh= subset(thresh.mean, thresh.mean$Island == "Haiti")
Bahamas_thresh = subset(thresh.mean, thresh.mean$Island == "Bahamas")
Haiti_slope= subset(slope.mean, slope.mean$Island == "Haiti")
Bahamas_slope = subset(slope.mean, slope.mean$Island == "Bahamas")

##testing normality
var.test(Seagrass.P ~ Island, data = thresh.mean) #Good
var.test(Shoot.growth ~ Island, data = thresh.mean) #Good
var.test(Seagrass.C13 ~ Island, data = thresh.mean) #Good
var.test(Seagrass.P ~ Island, data = slope.mean) #Good
var.test(Shoot.growth ~ Island, data = slope.mean) #Good
var.test(Seagrass.C13 ~ Island, data = slope.mean) #Good


#####t-tests####
t.test(Haiti_thresh$Shoot.growth, Bahamas_thresh$Shoot.growth) # p = 0.76
mean(Haiti_thresh$Shoot.growth, na.rm = T)
sd(Haiti_thresh$Shoot.growth, na.rm = T)
mean(Bahamas_thresh$Shoot.growth, na.rm = T)
sd(Bahamas_thresh$Shoot.growth, na.rm = T)

t.test(Haiti_thresh$Seagrass.P, Bahamas_thresh$Seagrass.P) # p = 0.50
mean(Haiti_thresh$Seagrass.P, na.rm = T)
sd(Haiti_thresh$Seagrass.P, na.rm = T)
mean(Bahamas_thresh$Seagrass.P, na.rm = T)
sd(Bahamas_thresh$Seagrass.P, na.rm = T)

t.test(Haiti_slope$Shoot.growth, Bahamas_slope$Shoot.growth) # p = 0.34
mean(Haiti_slope$Shoot.growth, na.rm = T)
sd(Haiti_slope$Shoot.growth, na.rm = T)
mean(Bahamas_slope$Shoot.growth, na.rm = T)
sd(Bahamas_slope$Shoot.growth, na.rm = T)

t.test(Haiti_slope$Seagrass.P, Bahamas_slope$Seagrass.P) # p = 0.037
mean(Haiti_slope$Seagrass.P, na.rm = T)
sd(Haiti_slope$Seagrass.P, na.rm = T)
mean(Bahamas_slope$Seagrass.P, na.rm = T)
sd(Bahamas_slope$Seagrass.P, na.rm = T)

t.test(Haiti_thresh$Seagrass.C13, Bahamas_thresh$Seagrass.C13) # p = 0.70
mean(Haiti_thresh$Seagrass.C13, na.rm = T)
sd(Haiti_thresh$Seagrass.C13, na.rm = T)
mean(Bahamas_thresh$Seagrass.C13, na.rm = T)
sd(Bahamas_thresh$Seagrass.C13, na.rm = T)

t.test(Haiti_slope$Seagrass.C13, Bahamas_slope$Seagrass.C13) # p = 0.011
mean(Haiti_slope$Seagrass.C13, na.rm = T)
sd(Haiti_slope$Seagrass.C13, na.rm = T)
mean(Bahamas_slope$Seagrass.C13, na.rm = T)
sd(Bahamas_slope$Seagrass.C13, na.rm = T)

# Thresholds per Island figure -----------------------------------------------------
prop.threshold <- thresholds
prop.threshold[is.na(prop.threshold)] <- 0

not.found = data.frame(prop.threshold[c(2:8,10)] %>% 
                         group_by(Island) %>% 
                         dplyr::summarise_each(funs(sum(.== 0))))
found = data.frame(prop.threshold[c(2:8,10)] %>% 
                     group_by(Island) %>% 
                     dplyr::summarise_each(funs(sum(.> 0))))

totals = found/(not.found + found)
totals$Island = c("Bahamas", "Haiti")

totals.mat = as.matrix(totals[c(2:length(totals))])
rownames(totals.mat) = totals$Island

#All variables together
head(prop.threshold)
converged <- melt(prop.threshold[c(2:8,10)],id = c("Island"))

not.found.all = data.frame(converged[,c(1,3)] %>% 
                             group_by(Island) %>% 
                             dplyr::summarise_each(funs(sum(.== 0))))
found.all = data.frame(converged[,c(1,3)] %>% 
                         group_by(Island) %>% 
                         dplyr::summarise_each(funs(sum(.> 0))))

totals.all = found.all/(not.found.all + found.all)
totals.all$Island = c("Bahamas", "Haiti")
trans.totals.all = t(as.matrix(totals.all[c(2:length(totals.all))]))
colnames(trans.totals.all) = totals.all$Island

totals.all <- cbind(totals.mat, totals.all[2])
colnames(totals.all)[8] <- "All"
totals.all = as.matrix(totals.all)

# Threshold and slopes - Linear models with predictors -------------------------------------------

#####linear models with predictors
thresh.preds.mean$Fish.NP = thresh.preds.mean$Fish.N/thresh.preds.mean$Fish.P*31/14
thresh.preds.mean$Seagrass.NP20 = thresh.preds.mean$Seagrass.N20/thresh.preds.mean$Seagrass.P20*31/14
thresh.preds.mean$Sediment.NP = thresh.preds.mean$Sediment.N/thresh.preds.mean$Sediment.P*31/14

slope.preds.mean$Fish.NP = slope.preds.mean$Fish.N/slope.preds.mean$Fish.P*31/14
slope.preds.mean$Seagrass.NP20 = slope.preds.mean$Seagrass.N20/slope.preds.mean$Seagrass.P20*31/14
slope.preds.mean$Sediment.NP = slope.preds.mean$Sediment.N/slope.preds.mean$Sediment.P*31/14


#Use if averaging predictors to one value per reef (Ambient and Sediment)
predators <- data.frame(thresh.preds.mean[c("Reef","Fish.N", "Fish.P","Fish.NP",
                                            "Seagrass.N20", "Seagrass.P20","Seagrass.NP20",
                                            "Sediment.N", "Sediment.P", "Sediment.NP")] %>%
                          group_by(Reef) %>%
                          summarise_all(.funs = c(. = "mean"), na.rm = T))
names(predators) <- gsub("_.", "", names(predators), fixed = TRUE)

table2.p <- data.frame(matrix(nrow = 6, ncol = 9))
colnames(table2.p) <- names(predators[2:10])
rownames(table2.p) <- c("Shoot growth threshold", "Seagrass %P threshold", "Seagrass C13 threshold",
                        "Shoot growth slope", "Seagrass %P slope", "Seagrass C13 slope")
table2 <- table2.p


resp <- c("Shoot.growth", "Seagrass.P", "Seagrass.C13")
par(mfrow = c(3,3))

#quick for loop
for (i in 2:10){
  # Shoot growth threshold
  predictor <- names(predators[i])
  
  for (j in 1:3){
    response <- resp[j]
    thresh <- unique(data.frame(na.omit(merge(thresh.preds.mean[c("Reef","colour",response)],
                                              predators[c("Reef", predictor)]))))
    slope <- unique(data.frame(na.omit(merge(slope.preds.mean[c("Reef","colour",response)],
                                             predators[c("Reef", predictor)]))))
    #Pearson cor
    # pear.slope <- cor.test(slope[,3], slope[,4], method="pearson")
    # pear.thresh <- cor.test(thresh[,3], thresh[,4], method="pearson")
    # 
    # table2.p[j,i-1] <- round(pear.thresh$p.value, 3)
    # table2.p[j+2,i-1] <- round(pear.slope$p.value,3)
    # 
    # table2[j,i-1] <- round(pear.thresh$estimate, 3)
    # table2[j+2,i-1] <- round(pear.slope$estimate,3)
    
    #linear models
    lm.slope <- summary(lm(slope[,3]~ slope[,4]))
    lm.thresh <- summary(lm(thresh[,3]~ thresh[,4]))
    
    table2.p[j,i-1] <- round(lm.thresh$coefficients[2,4], 3)
    table2.p[j+3,i-1] <- round(lm.slope$coefficients[2,4],3)
    
    # table2.r2[j,i-1] <- round(lm.thresh$adj.r.squared, 3)
    # table2.r2[j+3,i-1] <- round(lm.slope$adj.r.squared,3)
    
    
    table2[j,i-1] <- round(lm.thresh$coefficients[2,1], 3)
    table2[j+3,i-1] <- round(lm.slope$coefficients[2,1],3)
    
    
    #Plotting
    plot(slope[,4],slope[,3],  xlab = predictor,ylab = response, 
         ylim=c(min(slope[,3], na.rm = T), max(slope[,3], na.rm = T)), 
         xlim = c(min(slope[,4], na.rm = T), max(slope[,4], na.rm = T)),
         col = slope$colour, pch = 19, cex = 1.5)
    
    
  }
}


# write.csv(table2, "output/table2.csv")
# write.csv(table2.p, "output/table2.p.csv")

# ******Full figure code for paper ----------------------------------------------
#Order by ambient 20% P
reef.order <- predators[with(predators, order(-Seagrass.P20)),]
reef.order$Island <- sapply(reef.order$Reef,islandfunc)
reef.order<- reef.order[c("Island","Reef")]
haiti.order <- subset(reef.order, reef.order == "1")["Reef"]
bahamas.order <- subset(reef.order, reef.order == "2")["Reef"]
reef.order <- rbind(bahamas.order, haiti.order)

Haitiall <- Haitiall[order(factor(Haitiall$Reef, levels = rev(c("H5","H8","H3","H4","H2","H6","H1")))),]
Bahamasall <- Bahamasall[order(factor(Bahamasall$Reef, levels = rev(c("MOW1","MOW2","MOW3","LH1","LH3","LH2")))),]


# Bahamas GAM figs -----------------------------------------------------------

ylabs = data.frame(matrix(nrow = 7))
ylabs[1,1] = expression(mm^2~day^-1)
ylabs[2,1] = expression(g~m^-2~day^-1)
ylabs[3,1] = expression(mm^2~mg^-1)
ylabs[4,1] = expression("%")
ylabs[5,1] = expression("%")
ylabs[6,1] = expression(m^2~m^-2)
ylabs[7,1] = expression("\u2030")

## With the title descriptors in the labs 
# ylabs = data.frame(matrix(nrow = 6))
# ylabs[1,1] = expression(Shoot~growth~(mm^2~day^-1))
# ylabs[2,1] = expression(C~production~(g~m^-2~day^-1))
# ylabs[3,1] = expression(SLA~(mm^2~mg^-1))
# ylabs[4,1] = expression(Seagrass~P~content~"(%)")
# ylabs[5,1] = expression(Seagrass~N~content~"(%)")
# ylabs[6,1] = expression(Leaf~area~index~(mm^2~m^-2))


#main fig
ylims = data.frame(matrix(nrow = 7, ncol = 5))
colnames(ylims) = c("ymin", "ymax", "seq1", "seq2", "by")
summary(Haitiall$`Shoot growth`)
summary(Bahamasall$`Shoot growth`)
summary(Haitiall$`C production`)
summary(Bahamasall$`C production`)
summary(Haitiall$SLA)
summary(Bahamasall$SLA)
summary(Haitiall$`Seagrass %P`)
summary(Bahamasall$`Seagrass %P`)
summary(Haitiall$`Seagrass %N`)
summary(Bahamasall$`Seagrass %N`)
summary(Haitiall$LAI)
summary(Bahamasall$LAI)
summary(Haitiall$Seagrass.C13)
summary(Bahamasall$Seagrass.C13)

ylims[1,1:5] = c(0,350, 0, 300, 100)
ylims[2,1:5] = c(0,2.15,0, 2, 0.5)
ylims[3,1:5] = c(12,35,15,35, 10)
ylims[4,1:5] = c(0.05,0.2, 0.05,0.2, 0.05)
ylims[5,1:5] = c(1,2.5,1,2.5, 0.5)
ylims[6,1:5] = c(0,4,0,4, 1)
ylims[7,1:5] = c(-12,-6,-12,-6, 2)

min_reef <- NULL
max_reef <- NULL


#A4 has 16.8 cm for figure
layout.matrix=matrix(c(1:11,rep(12,4),13,14,14:17,14,14,18:20,14,14,21:23,rep(24,4)), byrow=TRUE, ncol=5,nrow=7)
layout(mat = layout.matrix,
       heights = lcm(c(2.5,2.5,0.7,(5/3),(5/3),(5/3),0.7)), # Heights of the rows
       widths = lcm(c(1.8,rep(3.7,4)))) # Widths of the columns


# Fig start ---------------------------------------------------------------


layout.matrix=matrix(c(1:11,rep(12,4),13,13,14:16,13,13,17:19,rep(20,5)), byrow=TRUE, ncol=5,nrow=6)
layout(mat = layout.matrix,
       heights = lcm(c(2.5,2.5,0.7,2.5,2.5,0.7)), # Heights of the rows
       widths = lcm(c(1.8,rep(3.7,4)))) # Widths of the columns

#Legend
par(mar = c(0, 0, 0.5, 0),mgp=c(0, 0, 0), lwd = 0.75,
    family = "serif", font = 1, font.main = 1, cex.lab = 1, cex.axis = 1, 
    cex.main = 1, cex.sub = 1) 
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)
# text(-1,1,"Bahamas", xjust = 0, line = -0.5)

legend(-1,1.2, c(unique(as.character(Bahamasall$Reef))), 
       pt.bg=unique(Bahamasall$colour), horiz=F, cex=1,bty = "n", text.width = 0.4, 
       pch=22, pt.cex = 2.5,xjust = 0,title = "Haiti", title.adj = 0,adj = 0,
       y.intersp=0.8,x.intersp=1, pt.lwd = 0.75) 


for(i in c(2,1,4,7)){
  
  var=Bahamasall[,i]
  Dist=as.numeric(Bahamasall$Distance)
  
  for(Reef in unique(Bahamasall$Reef)){
    x=Bahamasall[,i][Bahamasall$Reef == Reef]
    Distance = Bahamasall$Distance[Bahamasall$Reef == Reef]
    Reef=Reef
    
    #Gam lines to get ylims
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    min_reef[Reef]=min(pred.resp$pred, na.rm=T)
    max_reef[Reef]=max(pred.resp$pred, na.rm=T)
    
    
  }
  # ymin=min(var, na.rm=T)
  # ymax=max(var, na.rm=T)
  
  ymin = ylims[i,1]
  ymax = ylims[i,2]
  at = seq(ylims[i,3], ylims[i,4], by = ylims[i,5])
  
  par(mar = c(0, 3, 1, 0),mgp=c(1.7, 0.5, 0))
  
  plot(Dist,y=var,ylim=c(ymin,ymax),type='n',col="white", xlab = "", main = paste(names(Bahamasall[i])), 
       ylab = ylabs[i,1],  xaxt="n",yaxt="n",xlim= c(0,12),frame.plot=F)
  
  axis(2, pos = 0,at = c(ymin,ymax), las = 2,lwd.tick=0,labels=FALSE, lwd = 0.75)
  axis(2, pos = 0,at = at, las = 2,lwd = 0, lwd.tick = 0.75, tck = -0.05)
  axis(1, pos = ymin, at = seq(0,14,2),labels=FALSE,lwd.tick = 0.75, lwd = 0.75, tck = -0.05)
  # axis(3, at = seq(0,14,2), lwd.tick=0, labels=FALSE)
  # axis(4, pos = 12.5, at = c(ymin,ymax),lwd.tick=0, labels=FALSE)
  
  
  for(Reef in unique(Bahamasall$Reef)){
    x=Bahamasall[,i][Bahamasall$Reef == Reef]
    Distance = Bahamasall$Distance[Bahamasall$Reef == Reef]
    reefcols = Bahamasall$colour[Bahamasall$Reef == Reef]
    
    #Linear model
    mod.lm <- lm(x ~ Distance, na.action = "na.omit")
    
    #Gam lines
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    
    #Average points
    data.points=data.frame(cbind(x, Distance))
    # summary <- aggregate(data.points$x, list(data.points$Distance), MeanCI, na.rm=T)
    # summary<- setNames(data.frame(summary$Group.1, data.frame(summary$x)), c("Distance", "x", "lower", "upper"))
    
    
    if( summary(gam1)$s.table[4] > 0.05 | AIC(gam1) > AIC(mod.lm) ) {
      lines(pred.resp$pred~pred.resp$Distance, lwd=1, col = alpha(reefcols, 0.2))
      points(data.points$x~data.points$Distance, col = alpha(reefcols, 0.2), pch = 16, cex = 0.5)
      # segments(summary$Distance, summary$lower,
      #          summary$Distance, summary$upper,
      #          col = alpha(reefcols, 0.2),lwd = 2, lty = 1.2 )
      
    }
    else {
      lines(pred.resp$pred~pred.resp$Distance, data=pred.resp,lwd=2, col = reefcols)
      points(data.points$x~data.points$Distance,col = reefcols, pch = 16, cex = 0.5)
      # segments(summary$Distance, summary$lower,
      #          summary$Distance, summary$upper,
      #          col = reefcols,lwd = 2, lty = 1.2 )
      
    }
  }
}


# Haiti GAM figs -----------------------------------------------------------------
#main fig
min_reef <- NULL
max_reef <- NULL

#Legend
par(mar = c(0, 0, 0.5, 0),mgp=c(0, 0, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)

legend(-1,1.2, c(unique(as.character(Haitiall$Reef))), 
       pt.bg=unique(Haitiall$colour), horiz=F, cex=1,bty = "n", text.width = 0.4, 
       pch=22, pt.cex = 2.5,xjust = 0,title = "Haiti", title.adj = 0,adj = 0,
       y.intersp=0.8,x.intersp=1, pt.lwd = 0.75) 


options(na.action = na.omit)
for(i in c(2,1,4,7)){
  
  var=Haitiall[,i]
  Dist=as.numeric(Haitiall$Distance)
  
  for(Reef in unique(Haitiall$Reef)){
    x=Haitiall[,i][Haitiall$Reef == Reef]
    Distance = Haitiall$Distance[Haitiall$Reef == Reef]
    Reef=Reef
    
    #Gam lines to get ylims
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    min_reef[Reef]=min(pred.resp$pred, na.rm=T)
    max_reef[Reef]=max(pred.resp$pred, na.rm=T)
    
    
  }
  # ymin=min(var, na.rm=T)
  # ymax=max(var, na.rm=T)
  ymin = ylims[i,1]
  ymax = ylims[i,2]
  at = seq(ylims[i,3], ylims[i,4], by = ylims[i,5])
  
  par(mar = c(0, 3, 1, 0),mgp=c(1.7, 0.5, 0))
  
  plot(Dist,y=var,ylim=c(ymin,ymax),type='n',col="white", xlab = "", main = "", 
       ylab = ylabs[i,1],  xaxt="n",yaxt="n",xlim= c(0,12),frame.plot=F)
  
  axis(2, pos = 0,at = c(ymin,ymax), las = 2,lwd.tick=0,labels=FALSE, lwd = 0.75)
  axis(2, pos = 0,at = at, las = 2,lwd = 0, lwd.tick = 0.75, tck = -0.05)
  axis(1, pos = ymin, at = seq(0,14,2),labels=T,lwd.tick = 0.75, lwd = 0.75, tck = -0.05)
  # axis(3, at = seq(0,14,2), lwd.tick=0, labels=FALSE)
  # axis(4, pos = 12.5, at = c(ymin,ymax),lwd.tick=0, labels=FALSE)
  
  for(Reef in unique(Haitiall$Reef)){
    x=Haitiall[,i][Haitiall$Reef == Reef]
    Distance = Haitiall$Distance[Haitiall$Reef == Reef]
    reefcols = Haitiall$colour[Haitiall$Reef == Reef]
    
    #Linear model
    mod.lm <- lm(x ~ Distance, na.action = "na.omit")
    
    #Gam lines
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    
    #Average points
    data.points=data.frame(cbind(x, Distance))
    # summary <- aggregate(data.points$x, list(data.points$Distance), MeanCI, na.rm=T)
    # summary<- setNames(data.frame(summary$Group.1, data.frame(summary$x)), c("Distance", "x", "lower", "upper"))
    
    
    if( summary(gam1)$s.table[4] > 0.05 | AIC(gam1) > AIC(mod.lm) ) {
      lines(pred.resp$pred~pred.resp$Distance, lwd=1, col = alpha(reefcols, 0.2))
      points(data.points$x~data.points$Distance, col = alpha(reefcols, 0.2), pch = 16, cex = 0.5)
      # segments(summary$Distance, summary$lower,
      #          summary$Distance, summary$upper,
      #          col = alpha(reefcols, 0.2),lwd = 2, lty = 1.2 )
      
    }
    else {
      lines(pred.resp$pred~pred.resp$Distance, data=pred.resp,lwd=2, col = reefcols)
      points(data.points$x~data.points$Distance,col = reefcols, pch = 16, cex = 0.5)
      # segments(summary$Distance, summary$lower,
      #          summary$Distance, summary$upper,
      #          col = reefcols,lwd = 2, lty = 1.2 )
      
    }
  }
}



# Threshold fig -----------------------------------------------------------
# par(mar = c(2, 2, 2, 1),mgp=c(3, 1, 0))
# 
# plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
# meanthresh <- meanthresh[order(factor(meanthresh$Reef, levels = factor(c(reef.order)))),]
# 
# meanthresh<-meanthresh[order(factor(meanthresh$Reef,levels=rev(c("LH2", "LH3", "LH1", "MOW3", "MOW2", "MOW1", 
#                                                                  "H1", "H6", "H2", "H4", "H3", "H8", "H5")))),]
# sdthresh<-sdthresh[order(factor(sdthresh$Reef,levels=rev(c("LH2", "LH3", "LH1", "MOW3", "MOW2", "MOW1", 
#                                                            "H1", "H6", "H2", "H4", "H3", "H8", "H5")))),]
# 
# 
# Haiti.thresh= subset(meanthresh, meanthresh$Island == "1")
# Bahamas.thresh= subset(meanthresh, meanthresh$Island == "2")
# 
# #getting alpha for non-significant GAMs
# alfalfa <- function(x) {
#   if (x == "NaN") return(0.2) 
#   if(x > 0.2) return(1) 
# }
# 
# #Dotplot
# for(i in c(4:5,7:8)){
#   
#   var=meanthresh[,i]
#   Reef.pos =1:13
#   Reef = unique(meanthresh$Reef)
#   Reef2= factor(Reef, levels = c(reef.order[,1]))
#   alpha <- sapply(var,alfalfa)
#   var[is.na(var)] <- 0.3
#   sd=sdthresh[,i]
#   ci=qt(0.975,df=boot-1)*sd/sqrt(boot)
#   ci[is.na(ci)] <- 0
#   
#   line.haiti <- mean(Haiti.thresh[,i], na.rm = T)
#   line.bahamas <- mean(Bahamas.thresh[,i], na.rm = T)
#   
#   colour = unique(meanthresh$colour)
#   par(mar = c(2, 4.5, 2, 0.5))
#   
#   plot(0, 0, type='n', xlim= c(0,12), ylim = c(1,13), yaxt = "n", xaxt = "n",
#        ylab = "", xlab="",frame.plot=F)
#   
#   segments(var-ci , Reef.pos,
#            var+ci , Reef.pos,
#            col = alpha(colour, alpha),lwd = 2, lty = 1.2 )
#   arrows(x0=line.bahamas, y0=7, x1=line.bahamas, y1=13, code=2, col="#6A51A3", lwd=1, 
#          length = 0, lty = 2)
#   arrows(x0=line.haiti, y0=1, x1=line.haiti, y1=6, code=2, col="#238B45", lwd=1, 
#          length = 0, lty = 2)
#   points(var, Reef.pos, pch = 16, col = alpha(colour, alpha),lwd = 1, cex = 2.2)
#   if(names(meanthresh[i]) == "Shoot.growth_mean"){
#     axis(2, at=c(0:14), labels=c("",as.character(Reef),""),pos = 0,las = 2, cex.axis = 1)
#   }else{
#     axis(2, at=c(0:14), labels=FALSE,pos = 0,las = 2, cex.axis = 1)
#   }
#   axis(1, pos = 0.5, at = seq(0,14,2))
#   # axis(3, pos = 13.5,at = seq(0,14,2), lwd.tick=0, labels=FALSE)
#   # axis(4, pos = 12.5, at = c(0:14),lwd.tick=0, labels=FALSE)
#   
# }
# 
# 

par(mar = c(0, 0, 0.5, 0),mgp=c(0, 0, 0))

# par(mar = c(0, 1, 1, 0.5),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')


par(mar = c(0, 0, 2, 0),mgp=c(1.7, 0.5, 0))

# par(mar = c(0, 4.5, 3, 0.5),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n',bty='n', main = "Distance (m)", xaxt='n', yaxt='n', ylab = "")

#17x7
# Threshold proportions ---------------------------------------------------
# par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
# plot(0, 0, type='n', bty='n'
#      , xaxt='n', yaxt='n'
# )

# par(mar = c(2, 5,1,1),mgp=c(3.5, 0.7, 0))
par(mar = c(0, 2, 0, 0),mgp=c(2, 0.5, 0))


#Changeplot
x=barplot(totals.all[,c(2,1,4,7,5,3,6,8)],
          col=c("#6A51A3","#238B45"),
          border="black",
          beside=T,
          ylab="Proportion of ARs",
          ylim = c(0,1.1),
          xaxt = "n", yaxt = "n"
          # names.arg=c("Shoot growth","C production","%P","Seagrass %N","SLA","LAI","13CSeagrass","Overall"))
)
axis(1, at = c(5, 11, 17, 23), labels = c("Shoot growth","d13C","SLA","Overall"),
     col = "white", las = 1, pos = -0.07,lwd.ticks = 0.75,tck = -0.05, lwd = 0.75)
axis(1, at = c(0, 2, 5, 8, 11, 14, 17, 20, 23), labels = c(NA,"C prod.",NA,"%P",NA,"%N",NA,"LAI",NA),
     lwd.ticks = 0.75,tck = -0.02, lwd = 0.75)
axis(2, at = seq(0,100,0.25), las = 2,lwd.ticks = 0.75,tck = -0.02, lwd = 0.75)
abline(v=21.5, lty = 2, las = 1)
abline(h=0)

# Threshold across island -----------------------------------------------------
par(mar = c(0, 3, 1, 0),mgp=c(1.7, 0.5, 0))

#Shoot growth
boxplot(Shoot.growth ~ Island, data = thresh.mean, col = "white", border = "white", ylim = c(1.5,4.5),
        ylab = "Threshold (m)", xlab = NA,las = 1, yaxt = "n", main = "Shoot growth",
        frame.plot = F, xaxt = "n")
# title("Shoot growth", adj = 0, font = 1)
min(thresh.mean$Shoot.growth, na.rm = T)
max(thresh.mean$Shoot.growth, na.rm = T)

axis(2, pos = 0.5, at = c(1.5,2.5,3.5,4.5), las = 2, lwd = 0, lwd.ticks = 0.75,
     tck = -0.05)
axis(2, pos = 0.5, at = c(1.5,4.5), las = 2, labels = F,lwd.ticks = 0, lwd = 0.75)
text(2.5, 4.5, "NS", adj = c(1,1), cex = 1)

axis(1, pos = 1.5, at = c(0,1,2,3), labels = c("","", "",""), lwd = 0.75)

# Add data points
mylevels <- levels(thresh.mean$Island)
levelProportions <- summary(thresh.mean$Island)/nrow(thresh.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- thresh.mean[thresh.mean$Island==thislevel, "Shoot.growth"]
  color = thresh.mean$colour[thresh.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(7)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 1.5) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=0.75, angle = 90, 
         length = 0.02)
  points(i, ymean , pch = 23, bg = "black", cex = 1.5)
  
}

#Seagrass %P
min(thresh.mean$Seagrass.P, na.rm = T)
max(thresh.mean$Seagrass.P, na.rm = T)

boxplot(Seagrass.P ~ Island, data = thresh.mean, col = "white", border = "white", ylim = c(1.5,4.5),
        ylab = "", xlab = NA,las = 1, yaxt = "n", main = "Seagrass %P",
        frame.plot = F, xaxt = "n")
text(2.5, 4.5, "NS", adj = c(1,1), cex = 1)

axis(2, pos = 0.5, at = c(1.5,2.5,3.5,4.5), las = 2, lwd = 0, lwd.ticks = 0.75,
     tck = -0.05)
axis(2, pos = 0.5, at = c(1.5,4.5), las = 2, labels = F,lwd.ticks = 0, lwd = 0.75)
axis(1, pos = 1.5, at = c(0,1,2,3), labels = c("","", "",""), lwd = 0.75)

# Add data points
mylevels <- levels(thresh.mean$Island)
levelProportions <- summary(thresh.mean$Island)/nrow(thresh.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- thresh.mean[thresh.mean$Island==thislevel, "Seagrass.P"]
  color = thresh.mean$colour[thresh.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(8)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 1.5) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=0.75, angle = 90, 
         length = 0.02)
  points(i, ymean , pch = 23, bg = "black", cex = 1.5)
  
}

#Seagrass 13C
min(thresh.mean$Seagrass.C13, na.rm = T)
max(thresh.mean$Seagrass.C13, na.rm = T)

boxplot(Seagrass.C13 ~ Island, data = thresh.mean, col = "white", border = "white", ylim = c(1.5,4.5),
        ylab = "", xlab = NA,las = 1, yaxt = "n", main = "Seagrass d13C",
        frame.plot = F, xaxt = "n")
text(2.5, 4.5, "NS", adj = c(1,1), cex = 1)

axis(2, pos = 0.5, at = c(1.5,2.5,3.5,4.5), las = 2, lwd = 0, lwd.ticks = 0.75,
     tck = -0.05)
axis(2, pos = 0.5, at = c(1.5,4.5), las = 2, labels = F,lwd.ticks = 0, lwd = 0.75)
axis(1, pos = 1.5, at = c(0,1,2,3), labels = c("","", "",""), lwd = 0.75)

# Add data points
mylevels <- levels(thresh.mean$Island)
levelProportions <- summary(thresh.mean$Island)/nrow(thresh.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- thresh.mean[thresh.mean$Island==thislevel, "Seagrass.C13"]
  color = thresh.mean$colour[thresh.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(8)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 1.5) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=0.75, angle = 90, 
         length = 0.02)
  points(i, ymean , pch = 23, bg = "black", cex = 1.5)
  
}

# Slope across island -----------------------------------------------------
#Shoot growth
boxplot(Shoot.growth ~ Island, data = slope.mean, col = "white", border = "white", ylim = c(-70,0),
        ylab = "Effect size (slope)", xlab = NA,las = 1, yaxt = "n",
        frame.plot = F, xaxt = "n")
# title("Shoot growth", adj = 0, font = 1)
min(slope.mean$Shoot.growth, na.rm = T)
max(slope.mean$Shoot.growth, na.rm = T)

axis(2, pos = 0.5, at = c(-60,-40,-20,0), las = 2, lwd = 0, lwd.ticks = 0.75,
     tck = -0.05)
axis(2, pos = 0.5, at = c(-70,0), las = 2, labels = F,lwd.ticks = 0, lwd = 0.75)
text(2.5, 0, "NS", adj = c(1,1), cex = 1)

axis(1, pos = -70, at = c(0,1,2,3), labels = c("","Bahamas", "Haiti",""), lwd = 0.75)

# Add data points
mylevels <- levels(slope.mean$Island)
levelProportions <- summary(slope.mean$Island)/nrow(slope.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- slope.mean[slope.mean$Island==thislevel, "Shoot.growth"]
  color = slope.mean$colour[slope.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(7)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 1.5) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=0.75, angle = 90, 
         length = 0.02)
  points(i, ymean , pch = 23, bg = "black", cex = 1.5)
  
}

#Seagrass %P
boxplot(Seagrass.P ~ Island, data = slope.mean, col = "white", border = "white", ylim = c(-0.040,0),
        ylab = "", xlab = NA,las = 1, yaxt = "n",
        frame.plot = F, xaxt = "n")
# title("Shoot growth", adj = 0, font = 1)
min(slope.mean$Seagrass.P, na.rm = T)
max(slope.mean$Seagrass.P, na.rm = T)

axis(2, pos = 0.5, at = c(-0.04,-0.03,-0.02,-0.01,0), las = 2, lwd = 0, lwd.ticks = 0.75,
     tck = -0.05)
axis(2, pos = 0.5, at = c(-0.04,0), las = 2, labels = F,lwd.ticks = 0, lwd = 0.75)
text(2.5, 0, "**", adj = c(1,1), cex = 1)

axis(1, pos = -0.04, at = c(0,1,2,3), labels = c("","Bahamas", "Haiti",""), lwd = 0.75)

# Add data points
mylevels <- levels(slope.mean$Island)
levelProportions <- summary(slope.mean$Island)/nrow(slope.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- slope.mean[slope.mean$Island==thislevel, "Seagrass.P"]
  color = slope.mean$colour[slope.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(8)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 1.5) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=0.75, angle = 90, 
         length = 0.02)
  points(i, ymean , pch = 23, bg = "black", cex = 1.5)
  
}

#Seagrass 13C
boxplot(Seagrass.C13 ~ Island, data = slope.mean, col = "white", border = "white", ylim = c(-1,1.5),
        ylab = "", xlab = NA,las = 1, yaxt = "n",
        frame.plot = F, xaxt = "n")
# title("Shoot growth", adj = 0, font = 1)
min(slope.mean$Seagrass.C13, na.rm = T)
max(slope.mean$Seagrass.C13, na.rm = T)

axis(2, pos = 0.5, at = c(-1,-0.5,0,0.5,1,1.5), las = 2, lwd = 0, lwd.ticks = 0.75,
     tck = -0.05)
axis(2, pos = 0.5, at = c(-1,1.5), las = 2, labels = F,lwd.ticks = 0, lwd = 0.75)
text(2.5, 1.5, "**", adj = c(1,1), cex = 1)

axis(1, pos = -1, at = c(0,1,2,3), labels = c("","Bahamas", "Haiti",""), lwd = 0.75)

# Add data points
mylevels <- levels(slope.mean$Island)
levelProportions <- summary(slope.mean$Island)/nrow(slope.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- slope.mean[slope.mean$Island==thislevel, "Seagrass.C13"]
  color = slope.mean$colour[slope.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(8)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 1.5) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=0.75, angle = 90, 
         length = 0.02)
  points(i, ymean , pch = 23, bg = "black", cex = 1.5)
  
}

# Slope vs. predictors ----------------------------------------------------
par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)

par(mar = c(2, 5,1,1),mgp=c(3.5, 0.7, 0))

slope.thresh.v.pred <- na.omit(merge(predators[c("Reef","Sediment.N", "Sediment.P")],
                                     unique(slope.preds.mean[c("Reef","Seagrass.P")]),
                                     by = "Reef") %>% 
                                 merge(unique(thresh.preds.mean[c("Reef","Seagrass.P","colour")]), by = "Reef"))

colnames(slope.thresh.v.pred) = c("Reef", "Sediment.N", "Sediment.P","Seagrass.P.slope", "Seagrass.P.thresh", "colour")

plot(slope.thresh.v.pred$Sediment.N,slope.thresh.v.pred$Seagrass.P.thresh,
     yaxt = "n",
     xaxt = "n",
     xlab = "Sediment %N content",ylab = "Threshold (m)",
     ylim=c(min(slope.thresh.v.pred$Seagrass.P.thresh)*0.9, 
            max(slope.thresh.v.pred$Seagrass.P.thresh)*1.1),
     xlim = c(min(slope.thresh.v.pred$Sediment.N)*0.9, 
              max(slope.thresh.v.pred$Sediment.N)*1.1),
     col = slope.thresh.v.pred$colour, pch = 19, cex = 2.2, las=1, frame.plot = F)
title("Seagrass %P", adj = 0, font = 1)
axis(2, at = c(0,2,2.5,3,3.5,4,4.5), las =1)
axis(1, at = c(0,0.02,0.04,0.06,0.08), las =1)

mod3 <- cor.test(slope.thresh.v.pred$Seagrass.P.thresh,slope.thresh.v.pred$Sediment.N,
                 method = "pearson")
r = round(mod3$estimate,2)
mod3 <- lm(Seagrass.P.thresh ~ Sediment.N, data = slope.thresh.v.pred)
# summary(mod)
abline(mod3, lwd =2)
# modsum3 = summary(mod3)
# r2.3 = modsum3$adj.r.squared
# my.p.3 = modsum3$coefficients[2,4]
# my.slope3 = modsum3$coefficients[2,1]

text(0.08, 2, paste("r =",as.character(r)), adj = c(1,0), cex = 2)


plot(slope.thresh.v.pred$Sediment.N,slope.thresh.v.pred$Seagrass.P.slope,
     yaxt = "n",xaxt = "n",
     xlab = "Sediment %N content",ylab = "Effect size (slope)", 
     ylim=c(min(slope.thresh.v.pred$Seagrass.P.slope)*1.1, 
            max(slope.thresh.v.pred$Seagrass.P.slope)*0.9), 
     xlim = c(min(slope.thresh.v.pred$Sediment.N)*0.9, 
              max(slope.thresh.v.pred$Sediment.N)*1.1),
     col = slope.thresh.v.pred$colour, pch = 19, cex = 2.2, las=1, frame.plot = F)
title("Seagrass %P", adj = 0, font = 1)

axis(2, at = c(-0.05,-0.04,-0.03,-0.02,-0.01, 0.005), las =1, labels = F)
axis(2, at = c(-0.05,-0.04,-0.03,-0.02,-0.01), las =1, lwd = 0)
axis(1, at = c(0,0.02,0.04,0.06,0.08), las =1)

mod <- cor.test(slope.thresh.v.pred$Seagrass.P.slope,slope.thresh.v.pred$Sediment.N,
                method = "pearson")
rx = round(mod$estimate,2)

mod <- lm(Seagrass.P.slope ~ Sediment.N, data = slope.thresh.v.pred)
# 
abline(mod, lwd =2)
# modsum = summary(mod)
# r2 = modsum$adj.r.squared
# my.p = modsum$coefficients[2,4]
# my.slope = modsum$coefficients[2,1]

text(0.08, -0.04, paste("r =",as.character(rx)), adj = c(1,0), cex = 2)

par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)

par(mar = c(2, 2, 1.5, 1))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')

#13x12
# ******Supplement GAM figure ---------------------------------------------------
# Bahamas GAM figs -----------------------------------------------------------
layout.matrix=matrix(c(1:(4*3),13,rep(14,3)), byrow=TRUE, ncol=4,nrow=4)
layout(mat = layout.matrix,
       heights = c(2,2,2,1), # Heights of the rows
       widths = c(1.5,rep(3,3))) # Widths of the columns
min_reef <- NULL
max_reef <- NULL


#Legend
par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0),
    family = "serif", font = 1, font.main = 1, cex.lab = 2, cex.axis = 2, 
    cex.main = 2)


plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)
# text(-1,1,"Bahamas", xjust = 0, line = -0.5, cex.main = 1.5)

legend(-1,0.9, c(unique(as.character(Bahamasall$Reef))), 
       pt.bg=unique(Bahamasall$colour), horiz=F, cex=1.5,bty = "n", text.width = 0.4, 
       pch=22, pt.cex = 3,xjust = 0,title = "Haiti", title.adj = 0.5,adj = 0,
       y.intersp=0.7,x.intersp=0.4) 




for(i in c(5,3,6)){
  
  var=Bahamasall[,i]
  Dist=as.numeric(Bahamasall$Distance)
  
  for(Reef in unique(Bahamasall$Reef)){
    x=Bahamasall[,i][Bahamasall$Reef == Reef]
    Distance = Bahamasall$Distance[Bahamasall$Reef == Reef]
    Reef=Reef
    
    #Gam lines to get ylims
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    min_reef[Reef]=min(pred.resp$pred, na.rm=T)
    max_reef[Reef]=max(pred.resp$pred, na.rm=T)
    
    
  }
  # ymin=min(min_reef, na.rm=T)
  # ymax=max(max_reef, na.rm=T)
  
  ymin = ylims[i,1]
  ymax = ylims[i,2]
  at = seq(ylims[i,3], ylims[i,4], by = ylims[i,5])
  
  par(mar = c(0, 4.5, 2, 0.5),mgp=c(2.8, 0.7, 0))
  
  plot(Dist,y=var,ylim=c(ymin,ymax),type='n',col="white", xlab = "", main = paste(names(Bahamasall[i])), 
       ylab = ylabs[i,1],  xaxt="n",yaxt="n",xlim= c(0,12), cex.lab = 1.5, cex.axis = 1.5,frame.plot=F)
  
  axis(2, pos = 0,at = c(ymin,ymax), las = 2,cex.axis = 1.5,lwd.tick=0,labels=FALSE)
  axis(2, pos = 0,at = at, las = 2,cex.axis = 1.5,lwd = 0, lwd.tick = 1)
  axis(1, pos = ymin, at = seq(0,14,2), cex.axis = 1.5,labels=FALSE,lwd.tick = 1)
  # axis(3, at = seq(0,14,2), lwd.tick=0, labels=FALSE)
  # axis(4, pos = 12.5, at = c(ymin,ymax),lwd.tick=0, labels=FALSE)
  
  
  for(Reef in unique(Bahamasall$Reef)){
    x=Bahamasall[,i][Bahamasall$Reef == Reef]
    Distance = Bahamasall$Distance[Bahamasall$Reef == Reef]
    reefcols = Bahamasall$colour[Bahamasall$Reef == Reef]
    
    #Linear model
    mod.lm <- lm(x ~ Distance, na.action = "na.omit")
    
    #Gam lines
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    
    #Average points
    means=data.frame(cbind(x, Distance))
    means=aggregate(means[,1], list(means$Distance), mean, na.rm=T)    
    
    if( summary(gam1)$s.table[4] > 0.05 | AIC(gam1) > AIC(mod.lm) ) {
      lines(pred.resp$pred~pred.resp$Distance, data=pred.resp,lwd=1, col = alpha(reefcols, 0.2))
    }
    else {
      lines(pred.resp$pred~pred.resp$Distance, data=pred.resp,lwd=3, col = reefcols)
    }
  }
}


# Haiti GAMs ---------------------------------------------------------------

#Legend
par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)
# text(-1,1,"Haiti", xjust = 0, line = -0.5, cex.main = 1.5)

legend(-1,0.9, c(unique(as.character(Haitiall$Reef))), 
       pt.bg=unique(Haitiall$colour), horiz=F, cex=1.5,bty = "n", text.width = 0.4, 
       pch=22, pt.cex = 3,xjust = 0,title = "Haiti", title.adj = 0.5,adj = 0,
       y.intersp=0.7,x.intersp=0.4) 


options(na.action = na.omit)
for(i in c(5,3,6)){
  
  var=Haitiall[,i]
  Dist=as.numeric(Haitiall$Distance)
  
  for(Reef in unique(Haitiall$Reef)){
    x=Haitiall[,i][Haitiall$Reef == Reef]
    Distance = Haitiall$Distance[Haitiall$Reef == Reef]
    Reef=Reef
    
    #Gam lines to get ylims
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    min_reef[Reef]=min(pred.resp$pred, na.rm=T)
    max_reef[Reef]=max(pred.resp$pred, na.rm=T)
    
    
  }
  # ymin=min(min_reef, na.rm=T)
  # ymax=max(max_reef, na.rm=T)
  
  ymin = ylims[i,1]
  ymax = ylims[i,2]
  at = seq(ylims[i,3], ylims[i,4], by = ylims[i,5])
  
  par(mar = c(0, 4.5, 2, 0.5),mgp=c(2.8, 0.7, 0))
  
  plot(Dist,y=var,ylim=c(ymin,ymax),type='n',col="white", xlab = "", main = NA, ylab = ylabs[i,1], 
       xaxt="n",yaxt="n",xlim= c(0,12), cex.lab = 1.5, cex.axis = 1.5,frame.plot=F)
  
  axis(2, pos = 0,at = c(ymin,ymax), las = 2,cex.axis = 1.5,lwd.tick=0,labels=FALSE)
  axis(2, pos = 0,at = at, las = 2,cex.axis = 1.5,lwd = 0, lwd.tick = 1)
  axis(1, pos = ymin, at = seq(0,14,2), cex.axis = 1.5,labels=FALSE,lwd.tick = 1)
  # axis(3, at = seq(0,14,2), lwd.tick=0, labels=FALSE)
  # axis(4, pos = 12.5, at = c(ymin,ymax),lwd.tick=0, labels=FALSE)
  
  
  for(Reef in unique(Haitiall$Reef)){
    x=Haitiall[,i][Haitiall$Reef == Reef]
    Distance = Haitiall$Distance[Haitiall$Reef == Reef]
    reefcols = Haitiall$colour[Haitiall$Reef == Reef]
    
    #Linear model
    mod.lm <- lm(x ~ Distance, na.action = "na.omit")
    
    #Gam lines
    gam1 = gam(x~s(Distance, k = ns, bs="cr"), na.action = "na.omit")
    
    val.for.pred <- data.frame(Distance=seq(min(Distance),max(Distance),length.out=length(Distance)))
    pred <- predict(gam1, val.for.pred, type="response") ## map coefs to fitted curves
    pred.resp = data.frame(val.for.pred, pred)
    
    #Average points
    means=data.frame(cbind(x, Distance))
    means=aggregate(means[,1], list(means$Distance), mean, na.rm=T)    
    
    if( summary(gam1)$s.table[4] > 0.05 | AIC(gam1) > AIC(mod.lm) ) {
      lines(pred.resp$pred~pred.resp$Distance, data=pred.resp,lwd=1, col = alpha(reefcols, 0.2))
    }
    else {
      lines(pred.resp$pred~pred.resp$Distance, data=pred.resp,lwd=3, col = reefcols)
    }
  }
}




# Threshold fig -----------------------------------------------------------
par(mar = c(2, 2, 2, 1),mgp=c(3, 1, 0))

plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
meanthresh <- meanthresh[order(factor(meanthresh$Reef, levels = factor(c(reef.order)))),]

meanthresh<-meanthresh[order(factor(meanthresh$Reef,levels=rev(c("MOW1","MOW2","MOW3","LH1","LH3","LH2",
                                                                 "H5","H8","H3","H4","H2","H6","H1")))),]

sdthresh<-sdthresh[order(factor(sdthresh$Reef,levels=rev(c("MOW1","MOW2","MOW3","LH1","LH3","LH2",
                                                           "H5","H8","H3","H4","H2","H6","H1")))),]

Haiti.thresh= subset(meanthresh, meanthresh$Island == "1")
Bahamas.thresh= subset(meanthresh, meanthresh$Island == "2")

#getting alpha for non-significant GAMs
alfalfa <- function(x) {
  if (x == "NaN") return(0.2) 
  if(x > 0.2) return(1) 
}

#Dotplot
for(i in c(8,6,9)){
  
  var=meanthresh[,i]
  Reef.pos =1:13
  Reef = unique(meanthresh$Reef)
  Reef2= factor(Reef, levels = c(reef.order[,1]))
  alpha <- sapply(var,alfalfa)
  var[is.na(var)] <- 0.3
  sd=sdthresh[,i]
  ci=qt(0.975,df=boot-1)*sd/sqrt(boot)
  ci[is.na(ci)] <- 0
  
  line.haiti <- mean(Haiti.thresh[,i], na.rm = T)
  line.bahamas <- mean(Bahamas.thresh[,i], na.rm = T)
  
  colour = unique(meanthresh$colour)
  par(mar = c(2, 4.5, 2, 0.5))
  
  plot(0, 0, type='n', xlim= c(0,12), ylim = c(1,13), yaxt = "n", xaxt = "n",
       ylab = "", xlab="",frame.plot=F)
  
  segments(var-ci , Reef.pos,
           var+ci , Reef.pos,
           col = alpha(colour, alpha),lwd = 2, lty = 1.2 )
  arrows(x0=line.bahamas, y0=7, x1=line.bahamas, y1=13, code=2, col="#6A51A3", lwd=1, 
         length = 0, lty = 2)
  arrows(x0=line.haiti, y0=1, x1=line.haiti, y1=6, code=2, col="#238B45", lwd=1, 
         length = 0, lty = 2)
  points(var, Reef.pos, pch = 16, col = alpha(colour, alpha),lwd = 1, cex = 2.2)
  if(names(meanthresh[i]) == "Seagrass.N_mean"){
    axis(2, at=c(0:14), labels=c("",as.character(Reef),""),pos = 0,las = 2, cex.axis = 1)
  }else{
    axis(2, at=c(0:14), labels=FALSE,pos = 0,las = 2, cex.axis = 1)
  }
  axis(1, pos = 0.5, at = seq(0,14,2), cex.axis = 1.5)
  # axis(3, pos = 13.5,at = seq(0,14,2), lwd.tick=0, labels=FALSE)
  # axis(4, pos = 12.5, at = c(0:14),lwd.tick=0, labels=FALSE)
}


par(mar = c(2, 2, 1.5, 1))
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
plot(0, 0, type='n',bty='n', main = "Distance (m)", xaxt='n', yaxt='n', ylab = "", cex.main=1.5)

#9 x 7

# Supplemental thresholds and slope per island -------------------------------------------

#####Threshold per island figures ------------------------------------------------
par(mar = c(2, 5, 1.5, 1))

par(mfrow = c(2,2))


boxplot(Shoot.growth ~ Island, data = thresh.mean, col = "white", border = "white",ylim = c(1.5,4.5),
        ylab = "Threshold (m)", xlab = NA, main = "Shoot growth", las = 2, xaxt = "n")
axis(at = c(1,2),1, labels = c("Bahamas", "Haiti"))


# Add data points
mylevels <- levels(thresh.mean$Island)
levelProportions <- summary(thresh.mean$Island)/nrow(thresh.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- thresh.mean[thresh.mean$Island==thislevel, "Shoot.growth"]
  color = thresh.mean$colour[thresh.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(7)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 2.2) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=1, angle = 90, length = 0.1)
  points(i, ymean , pch = 23, bg = "black", cex = 3)
  
}

boxplot(Seagrass.P ~ Island, data = thresh.mean, col = "white", border = "white",ylim = c(1.5,4.5),
        ylab = "Threshold (m)", xlab = NA, main = "Seagrass %P", las = 2, xaxt = "n")
axis(at = c(1,2),1, labels = c("Bahamas", "Haiti"))

# Add data points
mylevels <- levels(thresh.mean$Island)
levelProportions <- summary(thresh.mean$Island)/nrow(thresh.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- thresh.mean[thresh.mean$Island==thislevel, "Seagrass.P"]
  color = thresh.mean$colour[thresh.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(7)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 2.2) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=1, angle = 90, length = 0.1)
  points(i, ymean , pch = 23, bg = "black", cex = 3)
  
}


#####Slope per island figures ------------------------------------------------


boxplot(Shoot.growth ~ Island, data = slope.mean, col = "white", border = "white", ylim = c(-65,0),
        ylab = "Slope", xlab = NA, main = "Shoot growth", las = 2, xaxt = "n")
axis(at = c(1,2),1, labels = c("Bahamas", "Haiti"))

# Add data points
mylevels <- levels(slope.mean$Island)
levelProportions <- summary(slope.mean$Island)/nrow(slope.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- slope.mean[slope.mean$Island==thislevel, "Shoot.growth"]
  color = slope.mean$colour[slope.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(7)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 2.2) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=1, angle = 90, length = 0.1)
  points(i, ymean , pch = 23, bg = "black", cex = 3)
  
}

boxplot(Seagrass.P ~ Island, data = slope.mean, col = "white", border = "white",ylim = c(-0.040,0),
        ylab = "Slope", xlab = NA, main = "Seagrass %P", las = 2, xaxt = "n")
axis(at = c(1,2),1, labels = c("Bahamas", "Haiti"))

# Add data points
mylevels <- levels(slope.mean$Island)
levelProportions <- summary(slope.mean$Island)/nrow(slope.mean)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- slope.mean[slope.mean$Island==thislevel, "Seagrass.P"]
  color = slope.mean$colour[slope.mean$Island==thislevel]
  # take the x-axis indices and add a jitter, proportional to the N in each level
  set.seed(8)
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/4)
  ymean = mean(thisvalues, na.rm = T)
  sd = sd(thisvalues, na.rm = T)
  ci = CI(na.omit(thisvalues), ci = 0.95)
  points(myjitter, thisvalues, pch=20, col=color, cex = 2.2) 
  arrows(x0=i, y0=ymean-sd, x1=i, y1=ymean+sd, code=3, col="black", lwd=1, angle = 90, length = 0.1)
  points(i, ymean , pch = 23, bg = "black", cex = 3)
  
}


# Choosing K --------------------------------------------------------------
#this is the 'k' parameter in the gam
ns <- 2

all.reefs.edf <- list()
all.reef.k.index <- list()
all.reef.p <- list()
all.reef.AIC <- list()


for(ar in 1 : length(uniq.reefs) ) {
  #ar = 1
  # print(uniq.reefs[ar])
  
  edf.mat <- matrix(nrow = 3, ncol = length(params.of.interest)+2, dimnames = list(NULL, c("reef","k", params.of.interest)))
  edf.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), 3)
  edf.mat[,2] <- seq(3,5,1)
  
  k.index.mat <- matrix(nrow = 3, ncol = length(params.of.interest)+2, dimnames = list(NULL, c("reef","k", params.of.interest)))
  k.index.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), 3)
  k.index.mat[,2] <- seq(3,5,1)
  
  p.mat <- matrix(nrow = 3, ncol = length(params.of.interest)+2, dimnames = list(NULL, c("reef","k", params.of.interest)))
  p.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), 3)
  p.mat[,2] <- seq(3,5,1)
  
  AIC.mat <- matrix(nrow = 3, ncol = length(params.of.interest)+2, dimnames = list(NULL, c("reef","k", params.of.interest)))
  AIC.mat[,1] <- rep(uniq.reefs[ar], length(uniq.reefs), 3)
  AIC.mat[,2] <- seq(3,5,1)
  
  # par(mfrow = c(1,length(params.of.interest)), mar = c(2,2,2,2))
  
  #gam.predicts for reef-level data - for MS figure
  length.x.predict <- 100 # this sets the length of the vector to predit from the gams
  gam.predict <- matrix( ncol = length(params.of.interest), nrow = length.x.predict, dimnames = list(NULL, params.of.interest))
  r2_pval.gam <- matrix( nrow = length(params.of.interest), ncol = 2, dimnames = list(params.of.interest, c("r2", "p-val")) )
  
  
  for (i in 1: length(params.of.interest) ) {
    #i = 1
    #match the color with the island
    
    now <- na.omit ( data[data$Reef == uniq.reefs[ar],c( "Distance", params.of.interest[i])] )
    x = now[,1]
    y = now[,2]
    # print(ar)
    # print(i)
    
    
    for (j in 1:3){
      mod <-  gam(y ~ s(x, k=ns+j, bs = "cr"), na.action = "na.omit")
      val.for.pred <- data.frame(x=seq(min(x), max(x), length.out=length.x.predict ))
      
      par(mfrow = c(3,2))
      print(ns+j)
      
      if(ns+j == 3){
        print(gam.check(mod,pch=19,cex=1))
        plot(mod,residuals=TRUE,pch=19,cex=1)
        
      }
      par(mfrow = c(3,2))
      
      check = capture.output(gam.check(mod,pch=19,cex=.3))
      plot(mod,residuals=TRUE,pch=19,cex=1)
      
      check1 <- as.character(check[12])
      `check2` <- strsplit(check1, " ")
      check3 = na.omit(as.vector(as.numeric(unlist(check2))))
      
      mod.lm <- lm(y ~ x, na.action = "na.omit")
      
      if(
        summary(mod)$s.table[4] > 0.05 |
        AIC(mod) > AIC(mod.lm) ) { 
        edf.mat[j,i+2] <- NA
        k.index.mat[j,i+2] <- NA
        p.mat[j,i+2] <- NA
        AIC.mat[j,i+2] <- NA
        
      } else {
        
        edf.mat[j,i+2] <- check3[2]
        k.index.mat[j,i+2] <- check3[3]
        p.mat[j,i+2] <- check3[4]
        AIC.mat[j,i+2] <- AIC(mod)
      }
      
      
    } # end 'j' gam models with different k
    
    #export data for manuscript figure - these are the gam predicts for the all the reef-level data
    #sample with replacement from the data
    
  } # end params loop "i"
  
  # dev.off()
  
  all.reefs.edf[[ar]] <- data.frame(edf.mat)
  all.reef.k.index[[ar]] <- data.frame(k.index.mat)
  all.reef.p[[ar]] <- data.frame(p.mat)
  all.reef.AIC[[ar]] <- data.frame(AIC.mat)
  
  
}#end "ar" loop  

edf.compare = do.call("rbind", all.reefs.edf)
k.index.compare = do.call("rbind", all.reef.k.index)
p.compare = do.call("rbind", all.reef.p)
AIC.compare = do.call("rbind", all.reef.AIC)


# K-index summary ---------------------------------------------------------

k.index.compare2<-reshape2::melt(data = k.index.compare, id.vars = c("reef", "k"), 
                                 measure.vars = c(colnames(k.index.compare[3:length(k.index.compare)])))

k.index.compare3 = data.frame(k.index.compare2 %>% 
                                group_by(reef, k, variable) %>% 
                                filter(any(!is.na(value))))

k3 = (sum(subset(k.index.compare3, k.index.compare3$k == 3)$value <1)/
        sum(subset(k.index.compare3, k.index.compare3$k == 3)$value > 0))*100

k4 = (sum(subset(k.index.compare3, k.index.compare3$k == 4)$value <1)/
        sum(subset(k.index.compare3, k.index.compare3$k == 4)$value > 0))*100

k5 = (sum(subset(k.index.compare3, k.index.compare3$k == 5)$value <1)/
        sum(subset(k.index.compare3, k.index.compare3$k == 5)$value > 0))*100

for (i in 3:length(k.index.compare)){
  k.index.lm = aov(k.index.compare[,i] ~ k, data = k.index.compare[c(1:2,i)])
  print(anova(k.index.lm))
  print(TukeyHSD(x=k.index.lm, conf.level=0.95))
}

#Results: 4-3 Seagrass %P is different
#Results: 5-3 Seagrass %P is different


# P-value summary ---------------------------------------------------------

p.compare2<-reshape2::melt(data = p.compare, id.vars = c("reef", "k"), 
                           measure.vars = c(colnames(p.compare[3:length(p.compare)])))

p.compare3 = data.frame(p.compare2 %>% group_by(reef, k, variable) %>% filter(any(!is.na(value))))

p3 = (sum(subset(p.compare3, p.compare3$k == 3)$value <0.1)/
        sum(subset(p.compare3, p.compare3$k == 3)$value > 0))*100

p4 = (sum(subset(p.compare3, p.compare3$k == 4)$value <0.1)/
        sum(subset(p.compare3, p.compare3$k == 4)$value > 0))*100

p5 = (sum(subset(p.compare3, p.compare3$k == 5)$value <0.1)/
        sum(subset(p.compare3, p.compare3$k == 5)$value > 0))*100


# EDF summary -------------------------------------------------------------
edf.compare2<-reshape2::melt(data = edf.compare, id.vars = c("reef", "k"), 
                             measure.vars = c(colnames(edf.compare[3:length(edf.compare)])))

edf.compare3 = edf.compare2 %>% group_by(reef, k, variable) %>% filter(any(!is.na(value)))

edf3 = (as.numeric(subset(k.index.compare3, k.index.compare3$k == 3)$value)/2)*100

edf.compare4 <- edf.compare3 %>% 
  group_by(reef, variable, k) %>%
  mutate(k. = as.numeric(k)-1,
         percent = as.numeric(value)/k.)

edf3 = round(mean(subset(edf.compare4, edf.compare4$k == 3)$percent),2)
edf4 = round(mean(subset(edf.compare4, edf.compare4$k == 4)$percent),2)
edf5 = round(mean(subset(edf.compare4, edf.compare4$k == 5)$percent),2)


# AIC summary -------------------------------------------------------------

AIC.compare2<-reshape2::melt(data = AIC.compare, id.vars = c("reef", "k"), 
                             measure.vars = c(colnames(AIC.compare[3:length(AIC.compare)])))

AIC.compare3 = data.frame(AIC.compare2 %>% 
                            group_by(reef, k, variable) %>% 
                            filter(any(!is.na(value))))

str(AIC.compare3)
AIC.compare3$value <- as.numeric(AIC.compare3$value)

AIC.compare3.min <- AIC.compare3 %>% 
  dplyr::group_by(reef, variable) %>%
  dplyr::slice_min(value, n = 1)

AIC.compare3.max <- AIC.compare3 %>% 
  dplyr::group_by(reef, variable) %>%
  dplyr::slice_max(value, n = 1)

AIC.compare4 <- rbind(AIC.compare3.min, AIC.compare3.max)

AIC.compare5 <- AIC.compare4 %>% 
  group_by(reef, variable) %>%
  dplyr::summarise(diffi = round(diff(as.numeric(value)), 1))

#delta AC with the best model k
AIC.compare.fin = merge(AIC.compare5, AIC.compare4, by=c("reef", "variable"))

# Visualisation  ---------------------------------------
#Data
p.compare3$color <- sapply(p.compare3$reef,colorfunc)
p.compare3$value <- as.numeric(p.compare3$value)
p.compare3$k <- as.factor(p.compare3$k)
k.index.compare3$color <- sapply(k.index.compare3$reef,colorfunc)
k.index.compare3$value <- as.numeric(k.index.compare3$value)
k.index.compare3$k <- as.factor(k.index.compare3$k)


#Figure
layout.matrix=matrix(c(1:18,rep(19,9)), byrow=T, ncol=9,nrow=3)
layout(mat = layout.matrix,
       heights = c(2,2,0.5), # Heights of the rows
       widths = c(1,rep(3,7),0.5)) # Widths of the columns
plot.names<- c("Shoot growth", "C production", "SLA", "Seagrass %P", "Seagrass %N","LAI", "Seagrass.C13")

#Empty plot
par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)

par(mar = c(0, 2, 2, 0),mgp=c(3, 1, 0))

mylevels <- levels(p.compare3$k)
levelProportions <- summary(p.compare3$k)/nrow(p.compare3)

for (i in c(1,2,4,5,3,6,7)){
  param <- params.of.interest[i]
  main = plot.names[i]
  print(param)
  data = subset(p.compare3, p.compare3$variable == param)
  if(param == "Shoot.growth"){
    boxplot(value ~ k, data = data,ylim = c(0,1), col = c("grey80", "grey50", "grey30"),
            ylab = "p-value", xlab = NA, xaxt = "n",yaxt = "n",las = 1, cex.axis = 1.5, cex.lab = 1.5,
            main = main)
    axis(2, at = seq(0,1,0.25), las = 1,cex.axis = 1.5)
    axis(1, at = c(1,2,3), las = 2,cex.axis = 1.5, labels = NA)
    
  }
  
  else{
    boxplot(value ~ k, data = data,ylim = c(0,1), col = c("grey80", "grey50", "grey30"),
            ylab = NA, xlab = NA, xaxt = "n",yaxt = "n",las = 1, cex.axis = 1.5, cex.lab = 1.5,
            main = main)
    axis(2, at = seq(0,1,0.25), las = 2,cex.axis = 1.5, labels = NA)
    axis(1, at = c(1,2,3), las = 2,cex.axis = 1.5, labels = NA)
    
  }
  for(j in 1:length(mylevels)){
    thislevel <- mylevels[j]
    thisvalues <- data[data$k==thislevel,"value"]
    color = data$color[data$k==thislevel]
    set.seed(7)
    myjitter <- jitter(rep(j, length(thisvalues)), amount=levelProportions[j]/4)
    points(myjitter, thisvalues, pch=20, col=color, cex = 2.2) 
  }
}

#Empty plots
par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)

par(mar = c(0, 2, 1, 0),mgp=c(3, 1, 0))

mylevels <- levels(k.index.compare3$k)
levelProportions <- summary(k.index.compare3$k)/nrow(k.index.compare3)

for (i in c(1,2,4,5,3,6,7)){
  param <- params.of.interest[i]
  print(param)
  data = subset(k.index.compare3, k.index.compare3$variable == param)
  if(param == "Shoot.growth"){
    boxplot(value ~ k, data = data,ylim = c(0.5,1.65), col = c("grey80", "grey50", "grey30"),
            ylab = "k-index",yaxt = "n", xlab = NA, las = 1, cex.axis = 1.5, cex.lab = 1.5)
    axis(2, at = seq(0.5,1.5,0.25), las = 1,cex.axis = 1.5)
    axis(1, at = c(1,2,3), las = 2,cex.axis = 1.5, labels = NA)
    
    
  }
  else{
    boxplot(value ~ k, data = data,ylim = c(0.5,1.65), col = c("grey80", "grey50", "grey30"),
            ylab = NA, xlab = NA,yaxt = "n",las = 1, cex.axis = 1.5, cex.lab = 1.5)
    axis(2, at = seq(0.5,1.5,0.25), las = 2,cex.axis = 1.5, labels = NA)
    axis(1, at = c(1,2,3), las = 2,cex.axis = 1.5, labels = NA)
    
  }
  for(j in 1:length(mylevels)){
    thislevel <- mylevels[j]
    thisvalues <- data[data$k==thislevel,"value"]
    color = data$color[data$k==thislevel]
    set.seed(7)
    myjitter <- jitter(rep(j, length(thisvalues)), amount=levelProportions[j]/4)
    points(myjitter, thisvalues, pch=20, col=color, cex = 2.2) 
  }
  
}

par(mar = c(0, 0,2,0),mgp=c(2.8, 0.7, 0))
plot(0, 0, type='n', bty='n'
     , xaxt='n', yaxt='n'
)
par(mar = c(0, 4.5, 1, 1),mgp=c(3, 1, 0))

#15x5
