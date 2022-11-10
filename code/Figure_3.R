
## CODE RUNS BUT NEED TO ADD IN SEAGRASS %N and maybe one more?
## ALSO 

rm(list = ls())
library(gamm4)
library(mgcv)
library(MuMIn)
library(chngpt)
library("MASS") ## for mvrnorm
library(data.table)
library(RColorBrewer)

#Load and organize data####

#predictors <- read.csv("input/Predictors_for_threshold_slope.csv",
#                 stringsAsFactors = FALSE, sep = ",")
#predictors <- predictors[predictors$Reef != "ES1",]
resp <- read.csv("input/Simulation_dataset_Jan2022.csv",
                 stringsAsFactors = FALSE, sep = ",")
resp <- resp[resp$Reef != "ES1",]

#organize and set reef order
ir <- data.frame( Island = tapply(resp$Island,resp$Reef,  unique),
                  Reef = names( tapply(resp$Island,resp$Reef,  unique) ) )

nut.reef.ord <- rev(c("LH2", "LH3", "LH1", "MOW3", "MOW2", "MOW1", "H1", "H6", "H2", "H4", "H3", "H8", "H5"))
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

reef.order <- data.frame( Reef = nut.reef.ord)

reef.order$colores <- sapply(reef.order$Reef,colorfunc)


#get ya predictors straight
#preds <- names(predictors)[-1]
#pred <- merge(predictors,ir)
#pred <- pred[ reef.order$Reef, c("Island","Reef",preds)]

#get ya responses straight
params <- c("Shoot.growth", "C.production", "SLA", "LAI", "Seagrass.C", 
            "Seagrass.N", "Seagrass.P","Shoot.density","Seagrass.C13", "Bites.area", "Epiphytes.area")
params.sd <- paste( params, ".sd", sep ="" )

# param.names <-  c("Shoot growth", 
#                   "C Production", 
#                   "SLA", 
#                   "LAI", 
#                   #"Seagrass %N", 
#                   "Seagrass %P", 
#                   "Seagrass %C", 
#                   "Shoot Density", 
#                   expression(delta^13~C),
#                   "Herbivory",
#                   "Epiphytes")

#fancy labels
param.names <-  c(expression(paste("S. Growth ", "(mm"^{2}, " shoot"^{-1},")")), #"Shoot growth (mm2/shoot)",
                  expression(paste("C Prod. ", "(g C ","m"^{-2}," day"^{-1},")")),#"C Production", 
                  expression(paste("SLA ", "(mm"^{2},"/mg)")),# "SLA (mm-2)/ shoot mass (mg)) ", "
                  expression(paste("LAI ", "(m"^{2}," m"^{-2},")")),#LAI shoot area (mm-2) × shoot density (m-2) ", 
                  "Seagrass %C", #
                  "Seagrass %N", 
                  "Seagrass %P", 
                  expression(paste("S. density ", "(m"^{-2},")")), #"Shoot Density", 
                  expression(delta^13*"C"),
                  expression(paste("Herbivory ", "(bites mm"^{-2},")") ), # "Bites")
                  expression(paste("Epiphytes ", "(mm"^{-2},")") ) ) #epiphytes



#condense the pred data
#isolate Distance 1
mmm <- data.table( resp [ resp$Distance == 1, ] )
resp.d1 <- data.frame( mmm [ , list( mean(Shoot.growth,na.rm = T) , sd(Shoot.growth,na.rm = T), 
                                   mean(C.production,na.rm = T) , sd(C.production,na.rm = T),
                                   mean(SLA,na.rm = T) , sd(SLA,na.rm = T),
                                   mean(LAI,na.rm = T) , sd(LAI,na.rm = T),
                                   mean(Seagrass.C,na.rm = T) , sd(Seagrass.C,na.rm = T),
                                   mean(Seagrass.N,na.rm = T) , sd(Seagrass.N,na.rm = T),
                                   mean(Seagrass.P,na.rm = T) , sd(Seagrass.P,na.rm = T),
                                   mean(Shoot.density,na.rm = T) , sd(Shoot.density,na.rm = T),
                                   mean(Seagrass.C13,na.rm = T) , sd(Seagrass.C13,na.rm = T),
                                   mean(Bites.area,na.rm = T) , sd(Bites.area,na.rm = T),
                                   mean(Epiphytes.area,na.rm = T) , sd(Epiphytes.area,na.rm = T)
                                   
                                   ), 
                           by = list(Island,Reef,Distance) ])
colnames(resp.d1)[c( seq(4,24,2))] <-  params
colnames(resp.d1)[c( seq(5,25,2))] <-  params.sd
#resp.d1 <- resp.d1[ reef.order$Reef,]

#isolate distance 12 adn 20 and average
mmm <- data.table( resp[resp$Distance == "20" , ]) #| resp$Distance == "12"
resp.d1220 <- data.frame( mmm [ , list( mean(Shoot.growth,na.rm = T) , sd(Shoot.growth,na.rm = T), 
                                        mean(C.production,na.rm = T) , sd(C.production,na.rm = T),
                                        mean(SLA,na.rm = T) , sd(SLA,na.rm = T),
                                        mean(LAI,na.rm = T) , sd(LAI,na.rm = T),
                                        mean(Seagrass.C,na.rm = T) , sd(Seagrass.C,na.rm = T),
                                        mean(Seagrass.N,na.rm = T) , sd(Seagrass.N,na.rm = T),
                                        mean(Seagrass.P,na.rm = T) , sd(Seagrass.P,na.rm = T),
                                        mean(Shoot.density,na.rm = T) , sd(Shoot.density,na.rm = T),
                                        mean(Seagrass.C13,na.rm = T) , sd(Seagrass.C13,na.rm = T),
                                        mean(Bites.area,na.rm = T) , sd(Bites.area,na.rm = T),
                                        mean(Epiphytes.area,na.rm = T) , sd(Epiphytes.area,na.rm = T)
                                        
), 
by = list(Island,Reef,Distance) ])


#H4 has shit data for distance 20 so using 12
m4 <- data.table( resp[resp$Distance == "20" & resp$Reef == "H4"|resp$Distance == "12" & resp$Reef == "H4" , ]) #| resp$Distance == "12"
resp.h4 <- data.frame( m4 [ , list( mean(Shoot.growth,na.rm = T) , sd(Shoot.growth,na.rm = T), 
                                        mean(C.production,na.rm = T) , sd(C.production,na.rm = T),
                                        mean(SLA,na.rm = T) , sd(SLA,na.rm = T),
                                        mean(LAI,na.rm = T) , sd(LAI,na.rm = T),
                                        mean(Seagrass.C,na.rm = T) , sd(Seagrass.C,na.rm = T),
                                        mean(Seagrass.N,na.rm = T) , sd(Seagrass.N,na.rm = T),
                                        mean(Seagrass.P,na.rm = T) , sd(Seagrass.P,na.rm = T),
                                        mean(Shoot.density,na.rm = T) , sd(Shoot.density,na.rm = T),
                                        mean(Seagrass.C13,na.rm = T) , sd(Seagrass.C13,na.rm = T),    
                                        mean(Bites.area,na.rm = T) , sd(Bites.area,na.rm = T),
                                        mean(Epiphytes.area,na.rm = T) , sd(Epiphytes.area,na.rm = T)
                                        
), 
by = list(Island, Reef) ])

#resp.h4$Distance <- rep("12.20",length(resp.h4[,1]))
#resp.h4 <- resp.h4[ , c( 1,2, 19, 3:18 )] #get the order back
#colnames(resp.h4)[c( seq(4,18,2))] <-  params
#colnames(resp.h4)[c( seq(5,19,2))] <- params.sd

resp.d1220$Distance <- rep("12.20",length(resp.d1220[,1]))
#resp.d1220 <- resp.d1220[ , c( 1,2, 21, 3:20 )] #get distance back in the right order
colnames(resp.d1220)[c( seq(4,24,2))] <-  params
colnames(resp.d1220)[c( seq(5,25,2))] <- params.sd

resp.d1220[which(resp.d1220$Reef == "H4"),4:19] <- resp.h4[,3:18]

#order reefs by impact

resp.d1 <- resp.d1[order(factor(resp.d1$Reef, levels = nut.reef.ord)),]
resp.d1220 <- resp.d1220[order(factor(resp.d1220$Reef, levels = nut.reef.ord)),]
# ####

#Set y-values for legends
xleg = data.frame(matrix(nrow = 10))
xleg[2,1] = 0.8
xleg[3,1] = 24
xleg[4,1] = 2.5
xleg[5,1] = 38
xleg[6,1] = 2
xleg[7,1] = 0.16
xleg[8,1] = 100
xleg[9,1] = -11
xleg[10,1] = 0.01
xleg[11,1] = 0.03

slipnum <- round(length(params)  /2 )                         
quartz(width = 12, height = 7)
par(mfrow = c(2,slipnum), oma = c(1,5,1,1),
    mar = c(4,2,2,1),
    family = "serif")

#for(g in 1:2){
#  g=1
#  if(g==1) {
    data <- rbind( resp.d1, resp.d1220 )
    
    for( i in 1:length(params) ) { #the different responses
      #i=1
      # x.min <- min( data[, params[i]] )
      x.min <-min(c(data[, params[i]],data[, params[i]] - data[,params.sd[i]]), na.rm = T)
      # if(i != 9) {x.max <- max( data[, params[i]] )*1.1} else {x.max <- max( data[, params[i]] )*.9}
      x.max <-max(c(data[, params[i]],data[, params[i]] + data[,params.sd[i]]), na.rm = T)
      
      xs <- seq( x.min , x.max, (x.max - x.min) / (13) )

      
      plot(xs, 0.5:13.5, 
           ylab = "", 
           xlab = param.names[i], 
           yaxt = "n",
           col = "white",
           las = 1,
           cex.lab = 1.5,
           cex.axis = 1.3)
      
      if(i == 1) { axis(2 ,at = 1:13,tick = TRUE, labels = reef.order$Reef, las = 2, cex.axis = 1.3) 
        legend( x = 100, y = 14, 
                #fill = c ( brewer.pal(7,"Greens")[7], brewer.pal(7,"Purples")[7] ),
                c("1m", "20m"),
                pch = c(16, 17),
                col = c("black","red"),
                text.col = c("black","red"),
                cex = 1.3,
                bty = "n",
                x.intersp = 0.5)
        
        }
      
      if(i == (slipnum +1)) { axis(2 ,at = 1:13,tick = TRUE, labels = reef.order$Reef, las = 2, cex.axis = 1.3) 
      }
      
      #Add the means per 1m and 20 m ####
      tt1 <- t.test(resp.d1[,params[i]] ~resp.d1$Island)
      #Haiti
      segments(tt1$estimate[2], 1,
               tt1$estimate[2], 7,
               col = "black",lwd = 1, lty = 3 )
      #Bahamas
      segments(tt1$estimate[1], 8,
               tt1$estimate[1], 13,
               col = "black",lwd = 1, lty = 3 )
      
      tt20 <- t.test(resp.d1220[,params[i]] ~resp.d1220$Island)
      #Haiti
      segments(tt20$estimate[2], 1,
               tt20$estimate[2], 7,
               col = "red",lwd = 1, lty = 3 )
      #Bahamas
      segments(tt20$estimate[1], 8,
               tt20$estimate[1], 13,
               col = "red",lwd = 1, lty = 3 )
      
      x.legend <- xleg[i,1]
      
      legend(x = x.legend, y = 14, c( ifelse( tt1$p.value <0.05, "**", "NS"), 
                            ifelse( tt20$p.value <0.05, "**", "NS") ),
             text.col = c( "black", "red" ), xjust = 0.5,
             bty = "n", cex = 1.4)
      # ####
      
      segments(resp.d1[,params[i]] + resp.d1[,params.sd[i]], 1:13-.25, 
               resp.d1[,params[i]] - resp.d1[,params.sd[i]], 1:13-.25,
               col = "grey20",lwd = 1.7 )
      segments(resp.d1[,params[i]] + resp.d1[,params.sd[i]], 1:13-.25, 
               resp.d1[,params[i]] - resp.d1[,params.sd[i]], 1:13-.25,
               col = reef.order$colores,lwd = 1.5 )
      points(resp.d1[,params[i]], 1:13-.25, 
             col = "grey20", pch = 16, cex = 2)
      points(resp.d1[,params[i]], 1:13-.25, 
             col = reef.order$colores, pch = 16, cex = 1.8)
      
      segments(resp.d1220[,params[i]] + resp.d1220[,params.sd[i]], 1:13+.25, 
               resp.d1220[,params[i]] - resp.d1220[,params.sd[i]], 1:13+.25, 
               col = "grey20",lwd = 1.7 )
      segments(resp.d1220[,params[i]] + resp.d1220[,params.sd[i]], 1:13+.25, 
               resp.d1220[,params[i]] - resp.d1220[,params.sd[i]], 1:13+.25, 
               col = reef.order$colores,lwd = 1.5 )
      points(resp.d1220[,params[i]], 1:13+.25, 
             col = "grey20", pch = 17, cex = 2)
      points(resp.d1220[,params[i]], 1:13+.25, 
             col = reef.order$colores, pch = 17, cex = 1.8)
      
  
    }
  

# mtext(c("The Bahamas "), side = 3, col = brewer.pal(7,"Purples")[6],#c("black"), 
#       cex = 1.5, line = 5, las = 1 , adj = -120, padj = -17)
# 
# mtext(c("   Haiti   "), side = 3, col = brewer.pal(7,"Greens")[6],#c("black"), 
#       cex = 1.5 , line = 5, adj = -1, padj = -17) 

# mtext(c("-"), side = 1, 
#       cex = 8, col =  brewer.pal(7,"Greens")[6], line = 6.5 , adj = -2.9 ) 
# mtext(c("-"), side = 1, 
#       cex = 8, col =  brewer.pal(7,"Greens")[6], line = 6.75 , adj = -2.9 ) 
# 
# mtext(c("-"), side = 1, 
#       cex = 8, col =  brewer.pal(7,"Purples")[6], line = 6.5 , adj = -1.3 ) 
# mtext(c("-"), side = 1, 
#       cex = 8, col =  brewer.pal(7,"Purples")[6], line = 6.75 , adj = -1.3 ) 


