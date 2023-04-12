#Figure 2
rm(list = ls())
library(vegan)
library(RColorBrewer)

comm <- read.csv("output/AllReefSpAbundPerTL_PerReef_2017+2018.csv",
                 stringsAsFactors = FALSE, sep = ",")[,-1]
comm <- comm[comm$reef != "ES1",]
bsdd <- read.csv("output/AllReefSpBodySizes_PerReef_2017+2018.csv",
                 stringsAsFactors = FALSE, sep = ",")[,-1]
bsdd <- bsdd[bsdd$reef != "ES1",]
biod <- read.csv("output/AllReefBiomassNP_PerReef_2017+2018.csv",
                 stringsAsFactors = FALSE, sep = ",")[,-1]
biod <- biod[biod$reef != "ES1",]
biod$reef <- matrix(unlist(strsplit(as.character(biod$reef.date),"_")), ncol = 2,byrow = TRUE)[,1]

#check I am using the right reefs and dates
tapply(bsdd$date, bsdd$reef, unique)

#organize and set reef order and colors
resp <- read.csv("input/Reef-level_4-2022.csv",
                 stringsAsFactors = FALSE, sep = ",")
ir <- data.frame( island = tapply(resp$Island,resp$Reef,  unique),
                  reef = names( tapply(resp$Island,resp$Reef,  unique) ) )[-1,]
nut.reef.ord <- c("LH2", "LH3", "LH1", "MOW3", "MOW2", "MOW1", "H1", "H6", "H2", "H4", "H3", "H8", "H5")

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

r.ord <- data.frame( reef = nut.reef.ord,
                     island = rev(ir$island))

r.ord$colores <- sapply(r.ord$reef,colorfunc)


h.col <- brewer.pal(9,"Greens")[8]
b.col <- brewer.pal(9,"Purples")[9]
Transparency<- '55' #for polygons down the way if I need it.


#add island to bsdd and biod
bsd <- merge(bsdd, r.ord)
bio <- merge(biod, r.ord)

rightside <- 4
leftsize <- 4
l.cex <- 1.2
mtext.cex = 1
quartz(width = 9, height = 6.5)
par( mfrow = c(3,2),
     mgp = c(3.2,1,0),
     mar = c(4,5.1,1,rightside), 
     family = "serif", 
     oma = c(4,2,0,4),
     cex.lab = 1.5,
     cex.axis = 1.2)
nam <- c("The Bahamas", "Haiti")

###########################
#### size structure  #### 

#first plot for the histograms
#island -wide
b1<-density(bsd[bsd$island == "Bahamas","tl"],adjust=2)
h1<-density(bsd[bsd$island == "Haiti","tl"],adjust=2)

for(isl in 1:2) {
  
  hist(bsd$tl,
       freq = F, 
       col = "white",
       border = "white",
       breaks = 15,
       las = 1,
       xlim = c(0, max(b1$x)*.7), 
       ylim = c(0, max(h1$y)*1.4),
       xlab = "" , 
       main = "" 
  )
  
  mtext("Fish Total Length (cm)", 1, line = 2.4, cex = mtext.cex)
 
  if(isl == 1) { 
    polygon(x=c(b1$x,0),y=c(b1$y,0),col=paste0(b.col,Transparency),border = paste0(b.col,Transparency)) } 
  else { 
    polygon(x=c(h1$x,0),y=c(h1$y,0),col=paste0(h.col,Transparency),border = paste0(h.col,Transparency)) }
  
  curr.reefs <- ( r.ord[ r.ord$island == c("Bahamas","Haiti")[isl], "reef"] )
  
  for (i in 1:length( curr.reefs )) {
    #i = 1 
    now <- density( bsd[bsd$reef == curr.reefs[i], "tl"], adjust = 3 )
   
    lines(now, col = as.character( bsd[bsd$reef == curr.reefs[i], "colores"][1] ), lwd = 2)
  }
  
  legend("right", legend = nam[isl], 
         text.col = ifelse(isl == 1, as.character(b.col),as.character(h.col) ), 
         cex = 1.7, bty = "n" )
  
}

###########################

###########################
#### N and P excretion #### 

par( mar = c(1,4.8,1,rightside), mgp <- c(3,1,5) )

bb <- bio[match(r.ord$reef, bio$reef  ),]
bb$biomass.kg <- bb$biomass/1000
bb$biomass.kg.sd <- bb$biomass.sd/1000
pees <- c("biomass.kg", "np", "n", "p")
pees.names <- c(expression(Fish~Biomass~kg~m^-2), "N:P", expression(N~g~m^-2~day^-1), expression(P~g~m^-2~day^-1))


#Excretion of N #
u = 3

seps <-  .2

dot <- bb[ ,pees[u] ] / 16
ciu <- (bb[ ,pees[u] ] + bb[ ,paste( pees[u], "sd", sep = ".") ]) / 16
cil <- (bb[ ,pees[u] ] - bb[ ,paste( pees[u], "sd", sep = ".") ]) / 16

tt.n <- t.test(dot[1:7] , dot[8:13])

plot( 1:13, dot,
      xlab = "", 
      ylab =  pees.names[u],
      ylim = c(0, max( ciu )*1.05),
      xaxt = "n",
      col = "white",
      las = 1)

axis(1 ,at = 1:13,tick = TRUE, labels = NA, las = 2)#, cex.axis = 1.3) 

segments((1 - seps): (13 - seps), cil,
         (1 - seps): (13 - seps), ciu,
         col = "grey20",lwd = 1.7 )

points( (1 - seps): (13 - seps), dot,
        col = "black", pch = 16, cex = 1.7)
points( (1 - seps): (13 - seps), dot,
        col = as.character( bb$colores ), pch = 16, cex = 1.6)

legend("topleft", 
       legend = c( paste("N", " = ", ifelse( tt.n$p.value <0.05, "**", "NS") )),
       pch = c(16),
       cex = l.cex, bty = "n" )

#P excretion 
u = 4

dot <- bb[ ,pees[u] ] / 16
ciu <- (bb[ ,pees[u] ] + bb[ ,paste( pees[u], "sd", sep = ".") ]) / 16
cil <- (bb[ ,pees[u] ] - bb[ ,paste( pees[u], "sd", sep = ".") ]) / 16

tt.p <- t.test(dot[1:7] , dot[8:13])

plot( 1:13, dot,
      xlab = "", 
      ylab =  pees.names[u],
      ylim = c(0, max( ciu )*1.05),
      xaxt = "n",
      col = "white",
      las = 1)

axis(1 ,at = 1:13,tick = TRUE, labels = NA, las = 2, cex.axis = 1.3) 

tt <- t.test(dot[1:7] , dot[8:13])

legend( "topright",
        c( ifelse( tt$p.value <0.05, "**", "NS") ),
        cex = l.cex,
        bty = "n")

segments((1 + seps): (13 + seps), cil,
         (1 + seps): (13 + seps), ciu,
         col = "grey20",lwd = 1.7 )

points( (1 + seps): (13 + seps), dot,
        col = "black", pch = 15, cex = 1.7)
points( (1 + seps): (13 + seps), dot,
        col = as.character( bb$colores ), pch = 15, cex = 1.6)

legend("topleft", 
       legend = c( paste("P", " = ", ifelse( tt.p$p.value <0.05, "**", "NS") ) ),
       pch = c(15),
       cex = l.cex, bty = "n" )

###########################

###########################
#### reef-level data #### 

rpams <- c("TN", "TP", "Sediment.N", "Sediment.P", "Sediment.C", "Seagrass.P20","Seagrass.N20")
  
resp <- resp[resp$Reef != "ES1",]

par( mar = c(1,4.8,1,leftsize), mgp <- c(3,1,5) , cex.lab = 1.6 )
     #cex.axis = 1.4 )
pees <- c( "Seagrass.N20","Seagrass.P20")
pees.names <- c( "Amb. SG %N", "Amb. SG %P")

###########################

##########################
###### Amb N and Amb P #########

##Amb N##
u = 1

dot <- tapply(resp[,pees[u]], resp$Reef, mean, na.rm=T)[r.ord$reef]#resp[ ,c("Reef",pees[u]) ]
dot.sd <- tapply(resp[,pees[u]], resp$Reef, sd, na.rm=T)[r.ord$reef]
ciu <- dot+dot.sd
cil <- dot-dot.sd

plot( 1:13, dot,
      xlab = "", 
      ylab =  pees.names[u],
      ylim = c(1, max( ciu )*1.05),
      xaxt = "n",
      col = "white",
      las = 1)

axis(1 ,at = 1:13,tick = TRUE, labels = bb$reef, las = 2, cex.axis = 1.3) 
tt.n <- t.test(dot[1:7] , dot[8:13])

segments((1 - seps): (13 - seps), cil,
         (1 - seps): (13 - seps), ciu,
         col = "grey20",lwd = 1.7 )

points( (1 - seps): (13 - seps), dot,
        col = "black", pch = 16, cex = 1.7)
points( (1 - seps): (13 - seps), dot,
        col = as.character( bb$colores ), pch = 16, cex = 1.6)

legend("topleft", 
       legend = c( paste("N", " = ", ifelse( tt.n$p.value <0.05, "**", "NS") )
                    ),
       pch = c(16),
       cex = l.cex, bty = "n" )

##Amb P##
u=2

dot <- tapply(resp[,pees[u]], resp$Reef, mean, na.rm=T)[r.ord$reef]#resp[ ,c("Reef",pees[u]) ]
dot.sd <- tapply(resp[,pees[u]], resp$Reef, sd, na.rm=T)[r.ord$reef]
ciu <- dot+dot.sd
cil <- dot-dot.sd

plot( 1:13, dot,
      xlab = "", 
      ylab =  pees.names[u],
      ylim = c(0.05, max( ciu )*1.05),
      xaxt = "n",
      col = "white",
      las = 1)

tt.p <- t.test(dot[1:7] , dot[8:13])

legend("topleft", 
       legend = c( 
                   paste("P", " = ", ifelse( tt.p$p.value <0.05, "**", "NS") ) ),
       pch = c(15),
       cex = l.cex, bty = "n" )

segments((1 + seps): (13 + seps), cil,
         (1 + seps): (13 + seps), ciu,
         col = "grey20",lwd = 1.7 )

points( (1 + seps): (13 + seps), dot,
        col = "black", pch = 15, cex = 1.7)
points( (1 + seps): (13 + seps), dot,
        col = as.character( bb$colores ), pch = 15, cex = 1.6)
axis(1 ,at = 1:13,tick = TRUE, labels = bb$reef, las = 2, cex.axis = 1.3) 

###########################

mtext("A)", side = 3, cex = 1.1, col =  "black",
      adj = -1.7, padj = -32)
mtext("B)", side = 3, cex = 1.1, col =  "black", 
      adj = -0.25, padj = -32)
mtext("C)", side = 3, cex = 1.1, col =  "black", 
      adj = -1.7,  padj = -16)
mtext("D)", side = 3, cex = 1.1, col =  "black", 
      adj = -0.25, padj = -16)
mtext("E)", side = 3, cex = 1.1, col =  "black", 
      adj = -1.7,  padj = -0)
mtext("F)", side = 3, cex = 1.1, col =  "black", 
      adj = -0.25,  padj = -0)
