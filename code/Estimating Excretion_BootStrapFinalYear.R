#NOTES: there are some pretty epic disparities in H6 from 6/5 to 6/17 this is mostly due to the presnce of 
#2 morays vs 1 and in the second survey I recorded them at >40 , which I changed in the spreadsheet just now to 100. 
#I changed it from 100 to 60

rm(list = ls())
library(msm)
library(base)
library(modeest)
library(reshape)
#source("/Users/jacoboallgeier/My Work Biatch/Bahamas Data Comp/YoMama Reefs/2013 Big Sampling/Analyses/R code/Initial Data Conversion for YoMama.R")

#bring all the surveys in
bh2018 <- read.csv("input/Fish Surveys/AllFishSurveys_Haiti+Bahamas_2018+LH12017.csv",
                   stringsAsFactors = FALSE, sep = ",")[,-1]
b2015.17 <- read.csv("input/Fish Surveys/FishSurveys_Bahamas_2015-16-17.csv",
                     stringsAsFactors = FALSE, sep = ",")
h2015.17 <- read.csv("input/Fish Surveys/FishSurveys_Haiti_2015-16.csv",
                     stringsAsFactors = FALSE, sep = ",")

bh2018[is.na(bh2018)]<-0
b2015.17[is.na(b2015.17)]<-0
h2015.17[is.na(h2015.17)]<-0

names(bh2018)
names(b2015.17)
names(h2015.17)

ge <- rbind(bh2018, 
            b2015.17[,names(bh2018)],
            h2015.17[,names(bh2018)])

`%notin%` <- Negate(`%in%`)
reefs.to.use <- unique(ge$Site)[ which( unique(ge$Site) %notin% c("HCR2-Busted", "HCR2-Clock A", "HCR2-Clock B", "SC1", "SC", "H7") ) ]

#look at the size of this fucking lionfish 40cm. I guess I will believe it, but shit- that was from CAL.
check <- ge[ge$Site == "MOW2" & ge$Species == "lionfish",]

#ge$uniqID = paste(ge$sp.gen,ge$columnID,ge$Reef,ge$Date,sep = "_")
c.ID <- read.csv("input/Fish Surveys/CommonName-ColumnIDConvert.csv", stringsAsFactors = FALSE, sep = ",")
c.ID$sp.gen = paste(c.ID$Genus,c.ID$Species, sep = " ")
c <- c.ID[,c("CommonName", "sp.gen","ColumnID")]
colnames(c) <- c("Species","sp.gen", "columnID")

#these species I am calling transient - I pull these later to see how much they actually contribute
trans.sp <- merge( data.frame( Species = c("bridled burrfish"  , "blue runner", "spotted burrfish", 
                                           "bar jack", "ocean triggerfish", "mutton snapper", 
                                           "horse-eye jack", "jolthead porgy") ),
                   
                   c)

#bring the columnID into the fold
gge <- merge(ge[ge$Site %in% reefs.to.use,], c)

#create the uniqueID
gge$uniqID = paste(gge$sp.gen,gge$columnID,gge$Site,gge$Date,sep = "_")

gn = data.frame(cbind(gge$uniqID,gge[,10:50]))
g = melt(gn)
#get rid of some shit left over from the original spreadsheet
#g = g[g$ge.uniqID != "___",]
#g = g[g$variable != "sum",]

yo = matrix(unlist(strsplit(as.character(g$gge.uniqID),"_")), ncol = 4,byrow = TRUE)
#here make the full dataframe of the sp with columnID per reef per date - this can be linked then with the models
t = data.frame(date = yo[,4],
               reef = yo[,3],
               ColumnID = as.character(yo[,2]),
               sp.gen = as.character(yo[,1]),
               tl =  (matrix(unlist(strsplit(as.character(g$variable),"X")), ncol = 2,byrow = TRUE)[,2]),
               abund = as.numeric(g$value) )
t = t[t$abund != 0,]
t$tl <- as.numeric(as.character(t$tl))
dim(t)

#check on the bigs:
bigs <- t[t$tl > 30,]
#convert to mass
lw = read.csv("input/Fish Surveys/LengthWeight_AllFish.csv",
              stringsAsFactors = FALSE, sep = ",")

tt = merge(t,lw[,c("sp.gen","a.final","b.final")],"sp.gen")

nuts = read.csv("input/Fish Surveys/ExcretionModels+bodynutrients.csv",
                stringsAsFactors = FALSE, sep = ",")

ts = merge(tt,nuts, "ColumnID")

head(ts)
tapply(lw$a.final,lw$ColumnID,sd )

ts$biomass = as.numeric(as.character(ts$a.final))*as.numeric(as.character(ts$tl))^as.numeric(as.character(ts$b.final))
ts$biomass.tot =( as.numeric(as.character(ts$a.final))*as.numeric(as.character(ts$tl))^as.numeric(as.character(ts$b.final)) ) * as.numeric(as.character(ts$abund))

ts$N.int.new=ifelse(ts$N.int<0,0.0001,as.numeric(ts$N.int))  # if the int is less than 0 - force it to be zero

ts$N.intSD.new=ifelse(ts$N.intCI==0,
                      mean(tapply(ts$N.intCI,ts$ColumnID,mean)[which(tapply(ts$N.intCI,ts$ColumnID,mean)>0)]),
                      as.numeric(ts$N.intCI))

ts$n = ((ts$N.int.new + ts$N.slope*ts$biomass)*as.numeric(ts$abund))

#Again for P I am using the gamma dist for the intercept and the rnorm for the slope - 
#here there are a few CIslopes that are greater than the Slope but not too many 
#and these will just be overestimated
ts$P.int.new=ifelse(ts$P.int<0,0.0001,as.numeric(ts$P.int))  # if the int is less than 0 force it to be zero

ts$p=((ts$P.int.new + ts$P.slope*ts$biomass)*as.numeric(ts$abund))


###################################################
#END CALCULATIONS FOR NUTRIENTS####
###################################################

###################################################
#START THE BOOSTRAP PROCESS FOR EACH REEF
###################################################

#This is all to learn the data a bit more. 
ts$reef.date <- paste(ts$reef, ts$date, sep = "_")

#this is quite interesting - but I am going to pull all these trans fish
trans <- droplevels(ts[ts$sp.gen %in% trans.sp$sp.gen,])
tapply(trans$biomass, list(trans$reef.date, trans$sp.gen), sum)

tts <- droplevels(ts[ts$sp.gen %notin% trans.sp$sp.gen,])
uniq.reef <- unique(tts$reef)
tts$year <- matrix(unlist(strsplit(as.character(tts$date),"/")), ncol = 3,byrow = TRUE)[,3]

#this is a bit messed up, but H5 and H6 were surveyed twice in 2018 so I need to do the boostrapping for each date superately
#to do this I am essentially creating an new reef

ttt <- tts[tts$year == "17" | tts$year == "18",]
uniq.reef.date <- unique(ttt$reef.date)

#here bootstrap through each reef to generate means and CIs for each reef.data from 2017-2018
#for LH1 I use 2017 because I wasn't able to survey that year
its = 100

reefsy.full <- list()
reefsy.bionp <- list()
tocalc.bsdist <- list()

for (i in 1:length( uniq.reef.date ) ) {
  
  bootsy.bionp <- list()
  bootys.div <- list()
  
  now <- tts[tts$reef.date == uniq.reef.date[i],]
  
  #if(max( unique(now$year) )  == 18 ) {
  #  now <- now[now$year == 18,] } else {
  #    now <- now[now$year == 17,]
  #  }
  
  print(unique(now$reef.date))
  
  for(ii in 1:its){
    
    dat <- data.frame(now[sample(1:dim(now)[1], replace = T),][,c("ColumnID", "sp.gen", "tl", "abund", "n", "p", "biomass.tot")],
                       iteration = ii )
    
    bootsy.bionp[[ii]] <- dat
    #this is if I want to calculate diversity metrics or skewness or some shit
    #bootsy.div[[ii]] <- data.frame(ColumnID = rep(dat$ColumnID, dat$abund),
    #                               sp.gen = rep(dat$sp.gen, dat$abund),
    #                               tl = rep(dat$tl, dat$abund))
  }
  
  reefsy.full[[i]] <- do.call(rbind,bootsy.bionp)
  reefsy.bionp[[i]] <- do.call(rbind, 
                                lapply(bootsy.bionp, function(x) { data.frame(biomass.tot = sum(x$biomass.tot),
                                                                        n = sum(x$n),
                                                                        p = sum(x$p),
                                                                        rich = length(unique(x$sp.gen))) } ) )
  
  #reefsy.div[[i]] <- do.call(rbind, 
  #                             lapply(bootsy.div, function(x) { data.frame(biomass = sum(x$biomass),
  #                                                                     n = sum(x$n),
  #                                                                     p = sum(x$p) ) } ) )
  
  tocalc.bsdist[[i]] <- now
  

}

names(tocalc.bsdist) <- uniq.reef.date
names(reefsy.bionp) <- uniq.reef.date
cis <- function(x) { qnorm(0.975)*sd(x)/sqrt(length(x))}


#looking at the data
tocalc.bsdist[["H6_6/17/18"]][,c("sp.gen", "tl", "abund", "biomass", "biomass.tot")]
tocalc.bsdist[["H6_6/5/18"]][,c("sp.gen", "tl", "abund", "biomass", "biomass.tot")]


biomass.comp <- data.frame(reef.date = uniq.reef.date,
                   do.call( rbind, lapply(reefsy.bionp, function(x) { data.frame(#biomass = mean(x$biomass),
                                        #biomass.sd = sd(x$biomass),
                                        #biomass.ciu =  mean(x$biomass) + cis(x$biomass),
                                        #biomass.cil = mean(x$biomass) - cis(x$biomass),
                                        biomass = mean(x$biomass.tot),
                                        biomass.sd = sd(x$biomass.tot),
                                        biomass.ciu =  mean(x$biomass.tot) + cis(x$biomass.tot),
                                        biomass.cil = mean(x$biomass.tot) - cis(x$biomass.tot),
                                        n = mean(x$n),
                                        n.sd = sd(x$n),
                                        n.ciu = mean(x$n) + cis(x$n),
                                        n.cil = mean(x$n) - cis(x$n),
                                        np = mean(x$n*31/x$p*14),
                                        np.sd = sd(x$n*31/x$p*14),
                                        np.ciu = mean(x$n*31/x$p*14) + cis(x$n*31/x$p*14),
                                        np.cil = mean(x$n*31/x$p*14) - cis(x$n*31/x$p*14),
                                        p = mean(x$p),
                                        p.sd = sd(x$p), 
                                        p.ciu = mean(x$p) + cis(x$p),
                                        p.cil = mean(x$p) - cis(x$p)  ) } ) ) )   


#This collates the boot data and makes a big dataframe that can allow richness and body size distributions to be quantified.
spabundtl <- do.call(rbind, tocalc.bsdist)[,c("reef.date", "date", "reef", "sp.gen", "tl", "abund", "biomass", "biomass.tot")]
zs <- do.call(rbind, tocalc.bsdist)
bsdist <- data.frame(reef.date = rep(zs$reef.date, zs$abund),
                      date = rep(zs$date, zs$abund),
                      reef = rep(zs$reef, zs$abund),
                      ColumnID = rep(zs$ColumnID, zs$abund),
                      sp.gen = rep(zs$sp.gen, zs$abund),
                      tl = rep(zs$tl, zs$abund))


#all the reef.dates
write.csv(data.frame(spabundtl),file="output/AllReefSpAbundPerTL_PerReef_2017+2018_allreef.dates.csv") #the body size distributions per reef
write.csv(data.frame(bsdist),file="output/AllReefSpBodySizes_PerReef_2017+2018_allreef.dates.csv") #the body size distributions per reef
write.csv(data.frame(biomass.comp),file="output/AllReefBiomassNP_PerReef_2017+2018_allreef.dates.csv") #the means per each sp per reefs for the last time they were sampled


#remove (at least for now) the reef dates I am not using
reef.date.omit <- c( "H5_6/5/18", "H6_6/5/18", "LH2_5/24/17", "LH3_5/21/18", "ES1_5/21/17", "MOW1_5/21/17", "MOW2_5/21/17", "MOW3_5/21/17", "MOW4_5/20/18", "MOW4_5/21/17")
###
`%notin%` <- Negate(`%in%`)

write.csv(data.frame(spabundtl[spabundtl$reef.date %notin% reef.date.omit,]),file="output/AllReefSpAbundPerTL_PerReef_2017+2018.csv") #the body size distributions per reef
write.csv(data.frame(bsdist[bsdist$reef.date %notin% reef.date.omit,]),file="output/AllReefSpBodySizes_PerReef_2017+2018.csv") #the body size distributions per reef
write.csv(data.frame(biomass.comp[biomass.comp$reef.date %notin% reef.date.omit,]),file="output/AllReefBiomassNP_PerReef_2017+2018.csv") #the means per each sp per reefs for the last time they were sampled
