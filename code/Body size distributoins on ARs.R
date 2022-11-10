#NOTES: there are some pretty epic disparities in H6 from 6/5 to 6/17 this is mostly due to the presnce of 2 morays vs 1 and in the second survey I recorded them at >40 , which I changed in the spreadsheet just now to 100. 
#I changed it from 100 to 60

rm(list = ls())
library(msm)
library(base)
library(modeest)
library(reshape)
library(data.table)
library(splitstackshape)
#source("/Users/jacoboallgeier/My Work Biatch/Bahamas Data Comp/YoMama Reefs/2013 Big Sampling/Analyses/R code/Initial Data Conversion for YoMama.R")

ge <- read.csv("/Users/jeallg/My Work Biatch/Grants/Current Grants/NSF_2013_Global/Analysis/R input/AllFishSurveys_Haiti+Bahamas_2018.csv",
               stringsAsFactors = FALSE, sep = ",")

ge[is.na(ge)]<-0
names(ge)

ge$uniqID = paste(ge$sp.gen,ge$columnID,ge$Reef,ge$Date,sep = "_")
c.ID <- read.csv("/Users/jeallg/My Work Biatch/Fish Datasets/Fish_SurveyToExcretion/R input/CommonName-ColumnIDConvert.csv", stringsAsFactors = FALSE, sep = ",")
c.ID$sp.gen = paste(c.ID$Genus,c.ID$Species, sep = " ")
c <- c.ID[,c("CommonName", "sp.gen","ColumnID")]
colnames(c) <- c("Species","sp.gen", "columnID")
gge <- merge(ge, c)

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
dim(t)

#slick ways to expand the rows
# df <- data.frame(
#   name=c("Person 1", "Person 2", "Person 3", "Person 1", "Person 2", "Person 3"),
#   group=c("A", "A", "A", "B", "B", "B"),
#   count=c(3,1,0,5,0,1))
# 
# expanded <- data.frame(name = rep(df$name, df$count),
#                        group = rep(df$group, df$count))
# 
# 
# df[rep(seq_len(nrow(df)), df$count), 1:2]

zz <- data.frame(date = rep(t$date, t$abund),
                 reef = rep(t$reef, t$abund),
                 ColumnID = rep(t$ColumnID, t$abund),
                 sp.gen = rep(t$sp.gen, t$abund),
                 tl = rep(t$tl, t$abund))

if(sum(t$abund) == dim(zz)[1]) { " FUCK YEAH ! " }

zz$tl <- as.numeric(as.character(zz$tl))
quartz()

hist(zz$tl)

horb <- data.frame(reef = unique(zz$reef),
                   horb = c("h","b","h","h","b","h","h","h","b","b","b","h","h","h","h","b","b"))

zzz <- merge(zz,horb)

quartz(width = 10)
par(mfrow = c(2,9), mar = c(2,3,3,1))

for(i in 1:length(unique(zzz$reef)) ){
  
  now <- zzz[zzz$reef == unique(zzz$reef)[i],]
  
  hist(now$tl,
       main = unique(zzz$reef)[i],
       xlab = "", ylab = "",
       xlim = c(0,50))
  
}


quartz(width = 10)
par(mfrow = c(1,2), mar = c(2,3,3,1))
nam <- c("The Bahamas", "Haiti")
for(i in 1:length(unique(zzz$horb)) ){
  
  now <- zzz[zzz$horb == unique(zzz$horb)[i],]
  
  hist(now$tl,
       main = unique(zzz$reef)[i],
       xlab = "", ylab = "",
       xlim = c(0,50),
       ylim = c(0,1000))
  
}

quartz(width = 10)
par( mar = c(4.5,4.5,3,1), family = "serif", cex.lab = 1.6,cex.axis = 1.4)
nam <- c("The Bahamas", "Haiti")
  
  now <- zzz[zzz$horb == unique(zzz$horb)[2],]
  
  hist(now$tl,
       main = "",
       xlab = "Total Lenght (cm)", ylab = "Frequency",
       xlim = c(0,40),
       ylim = c(0,700),
       col = "#43a2ca80",
       breaks = 40)

  now <- zzz[zzz$horb == unique(zzz$horb)[1],]
  
  hist(now$tl,
       main = "",
       xlab = "", ylab = "",
       xlim = c(0,40),
       ylim = c(0,700),
       col = "#31a35480" ,
       add = T,
       breaks = 40)

  legend('topright', c("The Bahamas", "Haiti"), text.col = c("#31a354","#43a2ca"),
         bty = 'n', cex = 2.2)


