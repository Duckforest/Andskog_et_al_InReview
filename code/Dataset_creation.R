###Creating Simulation_dataset###
library(tidyverse)
library(broom)
rm(list = ls())

# Working the data --------------------------------------------------------


#Loading data
full            <-read.csv("input/Full_dataset.csv", dec=".",sep = ",",stringsAsFactors = FALSE)
bahamas_growth  <-read.csv("input/Bahamas_growth.csv", dec=".",sep = ",",stringsAsFactors = FALSE)
Drichall        <-read.csv("input/Drichall.csv", dec=".",sep = ",",stringsAsFactors = FALSE)

eighteen <- full 
#clean up
eighteen[ eighteen == "#VALUE!" ] <- NA
eighteen[ eighteen == "NaN" ] <- NA
eighteen[ eighteen == "" ] <- NA

#Number of blades we measured
unique.blades <- unique(eighteen["Blade.ID"])

#Number of shoots we measured
unique.shoots <- unique(eighteen["Shoot.ID"])

#Number of blades with top bitten off
bitten <- subset(eighteen, eighteen$Bitten == "yes")
summary(bitten)

#Making numeric and factors
eighteen[10:length(eighteen)] <- lapply(eighteen[10:length(eighteen)], as.numeric)
eighteen[1:9] <- lapply(eighteen[1:9], as.factor)
eighteen$Epiphyte.Wt[ eighteen$Epiphyte.Wt < 0] <- 0 #replacing negative epiphyte weight with 0

##Shoot level data
names(eighteen)
shoot<- eighteen %>% 
  group_by(Shoot.ID) %>% 
  summarise(Transect.ID = first(Transect.ID),
            Distance.ID=first(Distance.ID),
            Island=first(Island),
            Reef= first(Reef),
            Transect= first(Transect),
            Distance= first(Distance),
            #Seagrass metrics
            Shoot.density=mean(Shoot.density, na.rm=TRUE)*100,#per m2
            Cover=mean(Cover, na.rm=TRUE),
            Max.length = max(Length),
            Mean.length = mean(Length, na.rm=TRUE),
            Shoot.area = sum(Blade.area, na.rm=TRUE),
            Shoot.Wt = mean(Shoot.Wt, na.rm=TRUE)*1000,#Convert to mg
            Sum.growth.area = sum(Growth.area, na.rm=TRUE),
            Mean.growth.area = mean(Growth.area, na.rm=TRUE),
            #Herbivory
            Bites = mean(Bites, na.rm=TRUE), #is already shoot level
            Epiphytes = mean(Epiphyte.Wt, na.rm=TRUE)*1000,#Convert to mg, #is already shoot level
            #Seagrass stoichiometry
            Seagrass.P=mean(Seagrass.P, na.rm=TRUE),
            Seagrass.N=mean(Seagrass.N, na.rm=TRUE),
            Seagrass.C=mean(Seagrass.C, na.rm=TRUE),
            Seagrass.C13 = mean(d13C, na.rm= TRUE),
            Seagrass.N15 = mean(d15N, na.rm= TRUE),
            #Fish
            Fish.Biomass = mean(Fish.Biomass, na.rm=TRUE),
            Fish.N = mean(Fish.N, na.rm=TRUE),
            Fish.P = mean(Fish.P, na.rm=TRUE),
            #Environmental
            TN = mean(TDN..ug.L., na.rm=T),
            TP = mean(TDP..ug.L., na.rm=T),
            NO3 = mean(NO3, na.rm=T),
            PO4 = mean(PO4, na.rm=T),
            Turbidity = mean(Turbidity, na.rm=T),
            Sediment.N = mean(Sediment.N, na.rm=T),
            Sediment.P = mean(Sediment.P, na.rm=T),
            Sediment.C = mean(Sediment.C, na.rm=T),
            Sediment.13C = mean(Sediment.dC13, na.rm = T),
            Sediment.13Corg = mean(Sediment.dCorg13, na.rm = T),
            Sediment.15N = mean(Sediment.dN15, na.rm = T))

#Adding new columns to normalise bites and epiphytes to area and calculate SLA
shoot = shoot %>%
  mutate(Bites.area = Bites/Shoot.area,
         Epiphytes.area = Epiphytes/Shoot.area,
         SLA = Shoot.area/Shoot.Wt)

shoot$Sum.growth.area[ shoot$Sum.growth.area == 0] <- NA
shoot[shoot == "NaN" ] <- NA

##For reviewers epiphyte:shoot ratio
shoot$epishoot <- shoot$Epiphytes/shoot$Shoot.Wt
summary(shoot$epishoot)
mean(shoot$epishoot, na.rm = T)
sd(shoot$epishoot, na.rm = T)
boxplot(shoot$epishoot~shoot$Island)
bwplot(epishoot ~ as.factor(Distance) | Reef,
       data = shoot, pch = 19,
       layout = c(3,5))

#Getting area to biomass slope
WA <-read.csv("input/New weight area.csv", dec=".",sep = ",",stringsAsFactors = FALSE)

WAmod<-lm(Weight~Area-1, data=WA) ##Area is mm, weight is mg. -1 is constraining the intercept
summary(WAmod) #R2 = 0.97
coeff=coefficients(WAmod)
plot(Weight~Area, data=WA)

#Summarising to distance level
distance <- shoot %>% 
  group_by(Distance.ID) %>% 
  summarise(Transect.ID = first(Transect.ID),
            Island=first(Island),
            Reef= first(Reef),
            Transect= first(Transect),
            Distance= first(Distance),
            #Seagrass metrics
            Shoot.density=mean(Shoot.density, na.rm=TRUE),
            Cover=mean(Cover, na.rm=TRUE),
            Max.length = mean(Max.length, na.rm=TRUE),
            Mean.length = mean(Mean.length, na.rm=TRUE),
            Shoot.area = mean(Shoot.area, na.rm=TRUE),
            Shoot.Wt = mean(Shoot.Wt, na.rm=TRUE),
            Sum.growth.area = mean(Sum.growth.area, na.rm=TRUE),
            Mean.growth.area = mean(Mean.growth.area, na.rm=TRUE),
            SLA = mean(SLA, na.rm=T),
            #Herbivory
            Bites.total = mean(Bites,na.rm=TRUE),
            Bites.area=mean(Bites.area, na.rm=T),
            Epiphytes.total = mean(Epiphytes, na.rm=TRUE),
            Epiphytes.area = mean(Epiphytes.area, na.rm=T),
            #Seagrass stoichiometry
            Seagrass.P=mean(Seagrass.P, na.rm=TRUE),
            Seagrass.N=mean(Seagrass.N, na.rm=TRUE), 
            Seagrass.C=mean(Seagrass.C, na.rm=TRUE), 
            Seagrass.CN=sum(Seagrass.C/Seagrass.N *14/12, na.rm=TRUE),
            Seagrass.NP = sum(Seagrass.N/Seagrass.P * 31/14, na.rm = TRUE),
            Seagrass.C13 = mean(Seagrass.C13, na.rm= TRUE),
            Seagrass.N15 = mean(Seagrass.N15, na.rm= TRUE),
            #Fish
            Fish.Biomass = mean(Fish.Biomass, na.rm=TRUE)/1000,#Convert to kg
            Fish.N = mean(Fish.N, na.rm=TRUE),
            Fish.P = mean(Fish.P, na.rm=TRUE),
            #Environmental
            TN = mean(TN, na.rm=T),
            TP = mean(TP, na.rm=T),
            NO3 = mean(NO3, na.rm=T),
            PO4 = mean(PO4, na.rm=T),
            Turbidity = mean(Turbidity, na.rm=T),
            Sediment.N = mean(Sediment.N, na.rm=T),
            Sediment.P = mean(Sediment.P, na.rm=T),
            Sediment.C = mean(Sediment.C, na.rm=T),
            Sediment.13C = mean(Sediment.13C, na.rm = T),
            Sediment.13Corg = mean(Sediment.13Corg, na.rm = T),
            Sediment.15N = mean(Sediment.15N, na.rm = T))

            
#Merging Bahamas growth column with Haiti
distance[ distance == "NaN" ] <- NA
distance$Seagrass.CN[ distance$Seagrass.CN == 0 ] <- NA
distance$Seagrass.NP[ distance$Seagrass.NP == 0 ] <- NA

#Merging with Bahamas growth
bahamasshoot<- bahamas_growth %>% 
  group_by(Shoot.ID) %>% 
  summarise(Transect.ID = first(Transect.ID),
            Distance.ID=first(Distance.ID),
            Island=first(Island),
            Reef= first(Reef),
            Transect= first(Transect),
            Distance= first(Distance),
            #Growth metrics
            Sum.growth.area = sum(Growth.area, na.rm=TRUE),
            Mean.growth.area = mean(Growth.area, na.rm=TRUE))

##Summarising Bahamas growth to distance
bahamasdistance <- bahamasshoot %>% 
  group_by(Distance.ID) %>% 
  summarise(#Seagrass growth
    Sum.growth.area = mean(Sum.growth.area, na.rm=TRUE),
    Mean.growth.area = mean(Mean.growth.area, na.rm=TRUE))

all <- merge(distance, bahamasdistance, by = c("Distance.ID"), all = TRUE)
all = all %>%
  rowwise() %>% 
  mutate(Sum.growth.area.all = sum(Sum.growth.area.x,Sum.growth.area.y, na.rm=T),
         Mean.growth.area.all = sum(Mean.growth.area.x, Mean.growth.area.y, na.rm=T),
         Sum.growth.biomass = Sum.growth.area.all*coeff,
         Mean.growth.biomass = Mean.growth.area.all*coeff)#Merge Haiti and Bahamas shoot growth

##Growth biomass is now in mg!
all$Sum.growth.area.all[ all$Sum.growth.area.all == 0 ] <- NA
all$Mean.growth.area.all[ all$Mean.growth.area.all == 0 ] <- NA
all$Sum.growth.biomass[ all$Sum.growth.biomass == 0 ] <- NA
all$Mean.growth.biomass[ all$Mean.growth.biomass == 0 ] <- NA

#Creating LAI and AG biomass and growth
all = all %>%
  rowwise() %>%
  mutate(LAI = (Shoot.area*Shoot.density)/1000000, #m2 per m2
         AG.biomass = Shoot.Wt*Shoot.density,
         AG.growth.sum = (Sum.growth.biomass*Shoot.density)/1000, #In grams
         AG.growth.mean = (Mean.growth.biomass*Shoot.density)/1000, #In grams
         C.production = AG.growth.sum*(Seagrass.C/100))

all = subset(all, select = -c(Sum.growth.area.x, Mean.growth.area.x, Sum.growth.area.y, Mean.growth.area.y))
all$Island[is.na(all$Island)] <- "Haiti"
all[ all == "NaN" ] <- NA


vars <- c("Island","Reef",  "Transect", "Distance",
          "Sum.growth.area.all", "C.production","SLA","Seagrass.P","Seagrass.N","Seagrass.C","Shoot.density", "LAI", 
          "Seagrass.C13", "Bites.total", "Bites.area", "Epiphytes.area", "Seagrass.C", "Shoot.area")

vars2 <- c("Island","Reef",  "Transect", "Distance",
          "AG.growth.sum","Sum.growth.biomass")
Dataset2 <- all[vars2]

summary <- Dataset2 %>% 
  group_by(Island, Reef, Distance) %>% 
  summarise(AG.growth.sum = mean(AG.growth.sum, na.rm = T),
            Sum.growth.biomass = mean(Sum.growth.biomass, na.rm = T))

Dataset <- all[vars]
colnames(Dataset)[colnames(Dataset) == 'Sum.growth.area.all'] <- 'Shoot.growth'
# write.csv(Dataset,"input/Simulation_dataset_Jan2022.csv", row.names = FALSE)

# Reef variables ----------------------------------------------------------
reefvars <- c("Island","Reef",  "Transect", "Distance",
              "Fish.Biomass", "Fish.N","Fish.P","TN","TP","NO3","PO4", "Turbidity","Sediment.N","Sediment.P","Sediment.C",
              "Sediment.13Corg","Sediment.15N")

Reef_dataset <- subset(all, all$Distance < 20)[reefvars]

#Ambient nutrients
#Add %P and %N at 20 meter
controlvars = c("Island", "Reef","Transect","Distance" ,"Seagrass.P", "Seagrass.N")
twenty <- all[controlvars]
twenty <-twenty[which(twenty$Distance==20),] #Create dataset of 20m
twenty$Distance <- NULL
twelve <- all[controlvars]
twelve <-twelve[which(twelve$Distance==12),] #Create dataset of 20m
twelve$Distance <- NULL

colnames(twenty)[colnames(twenty)=="Seagrass.P"] <- "Seagrass.P20" #change names
colnames(twenty)[colnames(twenty)=="Seagrass.N"] <- "Seagrass.N20" #Change names

Reef_dataset = merge(Reef_dataset, twenty, by = c("Island", "Reef","Transect"), all=T) #merging the two datasets
Reef_dataset <- unique(Reef_dataset[c(1:3,5:length(Reef_dataset))])
# write.csv(Reef_dataset,"input/Reef_dataset_Sep2021.csv", row.names = FALSE)

