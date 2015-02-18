#Set Working Directory
#Windows @ UA

setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

#add these libraries
library(unmarked)
library(AICcmodavg)

#fivebin4 <- as.matrix(read.csv('Sora_5bin_4Round.csv', header=TRUE))
fivebin1 <- as.matrix(read.csv('Sora_5bin_MergedRounds.csv'))
fivebin1213 <- as.matrix(read.csv('Sora_1213_5bin_MergedRounds.csv'))
#yDat <- as.matrix(read.csv('BirdsBinned.csv', header=TRUE))

covs <- read.csv('Veg12_13_min.csv', header=TRUE)

# dists<-read.csv('Sora13.csv', header=TRUE) #Read in the bird detection information
# 
# soracov<-read.csv('Veg12_13_min.csv', header=TRUE )#reading in the habitat information
# 
# tran.obs = unique(dists$Impound) # List of all transects from observations
# 
# tran.all = unique(soracov$Impound)# List of all transects from vegetation measurements
# 
# list = tran.all[tran.all%in%tran.obs==FALSE]# List of transects without observations 
# 
# dists$Impound<-factor(dists$Impound, levels=tran.all) #turns the impoundment #'s into factors
# 
# dists$Occasion<-factor(dists$Occasion, levels=tran.all) #turns the occasion #'s into factors
# 
# head(dists) #just make sure everything looks like it should still, no weird NAs showing up

cutPT = c(0,3,6,9,12,15) #the fartherest distance is 12

# yDat = formatDistData(dists, distCol="Distance", transectNameCol="Impound", occasionCol="Occasion",
#                       dist.breaks=cutPT) 

#umfG = unmarkedFrameGDS(y=fivebin4, numPrimary=4, survey="line", #numPrimary=4 because there are four rounds
 #     dist.breaks=cutPT,  unitsIn="m", tlength=covs$effort_m) 

umf = unmarkedFrameDS(y=fivebin1213, survey="line", dist.breaks=cutPT, siteCovs=covs, unitsIn="m", tlength=covs$Effort_M) 


head(umf) #make sure there are some non-zero values in here somewhere, otherwise it didn't work

#null = gdistsamp(~1, ~1, ~1, umfG) #basic null model

null = distsamp(~1~1, umf, starts=c(0,0,0))
round = distsamp(~1~Round, umf)
region = distsamp(~1~Region, umf)
impound = distsamp(~1~Impoundment, umf)
year = distsamp(~1~Year, umf)
#Graphs the distrbution of observations from teh transect line in a histogram

hist(umf)



##################################################
#### Modeling ####################################
##################################################

#in 'distsamp' the space after the first ~ is for modeling factors
#affecting detection and the space after the second ~ is for modeling
#factors affecting density.

##################################################
#### Habitat Management #########################
#################################################

Cand.modG = list()
Cand.modG[[1]] = gdistsamp(~Habitat~1, umf, keyfun="hazard")
Cand.modG[[2]] = gdistsamp(~Habitat~RegionA + Habitat + Year, umf, keyfun="hazard")
Cand.modG[[3]] = gdistsamp(~Habitat~Habitat + Year, umf, keyfun="hazard")
Cand.modG[[4]] = gdistsamp(~Habitat~RegionA, umf, keyfun="hazard")
Cand.modG[[5]] = gdistsamp(~Habitat~AreaA + RegionA, umf, keyfun="hazard")
Cand.modG[[6]] = gdistsamp(~Habitat~AreaA, umf, keyfun="hazard")
 
Modnames = c("null", "Region+Hab+Dist", "Hab+Dist", "Region", "Area+Region", "Area")
 
detect.table = aictab(cand.set=Cand.modG, modnames=Modnames)
detect.table

###########################
###########PLOTS###########Region
##########################

#Round

# DataFrame <- data.frame(RoundA=c("A", "B", "C"))
# model <- distsamp(~1~RoundA, umf, keyfun="hazard")
# Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)


RoundNames=data.frame(RoundNames=c("Round1", "Round2", "Round3"))

Elambda

par(mfrow=c(1,5))
with(Elambda, {
  x <- barplot(Predicted, names=RoundA, xlab="Round", col="#FFC000", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})



#Region

DataFrame <- data.frame(RegionA=c("NW", "NC", "NE", "SE"))
model <- distsamp(~1~RegionA, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)

jpeg('Region.jpg')
par(mfrow=c(1,1))
with(Elambda, {
  x <- barplot(Predicted, names=RegionA, xlab="Region", col="#FFC000", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})
dev.off()


#Disturbance

DataFrame <- data.frame(Disturbance_Red=c("Mowed", "Disced", "None"))
model <- distsamp(~1~Disturbance_Red, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)



jpeg('DisturbanceByRegion')
par(new, mfrow=c(1,4))
with(Elambda, {
  x <- barplot(Predicted, names=Disturbance_Red, col="#FFC000", xlab="Disturbance", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})


#Habitat
DataFrame <- data.frame(Habitat=c("PE", "MS", "UP"))
model <- distsamp(~1~Habitat, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)



Elambda
ElambdaNC
ElambdaNE
ElambdaSE

par(mfrow=c(2,2))
with(Elambda, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaNW, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaNC, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaNE, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaSE, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})


#Area
DataFrame <- data.frame(AreaA=c("SC", "NV", "SL", "BK", "CC", "FG", "GP", "TS", "MN", "OS", "DC"))
model <- distsamp(~1~AreaA, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)



par(mfrow=c(2,2))
with(Elambda, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaNW, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaNC, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaNE, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(ElambdaSE, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})


###########################
###########PLOTS###########ROUND
##########################

#Round

DataFrame <- data.frame(RoundA=c("A", "B", "C"))
model <- distsamp(~1~RoundA, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)

par(mfrow=c(1,1))
with(Elambda, {
  x <- barplot(Predicted, names=RoundA, col="#FFC000", xlab="Round", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})


#Region

DataFrame <- data.frame(RegionA=c("NW", "NC", "NE", "SE"))
model <- distsamp(~1~RegionA, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)

par(mfrow=c(2,2))
with(Elambda, {
  x <- barplot(Predicted, names=RegionA, col="#FFC000", xlab="Region", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda1, {
  x <- barplot(Predicted, names=RegionA, col="#FFC000", xlab="Region", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda2, {
  x <- barplot(Predicted, names=RegionA, col="#FFC000", xlab="Region", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda3, {
  x <- barplot(Predicted, names=RegionA, col="#FFC000", xlab="Region", ylab="Sora/ha", 
               ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})
#Disturbance

DataFrame <- data.frame(Disturbance_Red=c("Mowed", "Disced", "None"))
model <- distsamp(~1~Disturbance_Red, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)

par(mfrow=c(2,2))
with(Elambda, {
  x <- barplot(Predicted, names=Disturbance_Red, col="#FFC000", xlab="Disturbance", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda1, {
  x <- barplot(Predicted, names=Disturbance_Red, col="#FFC000", xlab="Disturbance", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda2, {
  x <- barplot(Predicted, names=Disturbance_Red, col="#FFC000", xlab="Disturbance", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda3, {
  x <- barplot(Predicted, names=Disturbance_Red, col="#FFC000", xlab="Round", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

#Habitat
DataFrame <- data.frame(Habitat_Red=c("PE", "MS", "UP"))
model <- distsamp(~1~Habitat_Red, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)

par(mfrow=c(2,2))
with(Elambda, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda1, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda2, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda3, {
  x <- barplot(Predicted, names=Habitat_Red, col="#FFC000", xlab="Habitat Type", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

#Area
DataFrame <- data.frame(AreaA=c("SC", "NV", "SL", "BK", "CC", "FG", "GP", "TS", "MN", "OS", "DC"))
model <- distsamp(~1~AreaA, umf, keyfun="hazard")
Elambda <- predict(model, type="state", newdata=DataFrame,  appendData=TRUE)

par(mfrow=c(2,2))
with(Elambda, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda1, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  box()
})

with(Elambda2, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda3, {
  x <- barplot(Predicted, names=AreaA, col="#FFC000", xlab="Management Area", 
               ylab="Sora/ha", ylim=c(0,25), cex.names=0.7, cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05) 
  arrows(x, Predicted, x, Predicted-SE, code=3, angle=90, length=0.05) 
  box()
})

Elambda
Elambda1
Elambda2
Elambda3
