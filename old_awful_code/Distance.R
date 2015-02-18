#Set Working Directory
#Windows @ UA

setwd("C:/Users/avanderlaar/Dropbox/R/Distance/")

#add these libraries
library(unmarked)
library(AICcmodavg)

#Read in the bird detection information
dists<-read.csv('Sora13.csv', header=TRUE) 

#reading in the habitat information
soracov<-read.csv('Veg13_min.csv', header=TRUE )

# List of all transects from observations
tran.obs = unique(dists$Impound) 

# List of transects without observations 
#list = tran.all[tran.all%in%tran.obs==FALSE]

#dists$Impound<-factor(dists$Impound, levels=tran.all)
dists$Impound<-factor(dists$Impound, levels=c(1:51))

dists$Transect<-factor(dists$Transect, levels=c(1:281))

dists$Occasion<-factor(dists$Occasion, levels=c(1:25))

cutPT = seq(0,13,by=1)
#yDat = formatDistData(dists, distCol="Distance", transectNameCol="Transect", occasionCol="Occasion",
                      dist.breaks=cutPT) 

yDat = formatDistData(dists, distCol="Distance", transectNameCol="Transect", dist.breaks=cutPT) 

# umf = unmarkedFrameGDS(y=yDat, yearlySiteCovs=soracov, 
#     numPrimary=2, survey="line", dist.breaks=seq(0, 20, by=2), 
#     unitsIn="m", tlength=soracov$Effort_M)

#numPrimary=4 because there are four rounds
#tlength = soracov$Km because that is the column in soracov that has the length in km
umf = unmarkedFrameGDS(y=yDat, numPrimary=25, survey="line", dist.breaks=cutPT,  unitsIn="km", tlength=soracov$Km)

cutPT2=c(0:13)
umfD = unmarkedFrameDS(y=yDat, dist.breaks=cutPT2, tlength=soracov$Km, survey="line", unitsIn="km")

head(umf)

nullD = distsamp(~1~1, umfD)

null = gdistsamp(~1, ~1, ~1, umf)

str(umf)
summary(umf)

#Graphs the distrbution of observations from teh transect line in a histogram

hist(dists$Distance)

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
