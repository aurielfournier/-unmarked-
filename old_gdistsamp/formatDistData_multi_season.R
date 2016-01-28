########
# This code creates the input files (both veg and bird related) for input into gdistsamp
# This code specifically stacks all of the surveys from one year into one input file
####

###
# Needed Packages
###
library(unmarked)

#####---------------------------
dist.breaks <- c(0,1,2,3,4,5) 

birds <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/all_birds.csv",header=T) 
birds <- birds[birds$species=="sora",]
birds <- birds[birds$distance<=5,] # removing the few detections we have that are over 5 meters away from the line
birds <- birds[!is.na(birds$round),]
birds <- birds[!is.na(birds$distance),]

birds$jdate <- as.factor(birds$jdate)
birds$night <- as.factor(birds$night)
birds <- birds[!(birds$night==4.2|birds$night==4.1),]

birds12 <- birds[birds$year==2012,]
birds12$night <- factor(birds12$night, labels=c(1,2,3))
birds12$dist <- factor(round(birds12$distance))

birds13 <- birds[birds$year==2013,]
birds13$night <- factor(birds13$night, labels=c(1.1,1.2,2.1,2.2,3.1,3.2))
birds13$dist <- factor(round(birds13$distance))

birds14 <- birds[birds$year==2014,]
birds14$night <- factor(birds14$night, labels=c(1.1,1.2,2.1,2.2,3.1,3.2))
birds14$dist <- factor(round(birds14$distance))

########## 2012

birds12 <- birds12[,c("impound","night","dist","round")]
birds12$num <- 1
distdf1 <- data.frame(impound="afiller",night=rep(c(1,2,3),6),dist=rep(c(0,1,2,3,4,5),each=3),round=1,num=0)
distdf2 <- data.frame(impound="afiller",night=rep(c(1,2,3),6),dist=rep(c(0,1,2,3,4,5),each=3),round=2,num=0)
distdf3 <- data.frame(impound="afiller",night=rep(c(1,2,3),6),dist=rep(c(0,1,2,3,4,5),each=3),round=3,num=0)
birds12r <- rbind(birds12, distdf1,distdf2,distdf3)
gg1 <- cast(data=birds12r[birds12r$round==1,], impound ~ night + dist)
gg2 <- cast(data=birds12r[birds12r$round==2,], impound ~ night + dist)
gg3 <- cast(data=birds12r[birds12r$round==3,], impound ~ night + dist)
gg12 <- merge(gg1, gg2, by="impound", all=TRUE)
sora12 <- merge(gg12, gg3, by="impound", all=TRUE)
colnames(sora12) <- c("impound", paste0("r",rep(c(1,2,3),each=18),"n",rep(c(1,2,3),each=6, times=3),"d",rep(c(0,1,2,3,4,5),times=9)))
sora12 <- sora12[sora12$impound!="afiller",]

########## 2013

birds13 <- birds13[,c("impound","night","dist","round")]
birds13$num <- 1
distdf1 <- data.frame(impound="afiller",night=rep(c(1.1,1.2,2.1,2.2,3.1,3.2),6),dist=rep(c(0,1,2,3,4,5),each=6),round=1,num=0)
distdf2 <- data.frame(impound="afiller",night=rep(c(1.1,1.2,2.1,2.2,3.1,3.2),6),dist=rep(c(0,1,2,3,4,5),each=6),round=2,num=0)
distdf3 <- data.frame(impound="afiller",night=rep(c(1.1,1.2,2.1,2.2,3.1,3.2),6),dist=rep(c(0,1,2,3,4,5),each=6),round=3,num=0)
birds13r <- rbind(birds13, distdf1,distdf2,distdf3)
gg1 <- cast(data=birds13r[birds13r$round==1,], impound ~ night + dist)
gg2 <- cast(data=birds13r[birds13r$round==2,], impound ~ night + dist)
gg3 <- cast(data=birds13r[birds13r$round==3,], impound ~ night + dist)
gg13 <- merge(gg1, gg2, by="impound", all=TRUE)
sora13 <- merge(gg13, gg3, by="impound", all=TRUE)
colnames(sora13) <- c("impound", paste0("r",rep(c(1,2,3),each=36),"_n",rep(c(1.1,1.2,2.1,2.2,3.1,3.2),each=6, times=3),"_d",rep(c(0,1,2,3,4,5),times=18)))
sora13 <- sora13[(sora13$impound!="afiller"&sora13$impound!="ccmsu7"&sora13$impound!="m14"&sora13$impound!="m4"&sora13$impound!="ms2n"&sora13$impound!="swan lake"&sora13$impound!="r4/5"&sora13$impound!="ditch"&sora13$impound!="pool c"&sora13$impound!="pool i"&sora13$impound!="poole"&sora13$impound!="poolc"&sora13$impound!="pooli"&sora13$impound!="m13"&sora13$impound!="scmsu2"&sora13$impound!="scmsu3"&sora13$impound!="rail"),]



########## 2014

birds14 <- birds14[,c("impound","night","dist","round")]
birds14$num <- 1
distdf1 <- data.frame(impound="afiller",night=rep(c(1.1,1.2,2.1,2.2,3.1,3.2),6),dist=rep(c(0,1,2,3,4,5),each=6),round=1,num=0)
distdf2 <- data.frame(impound="afiller",night=rep(c(1.1,1.2,2.1,2.2,3.1,3.2),6),dist=rep(c(0,1,2,3,4,5),each=6),round=2,num=0)
distdf3 <- data.frame(impound="afiller",night=rep(c(1.1,1.2,2.1,2.2,3.1,3.2),6),dist=rep(c(0,1,2,3,4,5),each=6),round=3,num=0)
birds14r <- rbind(birds14, distdf1,distdf2,distdf3)
gg1 <- cast(data=birds14r[birds14r$round==1,], impound ~ night + dist)
gg2 <- cast(data=birds14r[birds14r$round==2,], impound ~ night + dist)
gg3 <- cast(data=birds14r[birds14r$round==3,], impound ~ night + dist)
gg14 <- merge(gg1, gg2, by="impound", all=TRUE)
sora14 <- merge(gg14, gg3, by="impound", all=TRUE)
colnames(sora14) <- c("impound", paste0("r",rep(c(1,2,3),each=36),"_n",rep(c(1.1,1.2,2.1,2.2,3.1,3.2),each=6, times=3),"_d",rep(c(0,1,2,3,4,5),times=18)))
sora14 <- sora14[sora14$impound!="afiller",]





####------
# read in effort data
surv <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/all_surveys.csv",header=T)
surv <- surv[,c("year","night","round","impound","length")]
surv <- surv[surv$night<=3.5,]
surv <- surv[!is.na(surv$night),]
#### 2012
ss1 <- cast(data=surv[surv$round==1&surv$year==2012,], impound ~ night, value="length", fun.aggregate=mean )
ss2 <- cast(data=surv[surv$round==2&surv$year==2012,], impound ~ night, value="length", fun.aggregate=mean )
ss3 <- cast(data=surv[surv$round==3&surv$year==2012,], impound ~ night, value="length", fun.aggregate=mean )

ss12 <- merge(ss1, ss2, by="impound", all=TRUE)
s12 <- merge(ss12, ss3, by="impound", all=TRUE)
colnames(s12) <- c("impound","length_r1_n1","length_r1_n2","length_r1_n3","length_r2_n1","length_r2_n2","length_r2_n3","length_r3_n1","length_r3_n2","length_r3_n3")
#### 2013
ss1 <- cast(data=surv[surv$round==1&surv$year==2013,], impound ~ night, value="length", fun.aggregate=mean )
ss2 <- cast(data=surv[surv$round==2&surv$year==2013,], impound ~ night, value="length", fun.aggregate=mean )
ss3 <- cast(data=surv[surv$round==3&surv$year==2013,], impound ~ night, value="length", fun.aggregate=mean )
ss13 <- merge(ss1, ss2, by="impound", all=TRUE)
s13 <- merge(ss13, ss3, by="impound", all=TRUE)
colnames(s13) <- c("impound",paste0("length_r",rep(c(1,2,3),each=6),"_n",rep(c(1.1,1.2,2.1,2.2,3.1,3.2),each=3)))


#### 2014
ss1 <- cast(data=surv[surv$round==1&surv$year==2014,], impound ~ night, value="length", fun.aggregate=mean )
ss2 <- cast(data=surv[surv$round==2&surv$year==2014,], impound ~ night, value="length", fun.aggregate=mean )
ss3 <- cast(data=surv[surv$round==3&surv$year==2014,], impound ~ night, value="length", fun.aggregate=mean )
ss14 <- merge(ss1, ss2, by="impound", all=TRUE)
s14 <- merge(ss14, ss3, by="impound", all=TRUE)
colnames(s14) <- c("impound",paste0("length_r",rep(c(1,2,3),each=6),"_n",rep(c(1.1,1.2,2.1,2.2,3.1,3.2),each=3)))


####-----------------------------------------------
# Input covariates
####----------------------------------------------
veg <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/all_veg.csv", header=T) 
fed <- c("scnwr","slnwr","ccnwr","mnwr")
state <- c("nvca","fgca","tsca","bkca","dcca","tmpca","osca","gpca")
veg$fs <- NA
veg[veg$area %in% fed,]$fs <- "fed"
veg[veg$area %in% state,]$fs <- "state"
## 2012 ##
veg12 <- veg[veg$year==2012,]
veg12 <- veg12[veg12$bv=="v",]
veg12 <- veg12[,c("habtype","spp","round","region","area","impound","int","short","pe","water","averagewater","fs")]

v12 <- melt(data=veg12, id=c("habtype","spp","region","area","impound","round","fs"))

vr12 <- cast(data=v12, impound + region + area + fs ~ variable, fun.aggregate=mean)
vv12 <- cast(data=v12, impound + region + area + fs ~ variable + round, value="value", fun.aggregate=mean)

vvv12 <- data.frame(vr12, vv12$averagewater_2, vv12$averagewater_2, vv12$averagewater_3)

colnames(vvv12) <- c("impound","region","area","fs","int","short","pe","water","averagewater","averagewater_1","averagewater_2","averagewater_3")

vegss <- scale(vvv12[,5:ncol(vvv12)])
colnames(vegss) <- paste("scale", colnames(vegss), sep = "_")
veg12 <- cbind(vvv12, vegss)


## 2013 ##
veg13 <- veg[veg$year==2013,]
veg13 <- veg13[veg13$bv=="v",]
veg13 <- veg13[,c("habtype","spp","round","region","area","impound","int","short","pe","water","averagewater","fs")]
v13 <- melt(data=veg13, id=c("habtype","spp","region","area","impound","round","fs"))
v13 <- v13[!is.na(v13$value),]
vr13 <- cast(data=v13, impound + region + area + fs ~ variable, fun.aggregate=mean)
vv13 <- cast(data=v13, impound + region + area + fs ~ variable + round, value="value", fun.aggregate=mean)
vvv13 <- data.frame(vr13, vv13$averagewater_2, vv13$averagewater_2, vv13$averagewater_3)
colnames(vvv13) <- c("impound","region","area","fs","int","short","pe","water","averagewater","averagewater_1","averagewater_2","averagewater_3")
vegss <- scale(vvv13[,5:ncol(vvv13)])
colnames(vegss) <- paste("scale", colnames(vegss), sep = "_")
veg13 <- cbind(vvv13, vegss)


## 2014 ##
veg14 <- veg[veg$year==2014,]
veg14 <- veg14[veg14$bv=="v",]
veg14 <- veg14[,c("habtype","spp","round","region","area","impound","int","short","pe","water","averagewater","fs")]
v14 <- melt(data=veg14, id=c("habtype","spp","region","area","impound","round","fs"))
v14 <- v14[!is.na(v14$value),]
vr14 <- cast(data=v14, impound + region + area + fs ~ variable, fun.aggregate=mean)
vv14 <- cast(data=v14, impound + region + area + fs ~ variable + round, value="value", fun.aggregate=mean)
vvv14 <- data.frame(vr14, vv14$averagewater_2, vv14$averagewater_2, vv14$averagewater_3)
colnames(vvv14) <- c("impound","region","area","fs","int","short","pe","water","averagewater","averagewater_1","averagewater_2","averagewater_3")
vegss <- scale(vvv14[,5:ncol(vvv14)])
colnames(vegss) <- paste("scale", colnames(vegss), sep = "_")
veg14 <- cbind(vvv14, vegss)

plant12 <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/2012_plants.csv")
plant13 <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/2013_plants.csv")
plant14 <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/2014_plants.csv")

vp12 <- merge(veg12, plant12, by="impound", all=TRUE)
vp13 <- merge(veg13, plant13, by="impound", all=TRUE)
vp14 <- merge(veg14, plant14, by="impound", all=TRUE)

vp12 <- vp12[!is.na(vp12$fs),]
vp12 <- vp12[!is.na(vp12$short),]

vp13 <- vp13[!is.na(vp13$fs),]
vp13 <- vp13[!is.na(vp13$short),]

vp14 <- vp14[!is.na(vp14$fs),]
vp14 <- vp14[!is.na(vp14$short),]



## Cut down files
vp12 <- vp12[(vp12$impound %in% sora12$impound),]
vp13 <- vp13[(vp13$impound %in% sora13$impound),]
vp14 <- vp14[(vp14$impound %in% sora14$impound),]

surv12 <- s12[(s12$impound %in% vp12$impound),]
surv13 <- s13[(s13$impound %in% vp13$impound),]
surv14 <- s14[(s14$impound %in% vp14$impound),]

vs12 <- merge(vp12, surv12, by="impound",all=TRUE)
vs13 <- merge(vp13, surv13, by="impound",all=TRUE)
vs14 <- merge(vp14, surv14, by="impound",all=TRUE)

sora12 <- sora12[(sora12$impound %in% vp12$impound),]
sora13 <- sora13[(sora13$impound %in% vp13$impound),]
sora14 <- sora14[(sora14$impound %in% vp14$impound),]

### create bird input files11

write.csv(sora12, "C:/Users/avanderlaar/Documents/GitHub/data/2012_sora_multi_year_occ.csv", row.names=F)
write.csv(sora13, "C:/Users/avanderlaar/Documents/GitHub/data/2013_sora_multi_year_occ.csv", row.names=F)
write.csv(sora14, "C:/Users/avanderlaar/Documents/GitHub/data/2014_sora_multi_year_occ.csv", row.names=F)

# Create Covariate Files ----------------------------------------------------------------------------------------

write.csv(vs12, "C:/Users/avanderlaar/Documents/GitHub/data/2012_cov_multi_year_occ.csv", row.names=F)
write.csv(vs13, "C:/Users/avanderlaar/Documents/GitHub/data/2013_cov_multi_year_occ.csv", row.names=F)
write.csv(vs14, "C:/Users/avanderlaar/Documents/GitHub/data/2014_cov_multi_year_occ.csv", row.names=F)

