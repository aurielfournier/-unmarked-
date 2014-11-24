library(unmarked)

setwd("C:/Users/avanderlaar/Dropbox/data")
birds <- read.csv("all_birds.csv",header=T) #this is a csv with a row for each unique bird observation, 
#important columns are the impoundment where the bird was observed
# the distance from the line the bird was observed at
# and the night (in unmarked terms 'occasion') the survey occured on. 
# I have other crap in this file too, but it's not needed for this analysis

birds <- birds[birds$species=="sora",] #we don't have enough observations of the other rail spp, so we're just focusing on Sora
birds12 <- birds[birds$year==2012,]
birds12$night <- as.factor(birds12$night) #in 2012 we only did one survey at a night per night, so all the nights are "1.1 or 2.1 etc)
#birds12r1 doesn't exist, for a wide variety of reasons we won't get into here
birds12r2 <- birds12[birds12$round==2,] #each round is a two week period during which all the sites were surveyed, 
birds12r3 <- birds12[birds12$round==3,]
#birds12r4 doesn't exist, because we only visited sites three times in 2012
birds13 <- birds[birds$year==2013,]
birds13$night <- as.factor(birds13$night) #starting in 2013 we did two surveys at a night, so now we have 1.1, 1.2, 2.1. 2.2, etc. 
birds13r1 <- birds13[birds13$round==1,]
birds13r2 <- birds13[birds13$round==2,]
birds13r3 <- birds13[birds13$round==3,]
birds13r4 <- birds13[birds13$round==4,]
birds14 <- birds[birds$year==2014,]
birds14$night <- as.factor(birds14$night)
birds14r1 <- birds14[birds14$round==1,]
birds14r2 <- birds14[birds14$round==2,]
birds14r3 <- birds14[birds14$round==3,]
birds14r4 <- birds14[birds14$round==4,]

dist.breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13) #our greatest distance is 13  

#remember 2012 round 1 doesn't exisit
gd12r2 <- formatDistData(birds12r2, "distance", "impound", dist.breaks, "night" )
gd12r2 <- as.data.frame(cbind(rownames(gd12r2),gd12r2))
#so the file is the subset we just did above
# "distance" is the column name where the distance of the bird from the line is stored
# "impound" is my unique identifer for the transect ("impound" means wetland impoundment)
# dist.breaks is the vector we created above that has my distance breaks in it
# "night" is my occasion marker, which delineates the repeat surveys for each impound, 
gd12r3 <- formatDistData(birds12r3, "distance", "impound", dist.breaks, "night" )
gd12r3 <- as.data.frame(cbind(rownames(gd12r3),gd12r3))
#also 2012 round 4 doesn't exisit

gd13r1 <- formatDistData(birds13r1, "distance", "impound", dist.breaks, "night" )
gd13r1 <- as.data.frame(cbind(rownames(gd13r1),gd13r1))
gd13r2 <- formatDistData(birds13r2, "distance", "impound", dist.breaks, "night" )
gd13r2 <- as.data.frame(cbind(rownames(gd13r2),gd13r2))
gd13r3 <- formatDistData(birds13r3, "distance", "impound", dist.breaks, "night" )
gd13r3 <- as.data.frame(cbind(rownames(gd13r3),gd13r3))
gd13r4 <- formatDistData(birds13r4, "distance", "impound", dist.breaks, "night" )
gd13r4 <- as.data.frame(cbind(rownames(gd13r4),gd13r4))

gd14r1 <- formatDistData(birds14r1, "distance", "impound", dist.breaks, "night" )
gd14r1 <- as.data.frame(cbind(rownames(gd14r1),gd14r1))
gd14r2 <- formatDistData(birds14r2, "distance", "impound", dist.breaks, "night" )
gd14r2 <- as.data.frame(cbind(rownames(gd14r2),gd14r2))
gd14r3 <- formatDistData(birds14r3, "distance", "impound", dist.breaks, "night" )
gd14r3 <- as.data.frame(cbind(rownames(gd14r3),gd14r3))
gd14r4 <- formatDistData(birds14r4, "distance", "impound", dist.breaks, "night" )
gd14r4 <- as.data.frame(cbind(rownames(gd14r4),gd14r4))


#now these are in the format they need to be in to go into gdistsamp, which is nifty. 
# I have to do some additional monekying around with them because I have some surveys with no observations, 
#which arent' currently in here, and ther are also impoundments listed in these files that we didn't survey every time


surv <- read.csv("survey_all.csv",header=T)
surv <- surv[,c("year","night","round","impound","length")]

surv12 <- surv[surv$year==2012,]
#no 12r1
surv122 <- surv12[surv12$round==2,]
surv123 <- surv12[surv12$round==3,]
#no 12r4

surv13 <- surv[surv$year==2013,]
surv131 <- surv13[surv13$round==1,]
surv132 <- surv13[surv13$round==2,]
surv133 <- surv13[surv13$round==3,]
surv134 <- surv13[surv13$round==4,]

surv14 <- surv[surv$year==2014,]
surv141 <- surv14[surv14$round==1,]
surv142 <- surv14[surv14$round==2,]
surv143 <- surv14[surv14$round==3,]
surv144 <- surv14[surv14$round==4,]

#no 12r1
b12r2 <- gd12r2[(gd12r2$V1 %in% surv122$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b12r3 <- gd12r3[(gd12r3$V1 %in% surv123$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
#no 12r4
b13r1 <- gd13r1[(gd13r1$V1 %in% surv131$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b13r2 <- gd13r2[(gd13r2$V1 %in% surv132$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b13r3 <- gd13r3[(gd13r3$V1 %in% surv133$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b13r4 <- gd13r4[(gd13r4$V1 %in% surv134$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round

b14r1 <- gd14r1[(gd14r1$V1 %in% surv141$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b14r2 <- gd14r2[(gd14r2$V1 %in% surv142$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b14r3 <- gd14r3[(gd14r3$V1 %in% surv143$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round
b14r4 <- gd14r4[(gd14r4$V1 %in% surv144$impound),] #this makes it so that we only have the impoundments that were surveyed during that year and round


########################################################
##### Covariates (in my case, vegetation variables)
#########################################################
setwd("C:/Users/avanderlaar/Dropbox/R/Veg_Summary")
veg = read.csv("masterveg.csv", header=T) #a file where each row is a unique vegetation point, collected at a certain time 
#(points are resampled throughout the season)


### 2012 ###
veg12 <- veg[veg$year==2012,]
veg12v <- veg12[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")]
veg12w <- veg12[,c( "round",  "impound", "averagewater")]
meltv12v = melt(veg12v)
castveg12v = cast(meltv12v, impound + region~ variable, mean, fill=NA_real_,na.rm=T)
meltv12w = melt(veg12w, id=c("impound","round"))
castveg12w = cast(meltv12w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)
castveg12_all = cbind(castveg12v, castveg12w)

mlen12 <- melt(surv12, id=c("impound","round","night","year"))
clen12 <- cast(mlen12, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)

#remember no 12r1 or 12r4
vid12r2 = intersect(clen12$impound,castveg12_all$impound)
vid12r3 = intersect(clen12$impound,castveg12_all$impound)

castveg122_all = castveg12_all[(castveg12_all$impound %in% vid12r2),]
clen122 <- clen12[(clen12$impound %in% vid12r2),]

castveg123_all = castveg12_all[(castveg12_all$impound %in% vid12r3),]
clen123 <- clen12[(clen12$impound %in% vid12r3),]

v122 <- cbind(clen122, castveg122_all)
v123 <- cbind(clen123, castveg123_all)

### 2013 ###
veg13 <- veg[veg$year==2013,]
veg13v <- veg13[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")]
veg13w <- veg13[,c( "round",  "impound", "averagewater")]
meltv13v = melt(veg13v)
castveg13v = cast(meltv13v, impound + region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv13w = melt(veg13w, id=c("impound","round"))
castveg13w = cast(meltv13w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)
castveg13_all = cbind(castveg13v, castveg13w)


mlen13 <- melt(surv13, id=c("impound","round","night","year"))
clen13 <- cast(mlen13, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)

vid13r1 = intersect(clen13$impound,castveg13_all$impound)
vid13r2 = intersect(clen13$impound,castveg13_all$impound)
vid13r3 = intersect(clen13$impound,castveg13_all$impound)
vid13r4 = intersect(clen13$impound,castveg13_all$impound)

castveg131_all = castveg13_all[(castveg13_all$impound %in% vid13r1),]
clen131 <- clen13[(clen13$impound %in% vid13r1),]

castveg132_all = castveg13_all[(castveg13_all$impound %in% vid13r2),]
clen132 <- clen13[(clen13$impound %in% vid13r2),]

castveg133_all = castveg13_all[(castveg13_all$impound %in% vid13r3),]
clen133 <- clen13[(clen13$impound %in% vid13r3),]

castveg134_all = castveg13_all[(castveg13_all$impound %in% vid13r4),]
clen134 <- clen13[(clen13$impound %in% vid13r4),]

v131 <- cbind(clen131, castveg131_all)
v132 <- cbind(clen132, castveg132_all)
v133 <- cbind(clen133, castveg133_all)
v134 <- cbind(clen134, castveg134_all)

### 2014 ###
v14 <- veg[veg$year==2014,]

v14$treat[v14$impound=="sanctuary"|v14$impound=="scmsu2"|v14$impound=="pool2w"|v14$impound=="m10"|v14$impound=="ts2a"|v14$impound=="ts4a"|v14$impound=="ccmsu12"|v14$impound=="kt9"|v14$impound=="dc22"|v14$impound=="os23"|v14$impound=="pool i"|v14$impound=="pooli"|v14$impound=="ash"|v14$impound=="sgb"|v14$impound=="scmsu3"|v14$impound=="m11"|v14$impound=="kt2"|v14$impound=="kt6"|v14$impound=="r7"|v14$impound=="poolc"|v14$impound=="pool c"]<-"E"

v14$treat[v14$impound=="sgd"|v14$impound=="rail"|v14$impound=="pool2"|v14$impound=="m13"|v14$impound=="ts6a"|v14$impound=="kt5"|v14$impound=="dc14"|v14$impound=="os21"|v14$impound=="pool e"|v14$impound=="poole"|v14$impound=="r3"|v14$impound=="dc20"|v14$impound=="dc18"|v14$impound=="ccmsu2"|v14$impound=="ccmsu1"|v14$impound=="ts8a"|v14$impound=="pool3w"]<-"L"

v14v = v14[,c( "region","round","impound", "area", "int", "treat", "short","pe", "wood")]
v14w = v14[,c( "impound","round", "averagewater")]
v14w <- v14w[v14w$averagewater<900,]
v14w <- na.omit(v14w)
v14v$woodp = ifelse(v14$wood>0,1,0)
v14w$waterp = ifelse(v14w$averagewater>0,1,0)
meltv14v = melt(v14v,id=c("impound","round","treat","region"), na.rm=T)
castveg14v = cast(meltv14v, impound + treat + region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv14w = melt(v14w,id=c("impound","round"), na.rm=T)
castveg14w = cast(meltv14w, impound ~ variable + round ,na.rm=T, mean, fill=NA_real_)
castveg14_all <- cbind(castveg14v, castveg14w)

mlen14 <- melt(surv14, id=c("impound","round","night","year"))
clen14 <- cast(mlen14, impound ~ variable + round, mean, fill=NA_real_)

vid14r1 = intersect(clen14$impound,castveg14_all$impound)
vid14r2 = intersect(clen14$impound,castveg14_all$impound)
vid14r3 = intersect(clen14$impound,castveg14_all$impound)
vid14r4 = intersect(clen14$impound,castveg14_all$impound)

castveg141_all = castveg14_all[(castveg14_all$impound %in% vid14r1),]
clen141 <- clen14[(clen14$impound %in% vid14r1),]

castveg142_all = castveg14_all[(castveg14_all$impound %in% vid14r2),]
clen142 <- clen14[(clen14$impound %in% vid14r2),]

castveg143_all = castveg14_all[(castveg14_all$impound %in% vid14r3),]
clen143 <- clen14[(clen14$impound %in% vid14r3),]

castveg144_all = castveg14_all[(castveg14_all$impound %in% vid14r4),]
clen144 <- clen14[(clen14$impound %in% vid14r4),]

v141 <- cbind(clen141, castveg141_all)
v142 <- cbind(clen142, castveg142_all)
v143 <- cbind(clen143, castveg143_all)
v144 <- cbind(clen144, castveg144_all)

id12r2 = intersect(b12r2$V1,v122$impound)
id12r3 = intersect(b12r3$V1,v123$impound)
id13r1 = intersect(b13r1$V1,v131$impound)
id13r2 = intersect(b13r2$V1,v132$impound)
id13r3 = intersect(b13r3$V1,v133$impound)
id13r4 = intersect(b13r4$V1,v134$impound)
id14r1 = intersect(b14r1$V1,v141$impound)
id14r2 = intersect(b14r2$V1,v142$impound)
id14r3 = intersect(b14r3$V1,v143$impound)
id14r4 = intersect(b14r4$V1,v144$impound)


#create sora files
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")
mmerge12r2 <- b12r2[(b12r2$V1 %in% id12r2),]
colnames(mmerge12r2)[1] <- "impound"
write.csv(mmerge12r2, "2012r2_sora.csv")

mmerge12r3 <- b12r3[(b12r3$V1 %in% id12r3),]
colnames(mmerge12r3)[1] <- "impound"
write.csv(mmerge12r3, "2012r3_sora.csv", row.names=F)

mmerge13r1 <- b13r1[(b13r1$V1 %in% id13r1),]
colnames(mmerge13r1)[1] <- "impound"
write.csv(mmerge13r1, "2013r1_sora.csv", row.names=F)

mmerge13r2 <- b13r2[(b13r2$V1 %in% id13r2),]
colnames(mmerge13r2)[1] <- "impound"
write.csv(mmerge13r2, "2013r2_sora.csv", row.names=F)

mmerge13r3 <- b13r3[(b13r3$V1 %in% id13r3),]
colnames(mmerge13r3)[1] <- "impound"
write.csv(mmerge13r3, "2013r3_sora.csv", row.names=F)

mmerge13r4 <- b13r4[(b13r4$V1 %in% id13r4),]
colnames(mmerge13r4)[1] <- "impound"
write.csv(mmerge13r4, "2013r4_sora.csv", row.names=F)

mmerge14r1 <- b14r1[(b14r1$V1 %in% id14r1),]
colnames(mmerge14r1)[1] <- "impound"
write.csv(mmerge14r1, "2014r1_sora.csv", row.names=F)

mmerge14r2 <- b14r2[(b14r2$V1 %in% id14r2),]
colnames(mmerge14r2)[1] <- "impound"
write.csv(mmerge14r2, "2014r2_sora.csv", row.names=F)

mmerge14r3 <- b14r3[(b14r3$V1 %in% id14r3),]
colnames(mmerge14r3)[1] <- "impound"
write.csv(mmerge14r3, "2014r3_sora.csv", row.names=F)

mmerge14r4 <- b14r4[(b14r4$V1 %in% id14r4),]
colnames(mmerge14r4)[1] <- "impound"
write.csv(mmerge14r4, "2014r4_sora.csv", row.names=F)


#create covariate files
v122 <- v122[(v122$impound %in% id12r2),]
write.csv(v122, "2012r2_cov.csv", row.names=F)
v123 <- v123[(v123$impound %in% id12r3),]
write.csv(v123, "2012r3_cov.csv", row.names=F)
v131 <- v131[(v131$impound %in% id13r1),]
write.csv(v131, "2013r1_cov.csv", row.names=F)
v132 <- v132[(v132$impound %in% id13r2),]
write.csv(v132, "2013r2_cov.csv", row.names=F)
v133 <- v133[(v133$impound %in% id13r3),]
write.csv(v133, "2013r3_cov.csv", row.names=F)
v134 <- v134[(v134$impound %in% id13r4),]
write.csv(v134, "2013r4_cov.csv", row.names=F)
v141 <- v141[(v141$impound %in% id14r1),]
write.csv(v141, "2014r1_cov.csv", row.names=F)
v142 <- v142[(v142$impound %in% id14r2),]
write.csv(v142, "2014r2_cov.csv", row.names=F)
v143 <- v143[(v143$impound %in% id14r3),]
write.csv(v143, "2014r3_cov.csv", row.names=F)
v144 <- v144[(v144$impound %in% id14r4),]
write.csv(v144, "2014r4_cov.csv", row.names=F)

