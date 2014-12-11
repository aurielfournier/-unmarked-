library(unmarked)

setwd("C:/Users/avanderlaar/Dropbox/data")

birds <- read.csv("all_birds.csv",header=T) 
birds <- birds[birds$species=="sora",] 
birds$night <- as.factor(birds$night)
dist.breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13) 
gd12r2 <- as.data.frame(formatDistData(birds[birds$round==2&birds$year==2012,], "distance", "impound", dist.breaks, "night" ))
gd12r3 <- as.data.frame(formatDistData(birds[birds$round==3&birds$year==2012,], "distance", "impound", dist.breaks, "night" ))
gd13r1 <- as.data.frame(formatDistData(birds[birds$round==1&birds$year==2013,], "distance", "impound", dist.breaks, "night" ))
gd13r2 <- as.data.frame(formatDistData(birds[birds$round==2&birds$year==2013,], "distance", "impound", dist.breaks, "night" ))
gd13r3 <- as.data.frame(formatDistData(birds[birds$round==3&birds$year==2013,], "distance", "impound", dist.breaks, "night" ))
gd13r4 <- as.data.frame(formatDistData(birds[birds$round==4&birds$year==2013,], "distance", "impound", dist.breaks, "night" ))
gd14r1 <- as.data.frame(formatDistData(birds[birds$round==1&birds$year==2014,], "distance", "impound", dist.breaks, "night" ))
gd14r2 <- as.data.frame(formatDistData(birds[birds$round==2&birds$year==2014,], "distance", "impound", dist.breaks, "night" ))
gd14r3 <- as.data.frame(formatDistData(birds[birds$round==3&birds$year==2014,], "distance", "impound", dist.breaks, "night" ))
gd14r4 <- as.data.frame(formatDistData(birds[birds$round==4&birds$year==2014,], "distance", "impound", dist.breaks, "night" ))

surv <- read.csv("all_surveys.csv",header=T)
surv <- surv[,c("year","night","round","impound","length","jdate")]

b12r2 <- gd12r2[(rownames(gd12r2) %in% surv122$impound),] 
b12r3 <- gd12r3[(rownames(gd12r3) %in% surv123$impound),] 

b13r1 <- gd13r1[(rownames(gd13r1) %in% surv131$impound),]
b13r2 <- gd13r2[(rownames(gd13r2) %in% surv132$impound),] 
b13r3 <- gd13r3[(rownames(gd13r3) %in% surv133$impound),] 
b13r4 <- gd13r4[(rownames(gd13r4) %in% surv134$impound),] 

b14r1 <- gd14r1[(rownames(gd14r1) %in% surv141$impound),] 
b14r2 <- gd14r2[(rownames(gd14r2) %in% surv142$impound),] 
b14r3 <- gd14r3[(rownames(gd14r3) %in% surv143$impound),] 
b14r4 <- gd14r4[(rownames(gd14r4) %in% surv144$impound),] 

# Covariates -----------------------------------------------
veg <- read.csv("all_veg.csv", header=T) 
hec <- read.csv('hectares.csv',header=T)

### 2012 ###
veg12 <- veg[veg$year==2012,]
meltv12v <- melt(veg12[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")])
cveg12v <- cast(meltv12v, impound + area + region~ variable, mean, fill=NA_real_,na.rm=T)
meltv12w <- melt(veg12[,c( "round",  "impound", "averagewater")], id=c("impound","round"))
cveg12w <- cast(meltv12w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)
cveg12_all <- cbind(cveg12v, cveg12w)
mlen12 <- melt(surv[surv$year==2012,], id=c("impound","round","night","year"))
clen12 <- cast(mlen12, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)

v122 <- cbind(cbind(clen12[(clen12$impound %in% intersect(clen12$impound,cveg12_all$impound)),], cveg12_all[(cveg12_all$impound %in% intersect(clen12$impound,cveg12_all$impound)),]), hec[(hec$impound %in% clen122$impound),])
v123 <- cbind(cbind(clen12[(clen12$impound %in% intersect(clen12$impound,cveg12_all$impound)),], cveg12_all[(cveg12_all$impound %in% intersect(clen12$impound,cveg12_all$impound)),]), hec[(hec$impound %in% clen123$impound),])

### 2013 ###
veg13 <- veg[veg$year==2013,]
meltv13v = melt(veg13[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")])
castveg13v = cast(meltv13v, impound + area+ region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv13w = melt(veg13[,c("round",  "impound", "averagewater")], id=c("impound","round"))
castveg13w = cast(meltv13w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)
castveg13_all = cbind(castveg13v, castveg13w)
mlen13 <- melt(surv[surv$year==2013,], id=c("impound","round","night","year"))
clen13 <- cast(mlen13, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)

v131 <- cbind(cbind(clen13[(clen13$impound %in% intersect(clen13$impound,castveg13_all$impound)),], castveg13_all[(castveg13_all$impound %in% intersect(clen13$impound,castveg13_all$impound)),]), hec[(hec$impound %in% v131$impound),])
v132 <- cbind(cbind(clen13[(clen13$impound %in% intersect(clen13$impound,castveg13_all$impound)),], castveg13_all[(castveg13_all$impound %in% intersect(clen13$impound,castveg13_all$impound)),]), hec[(hec$impound %in% v132$impound),]) 
v133 <- cbind(cbind(clen13[(clen13$impound %in% intersect(clen13$impound,castveg13_all$impound)),], castveg13_all[(castveg13_all$impound %in% intersect(clen13$impound,castveg13_all$impound)),]), hec[(hec$impound %in% v133$impound),])
v134 <- cbind(cbind(clen13[(clen13$impound %in% intersect(clen13$impound,castveg13_all$impound)),], castveg13_all[(castveg13_all$impound %in% intersect(clen13$impound,castveg13_all$impound)),]), hec[(hec$impound %in% v134$impound),])

### 2014 ###
v14 <- veg[veg$year==2014&veg$averagewater<900,]
v14$treat[v14$impound=="sanctuary"|v14$impound=="scmsu2"|v14$impound=="pool2w"|v14$impound=="m10"|v14$impound=="ts2a"|v14$impound=="ts4a"|v14$impound=="ccmsu12"|v14$impound=="kt9"|v14$impound=="dc22"|v14$impound=="os23"|v14$impound=="pool i"|v14$impound=="pooli"|v14$impound=="ash"|v14$impound=="sgb"|v14$impound=="scmsu3"|v14$impound=="m11"|v14$impound=="kt2"|v14$impound=="kt6"|v14$impound=="r7"|v14$impound=="poolc"|v14$impound=="pool c"]<-"E"
v14$treat[v14$impound=="sgd"|v14$impound=="rail"|v14$impound=="pool2"|v14$impound=="m13"|v14$impound=="ts6a"|v14$impound=="kt5"|v14$impound=="dc14"|v14$impound=="os21"|v14$impound=="pool e"|v14$impound=="poole"|v14$impound=="r3"|v14$impound=="dc20"|v14$impound=="dc18"|v14$impound=="ccmsu2"|v14$impound=="ccmsu1"|v14$impound=="ts8a"|v14$impound=="pool3w"]<-"L"
v14$woodp = ifelse(v14$wood>0,1,0)
v14$waterp = ifelse(v14$averagewater>0,1,0)
meltv14v = melt(v14[,c( "region","round","impound", "area", "int", "treat", "short","pe", "wood")],id=c("impound","round","treat","region","area"), na.rm=T)
castveg14v = cast(meltv14v, impound + area+  treat + region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv14w = melt(na.omit(v14[,c( "impound","round", "averagewater")]),id=c("impound","round"), na.rm=T)
castveg14w = cast(meltv14w, impound ~ variable + round ,na.rm=T, mean, fill=NA_real_)
castveg14_all <- cbind(castveg14v, castveg14w)

mlen14 <- melt(surv[surv$year==2014,], id=c("impound","round","night","year"))
clen14 <- cast(mlen14, impound ~ variable + round, mean, fill=NA_real_)

v141 <- cbind(cbind(clen14[(clen14$impound %in% vid14r1),], castveg14_all[(castveg14_all$impound %in% intersect(clen14$impound,castveg14_all$impound)),]), hec[(hec$impound %in% v141$impound),])
v142 <- cbind(cbind(clen14[(clen14$impound %in% vid14r2),], castveg14_all[(castveg14_all$impound %in% intersect(clen14$impound,castveg14_all$impound)),]), hec[(hec$impound %in% v142$impound),])
v143 <- cbind(cbind(clen14[(clen14$impound %in% vid14r3),], castveg14_all[(castveg14_all$impound %in% intersect(clen14$impound,castveg14_all$impound)),]), hec[(hec$impound %in% v143$impound),])
v144 <- cbind(cbind(clen14[(clen14$impound %in% vid14r4),], castveg14_all[(castveg14_all$impound %in% intersect(clen14$impound,castveg14_all$impound)),]), hec[(hec$impound %in% v144$impound),])

# Create the Sora Input Files -----------------------------------------------------------------------------
mmerge12r2 <- b12r2[(b12r2$V1 %in% intersect(b12r2$V1,v122$impound)),]
colnames(mmerge12r2)[1] <- "impound"
write.csv(mmerge12r2, "2012r2_sora.csv")

mmerge12r3 <- b12r3[(b12r3$V1 %in% intersect(b12r3$V1,v123$impound)),]
colnames(mmerge12r3)[1] <- "impound"
write.csv(mmerge12r3, "2012r3_sora.csv", row.names=F)

mmerge13r1 <- b13r1[(b13r1$V1 %in% intersect(b13r1$V1,v131$impound)),]
colnames(mmerge13r1)[1] <- "impound"
write.csv(mmerge13r1, "2013r1_sora.csv", row.names=F)

mmerge13r2 <- b13r2[(b13r2$V1 %in%  intersect(b13r2$V1,v132$impound)),]
colnames(mmerge13r2)[1] <- "impound"
write.csv(mmerge13r2, "2013r2_sora.csv", row.names=F)

mmerge13r3 <- b13r3[(b13r3$V1 %in% intersect(b13r3$V1,v133$impound)),]
colnames(mmerge13r3)[1] <- "impound"
write.csv(mmerge13r3, "2013r3_sora.csv", row.names=F)

mmerge13r4 <- b13r4[(b13r4$V1 %in% intersect(b13r4$V1,v134$impound)),]
colnames(mmerge13r4)[1] <- "impound"
write.csv(mmerge13r4, "2013r4_sora.csv", row.names=F)

mmerge14r1 <- b14r1[(b14r1$V1 %in% intersect(b14r1$V1,v141$impound)),]
colnames(mmerge14r1)[1] <- "impound"
write.csv(mmerge14r1, "2014r1_sora.csv", row.names=F)

mmerge14r2 <- b14r2[(b14r2$V1 %in% intersect(b14r2$V1,v142$impound)),]
colnames(mmerge14r2)[1] <- "impound"
write.csv(mmerge14r2, "2014r2_sora.csv", row.names=F)

mmerge14r3 <- b14r3[(b14r3$V1 %in% intersect(b14r3$V1,v143$impound)),]
colnames(mmerge14r3)[1] <- "impound"
write.csv(mmerge14r3, "2014r3_sora.csv", row.names=F)

mmerge14r4 <- b14r4[(b14r4$V1 %in% intersect(b14r4$V1,v144$impound)),]
colnames(mmerge14r4)[1] <- "impound"
write.csv(mmerge14r4, "2014r4_sora.csv", row.names=F)

# Create Covariate Files ----------------------------------------------------------------------------------------

write.csv(v122[(v122$impound %in% intersect(b12r2$V1,v122$impound)),], "2012r2_cov.csv", row.names=F)
write.csv( v123[(v123$impound %in% intersect(b12r3$V1,v123$impound)),], "2012r3_cov.csv", row.names=F)
write.csv(v131[(v131$impound %in% intersect(b13r1$V1,v131$impound)),], "2013r1_cov.csv", row.names=F)
write.csv(v132[(v132$impound %in% intersect(b13r2$V1,v132$impound)),], "2013r2_cov.csv", row.names=F)
write.csv(v133[(v133$impound %in% intersect(b13r3$V1,v133$impound)),], "2013r3_cov.csv", row.names=F)
write.csv(v134[(v134$impound %in% intersect(b13r4$V1,v134$impound)),], "2013r4_cov.csv", row.names=F)
write.csv(v141[(v141$impound %in% intersect(b14r1$V1,v141$impound)),], "2014r1_cov.csv", row.names=F)
write.csv(v142[(v142$impound %in% intersect(b14r2$V1,v142$impound)),], "2014r2_cov.csv", row.names=F)
write.csv(v143[(v143$impound %in% intersect(b14r3$V1,v143$impound)),], "2014r3_cov.csv", row.names=F)
write.csv(v144[(v144$impound %in% intersect(b14r4$V1,v144$impound)),], "2014r4_cov.csv", row.names=F)

