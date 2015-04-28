library(unmarked)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

birds <- read.csv("all_birds.csv",header=T) 
birds$jdate <- as.factor(birds$jdate)
birds <- birds[birds$species=="sora",] 
birds$night <- as.factor(birds$night)
dist.breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13) 
birds <- birds[birds$night!=4.2,]
birds <- birds[birds$night!=4.1,]
birds12 <- birds[birds$year==2012,]
birds12$night <- factor(birds12$night, labels=c(1,2,3))
birds13 <- birds[birds$year==2013,]
birds13$night <- factor(birds13$night, labels=c(1.1,1.2,2.1,2.2,3.1,3.2))
birds14 <- birds[birds$year==2014,]
birds14$night <- factor(birds14$night, labels=c(1.1,1.2,2.1,2.2,3.1,3.2))

gd12 <- as.data.frame(formatDistData(birds12, "distance","impound", dist.breaks, "night"))
gd13 <- as.data.frame(formatDistData(birds13, "distance", "impound", dist.breaks, "night" ))
gd14 <- as.data.frame(formatDistData(birds14, "distance", "impound", dist.breaks, "night" ))

surv <- read.csv("all_surveys.csv",header=T)
surv$jdate <- as.factor(surv$jdate)
surv <- surv[,c("year","night","round","impound","length","jdate")]

b12r1 <- gd12r1[(rownames(gd12r1) %in% surv[surv$round==1&surv$year==2012,]$impound),]
b12r2 <- gd12r2[(rownames(gd12r2) %in% surv[surv$round==2&surv$year==2012,]$impound),] 
b12r3 <- gd12r3[(rownames(gd12r3) %in% surv[surv$round==3&surv$year==2012,]$impound),] 

b13r1 <- gd13r1[(rownames(gd13r1) %in% surv[surv$round==1&surv$year==2013,]$impound),]
b13r2 <- gd13r2[(rownames(gd13r2) %in% surv[surv$round==2&surv$year==2013,]$impound),] 
b13r3 <- gd13r3[(rownames(gd13r3) %in% surv[surv$round==3&surv$year==2013,]$impound),] 
b13r4 <- gd13r4[(rownames(gd13r4) %in% surv[surv$round==4&surv$year==2013,]$impound),] 

b14r1 <- gd14r1[(rownames(gd14r1) %in% surv[surv$round==1&surv$year==2014,]$impound),] 
b14r2 <- gd14r2[(rownames(gd14r2) %in% surv[surv$round==2&surv$year==2014,]$impound),] 
b14r3 <- gd14r3[(rownames(gd14r3) %in% surv[surv$round==3&surv$year==2014,]$impound),] 
b14r4 <- gd14r4[(rownames(gd14r4) %in% surv[surv$round==4&surv$year==2014,]$impound),] 

# Covariates -----------------------------------------------
veg <- read.csv("all_veg.csv", header=T) 
hec <- read.csv('hectares.csv',header=T)

### 2012 ###
veg12 <- veg[veg$year==2012,]
vegss <- veg12[,c(13:29,33:35)]
#vegss <- scale(vegs)
veg12 <- cbind(veg12[,1:12],vegss)
meltv12v <- melt(veg12[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")])
cveg12v <- cast(meltv12v, impound + area + region~ variable, mean, fill=NA_real_,na.rm=T)
meltv12w <- melt(veg12[,c( "round",  "impound", "averagewater")], id=c("impound","round"))
cveg12w <- cast(meltv12w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)
cveg12_all <- cbind(cveg12v, cveg12w)
mlen12 <- melt(surv[surv$year==2012,], id=c("impound","round","night","year"), na.rm=T)
clen12 <- cast(mlen12, impound ~ variable + round, max, fill=NA_real_,na.rm=T)

veg12 <- cbind(cbind(clen12[(clen12$impound %in% intersect(clen12$impound,cveg12_all$impound)),], cveg12_all[(cveg12_all$impound %in% intersect(clen12$impound,cveg12_all$impound)),]),hec[(hec$impound %in% intersect(clen12$impound,cveg12_all$impound)),])

veg12r1 <- veg12[,c(1,2,5,9,10,12:22,27)]
veg12r2 <- veg12[,c(1,3,6,9,10,12:22,24,27)]
veg12r3 <- veg12[,c(1,4,7,9:10,12:22,25,27)]

veg121 <- completeFun(veg12r1, c("length_1","area"))
veg122 <- completeFun(veg12r2, c("length_2","area"))
veg123 <- completeFun(veg12r3, c("length_3","area"))


### 2013 ###
veg13 <- veg[veg$year==2013,]
vegss <- veg13[,c(13:29,33:35)]
#vegss <- scale(vegs)
veg13 <- cbind(veg13[,1:12],vegss)
meltv13v = melt(veg13[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")])
castveg13v = cast(meltv13v, impound + area+ region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv13w = melt(veg13[,c("round",  "impound", "averagewater")], id=c("impound","round"))
castveg13w = cast(meltv13w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)
castveg13_all = cbind(castveg13v, castveg13w)
mlen13 <- melt(surv[surv$year==2013,], id=c("impound","round","night","year"), na.rm=T)
clen13 <- cast(mlen13, impound ~ variable + round, max, fill=NA_real_,na.rm=T)

veg13 <- cbind(cbind(clen13[(clen13$impound %in% intersect(clen13$impound,castveg13_all$impound)),], castveg13_all[(castveg13_all$impound %in% intersect(clen13$impound,castveg13_all$impound)),]), hec[(hec$impound %in% intersect(clen13$impound,castveg13_all$impound)),])

veg13r1 <- veg13[,c(1,2,6,11,12,14:24,26,31)]
veg13r2 <- veg13[,c(1,3,7,11,12,14:24,27,31)]
veg13r3 <- veg13[,c(1,4,8,11,12,14:24,28,31)]
veg13r4 <- veg13[,c(1,5,9,11,12,14:24,29,31)]

veg131 <- completeFun(veg13r1, c("length_1","area"))
veg132 <- completeFun(veg13r2, c("length_2","area"))
veg133 <- completeFun(veg13r3, c("length_3","area"))
veg134 <- completeFun(veg13r4, c("length_4","area"))

### 2014 ###
v14 <- veg[veg$year==2014&veg$averagewater<900,]
vegss <- v14[,c(13:29,33:35)]
#vegss <- scale(vegs)
v14 <- cbind(v14[,1:12],vegss)
v14$treat[v14$impound=="sanctuary"|v14$impound=="scmsu2"|v14$impound=="pool2w"|v14$impound=="m10"|v14$impound=="ts2a"|v14$impound=="ts4a"|v14$impound=="ccmsu12"|v14$impound=="kt9"|v14$impound=="dc22"|v14$impound=="os23"|v14$impound=="pool i"|v14$impound=="pooli"|v14$impound=="ash"|v14$impound=="sgb"|v14$impound=="scmsu3"|v14$impound=="m11"|v14$impound=="kt2"|v14$impound=="kt6"|v14$impound=="r7"|v14$impound=="poolc"|v14$impound=="pool c"]<-"E"
v14$treat[v14$impound=="sgd"|v14$impound=="rail"|v14$impound=="pool2"|v14$impound=="m13"|v14$impound=="ts6a"|v14$impound=="kt5"|v14$impound=="dc14"|v14$impound=="os21"|v14$impound=="pool e"|v14$impound=="poole"|v14$impound=="r3"|v14$impound=="dc20"|v14$impound=="dc18"|v14$impound=="ccmsu2"|v14$impound=="ccmsu1"|v14$impound=="ts8a"|v14$impound=="pool3w"]<-"L"
v14$woodp = ifelse(v14$wood>0,1,0)
v14$waterp = ifelse(v14$averagewater>0,1,0)
meltv14v = melt(v14[,c( "region","round","impound", "area", "int", "treat", "short","pe", "wood")],id=c("impound","round","treat","region","area"), na.rm=T)
castveg14v = cast(meltv14v, impound + area+  treat + region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv14w = melt(na.omit(v14[,c( "impound","round", "averagewater")]),id=c("impound","round"), na.rm=T)
castveg14w = cast(meltv14w, impound ~ variable + round ,na.rm=T, mean, fill=NA_real_)
castveg14_all <- cbind(castveg14v, castveg14w)
mlen14 <- melt(surv[surv$year==2014,], id=c("impound","round","night","year"),na.rm=T)
clen14 <- cast(mlen14, impound ~ variable + round, max, fill=NA_real_)

veg14 <- cbind(cbind(clen14[(clen14$impound %in% intersect(clen14$impound,castveg14_all$impound)),], castveg14_all[(castveg14_all$impound %in% intersect(clen14$impound,castveg14_all$impound)),]), hec[(hec$impound %in% intersect(clen14$impound,castveg14_all$impound)),])

veg14 <- veg14[,c(1,2,6,11:17,19,24)]

veg14 <- completeFun(veg14, c("length_1","area"))


# Create the Sora Input Files -----------------------------------------------------------------------------
b12 <- cbind(impound=rownames(b12),b12)
mmerge12 <- b12[(b12$impound %in% intersect(b12$impound,veg12$impound)),]
write.csv(mmerge12, "2012_sora.csv")

b13 <- cbind(impound=rownames(b13),b13)
mmerge12r2 <- b13[(b13$impound %in% intersect(b13$impound,veg13$impound)),]
write.csv(mmerge13, "2013_sora.csv")

b14 <- cbind(impound=rownames(b14),b14)
mmerge14 <- b14[(b14$impound %in% intersect(b14$impound,veg14$impound)),]
write.csv(mmerge14, "2014_sora.csv", row.names=F)



# Create Covariate Files ----------------------------------------------------------------------------------------


write.csv(veg12[(veg12$impound %in% intersect(b12$impound,veg12$impound)),], "2012_cov_nostand.csv", row.names=F)
write.csv(veg13[(veg13$impound %in% intersect(b13$impound,veg13$impound)),], "2013_cov_nostand.csv", row.names=F)
write.csv(veg14[(veg14$impound %in% intersect(b14$impound,veg14$impound)),], "2014_cov_nostand.csv", row.names=F)

