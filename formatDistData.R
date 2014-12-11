library(unmarked)

setwd("C:/Users/avanderlaar/Dropbox/data")
#this is a csv with a row for each unique bird observation
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



vid12r2 = intersect(clen12$impound,cveg12_all$impound)
vid12r3 = intersect(clen12$impound,cveg12_all$impound)

cveg122_all = cveg12_all[(cveg12_all$impound %in% vid12r2),]
clen122 <- clen12[(clen12$impound %in% vid12r2),]

castveg123_all = castveg12_all[(castveg12_all$impound %in% vid12r3),]
clen123 <- clen12[(clen12$impound %in% vid12r3),]

v122 <- cbind(clen122, castveg122_all)
hec122 <- hec[(hec$impound %in% v122$impound),]
v122 <- cbind(v122, hec122)
v123 <- cbind(clen123, castveg123_all)
hec123 <- hec[(hec$impound %in% v123$impound),]
v123 <- cbind(v123, hec123)

### 2013 ###
veg13 <- veg[veg$year==2013,]
meltv13v = melt(veg13[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")])
castveg13v = cast(meltv13v, impound + area+ region ~ variable, mean, fill=NA_real_,na.rm=T)
meltv13w = melt(veg13[,c("round",  "impound", "averagewater")], id=c("impound","round"))
castveg13w = cast(meltv13w, impound ~ variable + round, mean, fill=NA_real_,na.rm=T)

castveg13_all = cbind(castveg13v, castveg13w)

mlen13 <- melt(surv[surv$year==2013,], id=c("impound","round","night","year"))
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
hec131 <- hec[(hec$impound %in% v131$impound),]
v131 <- cbind(v131, hec131)

v132 <- cbind(clen132, castveg132_all)
hec132 <- hec[(hec$impound %in% v132$impound),]
v132 <- cbind(v132, hec132)

v133 <- cbind(clen133, castveg133_all)
hec133 <- hec[(hec$impound %in% v133$impound),]
v133 <- cbind(v133, hec133)

v134 <- cbind(clen134, castveg134_all)
hec134 <- hec[(hec$impound %in% v134$impound),]
v134 <- cbind(v134, hec134)

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
hec141 <- hec[(hec$impound %in% v141$impound),]
v141 <- cbind(v141, hec141)
v142 <- cbind(clen142, castveg142_all)
hec142 <- hec[(hec$impound %in% v142$impound),]
v142 <- cbind(v141, hec141)
v143 <- cbind(clen143, castveg143_all)
hec143 <- hec[(hec$impound %in% v143$impound),]
v143 <- cbind(v141, hec141)
v144 <- cbind(clen144, castveg144_all)
hec144 <- hec[(hec$impound %in% v144$impound),]
v144 <- cbind(v141, hec141)

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
setwd("C:/Users/avanderlaar/Dropbox/data")
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

