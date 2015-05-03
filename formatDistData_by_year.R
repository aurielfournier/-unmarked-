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

format_dist_data_by_round <- function(data, year){
  rounds <- unique(data$round)
  lis <- list()
  for(i in rounds){
    lis[[i]] <- as.data.frame(formatDistData(data[data$round==i,], "distance","impound",dist.breaks,"night"))
  }
  assign(paste("gd",year,sep=""),lis)
  }

gd2012 <- format_dist_data_by_round(birds12, 2012)
gd2013 <- format_dist_data_by_round(birds13, 2013)
gd2014 <- format_dist_data_by_round(birds14, 2014)

surv <- read.csv("all_surveys.csv",header=T)
surv$jdate <- as.factor(surv$jdate)
surv <- surv[,c("year","night","round","impound","length","jdate")]

list2012 <- list()
for(i in 1:3){
    bird <- gd2012[[i]]
    list2012[[i]] <- bird[(rownames(bird) %in% surv[surv$round==i&surv$year==2012,]$impound),]
}

list2013 <- list()
for(i in 1:4){
  bird <- gd2013[[i]]
  list2013[[i]] <- bird[(rownames(bird) %in% surv[surv$round==i&surv$year==2013,]$impound),]
}

list2014 <- list()
for(i in 1:4){
  bird <- gd2013[[i]]
  list2014[[i]] <- bird[(rownames(bird) %in% surv[surv$round==i&surv$year==2014,]$impound),]
}

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
veg12$awater1 <- NA
veg12r1 <- veg12[,c(1,2,5,9,10,12:22,28,27)]
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

veg14r1 <- veg14[,c(1,2,6,11:17,19,24)]
veg14r2 <- veg14[,c(1,3,7,11:17,20,24)]
veg14r3 <- veg14[,c(1,4,8,11:17,21,24)]
veg14r4 <- veg14[,c(1,5,9,11:17,22,24)]

veg14r1c <- completeFun(veg14r1, c("length_1","area"))
veg14r2c <- completeFun(veg14r2, c("length_2","area"))
veg14r3c <- completeFun(veg14r3, c("length_3","area"))
veg14r4c <- completeFun(veg14r4, c("length_4","area"))

# Create the Sora Input Files -----------------------------------------------------------------------------
s12 <- list()
for(i in 1:3){
  bird <- list2012[[i]]
  s <- cbind(impound=rownames(bird),bird)
  s12[[i]] <- s[(s$impound %in% intersect(s$impound, veg12r1$impound)),]
}
sora12 <- do.call(rbind, s12)
write.csv(sora12, "2012_sora.csv", row.names=F)

s13 <- list()
for(i in 1:3){
  bird <- list2013[[i]]
  s <- cbind(impound=rownames(bird),bird)
  s13[[i]] <- s[(s$impound %in% intersect(s$impound, veg13r1$impound)),]
}
sora13 <- do.call(rbind, s13)
write.csv(sora13, "2013_sora.csv", row.names=F)

s14 <- list()
for(i in 1:3){
  bird <- list2014[[i]]
  s <- cbind(impound=rownames(bird),bird)
  s14[[i]] <- s[(s$impound %in% intersect(s$impound, veg14r1$impound)),]
}
sora14 <- do.call(rbind, s14)
write.csv(sora14, "2014_sora.csv", row.names=F)

# Create Covariate Files ----------------------------------------------------------------------------------------
names <- c("impound","length","jdate","area","region","int","short","tall","up","water","wood","bg","other","crop","waterp","woodp","hectares")
colnames(veg121) <- names
colnames(veg122) <- names
colnames(veg123) <- names


write.csv(rbind(veg121, veg122, veg123), "2012_cov_nostand.csv", row.names=F)

names <- c("impound","length","jdate","area","region","int","short","tall","up","water","wood","bg","other","crop","waterp","woodp","awater","hectares")
colnames(veg131) <- names
colnames(veg132) <- names
colnames(veg133) <- names
colnames(veg134) <- names

write.csv(rbind(veg131, veg132, veg133, veg134), "2013_cov_nostand.csv", row.names=F)

names <- c("impound","length","jdate","area","treat","region","int","short","pe","wood","awater","hectares")

colnames(veg14r1c) <- names
colnames(veg14r2c) <- names
colnames(veg14r3c) <- names
colnames(veg14r4c) <- names

write.csv(rbind(veg14r1c, veg14r2c, veg14r3c, veg14r4c), "2014_cov_nostand.csv", row.names=F)

