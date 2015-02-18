#generating the input files for gdistsamp analyis
```{r}
library(reshape)

setwd("~/Dropbox/Field 2014")

#############################################################
###########Sora
############################################################

#2012 birds
birds <- read.csv("allbirds.csv",header=T)

birds <- birds[birds$species=="sora",]
birds12 <- birds[birds$year==2012,]
birds13 <- birds[birds$year==2013,]
birds14 <- birds[birds$year==2014,]

#first the data has to be binned. We use 12 bins 0-1, 1-2, etc etc since our farthest observation is a 12
birds12$bin1 = ifelse(birds12$distance<1,1,0)
birds12$bin2 = ifelse(birds12$distance<2,ifelse(birds12$distance>.9,1,0),0)
birds12$bin3 = ifelse(birds12$distance<3,ifelse(birds12$distance>1.9,1,0),0)
birds12$bin4 = ifelse(birds12$distance<4,ifelse(birds12$distance>2.9,1,0),0)
birds12$bin5 = ifelse(birds12$distance<5,ifelse(birds12$distance>3.9,1,0),0)
birds12$bin6 = ifelse(birds12$distance<6,ifelse(birds12$distance>4.9,1,0),0)
birds12$bin7 = ifelse(birds12$distance<7,ifelse(birds12$distance>5.9,1,0),0)
birds12$bin8 = ifelse(birds12$distance<8,ifelse(birds12$distance>6.9,1,0),0)
birds12$bin9 = ifelse(birds12$distance<9,ifelse(birds12$distance>7.9,1,0),0)
birds12$bin10 = ifelse(birds12$distance<10,ifelse(birds12$distance>8.9,1,0),0)
birds12$bin11 = ifelse(birds12$distance<11,ifelse(birds12$distance>9.9,1,0),0)
birds12$bin12 = ifelse(birds12$distance<12,ifelse(birds12$distance>10.9,1,0),0)

birds13$bin1 = ifelse(birds13$distance<1,1,0)
birds13$bin2 = ifelse(birds13$distance<2,ifelse(birds13$distance>.9,1,0),0)
birds13$bin3 = ifelse(birds13$distance<3,ifelse(birds13$distance>1.9,1,0),0)
birds13$bin4 = ifelse(birds13$distance<4,ifelse(birds13$distance>2.9,1,0),0)
birds13$bin5 = ifelse(birds13$distance<5,ifelse(birds13$distance>3.9,1,0),0)
birds13$bin6 = ifelse(birds13$distance<6,ifelse(birds13$distance>4.9,1,0),0)
birds13$bin7 = ifelse(birds13$distance<7,ifelse(birds13$distance>5.9,1,0),0)
birds13$bin8 = ifelse(birds13$distance<8,ifelse(birds13$distance>6.9,1,0),0)
birds13$bin9 = ifelse(birds13$distance<9,ifelse(birds13$distance>7.9,1,0),0)
birds13$bin10 = ifelse(birds13$distance<10,ifelse(birds13$distance>8.9,1,0),0)
birds13$bin11 = ifelse(birds13$distance<11,ifelse(birds13$distance>9.9,1,0),0)
birds13$bin12 = ifelse(birds13$distance<12,ifelse(birds13$distance>10.9,1,0),0)

birds14$bin1 = ifelse(birds14$distance<1,1,0)
birds14$bin2 = ifelse(birds14$distance<2,ifelse(birds14$distance>.9,1,0),0)
birds14$bin3 = ifelse(birds14$distance<3,ifelse(birds14$distance>1.9,1,0),0)
birds14$bin4 = ifelse(birds14$distance<4,ifelse(birds14$distance>2.9,1,0),0)
birds14$bin5 = ifelse(birds14$distance<5,ifelse(birds14$distance>3.9,1,0),0)
birds14$bin6 = ifelse(birds14$distance<6,ifelse(birds14$distance>4.9,1,0),0)
birds14$bin7 = ifelse(birds14$distance<7,ifelse(birds14$distance>5.9,1,0),0)
birds14$bin8 = ifelse(birds14$distance<8,ifelse(birds14$distance>6.9,1,0),0)
birds14$bin9 = ifelse(birds14$distance<9,ifelse(birds14$distance>7.9,1,0),0)
birds14$bin10 = ifelse(birds14$distance<10,ifelse(birds14$distance>8.9,1,0),0)
birds14$bin11 = ifelse(birds14$distance<11,ifelse(birds14$distance>9.9,1,0),0)
birds14$bin12 = ifelse(birds14$distance<12,ifelse(birds14$distance>10.9,1,0),0)

round2_12 <- birds12[birds12$round==2,]
round2_12s = round2_12[,c("impound", "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round3_12 <- birds12[birds12$round==3,]
round3_12s = round3_12[,c("impound",  "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round1_13 <- birds13[birds13$round==1,]
round1_13s = round1_13[,c("impound",  "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round2_13 <- birds13[birds13$round==2,]
round2_13s = round2_13[,c("impound",  "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round3_13 <- birds13[birds13$round==3,]
round3_13s = round3_13[,c("impound", "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round4_13 <- birds13[birds13$round==4,]
round4_13s = round4_13[,c("impound", "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round1_14 <- birds14[birds14$round==1,]
round1_14s = round1_14[,c("impound",  "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round2_14 <- birds14[birds14$round==2,]
round2_14s = round2_14[,c("impound",  "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round3_14 <- birds14[birds14$round==3,]
round3_14s = round3_14[,c("impound", "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]
round4_14 <- birds14[birds14$round==4,]
round4_14s = round4_14[,c("impound", "night", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12")]

surv <- read.csv("survey_all.csv",header=T)
surv <- surv[,c("year","night","round","impound")]

surv12 <- surv[surv$year==2012,]
surv122 <- surv14[surv12$round==2,]
surv123 <- surv14[surv12$round==3,]

surv13 <- surv[surv$year==2013,]
surv131 <- surv14[surv13$round==1,]
surv132 <- surv14[surv13$round==2,]
surv133 <- surv14[surv13$round==3,]
surv134 <- surv14[surv13$round==4,]

surv14 <- surv[surv$year==2014,]
surv141 <- surv14[surv14$round==1,]
surv142 <- surv14[surv14$round==2,]
surv143 <- surv14[surv14$round==3,]
surv144 <- surv14[surv14$round==4,]

uni_122 <- surv122[!(surv122$impound %in% round2_12s$impound),]
add_122 <- data.frame(matrix(ncol=14,nrow=nrow(uni_122)))
add_122[,1] = uni_122$impound
add_122[,2] = uni_122$night
add_122[,3:14] <- 0
colnames(add_122) <- colnames(round2_12s)
b141 <- rbind(round2_12s, add_122)

uni_123 <- surv123[!(surv123$impound %in% round3_12s$impound),]
add_123 <- data.frame(matrix(ncol=14,nrow=nrow(uni_123)))
add_123[,1] = uni_123$impound
add_123[,2] = uni_123$night
add_123[,3:14] <- 0
colnames(add_123) <- colnames(round3_12s)
b123 <- rbind(round3_12s, add_123)

uni_131 <- surv131[!(surv131$impound %in% round1_13s$impound),]
add_131 <- data.frame(matrix(ncol=14,nrow=nrow(uni_131)))
add_131[,1] = uni_131$impound
add_131[,2] = uni_131$night
add_131[,3:14] <- 0
colnames(add_131) <- colnames(round1_13s)
b131 <- rbind(round1_13s, add_131)

uni_132 <- surv132[!(surv132$impound %in% round2_13s$impound),]
add_132 <- data.frame(matrix(ncol=14,nrow=nrow(uni_132)))
add_132[,1] = uni_132$impound
add_132[,2] = uni_132$night
add_132[,3:14] <- 0
colnames(add_132) <- colnames(round2_13s)
b132 <- rbind(round2_13s, add_132)

uni_133 <- surv133[!(surv133$impound %in% round3_13s$impound),]
add_133 <- data.frame(matrix(ncol=14,nrow=nrow(uni_133)))
add_133[,1] = uni_133$impound
add_133[,2] = uni_133$night
add_133[,3:14] <- 0
colnames(add_133) <- colnames(round3_13s)
b133 <- rbind(round3_13s, add_133)

uni_134 <- surv134[!(surv134$impound %in% round4_13s$impound),]
add_134 <- data.frame(matrix(ncol=14,nrow=nrow(uni_134)))
add_134[,1] = uni_134$impound
add_134[,2] = uni_134$night
add_134[,3:14] <- 0
colnames(add_134) <- colnames(round4_13s)
b134 <- rbind(round4_13s, add_134)


uni_141 <- surv141[!(surv141$impound %in% round2_14s$impound),]
add_141 <- data.frame(matrix(ncol=14,nrow=nrow(uni_141)))
add_141[,1] = uni_141$impound
add_141[,2] = uni_141$night
add_141[,3:14] <- 0
colnames(add_141) <- colnames(round1_14s)
b141 <- rbind(round1_14s, add_141)

uni_142 <- surv142[!(surv142$impound %in% round2_14s$impound),]
add_142 <- data.frame(matrix(ncol=14,nrow=nrow(uni_142)))
add_142[,1] = uni_142$impound
add_142[,2] = uni_142$night
add_142[,3:14] <- 0
colnames(add_142) <- colnames(round2_14s)
b142 <- rbind(round2_14s, add_142)

#uni_143 <- surv143[!(surv143$impound %in% round3_14s$impound),]
#add_143 <- data.frame(matrix(ncol=14,nrow=nrow(uni_143)))
#add_143[,1] = uni_143$impound
#add_143[,2] = uni_143$night
#add_143[,3:14] <- 0
#colnames(add_143) <- colnames(round3_14s)
#b143 <- rbind(round3_14s, add_143)

#uni_144 <- surv144[!(surv144$impound %in% round4_14s$impound),]
#add_144 <- data.frame(matrix(ncol=14,nrow=nrow(uni_144)))
#add_144[,1] = uni_144$impound
#add_144[,2] = uni_144$night
#add_144[,3:14] <- 0
#colnames(add_144) <- colnames(round4_14s)
#b144 <- rbind(round4_14s, add_144)


#2012 round 2
melt12_2 = melt(b122, id=c("impound","night"))
cast12_2 = cast(melt12_2, impound ~ variable | night,sum , fill=NA_real_)
merge12_2a = merge(cast12_2$'1', cast12_2$'2', by="impound", all=T)
merge12_2b = merge(merge12_2a, cast12_2$'3', by="impound", all=T)

#2012 round 3
melt12_3 = melt(b123, id=c("impound","night"))
cast12_3 = cast(melt12_3, impound ~ variable | night, sum, fill=NA_real_)
merge12_3a = merge(cast12_3$'1', cast12_3$'2', by="impound", all=T)
merge12_3b = merge(merge12_3a, cast12_3$'3', by="impound", all=T)

#2013 round 1
melt13_1 = melt(b131, id=c("impound","night"))
cast13_1 = cast(melt13_1, impound ~ variable | night, sum, fill=NA_real_)
merge13_1a = merge(cast13_1$'1.1', cast13_1$'1.2', by="impound", all=T, suffixes=c("a", "b"))
merge13_1b = merge(merge13_1a, cast13_1$'2.1', by="impound", all=T, suffixes=c("b", "c"))
merge13_1c = merge(merge13_1b, cast13_1$'2.2', by="impound", all=T, suffixes=c("c", "d"))
merge13_1d = merge(merge13_1c, cast13_1$'3.1', by="impound", all=T, suffixes=c("d", "e"))
merge13_1e = merge(merge13_1d, cast13_1$'3.2', by="impound", all=T, suffixes=c("e", "f"))

#2013 round 2
melt13_2 = melt(b132, id=c("impound","night"))
cast13_2 = cast(melt13_2, impound ~ variable | night, sum, fill=NA_real_)
merge13_2a = merge(cast13_2$'1.1', cast13_2$'1.2', by="impound", all=T, suffixes=c("a", "b"))
merge13_2b = merge(merge13_2a, cast13_2$'2.1', by="impound", all=T, suffixes=c("b", "c"))
merge13_2c = merge(merge13_2b, cast13_2$'2.2', by="impound", all=T, suffixes=c("c", "d"))
merge13_2d = merge(merge13_2c, cast13_2$'3.1', by="impound", all=T, suffixes=c("d", 'e'))
merge13_2e = merge(merge13_2d, cast13_2$'3.2', by="impound", all=T, suffixes=c("f", 'g'))

#2013 round 3
melt13_3 = melt(b133, id=c("impound","night"))
cast13_3 = cast(melt13_3, impound ~ variable | night, sum, fill=NA_real_)
merge13_3a = merge(cast13_3$'1.1', cast13_3$'1.2', by="impound", all=T, suffixes=c("a", "b"))
merge13_3b = merge(merge13_3a, cast13_3$'2.1', by="impound", all=T, suffixes=c("b", "c"))
merge13_3c = merge(merge13_3b, cast13_3$'2.2', by="impound", all=T, suffixes=c("c", "d"))
merge13_3d = merge(merge13_3c, cast13_3$'3.1', by="impound", all=T, suffixes=c("d", "e"))
merge13_3e = merge(merge13_3d, cast13_3$'3.2', by="impound", all=T, suffixes=c("e", "f"))

#2013 round 4
melt13_4 = melt(b134, id=c("impound","night"))
cast13_4 = cast(melt13_4, impound ~ variable | night, sum, fill=NA_real_)
merge13_4a = merge(cast13_4$'1.1', cast13_4$'2.1', by="impound", all=T, suffixes=c("a", "b"))
merge13_4b = merge(merge13_4a, cast13_4$'3.1', by="impound", all=T, suffixes=c("b", "c"))
merge13_4c = merge(merge13_4b, cast13_4$'3.2', by="impound", all=T, suffixes=c("c", "d"))
merge13_4d = merge(merge13_4c, cast13_4$'4.1', by="impound", all=T, suffixes=c("d", "e"))
merge13_4e = merge(merge13_4d, cast13_4$'4.2', by="impound", all=T, suffixes=c("e", "f"))

#2014 round 1
melt14_1 = melt(b141, id=c("impound","night"))
cast14_1 = cast(melt14_1, impound ~ variable | night, sum, fill=NA_real_)
merge14_1a = merge(cast13_1$'1.1', cast13_1$'1.2', by="impound", all=T, suffixes=c("a", "b"))
merge14_1b = merge(merge14_1a, cast13_1$'2.1', by="impound", all=T, suffixes=c("b", "c"))
merge14_1c = merge(merge14_1b, cast13_1$'2.2', by="impound", all=T, suffixes=c("c", "d"))
merge14_1d = merge(merge14_1c, cast13_1$'3.1', by="impound", all=T, suffixes=c("d", "e"))
merge14_1e = merge(merge14_1d, cast13_1$'3.2', by="impound", all=T, suffixes=c("e", "f"))

#2014 round 2
melt14_2 = melt(b142, id=c("impound","night"))
cast14_2 = cast(melt14_2, impound ~ variable | night, sum, fill=NA_real_)
merge14_2a = merge(cast13_2$'1.1', cast13_2$'1.2', by="impound", all=T, suffixes=c("a", "b"))
merge14_2b = merge(merge14_2a, cast13_2$'2.1', by="impound", all=T, suffixes=c("b", "c"))
merge14_2c = merge(merge14_2b, cast13_2$'2.2', by="impound", all=T, suffixes=c("c", "d"))
merge14_2d = merge(merge14_2c, cast13_2$'3.1', by="impound", all=T, suffixes=c("d", 'e'))
merge14_2e = merge(merge14_2d, cast13_2$'3.2', by="impound", all=T, suffixes=c("f", 'g'))

#2014 round 3
melt14_3 = melt(b143, id=c("impound","night"))
cast14_3 = cast(melt14_3, impound ~ variable | night, sum, fill=NA_real_)
merge14_3a = merge(cast13_3$'1.1', cast13_3$'1.2', by="impound", all=T, suffixes=c("a", "b"))
merge14_3b = merge(merge14_3a, cast13_3$'2.1', by="impound", all=T, suffixes=c("b", "c"))
merge14_3c = merge(merge14_3b, cast13_3$'2.2', by="impound", all=T, suffixes=c("c", "d"))
merge14_3d = merge(merge14_3c, cast13_3$'3.1', by="impound", all=T, suffixes=c("d", "e"))
merge14_3e = merge(merge14_3d, cast13_3$'3.2', by="impound", all=T, suffixes=c("e", "f"))

# #2014 round 4
# melt14_4 = melt(b144, id=c("impound","night"))
# cast14_4 = cast(melt14_4, impound ~ variable | night, sum, fill=NA_real_)
# merge14_4a = merge(cast13_4$'1.1', cast13_4$'2.1', by="impound", all=T, suffixes=c("a", "b"))
# merge14_4b = merge(merge14_4a, cast13_4$'3.1', by="impound", all=T, suffixes=c("b", "c"))
# merge14_4c = merge(merge14_4b, cast13_4$'3.2', by="impound", all=T, suffixes=c("c", "d"))
# merge14_4d = merge(merge14_4c, cast13_4$'4.1', by="impound", all=T, suffixes=c("d", "e"))
# merge14_4e = merge(merge14_4d, cast13_4$'4.2', by="impound", all=T, suffixes=c("e", "f"))

########################################################
##### Covariates
#########################################################
setwd("~/Dropbox/R/Veg_Summary")
veg = read.csv("masterveg.csv", header=T)


### 2012 ###
veg12 <- veg[veg$year==2012,]
veg12v <- veg12[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")]
veg12w <- veg12[,c( "round",  "impound", "averagewater")]
meltv12v = melt(veg12v)
castveg12v = cast(meltv12v, impound + region~ variable, mean, fill=NA_real_)
meltv12w = melt(veg12w, id=c("impound","round"))
castveg12w = cast(meltv12w, impound ~ variable + round, mean, fill=NA_real_)
castveg12_all = cbind(castveg12v, castveg12w)

### 2013 ###
veg13 <- veg[veg$year==2013,]
veg13v <- veg13[,c("bv","region", "round", "habtype","point", "spp", "impound", "area", "int", "short", "tall", "up", "water", "wood", "bg", "other", "crop", "waterp", "woodp")]
veg13w <- veg13[,c( "round",  "impound", "averagewater")]
meltv13v = melt(veg13v)
castveg13v = cast(meltv13v, impound + region ~ variable, mean, fill=NA_real_)
meltv13w = melt(veg13w, id=c("impound","round"))
castveg13w = cast(meltv13w, impound ~ variable + round, mean, fill=NA_real_)
castveg13_all = cbind(castveg13v, castveg13w)

### 2014 ###
v14 <- veg[veg$year==2014,]
v14$treat[v14$impound=="kt2"|v14$impound=="kt6"|v14$impound=="dc21"|v14$impound=="ccmsu2"|v14$impound=="ash"|v14$impound=="pool3w"|v14$impound=="m11"|v14$impound=="ts8a"|v14$impound=="sgb"|v14$impound=="msu3"|v14$impound=="pool c"|v14$impound=="r7"|v14$impound=="r3"|v14$impound=="dc20"|v14$impound=="ccmsu1"|v14$impound=="dc18"]<-"C"
v14$treat[v14$impound=="sanctuary"|v14$impound=="scmsu2"|v14$impound=="pool2w"|v14$impound=="m10"|v14$impound=="ts2a"|v14$impound=="ts4a"|v14$impound=="ccmsu12"|v14$impound=="kt9"|v14$impound=="dc22"|v14$impound=="os23"|v14$impound=="pool i"]<-"E"
v14$treat[v14$impound=="sgd"|v14$impound=="rail"|v14$impound=="pool 2"|v14$impound=="m13"|v14$impound=="ts6a"|v14$impound=="kt5"|v14$impound=="dc14"|v14$impound=="os21"|v14$impound=="pool e"]<-"L"
v14v = v14[,c( "region","round","impound", "area", "int", "treat", "short","pe", "wood")]
v14w = v14[,c( "impound","round", "averagewater")]
v14v$woodp = ifelse(v14$wood>0,1,0)
v14w$waterp = ifelse(v14$water>0,1,0)
meltv14v = melt(v14v)
castveg14v = cast(meltv14v, impound + treat + region ~ variable, mean, fill=NA_real_)
meltv14w = melt(v14w,id=c("impound","round"))
castveg14w = cast(meltv14w, impound ~ variable + round , mean, fill=NA_real_)
castveg14_all <- cbind(castveg14v, castveg14w)



id12r2 = intersect(merge12_2b$impound,castveg12_all$impound)
id12r3 = intersect(merge12_3b$impound,castveg12_all$impound)
id13r1 = intersect(merge13_1b$impound,castveg13_all$impound)
id13r2 = intersect(merge13_2b$impound,castveg13_all$impound)
id13r3 = intersect(merge13_3b$impound,castveg13_all$impound)
id13r4 = intersect(merge13_4b$impound,castveg13_all$impound)
id14r1 = intersect(merge14_1b$impound,castveg14_all$impound)
id14r2 = intersect(merge14_2b$impound,castveg14_all$impound)
id14r3 = intersect(merge14_3b$impound,castveg14_all$impound)
#id14r4 = intersect(merge14_4b$impound,castveg14_all$impound)


#create sora files
setwd("~/Dropbox/R/Distance")
mmerge12r2 <- merge12_2b[(merge12_2b$impound %in% id12r2),]
write.csv(mmerge12r2, "2012r2_sora.csv", row.names=F)

mmerge12r3 <- merge12_3b[(merge12_3b$impound %in% id12r3),]
write.csv(mmerge12r3, "2012r3_sora.csv", row.names=F)

mmerge13r1 <- merge13_1b[(merge13_1e$impound %in% id13r1),]
write.csv(mmerge13r1, "2013r1_sora.csv", row.names=F)

mmerge13r2 <- merge13_2b[(merge13_2e$impound %in% id13r2),]
write.csv(mmerge13r2, "2013r2_sora.csv", row.names=F)

mmerge13r3 <- merge13_3b[(merge13_3e$impound %in% id13r3),]
write.csv(mmerge13r3, "2013r3_sora.csv", row.names=F)

mmerge13r4 <- merge13_4b[(merge13_4e$impound %in% id13r4),]
write.csv(mmerge13r4, "2013r4_sora.csv", row.names=F)

mmerge14r1 <- merge14_1b[(merge14_1e$impound %in% id14r1),]
write.csv(mmerge14r1, "2014r1_sora.csv", row.names=F)

mmerge14r2 <- merge14_2b[(merge14_2e$impound %in% id14r2),]
write.csv(mmerge14r2, "2014r2_sora.csv", row.names=F)

mmerge14r3 <- merge14_3b[(merge14_3e$impound %in% id14r3),]
write.csv(mmerge14r3, "2014r3_sora.csv", row.names=F)

#mmerge14r4 <- merge14_4b[(merge14_4e$impound %in% id14r4),]
#write.csv(mmerge14r4, "2014r4_sora.csv", row.names=F)

#create covariate files


mveg12r2 <- castveg12_all[(castveg12_all$impound %in% id12r2),]
write.csv(mveg12r2, "2012r2_cov.csv", row.names=F)

mveg12r3 <- castveg12_all[(castveg12_all$impound %in% id12r3),]
write.csv(mveg12r3, "2012r3_cov.csv", row.names=F)

mveg13r1 <- castveg13_all[(castveg13_all$impound %in% id13r1),]
write.csv(mveg13r1, "2013r1_cov.csv", row.names=F)

mveg13r2 <- castveg13_all[(castveg13_all$impound %in% id13r2),]
write.csv(mveg13r2, "2013r2_cov.csv", row.names=F)

mveg13r3 <- castveg13_all[(castveg13_all$impound %in% id13r3),]
write.csv(mveg13r3, "2013r3_cov.csv", row.names=F)

mveg13r4 <- castveg13_all[(castveg13_all$impound %in% id13r4),]
write.csv(mveg13r4, "2013r4_cov.csv", row.names=F)

mveg14r1 <- castveg14_all[(castveg14_all$impound %in% id14r1),]
write.csv(mveg14r1, "2014r1_cov.csv", row.names=F)

mveg14r2 <- castveg14_all[(castveg14_all$impound %in% id14r2),]
write.csv(mveg14r2, "2014r2_cov.csv", row.names=F)

mveg14r3 <- castveg14_all[(castveg14_all$impound %in% id14r3),]
write.csv(mveg14r3, "2014r3_cov.csv", row.names=F)

#mveg14r4 <- castveg14_all[(castveg14_all$impound %in% id14r4),]
#write.csv(mveg14r4, "2014r4_cov.csv", row.names=F)


