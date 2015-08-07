library(tidyr)
library(dplyr)
library(reshape)
## top three plant stuffs


veg <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/all_veg.csv", header=T) 
veg <- veg[veg$spp=="sora"|is.na(veg$spp),]
veg <- veg[,c("point","plant1","plant2","plant3","impound","year","bv")]
veg <- veg[!is.na(veg$plant1),]
veg <- veg[veg$bv!="day",]

mveg <- melt(data=veg, id=c("point","impound",'year',"bv"))
mveg <- mveg[!is.na(mveg$value),]
mveg$value <- as.character(mveg$value)
mveg[mveg$value=="japanese millett",]$value <- "japanese_millet"
mveg[mveg$value=="japanese millit",]$value <- "japanese_millet"
mveg[mveg$value=="catail",]$value <- "typha"
mveg[mveg$value=="cattail",]$value <- "typha"
mveg[mveg$value=="fluffy millet",]$value <- "millet"
mveg[mveg$value=="dead smartweed",]$value <- "none"
mveg[mveg$value=="dead millett",]$value <- "none"
mveg[mveg$value=="hardstem bullrush",]$value <- "bulrush"
mveg[mveg$value=="river bulrush",]$value <- "bulrush"
mveg[mveg$value=="dark millett",]$value <- "millet"
mveg[mveg$value=="walter's millett",]$value <- "millet"
mveg[mveg$value=="willow ",]$value <- "willow"
mveg[mveg$value=="sedge ",]$value <- "sedge"
mveg[mveg$value=="dead",]$value <- "none"
mveg[mveg$value=="false aster",]$value <- "upland"
mveg[mveg$value=="grass ",]$value <- "grass"
mveg[mveg$value=="upland ",]$value <- "upland"
mveg[mveg$value=="primrose ",]$value <- "primrose"
mveg[mveg$value=="sesge",]$value <- "sedge"
mveg[mveg$value=="panic",]$value <- "panic grass"
mveg[mveg$value=="panic grss",]$value <- "panic grass"
mveg[mveg$value=="short forb",]$value <- "forb"
mveg[mveg$value=="low forb",]$value <- "forb"
mveg[mveg$value=="poison ivy",]$value <- "vine"
mveg[mveg$value=="morning glory",]$value <- "vine"
mveg[mveg$value=="christmas tree grass",]$value <- "grass"
mveg[mveg$value=="japanese millet",]$value <- "japanese_millet"
mveg[mveg$value=="aspen",]$value <- "tree"
mveg[mveg$value=="oak",]$value <- "tree"
mveg[mveg$value=="shrub",]$value <- "tree"
mveg[mveg$value=="cezbania",]$value <- "sesbania"
mveg[mveg$value=="milllet",]$value <- "millet"
mveg[mveg$value=="yellow nut sedge",]$value <- "sedge"
mveg[mveg$value=="yellow bear sedge",]$value <- "sedge"
mveg[mveg$value=="woody",]$value <- "tree"
mveg[mveg$value=="sunflower",]$value <- "biden"
mveg[mveg$value=="aquaticplant",]$value <- "aquatic plant"
mveg[mveg$value=="cottonwood",]$value <- "tree"
mveg[mveg$value=="mallow",]$value <- "tree"
mveg[mveg$value=="red oak",]$value <- "tree"
mveg[mveg$value=="hard stemmed bullrush",]$value <- "bulrush"
mveg[mveg$value=="foxtails",]$value <- "grass"

mveg[mveg$value=="p. nodding",]$value <- "annual smartweed"
mveg[mveg$value=="p. penn",]$value <- "annual smartweed"
mveg[mveg$value=="p. aqua",]$value <- "perennial smartweed"
mveg[mveg$value=="p. piperodies",]$value <- "perennial smartweed"

mveg <- mveg[mveg$value!="none",]

mveg12 <- mveg[mveg$year==2012,]
mveg13 <- mveg[mveg$year==2013,]
mveg14 <- mveg[mveg$year==2014,]

cveg12 <- cast(data=mveg12, impound + year ~ value + bv)
colnames(cveg12) <- gsub("[ ]","_",colnames(cveg12))
cveg12s <- cveg12[,3:ncol(cveg12)]/rowSums(cveg12[,3:ncol(cveg12)])
colnames(cveg12s) <- paste0("scale_",colnames(cveg12s))

cveg13 <- cast(data=mveg13, impound + year ~ value + bv)
colnames(cveg13) <- gsub("[ ]","_",colnames(cveg13))
cveg13s <- cveg13[,3:ncol(cveg13)]/rowSums(cveg13[,3:ncol(cveg13)])
colnames(cveg13s) <- paste0("scale_",colnames(cveg13s))

cveg14 <- cast(data=mveg14, impound + year ~ value + bv)
colnames(cveg14) <- gsub("[ ]","_",colnames(cveg14))
cveg14s <- cveg14[,3:ncol(cveg14)]/rowSums(cveg14[,3:ncol(cveg14)])
colnames(cveg14s) <- paste0("scale_",colnames(cveg14s))


veg12 <- cbind(cveg12, cveg12s)
veg13 <- cbind(cveg13, cveg13s)
veg14 <- cbind(cveg14, cveg14s)

mmveg12 <- melt(veg12, id=c("impound","year"))
mmveg12 <- mmveg12 %>% separate(variable, into=c("variable","bv"), -2)
mmveg12$num <- 1
tab12 <- cast(data=mmveg12[grepl("scale",mmveg12$variable),], variable ~ num, value="value",  fun.aggregate=mean)
var_names12 <- tab12[tab12$"1">=.05,]$variable

mmveg12 <- mmveg12[mmveg12$variable %in% var_names12,]

chi12 <- cast(data=mmveg12, impound ~ variable + bv)

chi12list <- list()
cols <- seq(2,(ncol(chi12)-1),by=2)
for(i in cols){
    d <- chi12[,(i:(i+1))]
    chi12list[[colnames(d)[1]]]<-chisq.test(table(d))
}


mmveg13 <- melt(veg13, id=c("impound","year"))
mmveg13 <- mmveg13 %>% separate(variable, into=c("variable","bv"), -2)
mmveg13$num <- 1
tab13 <- cast(data=mmveg13[grepl("scale",mmveg13$variable),], variable ~ num, value="value",  fun.aggregate=mean)
var_names13 <- tab13[tab13$"1">=.05,]$variable

mmveg13 <- mmveg13[mmveg13$variable %in% var_names13,]

chi13 <- cast(data=mmveg13, impound ~ variable + bv)

chi13list <- list()
cols <- seq(2,(ncol(chi13)-1),by=2)
for(i in cols){
  d <- chi13[,(i:(i+1))]
  chi13list[[colnames(d)[1]]]<-chisq.test(table(d))
}

mmveg14 <- melt(veg14, id=c("impound","year"))
mmveg14 <- mmveg14 %>% separate(variable, into=c("variable","bv"), -2)
mmveg14$num <- 1
tab14 <- cast(data=mmveg14[grepl("scale",mmveg14$variable),], variable ~ num, value="value",  fun.aggregate=mean)
var_names14 <- tab14[tab14$"1">=.05,]$variable

mmveg14 <- mmveg14[mmveg14$variable %in% var_names14,]

chi14 <- cast(data=mmveg14, impound ~ variable + bv)

chi14list <- list()
cols <- seq(2,(ncol(chi14)-1),by=2)
for(i in cols){
  d <- chi14[,(i:(i+1))]
  chi14list[[colnames(d)[1]]]<-chisq.test(table(d))
}


t12list <- list()
cols <- seq(2,(ncol(chi12)-1),by=2)
for(i in cols){
  d <- chi12[,(i:(i+1))]
  t12list[[colnames(d)[1]]]<-t.test(d)
}

t13list <- list()
cols <- seq(2,(ncol(chi13)-1),by=2)
for(i in cols){
  d <- chi13[,(i:(i+1))]
  t13list[[colnames(d)[1]]]<-t.test(d)
}

t14list <- list()
cols <- seq(2,(ncol(chi14)-1),by=2)
for(i in cols){
  d <- chi14[,(i:(i+1))]
  t14list[[colnames(d)[1]]]<-t.test(d)
}


a12 <- ggplot(data=mmveg12, aes(x=variable, y=value, fill=bv))+geom_boxplot()+theme_krementz()

a13 <- ggplot(data=mmveg13, aes(x=variable, y=value, fill=bv))+geom_boxplot()+theme_krementz()

a14 <- ggplot(data=mmveg14, aes(x=variable, y=value, fill=bv))+geom_boxplot()+theme_krementz()

grid.arrange(a12, a13, a14, ncol=1)