
veg <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/all_veg.csv", header=T) 
veg <- veg[veg$spp=="sora"|is.na(veg$spp),]
#veg <- veg[,c("point","plant1","plant2","plant3","impound","year","bv")]
#veg <- na.omit(veg)
veg <- veg[!is.na(veg$plant1),]
veg <- veg[!is.na(veg$plant2),]
veg <- veg[!is.na(veg$plant3),]
veg <- veg[veg$bv!="day",]
veg$plant1 <- as.character(veg$plant1)
veg$plant2 <- as.character(veg$plant2)
veg$plant3 <- as.character(veg$plant3)

veg[veg$plant1=="p. nodding",]$plant1 <- "annual smartweed"
veg[veg$plant1=="p. penn",]$plant1 <- "annual smartweed"
veg[veg$plant1=="p. aqua",]$plant1 <- "perennial smartweed"
veg[veg$plant1=="p. piperodies",]$plant1 <- "perennial smartweed"
veg[veg$plant1=="milllet",]$plant1 <- "millet"
veg[veg$plant1=="walter's millett",]$plant1 <- "millet"
veg[veg$plant1=="sesge",]$plant1 <- "sedge"
veg[veg$plant1=="catail",]$plant1 <- "typha"
veg[veg$plant1=="cattail",]$plant1 <- "typha"
veg[veg$plant1=="dark millett",]$plant1 <- "millet"
veg[veg$plant1=="panic",]$plant1 <- "panic grass"
veg[veg$plant1=="christmas tree grass",]$plant1 <- "grass"
veg[veg$plant1=="foxtail",]$plant1 <- "grass"
veg[veg$plant1=="japanese millett",]$plant1 <- "japanese millet"
veg[veg$plant1=="japanese millit",]$plant1 <- "japanese millet"
veg[veg$plant1=="yellow nut sedge",]$plant1 <- "sedge"
veg <- veg[veg$plant1!="dead smartweed",]
veg <- veg[veg$plant1!="none",]
veg[veg$plant1=="primrose ",]$plant1 <- "primrose"
veg[veg$plant1=="willow ",]$plant1 <- "willow"
veg[veg$plant1=="upland ",]$plant1 <- "upland"

veg[veg$plant2=="yellow nut sedge",]$plant2 <- "sedge"
veg[veg$plant2=="yellow bear sedge",]$plant2 <- "sedge"
veg[veg$plant2=="p. nodding",]$plant2 <- "annual smartweed"
veg[veg$plant2=="p. penn",]$plant2 <- "annual smartweed"
veg[veg$plant2=="p. aqua",]$plant2 <- "perennial smartweed"
veg[veg$plant2=="p. piperodies",]$plant2 <- "perennial smartweed"
veg[veg$plant2=="christmas tree grass",]$plant2 <- "grass"
veg <- veg[!grepl("dead",veg$plant2),]
veg[veg$plant2=="walter's millett",]$plant2 <- "millet"
veg[veg$plant2=="japanese millett",]$plant2 <- "japanese millet"
veg[veg$plant2=="cattail",]$plant2 <- "typha"
veg[veg$plant2=="low forb",]$plant2 <- "forb"
veg[veg$plant2=="short forb",]$plant2 <- "forb"
veg[veg$plant2=="wood",]$plant2 <- "tree"
veg[veg$plant2=="grass ",]$plant2 <- "grass"




veg[veg$plant3=="p. nodding",]$plant3 <- "annual smartweed"
veg[veg$plant3=="p. penn",]$plant3 <- "annual smartweed"
veg[veg$plant3=="p. aqua",]$plant3 <- "perennial smartweed"
veg[veg$plant3=="p. piperodies",]$plant3 <- "perennial smartweed"
veg[veg$plant3=="christmas tree grass",]$plant3 <- "grass"
veg[veg$plant3=="cattail",]$plant3 <- "typha"
veg[veg$plant3=="yellow bear sedge",]$plant3 <- "sedge"
veg[veg$plant3=="dark millett",]$plant3 <- "millet"
veg[veg$plant3=="japanese millett",]$plant3 <- "japanese millet"
veg[veg$plant3=="low forb",]$plant3 <- "forb"
veg[veg$plant3=="fluffy millet",]$plant3 <- "millet"
veg[veg$plant3=="walter's millett",]$plant3 <- "millet"
veg <- veg[veg$plant3!="aquaticplant",]
veg <- veg[veg$plant3!="aquatic plant",]
veg[veg$plant3=="walter's millett",]$plant3 <- "millet"
veg[veg$plant3=="foxtails",]$plant3 <- "grass"
veg <- veg[veg$plant3!="dead",]
veg[veg$plant3=="oak",]$plant3 <- "tree"
veg[veg$plant3=="mallow",]$plant3 <- "tree"
veg[veg$plant3=="sunflower",]$plant3 <- "biden"
veg[veg$plant3=="sedge ",]$plant3 <- "sedge"
veg[veg$plant3=="cezbania",]$plant3 <- "sesbania"
veg[veg$plant3=="foxtail",]$plant3 <- "grass"
veg[veg$plant3=="aspen",]$plant3 <- "tree"
veg[veg$plant3=="cottonwood",]$plant3 <- "tree"
veg[veg$plant3=="grass ",]$plant3 <- "grass"
veg[veg$plant3=="panic grss",]$plant3 <- "grass"
veg[veg$plant3=="buttonbush",]$plant3 <- "button bush"
veg <- veg[veg$plant3!="dead smartweed",]


write.csv(vveg, "top_3_plants_sora_only.csv")
