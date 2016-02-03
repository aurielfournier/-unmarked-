########
# This code creates the input files (both veg and bird related) for input into gdistsamp
# This code specifically stacks all of the surveys from one year into one input file
####

###
# Needed Packages ----------------------------------------------------------------------------------------
###
library(unmarked)
library(raildata)


dist.breaks <- c(0,1,2,3,4,5) 

data(allbirds)
birds <- allbirds
birds <- birds[birds$species=="sora"|birds$species=="s",]
birds <- birds[birds$distance<=5,] 
birds <- birds[!is.na(birds$round),]
birds <- birds[!is.na(birds$distance),]

birds$jdate <- as.factor(birds$odate)
birds$impound <- as.character(birds$impound)
birds[birds$impound=="sanctuarysouth",]$impound <- "sanctuary"
birds[birds$impound=="sanctuarynorth",]$impound <- "sanctuary"
birds[birds$impound=="n mallard",]$impound <- "nmallard"
birds[birds$impound=="r4/5",]$impound <- "r45"

birds$iry <- paste0(birds$impound,"_",birds$round,"_",birds$year)



########################## ----------------------------------------------------------------------------------------


gd <- as.data.frame(formatDistData(birds, "distance","iry",dist.breaks))
gd$iry <- row.names(gd)


####-----------------------------------------------
# Input covariates ----------------------------------------------------------------------------------------
####----------------------------------------------

data(allveg)

veg <- allveg
veg <- veg[veg$averagewater<=100,]
veg <- veg[!is.na(veg$averagewater),]

veg$impound <- as.character(veg$impound)

veg[veg$impound=="n mallard",]$impound <- "nmallard"
veg[veg$impound=="r4/5",]$impound <- "r45"
veg[veg$impound=="redhead slough",]$impound <- "redhead"

veg$iry <- paste0(veg$impound,"_",veg$round,"_",veg$year)

veg$iy <- paste0(veg$impound,"_",veg$year)

water = melt(veg[,c( "region","iry","iy", "averagewater")],id=c("iry","iy","region"), na.rm=T)
veg = melt(veg[,c("iy","int","short")], na.rm=TRUE, id=c("iy"))

castwater <- cast(data=water, iry + iy+ region ~ variable, median)
castveg <- cast(data=veg, iy ~ variable, median)

all_veg <- merge(castwater, castveg, by="iy", all=FALSE)

data(allsurveys)
surv <- allsurveys

surv$impound <- as.character(surv$impound)
surv[surv$impound=="n mallard",]$impound <- "nmallard"
surv[surv$impound=="r4/5",]$impound <- "r45"

surv$iry <- paste0(surv$impound,"_",surv$round,"_",surv$year)

mjdate <- melt(surv[,c("jdate","iry")], id="iry")
mlength <- melt(surv[,c("iry","length")], id="iry")
clength <- cast(mlength, iry ~ variable, sum)
clength$length <- clength$length*1000

cjdate <- cast(mjdate, iry ~ variable, median)

all_veg_jdate <- merge(all_veg, cjdate, by="iry", all.x=TRUE)
all_veg_jdate_length <- merge(all_veg_jdate, clength, by="iry",all.x=TRUE)

all_veg_jdate_length$scale_short <- scale(all_veg_jdate_length$short)
all_veg_jdate_length$scale_int <- scale(all_veg_jdate_length$int)
all_veg_jdate_length$scale_averagewater <- scale(all_veg_jdate_length$averagewater)

all_veg_jdate_length_year <- separate(all_veg_jdate_length, "iy", into=c("impound","year"), by=-5)



veg_DONE <- all_veg_jdate_length_year[(all_veg_jdate_length_year$iry %in% gd$iry),]

sora_DONE <- gd[(gd$iry %in% all_veg_jdate_length_year$iry),]

nrow(sora_DONE) == nrow(veg_DONE)
unique(sora_DONE$iry) == unique(veg_DONE$iry)



write.csv(sora_DONE, "./data/sora_DONE.csv", row.names=F)
write.csv(veg_DONE, "./data/veg_DONE.csv", row.names=F)
