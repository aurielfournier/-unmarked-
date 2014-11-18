library(unmarked)

setwd("C:/Users/avanderlaar/Dropbox/Field 2014")
birds <- read.csv("allbirds.csv",header=T)

birds <- birds[birds$species=="sora",]
birds14 <- birds[birds$year==2014,]
birds14$night <- as.factor(birds14$night)
birds14r1 <- birds14[birds14$round==1,]
birds14r2 <- birds14[birds14$round==2,]
birds14r3 <- birds14[birds14$round==3,]
birds14r4 <- birds14[birds14$round==4,]

dist.breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)

gd14r1 <- formatDistData(birds14r1, "distance", "impound", dist.breaks, "night" )
gd14r2 <- formatDistData(birds14r2, "distance", "impound", dist.breaks, "night" )
gd14r3 <- formatDistData(birds14r3, "distance", "impound", dist.breaks, "night" )
gd14r4 <- formatDistData(birds14r4, "distance", "impound", dist.breaks, "night" )
