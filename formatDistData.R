library(unmarked)

setwd("C:/Users/avanderlaar/Dropbox/Field 2014")
birds <- read.csv("allbirds.csv",header=T)

birds <- birds[birds$species=="sora",]
birds14 <- birds[birds$year==2014,]
birds14r1 <- birds14[birds14$round==1,]
birds14r2 <- birds14[birds14$round==2,]
birds14r3 <- birds14[birds14$round==3,]
birds14r4 <- birds14[birds14$round==4,]

birds14r1