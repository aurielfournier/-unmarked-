#duck use days 2014

setwd("C:/Users/avanderlaar/Documents/SourceTree/data")
dat <- read.csv('2014_waterfowl.csv')
dat <- dat[dat$impound!="R3",]
dat <- dat[dat$impound!="R7",]
