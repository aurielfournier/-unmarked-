# tidy waterfowl data

library(tidyr)
dat <- read.csv("2014_waterfowl.csv")

gat <- gather(dat, key="species", value="count",waterfowl,cago,dabblers,mall, gadw,nopi, agwt, nosh, amwi, gwfg, rndu, coot, abdu, bwte)
write.csv(gat, "2014_waterfowl_tidy.csv")
