dat <- read.csv("abundances_2014.csv")


dat <- dat[!dat$region=="ne",]
dat <- dat[!(dat$area=="slnwr"&dat$round==3),]
dat <- dat[!(dat$area=="slnwr"&dat$round==4),]
dat <- dat[!(dat$area=="fgca"&dat$round==3),]
dat <- dat[!(dat$area=="fgca"&dat$round==3),]
glm <- glm(data=dat, mean ~ treat + region)
summary(glm)
