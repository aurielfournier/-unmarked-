library(ggplot2)
library(reshape)
library(reshape2)
library(xtable)

dat <- read.csv("all_surveys.csv",header=T)
dat<- subset(dat, jdate>=0)
sdat <- dat[,c("jdate","vira","time","year")]

smel <- melt(sdat, id=c("jdate","year"))

scast <- cast(jdate ~ variable + year, data=smel, sum, fill=NA_real_)
saverg <- cast(jdate ~ variable, data=smel, mean, fill=NA_real_)

scast$shr_2012 <- scast$vira_2012/scast$time_2012
scast$shr_2013 <- scast$vira_2013/scast$time_2013
scast$shr_2014 <- scast$vira_2014/scast$time_2014
saverg$shr <- saverg$vira/saverg$time

scast12 <- na.omit(data.frame(scast$jdate, scast$shr_2012))
colnames(scast12) <- c("jdate","shr")
scast13 <- na.omit(data.frame(scast$jdate, scast$shr_2013))
colnames(scast13) <- c("jdate","shr")
scast14 <- na.omit(data.frame(scast$jdate, scast$shr_2014))
colnames(scast14) <- c("jdate","shr")
scastav <- na.omit(data.frame(saverg$jdate, saverg$shr))
colnames(scastav) <- c("jdate","shr")

spline12 = smooth.spline(scast12$jdate, scast12$shr, spar=.6)
spline13 = smooth.spline(scast13$jdate, scast13$shr, spar=.6)
spline14 = smooth.spline(scast14$jdate, scast14$shr, spar=.6)
splineav = smooth.spline(saverg$jdate, saverg$shr, spar=.6)

scast12$year = as.factor(rep(2012))
scast13$year = as.factor(rep(2013))
scast14$year = as.factor(rep(2014))
scastav$year <- "Average"

melt1 = rbind(scast12, scast13)
meltall = rbind(melt1, scast14)
meltall = rbind(meltall, scastav)

smoothdf13 = data.frame(x=spline13$x, y=spline13$y, year="2013")
smoothdf12 = data.frame(x=spline12$x, y=spline12$y, year="2012")
smoothdf14 = data.frame(x=spline14$x, y=spline14$y, year="2014")
smoothdfav = data.frame(x=splineav$x, y=splineav$y, year="Average")

smoothdf = rbind(rbind(smoothdf12, smoothdf13),smoothdf14)

m <- melt(smoothdf, id=c("year", "x"))
 ct <- cast(x ~ year, data=m)


ks.test(ct$"2013",ct$"2014")
# p = .2621

ks.test(ct$"2012",ct$"2013")
# p = ..00000003053

ks.test(ct$"2012",ct$"2014")
# p = 0.00001822

