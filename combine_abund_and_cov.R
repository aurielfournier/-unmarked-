## my adviser wanted my abundance estimates in the same file as my covariates so he could look at them

setwd("~/GitHub/data")
## 2012
abund <- read.csv("abundances_2012.csv")
c12 <- read.csv("2012r3_cov_nostand.csv")

c12 <- c12[,c(1:2,6:15)]

d12 <- merge(abund,c12,by=c("impound"))
write.csv(d12, "abund_cov_2012_nostand.csv", row.names=F)

## 2013
abund <- read.csv("abundances_2013.csv")

c13 <- read.csv("2013r3_cov_nostand.csv")

c13 <- c13[,c(1:2,6:17)]

d13 <- merge(abund,c13,by=c("impound"))
write.csv(d13, "abund_cov_2013_nostand.csv", row.names=F)

## 2014
abund <- read.csv("abundances_2014.csv")
c14 <- read.csv("2014r3_cov_nostand.csv")
c14 <- c14[,c(1:2,7:11)]

d14 <- merge(abund,c14,by=c("impound"))
write.csv(d14, "abund_cov_2014_nostand.csv", row.names=F)
