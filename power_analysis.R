#power analysis
setwd("C:/Users/avanderlaar/Dropbox/Data")
library(reshape)
library(pwr)
library(compute.es)

a14r1 <- read.csv('abundance_14r1.csv',header=T)
a14r1 <- a14r1[,c("treat","area","mean")]
a14r2 <- read.csv('abundance_14r2.csv',header=T)
a14r2 <- a14r2[,c("treat","area","mean")]
a14r3 <- read.csv('abundance_14r3.csv',header=T)
a14r3 <- a14r3[,c("treat","area","mean")]
a14r4 <- read.csv('abundance_14r4.csv',header=T)
a14r4 <- a14r4[,c("treat","area","mean")]

m14r1 <- melt(a14r1)
c14r1 <- cast(data=m14r1, area ~ treat, mean)

m14r2 <- melt(a14r2)
c14r2 <- cast(data=m14r2, area ~ treat, mean)

m14r3 <- melt(a14r3)
c14r3 <- cast(data=m14r3, area ~ treat, mean)

m14r4 <- melt(a14r4)
c14r4 <- cast(data=m14r4, area ~ treat, mean)

pwr <- rbind(c14r1, c14r2)
pwr <- rbind(pwr, c14r3)
pwr <- rbind(pwr, c14r4)


e.s.treatall = mean(pwr$E, na.rm=T) - mean(pwr$L, na.rm=T)
e.s.treatr1 = mean(c14r1$E, na.rm=T) - mean(c14r1$L, na.rm=T)
e.s.treatr2 = mean(c14r2$E, na.rm=T) - mean(c14r2$L, na.rm=T)
e.s.treatr3 = mean(c14r3$E, na.rm=T) - mean(c14r3$L, na.rm=T)
e.s.treatr4 = mean(c14r4$E, na.rm=T) - mean(c14r4$L, na.rm=T)


all <- pwr.anova.test(k=2, n=NULL, f=1, sig.level=0.05, power=.99)
r1 <- pwr.anova.test(k=2, n=NULL, f=-e.s.treatr1, sig.level=0.05, power=.99)
r2 <- pwr.anova.test(k=2, n=NULL, f=-e.s.treatr2, sig.level=0.05, power=.99)
r3 <- pwr.anova.test(k=2, n=NULL, f=-e.s.treatr3, sig.level=0.05, power=.99)
r4 <- pwr.anova.test(k=2, n=NULL, f=-e.s.treatr4, sig.level=0.05, power=.99)


all <- pwr.t.test(n=NULL, d=e.s.treatall, sig.level=0.05, power=.99, type="paired")
r1 <- pwr.t.test(n=NULL, d=e.s.treatr1, sig.level=0.05, power=.99, type="paired")
r2 <- pwr.t.test(n=NULL, d=e.s.treatr2, sig.level=0.05, power=.99, type="paired")
r3 <- pwr.t.test(n=NULL, d=e.s.treatr3, sig.level=0.05, power=.99, type="paired")
r4 <- pwr.t.test(n=NULL, d=e.s.treatr4, sig.level=0.05, power=.99, type="paired")

