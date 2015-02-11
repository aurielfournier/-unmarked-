library(ggplot2)
library(reshape)
# smooth spline of abundance estimates for 2014

#setwd("~/SourceTree/data")
a12r1 <- read.csv("abundance_12r1.csv")
a12r1 <- a12r1[,c("mean","jdate")]
a12r2 <- read.csv("abundance_12r2.csv")
a12r2 <- a12r2[,c("mean","jdate")]
a12r3 <- read.csv("abundance_12r3.csv")
a12r3 <- a12r3[,c("mean","jdate")]


a13r1 <- read.csv("abundance_13r1.csv")
a13r1 <- a13r1[,c("mean","jdate")]
a13r2 <- read.csv("abundance_13r2.csv")
a13r2 <- a13r2[,c("mean","jdate")]
a13r3 <- read.csv("abundance_13r3.csv")
a13r3 <- a13r3[,c("mean","jdate")]
a13r4 <- read.csv("abundance_13r4.csv")
a13r4 <- a13r4[,c("mean","jdate")]

a14r1 <- read.csv("abundance_14r1.csv")
a14r1 <- a14r1[,c("mean","jdate")]
a14r2 <- read.csv("abundance_14r2.csv")
a14r2 <- a14r2[,c("mean","jdate")]
a14r3 <- read.csv("abundance_14r3.csv")
a14r3 <- a14r3[,c("mean","jdate")]
a14r4 <- read.csv("abundance_14r4.csv")
a14r4 <- a14r4[,c("mean","jdate")]

c12r1 <- melt(a12r1, id=c("jdate"))
c12r1 <- cast(jdate ~ variable, data=c12r1, sum, fill=NA_real_)
c12r2 <- melt(a12r2, id=c("jdate"))
c12r2 <- cast(jdate ~ variable, data=c12r2, sum, fill=NA_real_)
c12r3 <- melt(a12r3, id=c("jdate"))
c12r3 <- cast(jdate ~ variable, data=c12r3, sum, fill=NA_real_)
c1212 <- rbind(c12r1, c12r2)
c12 <- rbind(c1212, c12r3)
spline12 = smooth.spline(c12$jdate, c12$mean, spar=.8)
smoothdf12 = data.frame(x=spline12$x, y=spline12$y, year=2012)


c13r1 <- melt(a13r1, id=c("jdate"))
c13r1 <- cast(jdate ~ variable, data=c13r1, sum, fill=NA_real_)
c13r2 <- melt(a13r2, id=c("jdate"))
c13r2 <- cast(jdate ~ variable, data=c13r2, sum, fill=NA_real_)
c13r3 <- melt(a13r3, id=c("jdate"))
c13r3 <- cast(jdate ~ variable, data=c13r3, sum, fill=NA_real_)
c13r4 <- melt(a13r4, id=c("jdate"))
c13r4 <- cast(jdate ~ variable, data=c13r4, sum, fill=NA_real_)
c13r12 <- rbind(c13r1, c13r2)
c13r34 <- rbind(c13r3, c13r4)
c13 <- rbind(c13r12, c13r34)
spline13 = smooth.spline(c13$jdate, c13$mean, spar=.8)
smoothdf13 = data.frame(x=spline13$x, y=spline13$y, year=2013)

c14r1 <- melt(a14r1, id=c("jdate"))
c14r1 <- cast(jdate ~ variable, data=c14r1, sum, fill=NA_real_)
c14r2 <- melt(a14r2, id=c("jdate"))
c14r2 <- cast(jdate ~ variable, data=c14r2, sum, fill=NA_real_)
c14r3 <- melt(a14r3, id=c("jdate"))
c14r3 <- cast(jdate ~ variable, data=c14r3, sum, fill=NA_real_)
c14r4 <- melt(a14r4, id=c("jdate"))
c14r4 <- cast(jdate ~ variable, data=c14r4, sum, fill=NA_real_)
c14r12 <- rbind(c14r1, c14r2)
c14r34 <- rbind(c14r3, c14r4)
c14 <- rbind(c14r12, c14r34)
spline14 = smooth.spline(c14$jdate, c14$mean, spar=.7)
smoothdf14 = data.frame(x=spline14$x, y=spline14$y, year=2014)

xaxis <- data.frame(jdate=c(min(smoothdf14$x):max(smoothdf14$x)), value=rep(0))

smoothdf <- rbind(rbind(smoothdf12, smoothdf13),smoothdf14)
smoothdf$year <- as.factor(smoothdf$year)

m <- melt(smoothdf, id=c("year", "x"))
 ct <- cast(x ~ year, data=m)

ks.test(ct$"2013",ct$"2014")
# p = .0000007226

ks.test(ct$"2012",ct$"2013")
# p = .09943

ks.test(ct$"2012",ct$"2014")
# p = 0.0008699

m$week <-ifelse(m$x>=226&m$x<=232,1,
                 ifelse(m$x>=233&m$x<=239,2,
                 ifelse(m$x>=240&m$x<=246.9,3,
                 ifelse(m$x>=247&m$x<=253.9,4,
                 ifelse(m$x>=254&m$x<=260.9,5,
                 ifelse(m$x>=261&m$x<=267,6,
                 ifelse(m$x>=268&m$x<=274,7,
                 ifelse(m$x>=275&m$x<=281,8,
                 ifelse(m$x>=282&m$x<=288,9,
                 ifelse(m$x>=289&m$x<=295,10,
                 ifelse(m$x>=296&m$x<=300,11,NA)))))))))))

cdat <- cast(week ~ year, data=m, mean )

zero <- as.data.frame(matrix(0, nrow=2,ncol=4))
colnames(zero) <- colnames(cdat)

mo <- rbind(zero,cdat)


ks.test(cdat12$Sora,mo$"2012")

ks.test(cdat13$Sora,mo$"2013")

ks.test(cdat14$Sora,mo$"2014")
