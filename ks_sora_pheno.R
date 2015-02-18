library(ggplot2)
library(reshape)


a12 <- read.csv("abundances_2012.csv")
a12 <- a12[,c("mean","jdate")]
a13 <- read.csv("abundances_2013.csv")
a13 <- a13[,c("mean","jdate")]
a14 <- read.csv('abundances_2014.csv')
a14 <- a14[,c("mean","jdate")]

c12 <- melt(a12, id=c("jdate"))
c12 <- cast(jdate ~ variable, data=c12, mean, fill=NA_real_)

spline12 = smooth.spline(c12$jdate, c12$mean, spar=.8)
smoothdf12 = data.frame(x=spline12$x, y=spline12$y, year=2012)

c13 <- melt(a13, id=c("jdate"))
c13 <- cast(jdate ~ variable, data=c13, mean, fill=NA_real_)

spline13 = smooth.spline(c13$jdate, c13$mean, spar=.8)
smoothdf13 = data.frame(x=spline13$x, y=spline13$y, year=2013)

c14 <- melt(a14, id=c("jdate"))
c14 <- cast(jdate ~ variable, data=c14, mean, fill=NA_real_)

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

abund_week <- rbind(zero,cdat)


ks.test(abund_week$"2013",abund_week$"2012")

ks.test(abund_week$"2014",abund_week$"2013")

ks.test(abund_week$"2014",abund_week$"2012")
