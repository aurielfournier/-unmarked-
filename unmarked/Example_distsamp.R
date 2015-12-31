library(unmarked)
dists <- read.csv(system.file("csv", "distdata.csv", package="unmarked"))

levels(dists$transect) <- c(levels(dists$transect), "g")
levels(dists$transect)


yDat <- formatDistData(dists, distCol="distance",
                       transectNameCol="transect", dist.breaks=c(0, 5, 10, 15, 20))
(covs <- data.frame(canopyHt = c(5, 8, 3, 2, 4, 7, 5),
                    habitat = c('A','A','A','A','B','B','B'), row.names=letters[1:7]))
umf <- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=covs, survey="line",
                       dist.breaks=c(0, 5, 10, 15, 20), tlength=rep(100, 7),
                       unitsIn="m")
hist(umf)