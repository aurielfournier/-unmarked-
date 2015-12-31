# predictions from GDistsamp 2012

library(unmarked)


load("2014_models.Rdata")
cov <- read.csv('~/Documents/data/2014_cov.csv', header=T)


ab14 <- ranef(model$region_averagewater)
abund14 <- data.frame(matrix(ncol=4, nrow=nrow(cov)))
abund14$X1 <- bup(ab14, stat="mean")
abund14$X2 <- bup(ab14, stat="mode")
abund14[,3:4] <- confint(ab14, level=0.9) # 90% CI
abund14$impound <- cov$impound
abund14$jdate <- cov$jdate
abund14$region <- cov$region
abund14$hectares <- cov$hectares
abund14$area <- cov$area
abund14$year <- 2012
abund14$round <- cov$round
abund14$treat <- NA
abund14$scale_averagewater <- cov$scale_averagewater
abund14$averagewater <- cov$averagewater
colnames(abund14) <- c("mean","mode","CI1","CI2","impound","jdate","region","area","year","round","treat","scale_averagewater","averagewater")

rr <- abund14[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","area","year","round","scale_averagewater","averagewater")]

write.csv(rr, "C:/Users/avanderlaar/Documents/GitHub/data/abundances_2014.csv",row.names=F)
