# predictions from GDistsamp 2012
#setwd("~/Documents/data")
library(unmarked)
#read in the sora observations

load("2012_models.Rdata")
cov <- read.csv('~/Documents/data/2012_cov.csv', header=T)

ab12 <- ranef(model$averagewater_short)
abund12 <- data.frame(matrix(ncol=4, nrow=nrow(cov)))
abund12$X1 <- bup(ab12, stat="mean")
abund12$X2 <- bup(ab12, stat="mode")
abund12[,3:4] <- confint(ab12, level=0.9) # 90% CI
abund12$impound <- cov$impound
abund12$jdate <- cov$jdate
abund12$region <- cov$region
abund12$hectares <- cov$hectares
abund12$area <- cov$area
abund12$year <- 2012
abund12$round <- cov$round
abund12$treat <- NA
abund12$scale_averagewater <- cov$scale_averagewater
abund12$averagewater <- cov$averagewater
colnames(abund12) <- c("mean","mode","CI1","CI2","impound","jdate","region","area","year","round","treat","scale_averagewater","averagewater")


rr <- abund12[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","area","year","round","scale_averagewater","averagewater")]

write.csv(rr, "~/Documents/data/abundances_2012.csv",row.names=F)
