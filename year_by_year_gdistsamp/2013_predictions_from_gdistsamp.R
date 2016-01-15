# predictions from GDistsamp 2012
#setwd("~/GitHub/data")
library(unmarked)


load("C:/Users/avand/Documents/data/2013_models.Rdata")
cov <- read.csv('~/Documents/data/2013_cov.csv', header=T)

ab13 <- ranef(model$`~scale_averagewater-1`)
abund13 <- data.frame(matrix(ncol=4, nrow=nrow(cov)))
abund13$X1 <- bup(ab13, stat="mean")
abund13$X2 <- bup(ab13, stat="mode")
abund13[,3:4] <- confint(ab13, level=0.9) # 90% CI
abund13$impound <- cov$impound
abund13$jdate <- cov$jdate
abund13$region <- cov$region
abund13$hectares <- cov$hectares
abund13$area <- cov$area
abund13$year <- 2012
abund13$round <- cov$round
abund13$treat <- NA
abund13$scale_averagewater <- cov$scale_averagewater
abund13$averagewater <- cov$averagewater
colnames(abund13) <- c("mean","mode","CI1","CI2","impound","jdate","region","area","year","round","treat","scale_averagewater","averagewater")


rr <- abund13[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","area","year","round","scale_averagewater","averagewater")]

write.csv(rr, "~/Documents/data/abundances_2013.csv",row.names=F)
