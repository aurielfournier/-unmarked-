# saracov

cov12 <- read.csv('./data/2012_cov.csv', header=T)
cov13 <- read.csv('./data/2013_cov.csv', header=T)
cov14 <- read.csv('./data/2014_cov.csv', header=T)
cov14$year <- 2014
cov15 <- read.csv('./data/2015_cov.csv', header=T)
cov15$year <- 2015

cov12 <- cov12[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region","impound","int","short")]

cov <- rbind(cov13[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region.x","impound","int","short")],
             cov14[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region.x","impound","int","short")],
             cov15[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region.x","impound","int","short")])

colnames(cov) <- colnames(cov12)

cov <- rbind(cov, cov12)

cov$scale_averagewater2 <- cov$scale_averagewater^2


write.csv(cov, row.names=FALSE, file="./data/sara_cov.csv")
