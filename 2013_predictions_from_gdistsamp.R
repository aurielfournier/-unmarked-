# predictions from GDistsamp 2012
#setwd("~/GitHub/data")
library(unmarked)
#read in the sora observations
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_cov.csv', header=T)
#subset covaraites we need
cov <- cov[,c("region","length","impound","jdate","area", "scale_int","scale_short","scale_averagewater","round")]
# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,3:80]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                       numPrimary=6,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length,
)

r_w_i =gdistsamp(lambdaformula = ~region+scale_averagewater+region*scale_averagewater-1, 
               phiformula = ~1, 
               pformula = ~ 1,
               data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

ab13 <- ranef(r_w_i)
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
colnames(abund13) <- c("mean","mode","CI1","CI2","impound","jdate","region","area","year","round","treat","scale_averagewater")


rr <- abund13[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","area","year","round","scale_averagewater")]

write.csv(rr, "abundances_2013.csv",row.names=F)