# predictions from GDistsamp 2012

library(unmarked)
#read in the sora observations
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_cov.csv', header=T)
#subset covaraites we need

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

r_w14 =gdistsamp(lambdaformula = ~region+scale_averagewater-1, 
               phiformula = ~1, 
               pformula = ~ 1,
               data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

ab14 <- ranef(r_w14)
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
