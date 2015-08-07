# predictions from GDistsamp 2012

library(unmarked)
#read in the sora observations
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_sora.csv', header=T)

## removing impoundments which were confounded in the experiment or were not the appropriate habtiat type
#sora <- sora[!(sora$impound=="ccmsu12"|sora$impound=="ccmsu2"|sora$impound=="ccmsu1"|sora$impound=="ts2a"|sora$impound=="ts4a"|sora$impound=="ts6a"|sora$impound=="ts8a"|sora$impound=="kt2"|sora$impound=="kt5"|sora$impound=="kt5"|sora$impound=="kt6"|sora$impound=="kt9"|sora$impound=="pool2"|sora$impound=="pool2w"|sora$impound=="pool3w"|sora$impound=="m10"|sora$impound=="m11"|sora$impound=="m13"),]

#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_cov.csv', header=T)
## removing impoundments which were confounded in the experiment or were not the appropriate habtiat type
#cov <- cov[!(cov$impound=="ccmsu12"|cov$impound=="ccmsu2"|cov$impound=="ccmsu1"|cov$impound=="ts2a"|cov$impound=="ts4a"|cov$impound=="ts6a"|cov$impound=="ts8a"|cov$impound=="kt2"|cov$impound=="kt5"|cov$impound=="kt5"|cov$impound=="kt6"|cov$impound=="kt9"|cov$impound=="pool2"|cov$impound=="pool2w"|cov$impound=="pool3w"|cov$impound=="m10"|cov$impound=="m11"|cov$impound=="m13"),]

#subset covaraites we need

cov <- cov[,c("region","length","impound","jdate","area", "treat","scale_short","scale_averagewater","round","averagewater")]

# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:31]
cutpt = as.numeric(c(0,1,2,3,4,5)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                       numPrimary=6,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length
)

r_w14 =gdistsamp(lambdaformula = ~region+scale_averagewater-1, 
               phiformula = ~1, 
               pformula = ~ 1,
               data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

save(r_w14, file="2014_top_model.Rdata")

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
