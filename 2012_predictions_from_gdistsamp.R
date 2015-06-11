# predictions from GDistsamp 2012
#setwd("~/Documents/data")
library(unmarked)
#read in the sora observations
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
#subset covaraites we need
cov <- cov[,c("region","length","impound","jdate","area", "scale_int","scale_short","scale_averagewater","round")]
# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                         numPrimary=3,
                         siteCovs = cov,
                         survey="line", 
                         dist.breaks=cutpt,  
                         unitsIn="m", 
                         tlength=cov$length,
)

r =gdistsamp(lambdaformula = ~region-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", 
                      mixture="NB",se = T, output="abund",)

ab12 <- ranef(r)
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
colnames(abund12) <- c("mean","mode","CI1","CI2","impound","jdate","region","area","year","round","treat")


rr <- abund12[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","area","year","round")]

write.csv(rr, "C:/Users/avanderlaar/Documents/GitHub/data/abundances_2012.csv",row.names=F)
