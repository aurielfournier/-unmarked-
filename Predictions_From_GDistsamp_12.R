# predictions from GDistsamp 2012

#read in the sora observations
sora12r1 <- read.csv('2012r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r1 <- read.csv('2012r1_cov.csv', header=T)
#subset covaraites we need
cov12r1 <- cov12r1[,c("region","length_1","impound","jdate_1","hectares","area", "int","short","water")]
# #the distance bins

sora12r1 <- sora12r1[order(sora12r1$impound),]
cov12r1 <- cov12r1[order(cov12r1$impound),]

sora12r1 <- sora12r1[,3:41]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r1 = unmarkedFrameGDS(y=sora12r1, 
                           numPrimary=3,
                           siteCovs = cov12r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r1$length_1,
)


null12r1 = gdistsamp(lambdaformula = ~1, 
                   phiformula = ~1, 
                   pformula = ~ 1,
                   data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg12r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area12r1 = gdistsamp(lambdaformula = ~area-1, 
          phiformula = ~1, 
          pformula = ~ 1,
          data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w12r1 =gdistsamp(lambdaformula = ~region+water-1, 
          phiformula = ~1, 
          pformula = ~ 1,
          data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short12r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w12r1 =gdistsamp(lambdaformula = ~short+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global12r1 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

#read in the sora observations
sora12r2 <- read.csv('2012r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r2 <- read.csv('2012r2_cov.csv', header=T)
#subset covaraites we need
cov12r2 <- cov12r2[,c("region","length_2","averagewater_2","impound","jdate_2","hectares","area")]
# #the distance bins

sora12r2 <- sora12r2[order(sora12r2$impound),]
cov12r2 <- cov12r2[order(cov12r2$impound),]

sora12r2 <- sora12r2[,3:41]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r2 = unmarkedFrameGDS(y=sora12r2, 
                           numPrimary=3,
                           siteCovs = cov12r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r2$length_2,
)


reg12r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")



#read in the sora observations
sora12r3 <- read.csv("2012r3_sora.csv", header=T)
#read in the covariate data #organized by impoundment.
cov12r3 <- read.csv('2012r3_cov.csv', header=T)
#subset the covariates
cov12r3 <- cov12r3[,c("impound","region","length_3","averagewater_3","impound","jdate_3","hectares","area")]
# #the distance bins

#cov12r3 <- cov12r3[(cov12r3$impound %in% sora12r3$impound),]

sora12r3 <- sora12r3[order(sora12r3$impound),]
cov12r3 <- cov12r3[order(cov12r3$impound),]
sora12r3 <- sora12r3[,2:40]


cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r3 = unmarkedFrameGDS(y=sora12r3, 
                           numPrimary=3,
                           siteCovs = cov12r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r3$length_3,
)



reg12r3 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")



options(scipen=999) #disables scientific notation


ab12r1 <- ranef(reg12r1)
abund12r1 <- data.frame(matrix(ncol=4, nrow=12))
abund12r1$X1 <- bup(ab12r1, stat="mean")
abund12r1$X2 <- bup(ab12r1, stat="mode")
abund12r1[,3:4] <- confint(ab12r1, level=0.9) # 90% CI
abund12r1$impound <- cov12r1$impound
abund12r1$jdate <- cov12r1$jdate_1
abund12r1$region <- cov12r1$region
abund12r1$hectares <- cov12r1$hectares
abund12r1$area <- cov12r1$area
abund12r1$year <- 2012
abund12r1$round <- 1
colnames(abund12r1) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")


ab12r2 <- ranef(reg12r2)
abund12r2 <- data.frame(matrix(ncol=4, nrow=28))
abund12r2$X1 <- bup(ab12r2, stat="mean")
abund12r2$X2 <- bup(ab12r2, stat="mode")
abund12r2[,3:4] <- confint(ab12r2, level=0.9) # 90% CI
abund12r2$impound <- cov12r2$impound
abund12r2$jdate <- cov12r2$jdate_2
abund12r2$region <- cov12r2$region
abund12r2$hectares <- cov12r2$hectares
abund12r2$area <- cov12r2$area
abund12r2$year <- 2012
abund12r2$round <- 2
colnames(abund12r2) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")


ab12r3 <- ranef(reg12r3)
abund12r3 <- data.frame(matrix(ncol=4, nrow=21))
abund12r3$X1 <- bup(ab12r3, stat="mean")
abund12r3$X2 <- bup(ab12r3, stat="mode")
abund12r3[,3:4] <- confint(ab12r3, level=0.9) # 90% CI
abund12r3$impound <- cov12r3$impound
abund12r3$jdate <- cov12r3$jdate_3
abund12r3$region <- cov12r3$region
abund12r3$hectares <- cov12r3$hectares
abund12r3$area <- cov12r3$area
abund12r3$year <- 2012
abund12r3$round <- 3
colnames(abund12r3) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")

rr <- rbind(rbind(abund12r3,abund12r2),abund12r1)

rr$treat <- NA

rr <- rr[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares","area","year","round")]

write.csv(rr, "abundances_2012.csv",row.names=F)
