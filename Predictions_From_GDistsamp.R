library(unmarked)
library(ggplot2)
library(reshape)
library(gridExtra)
#read in sora data
sora14r1 <- read.csv('2014r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r1 <- read.csv('2014r1_cov.csv', header=T)
#subset the covariates we need
cov14r1 <- cov14r1[,c("region","area","length_1","averagewater_1","impound","treat","jdate_1","hectares")]

sora14r1 <- sora14r1[order(sora14r1$impound),]
cov14r1 <- cov14r1[order(cov14r1$impound),]

sora14r1 <- sora14r1[,2:79]

#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) #the fartherest distance is 12
#Unmarked Data Frame
umf14r1 = unmarkedFrameGDS(y=sora14r1, 
                           numPrimary=6,
                           siteCovs = cov14r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r1$length_1,
)


reg14r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

#read in the sora observations
sora14r2 <- read.csv('2014r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r2 <- read.csv('2014r2_cov.csv', header=T)
#subset covaraites we need
cov14r2 <- cov14r2[,c("region","area","length_2","averagewater_2","impound","treat","jdate_2","hectares")]
# #the distance bins

sora14r2 <- sora14r2[order(sora14r2$impound),]
cov14r2 <- cov14r2[order(cov14r2$impound),]

sora14r2 <- sora14r2[,2:79]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r2 = unmarkedFrameGDS(y=sora14r2, 
                           numPrimary=6,
                           siteCovs = cov14r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r2$length_2,
)


reg14r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")



#read in the sora observations
sora14r3 <- read.csv("2014r3_sora.csv", header=T)
#read in the covariate data #organized by impoundment.
cov14r3 <- read.csv('2014r3_cov.csv', header=T)
#subset the covariates
cov14r3 <- cov14r3[,c("region","area","length_3","averagewater_3","impound","treat","jdate_3","hectares")]
# #the distance bins

sora14r3 <- sora14r3[order(sora14r3$impound),]
cov14r3 <- cov14r3[order(cov14r3$impound),]
sora14r3 <- sora14r3[,2:79]

cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r3 = unmarkedFrameGDS(y=sora14r3, 
                           numPrimary=6,
                           siteCovs = cov14r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r3$length_3,
)



treat14r3 = gdistsamp(lambdaformula = ~treat-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


#sora 
sora14r4 <- read.csv('2014r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r4 <- read.csv('2014r4_cov.csv', header=T)
#subset the covariates
cov14r4 <- cov14r4[,c("region","area","length_4","averagewater_4","impound","treat","jdate_4","hectares")]
# the distance bins

sora14r4 <- sora14r4[order(sora14r4$impound),]
cov14r4 <- cov14r4[order(cov14r4$impound),]
sora14r4 <- sora14r4[,2:79]

cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r4 = unmarkedFrameGDS(y=sora14r4, 
                           numPrimary=6,
                           siteCovs = cov14r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r4$length_4,
)


treat14r4 = gdistsamp(lambdaformula = ~treat-1, 
                          phiformula = ~1, 
                          pformula = ~ 1,
                          data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


options(scipen=999) #disables scientific notation

ab14r1 <- ranef(reg14r1)
abund14r1 <- data.frame(matrix(ncol=4, nrow=33))
abund14r1$X1 <- bup(ab14r1, stat="mean")
abund14r1$X2 <- bup(ab14r1, stat="mode")
abund14r1[,3:4] <- confint(ab14r1, level=0.9) # 90% CI
abund14r1$impound <- cov14r1$impound
abund14r1$jdate <- cov14r1$jdate_1
abund14r1$region <- cov14r1$region
abund14r1$treat <- cov14r1$treat
abund14r1$hectares <- cov14r1$hectares
abund14r1$area <- cov14r1$area
abund14r1$year <- 2014
abund14r1$round <- 1
colnames(abund14r1) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares","area","year","round")
 

ab14r2 <- ranef(reg14r2)
abund14r2 <- data.frame(matrix(ncol=4, nrow=33))
abund14r2$X1 <- bup(ab14r2, stat="mean")
abund14r2$X2 <- bup(ab14r2, stat="mode")
abund14r2[,3:4] <- confint(ab14r2, level=0.9) # 90% CI
abund14r2$impound <- cov14r2$impound
abund14r2$jdate <- cov14r2$jdate_2
abund14r2$region <- cov14r2$region
abund14r2$treat <- cov14r2$treat
abund14r2$hectares <- cov14r2$hectares
abund14r2$area <- cov14r2$area
abund14r2$year <- 2014
abund14r2$round <- 2
colnames(abund14r2) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares","area","year","round")


ab14r3 <- ranef(treat14r3)
abund14r3 <- data.frame(matrix(ncol=4, nrow=32))
abund14r3$X1 <- bup(ab14r3, stat="mean")
abund14r3$X2 <- bup(ab14r3, stat="mode")
abund14r3[,3:4] <- confint(ab14r3, level=0.9) # 90% CI
abund14r3$impound <- cov14r3$impound
abund14r3$jdate <- cov14r3$jdate_3
abund14r3$region <- cov14r3$region
abund14r3$treat <- cov14r3$treat
abund14r3$hectares <- cov14r3$hectares
abund14r3$area <- cov14r3$area
abund14r3$year <- 2014
abund14r3$round <- 3
colnames(abund14r3) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares","area","year","round")


ab14r4 <- ranef(treat14r4)
abund14r4 <- data.frame(matrix(ncol=4, nrow=32))
abund14r4$X1 <- bup(ab14r4, stat="mean")
abund14r4$X2 <- bup(ab14r4, stat="mode")
abund14r4[,3:4] <- confint(ab14r4, level=0.9) # 90% CI
abund14r4$impound <- cov14r4$impound
abund14r4$jdate <- cov14r4$jdate_4
abund14r4$region <- cov14r4$region
abund14r4$treat <- cov14r4$treat
abund14r4$hectares <- cov14r4$hectares
abund14r4$area <- cov14r4$area
abund14r4$year <- 2014
abund14r4$round <- 4
colnames(abund14r4) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares","area","year","round")

rr <- rbind(rbind(rbind(abund14r4, abund14r3),abund14r2),abund14r1)

write.csv(rr, "abundances_2014.csv", row.names=F)
