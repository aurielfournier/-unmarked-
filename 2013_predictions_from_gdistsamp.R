# predictions from GDistsamp 2013
library(unmarked)
#read in sora data

sora13r1 <- read.csv('2013r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r1 <- read.csv('2013r1_cov.csv', header=T)
#subset the covariates we need
cov13r1 <- cov13r1[,c("region","length_1","averagewater_1","impound","jdate_1","hectares","area")]

sora13r1 <- sora13r1[order(sora13r1$impound),]
cov13r1 <- cov13r1[order(cov13r1$impound),]

sora13r1 <- sora13r1[,2:ncol(sora13r1)]

#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) #the fartherest distance is 12
#Unmarked Data Frame
umf13r1 = unmarkedFrameGDS(y=sora13r1, 
                           numPrimary=6,
                           siteCovs = cov13r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r1$length_1,
)

n13r1 = gdistsamp(lambdaformula = ~1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

#read in the sora observations
sora13r2 <- read.csv('2013r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r2 <- read.csv('2013r2_cov.csv', header=T)
#subset covaraites we need
cov13r2 <- cov13r2[,c("region","length_2","averagewater_2","impound","jdate_2","hectares","area")]
# #the distance bins

sora13r2 <- sora13r2[order(sora13r2$impound),]
cov13r2 <- cov13r2[order(cov13r2$impound),]

sora13r2 <- sora13r2[,2:79]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r2 = unmarkedFrameGDS(y=sora13r2, 
                           numPrimary=6,
                           siteCovs = cov13r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r2$length_2,
)


n13r2 = gdistsamp(lambdaformula = ~1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")



#read in the sora observations
sora13r3 <- read.csv("2013r3_sora.csv", header=T)
#read in the covariate data #organized by impoundment.
cov13r3 <- read.csv('2013r3_cov.csv', header=T)
#subset the covariates
cov13r3 <- cov13r3[,c("region","length_3","averagewater_3","impound","jdate_3","hectares","area")]
# #the distance bins

sora13r3 <- sora13r3[order(sora13r3$impound),]
cov13r3 <- cov13r3[order(cov13r3$impound),]
sora13r3 <- sora13r3[,2:79]
#cov13r3$jdate_3 <- as.factor(cov13r3$jdate_3)

cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r3 = unmarkedFrameGDS(y=sora13r3, 
                           numPrimary=6,
                           siteCovs = cov13r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r3$length_3,
)



n13r3 = gdistsamp(lambdaformula = ~1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


#sora 
sora13r4 <- read.csv('2013r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r4 <- read.csv('2013r4_cov.csv', header=T)
#subset the covariates
cov13r4 <- cov13r4[,c("region","length_4","averagewater_4","impound","jdate_4","hectares","area")]
# the distance bins

sora13r4 <- sora13r4[order(sora13r4$impound),]
cov13r4 <- cov13r4[order(cov13r4$impound),]
sora13r4 <- sora13r4[,2:79]

cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r4 = unmarkedFrameGDS(y=sora13r4, 
                           numPrimary=6,
                           siteCovs = cov13r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r4$length_4,
)


n13r4 = gdistsamp(lambdaformula = ~1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


options(scipen=999) #disables scientific notation

ab13r1 <- ranef(n13r1)
abund13r1 <- data.frame(matrix(ncol=4, nrow=27))
abund13r1$X1 <- bup(ab13r1, stat="mean")
abund13r1$X2 <- bup(ab13r1, stat="mode")
abund13r1[,3:4] <- confint(ab13r1, level=0.9) # 90% CI
abund13r1$impound <- cov13r1$impound
abund13r1$jdate <- cov13r1$jdate_1
abund13r1$region <- cov13r1$region
abund13r1$hectares <- cov13r1$hectares
abund13r1$area <- cov13r1$area
abund13r1$year <- 2013
abund13r1$round <- 1
colnames(abund13r1) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")


ab13r2 <- ranef(n13r2)
abund13r2 <- data.frame(matrix(ncol=4, nrow=27))
abund13r2$X1 <- bup(ab13r2, stat="mean")
abund13r2$X2 <- bup(ab13r2, stat="mode")
abund13r2[,3:4] <- confint(ab13r2, level=0.9) # 90% CI
abund13r2$impound <- cov13r2$impound
abund13r2$jdate <- cov13r2$jdate_2
abund13r2$region <- cov13r2$region
abund13r2$hectares <- cov13r2$hectares
abund13r2$area <- cov13r2$area
abund13r2$year <- 2013
abund13r2$round <- 2
colnames(abund13r2) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")


ab13r3 <- ranef(n13r3)
abund13r3 <- data.frame(matrix(ncol=4, nrow=30))
abund13r3$X1 <- bup(ab13r3, stat="mean")
abund13r3$X2 <- bup(ab13r3, stat="mode")
abund13r3[,3:4] <- confint(ab13r3, level=0.9) # 90% CI
abund13r3$impound <- cov13r3$impound
abund13r3$jdate <- cov13r3$jdate_3
abund13r3$region <- cov13r3$region
abund13r3$hectares <- cov13r3$hectares
abund13r3$area <- cov13r3$area
abund13r3$year <- 2013
abund13r3$round <- 3
colnames(abund13r3) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")


ab13r4 <- ranef(n13r4)
abund13r4 <- data.frame(matrix(ncol=4, nrow=10))
abund13r4$X1 <- bup(ab13r4, stat="mean")
abund13r4$X2 <- bup(ab13r4, stat="mode")
abund13r4[,3:4] <- confint(ab13r4, level=0.9) # 90% CI
abund13r4$impound <- cov13r4$impound
abund13r4$jdate <- cov13r4$jdate_4
abund13r4$region <- cov13r4$region
abund13r4$hectares <- cov13r4$hectares
abund13r4$area <- cov13r4$area
abund13r4$year <- 2013
abund13r4$round <- 4
colnames(abund13r4) <- c("mean","mode","CI1","CI2","impound","jdate","region","hectares","area","year","round")

rr <- rbind(rbind(rbind(abund13r3,abund13r2),abund13r1),abund13r4)

rr$treat <- NA

rr <- rr[,c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares","area","year","round")]

write.csv(rr, "abundances_2013.csv", row.names=F)
