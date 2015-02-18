#gdistsamp for 2012 Round 1

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora12R1 <- as.matrix(read.csv('Sora_2012_Round1.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov12R1 <- read.csv('Cov_2012_Round1.csv', header=TRUE)
#read in effort matrices
Effort12R1 <- read.csv('YCov_2012_R1_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in water depth matrices
Water_Depth12R1 <- read.csv('YCov_2012_R1_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in water presence matrices
Water_Pres12R1 <- read.csv('YCov_2012_R1_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in Water % Cover matrices
Water12R1 <- read.csv('YCov_2012_R1_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in wood % cover matrices
#read in Wood presence matrices
Wood_Pres12R1 <- read.csv('YCov_2012_R1_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in Interspersion matrices
Int12R1 <- read.csv('YCov_2012_R1_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in short % cover 
Short12R1 <- read.csv('YCov_2012_R1_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in tall % cover
Tall12R1 <- read.csv('YCov_2012_R1_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in PE % cover
PE12R1 <- read.csv('YCov_2012_R1_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#read in PE Presence matrices
PEP12R1 <- read.csv('YCov_2012_R1_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf12R1 = unmarkedFrameGDS(y=sora12R1, 
                           numPrimary=6,
                           siteCovs = cov12R1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12R1$effort_m,
                           yearlySiteCovs=list(PE=PE12R1, PEP=PEP12R1, short=Short12R1, tall=Tall12R1, Int=Int12R1, Water=Water12R1, WaterD=Water_Depth12R1, WaterP=Water_Pres12R1, Wood=Wood12R1, WoodP=Wood_Pres12R1))

#MODELS FOR 2012 ROUND 1

null12R1 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE
)

global12R1 = gdistsamp(lambdaformula = ~Int+WaterP+short+Hab_Type-1, 
                       phiformula = ~1,
                       pformula = ~Int+WaterP+short+Hab_Type-1, 
                       data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

A12R1 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodP-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD+WaterP+WoodP-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)


B12R1 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD+WaterP-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)


C12R1 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

D12R1 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~Int-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

E12R1 = gdistsamp(lambdaformula = ~short-1, 
                  phiformula = ~1, 
                  pformula = ~short-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

G12R1 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~WaterP-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

H12R1 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

I12R1 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type+Int-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

J12R1 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type+Int+WaterD-1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

K12R1 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf12R1, keyfun = "uniform", mixture="NB",se = TRUE)

list12R1 = fitList(null12R1, global12R1,A12R1,B12R1,C12R1,D12R1,E12R1,G12R1,H12R1,I12R1,J12R1,K12R1)
model12R1 = modSel(list12R1)