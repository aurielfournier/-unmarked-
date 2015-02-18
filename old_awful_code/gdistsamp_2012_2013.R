#gdistsamp for BOTH 2012 AND 2013


setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora13R1 <- as.matrix(read.csv('Sora_2013_Round1.csv', header=FALSE))
sora13R2 <- as.matrix(read.csv('Sora_2013_Round2.csv', header=FALSE))
sora13R3 <- as.matrix(read.csv('Sora_2013_Round3.csv', header=FALSE))
sora13R4 <- as.matrix(read.csv('Sora_2013_Round4.csv', header=FALSE))

#read in the covariate data #organized by impoundment.
cov13R1 <- read.csv('Cov_2013_Round1.csv', header=TRUE)
cov13R2 <- read.csv('Cov_2013_ROund2.csv', header=TRUE)
cov13R3 <- read.csv('Cov_2013_Round3.csv', header=TRUE)
cov13R4 <- read.csv('Cov_2013_ROund4.csv', header=TRUE)

#read in effort matrices
Effort13R1 <- read.csv('YCov_2013_R1_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Effort13R2 <- read.csv('YCov_2013_R2_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Effort13R3 <- read.csv('YCov_2013_R3_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Effort13R4 <- read.csv('YCov_2013_R4_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in water depth matrices
Water_Depth13R1 <- read.csv('YCov_2013_R1_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Water_Depth13R2 <- read.csv('YCov_2013_R2_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Water_Depth13R3 <- read.csv('YCov_2013_R3_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Water_Depth13R4 <- read.csv('YCov_2013_R4_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in water presence matrices
Water_Pres13R1 <- read.csv('YCov_2013_R1_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Water_Pres13R2 <- read.csv('YCov_2013_R2_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Water_Pres13R3 <- read.csv('YCov_2013_R3_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Water_Pres13R4 <- read.csv('YCov_2013_R4_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in Water % Cover matrices
Water13R1 <- read.csv('YCov_2013_R1_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Water13R2 <- read.csv('YCov_2013_R2_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Water13R3 <- read.csv('YCov_2013_R3_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Water13R4 <- read.csv('YCov_2013_R4_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in wood % cover matrices
Wood13R1 <- read.csv('YCov_2013_R1_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Wood13R2 <- read.csv('YCov_2013_R2_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Wood13R3 <- read.csv('YCov_2013_R3_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Wood13R4 <- read.csv('YCov_2013_R4_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in Wood presence matrices
Wood_Pres13R1 <- read.csv('YCov_2013_R1_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Wood_Pres13R2 <- read.csv('YCov_2013_R2_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Wood_Pres13R3 <- read.csv('YCov_2013_R3_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Wood_Pres13R4 <- read.csv('YCov_2013_R4_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in Interspersion matrices
Int13R1 <- read.csv('YCov_2013_R1_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Int13R2 <- read.csv('YCov_2013_R2_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Int13R3 <- read.csv('YCov_2013_R3_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Int13R4 <- read.csv('YCov_2013_R4_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in short % cover 
Short13R1 <- read.csv('YCov_2013_R1_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Short13R2 <- read.csv('YCov_2013_R2_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Short13R3 <- read.csv('YCov_2013_R3_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Short13R4 <- read.csv('YCov_2013_R4_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in tall % cover
Tall13R1 <- read.csv('YCov_2013_R1_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Tall13R2 <- read.csv('YCov_2013_R2_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
Tall13R3 <- read.csv('YCov_2013_R3_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
Tall13R4 <- read.csv('YCov_2013_R4_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in PE % cover
PE13R1 <- read.csv('YCov_2013_R1_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
PE13R2 <- read.csv('YCov_2013_R2_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
PE13R3 <- read.csv('YCov_2013_R3_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
PE13R4 <- read.csv('YCov_2013_R4_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#read in PE Presence matrices
PEP13R1 <- read.csv('YCov_2013_R1_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
PEP13R2 <- read.csv('YCov_2013_R2_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
PEP13R3 <- read.csv('YCov_2013_R3_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))
PEP13R4 <- read.csv('YCov_2013_R4_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA))

#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12

#Unmarked Data Frame
umf13R1 = unmarkedFrameGDS(y=sora13R1, 
                        numPrimary=6,
                        siteCovs = cov13R1,
                        survey="line", 
                        dist.breaks=cutpt,  
                        unitsIn="m", 
                        tlength=cov13R1$effort_m,
                        #yearlySiteCovs=list(PE=PE13R1, PEP=PEP13R1, short=Short13R1, tall=Tall13R1, Int=Int13R1, Water=Water13R1, WaterD=Water_Depth13R1, WaterPres=Water_Pres13R1, Wood=Wood13R1, WoodPres=Wood_Pres13R1))
)
#MODELS FOR 2013 ROUND 1

null13R1 = gdistsamp(lambdaformula = ~1,
              phiformula = ~1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE
              )
                 
global13R1 = gdistsamp(lambdaformula = ~WaterPres-1, 
              phiformula = ~WaterD+Int-1,
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)
                  
A13R1 = gdistsamp(lambdaformula = ~WaterP+WoodP-1, 
              phiformula = ~1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)
              
B13R1 = gdistsamp(lambdaformula = ~1, 
              phiformula = ~Int-1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

C13R1 = gdistsamp(lambdaformula = ~WoodP-1, 
              phiformula = ~Int-1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

             
D13R1 = gdistsamp(lambdaformula = ~WaterP+WoodP-1, 
              phiformula = ~WaterD-1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

E13R1 = gdistsamp(lambdaformula = ~WaterP+WoodP-1, 
              phiformula = ~Int-1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

G13R1 = gdistsamp(lambdaformula = ~WaterP+WoodP-1, 
              phiformula = ~1, 
              pformula = ~WaterD-1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

H13R1 = gdistsamp(lambdaformula = ~1, 
              phiformula = ~Int-1, 
              pformula = ~WaterD-1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)
              

list13R1 = fitList(null13R1, global13R1, A13R1,B13R1,C13R1,D13R1,E13R1,G13R1,H13R1)
modSel(list13R1)


