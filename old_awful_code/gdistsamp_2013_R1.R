#gdistsamp for 2013 Round 1

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora13R1 <- as.matrix(read.csv('Sora_2013_Round1.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov13R1 <- read.csv('Cov_2013_Round1.csv', header=TRUE)
#read in effort matrices
# Effort13R1 <- read.csv('YCov_2013_R1_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in water depth matrices
# Water_Depth13R1 <- read.csv('YCov_2013_R1_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in water presence matrices
# Water_Pres13R1 <- read.csv('YCov_2013_R1_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Water % Cover matrices
# Water13R1 <- read.csv('YCov_2013_R1_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in wood % cover matrices
# Wood13R1 <- read.csv('YCov_2013_R1_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Wood presence matrices
# Wood_Pres13R1 <- read.csv('YCov_2013_R1_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Interspersion matrices
# Int13R1 <- read.csv('YCov_2013_R1_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in short % cover 
# Short13R1 <- read.csv('YCov_2013_R1_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in tall % cover
# Tall13R1 <- read.csv('YCov_2013_R1_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in PE % cover
# PE13R1 <- read.csv('YCov_2013_R1_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in PE Presence matrices
# PEP13R1 <- read.csv('YCov_2013_R1_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #obs
# OBS13R1 <- read.csv('YCov_2013_R1_Obs.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))

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
                        #yearlySiteCovs=list(PE=PE13R1, PEP=PEP13R1, short=Short13R1, tall=Tall13R1, Int=Int13R1, Water=Water13R1, WaterD=Water_Depth13R1, WaterP=Water_Pres13R1, Wood=Wood13R1, WoodP=Wood_Pres13R1, Obs=OBS13R1))
)
#MODELS FOR 2013 ROUND 1

null13R1 = gdistsamp(lambdaformula = ~1,
              phiformula = ~1, 
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE
              )
                 
global13R1 = gdistsamp(lambdaformula = ~Int+WaterP+short+Hab_Type-1, 
              phiformula = ~1,
              pformula = ~1, 
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)
                  
A13R1 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodP-1, 
              phiformula = ~1, 
              pformula = ~1,
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)
              

B13R1 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
              phiformula = ~1, 
              pformula = ~1,
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

             
C13R1 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
              phiformula = ~1, 
              pformula = ~1,
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

D13R1 = gdistsamp(lambdaformula = ~Int-1, 
              phiformula = ~1, 
              pformula = ~1,
              data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

E13R1 = gdistsamp(lambdaformula = ~short-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

G13R1 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

H13R1 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

I13R1 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

J13R1 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

K13R1 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

L13R1 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

M13R1 = gdistsamp(lambdaformula = ~1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R1, keyfun = "uniform", mixture="NB",se = TRUE)

list13R1 = fitList(null13R1, global13R1,A13R1,B13R1,C13R1,D13R1,E13R1,G13R1,H13R1,I13R1,J13R1,K13R1, L13R1,M13R1)
model13R1 =modSel(list13R1)
model13R1



IntConstant0 <- data.frame(Int = 0,
                         Hab_Type=factor(c("PE", "MS")))


IntConstant3 <- data.frame(Int = 0.03,
                           Hab_Type=factor(c("PE", "MS")))

IntConstant7 <- data.frame(Int = 0.07,
                           Hab_Type=factor(c("PE", "MS")))


Elambda0 <- predict(I13R1, type="lambda", newdata=IntConstant0, appendData=TRUE)

Elambda3 <- predict(I13R1, type="lambda", newdata=IntConstant3,appendData=TRUE)

Elambda7 <- predict(I13R1, type="lambda", newdata=IntConstant7,appendData=TRUE)


par(mfrow=c(1, 3))
with(Elambda0, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 150), cex.names=0.7,
               cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda3, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 150), cex.names=0.7,
               cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  box()
})
with(Elambda7, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 150), cex.names=0.7,
               cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  box()
})




IntVarMS <- data.frame(Int = seq(0, .11, length=10),
                       Hab_Type=factor("MS", levels=c("PE", "MS")),
                       WaterD =12)

IntVarMSW <- data.frame(Int = .06,
                        Hab_Type=factor("MS", levels=c("PE", "MS")),
                        WaterD = seq(0,27,length=10))

EsigmaInt13R1 <- predict(J13R1, type="lambda", newdata=IntVarMS, appendData=TRUE)
EsigmaWater13R1 <- predict(J13R1, type="lambda", newdata=IntVarMSW, appendData=TRUE)

EsigmaIntAll[,5] <- EsigmaInt13R1[,1]
EsigmaIntAll[,6] <- EsigmaInt13R1[,2]

EsigmaWaterAll[,5] <- EsigmaWater13R1[,1]
EsigmaWaterAll[,6] <- EsigmaWater13R1[,2]

EsigmaIntAll
EsigmaWaterAll
