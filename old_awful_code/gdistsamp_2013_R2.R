#gdistsamp for 2013 Round 2

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora13R2 <- as.matrix(read.csv('Sora_2013_Round2.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov13R2 <- read.csv('Cov_2013_ROund2.csv', header=TRUE)
#read in effort matrices
# Effort13R2 <- read.csv('YCov_2013_R2_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in water depth matrices
# Water_Depth13R2 <- read.csv('YCov_2013_R2_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in water presence matrices
# Water_Pres13R2 <- read.csv('YCov_2013_R2_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Water % Cover matrices
# Water13R2 <- read.csv('YCov_2013_R2_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in wood % cover matrices
# Wood13R2 <- read.csv('YCov_2013_R2_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Wood presence matrices
# Wood_Pres13R2 <- read.csv('YCov_2013_R2_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Interspersion matrices
# Int13R2 <- read.csv('YCov_2013_R2_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in short % cover 
# Short13R2 <- read.csv('YCov_2013_R2_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in tall % cover
# Tall13R2 <- read.csv('YCov_2013_R2_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in PE % cover
# PE13R2 <- read.csv('YCov_2013_R2_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in PE Presence matrices
# PEP13R2 <- read.csv('YCov_2013_R2_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12

#Unmarked Data Frame
umf13R2 = unmarkedFrameGDS(y=sora13R2, 
                           numPrimary=6,
                           siteCovs = cov13R2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13R2$effort_m,
                           #yearlySiteCovs=list(PE=PE13R2, PEP=PEP13R2, short=Short13R2, tall=Tall13R2, Int=Int13R2, Water=Water13R2, WaterD=Water_Depth13R2, WaterP=Water_Pres13R2, Wood=Wood13R2, WoodP=Wood_Pres13R2))
)
#MODELS FOR 2013 ROUND 2
null13R2 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE
)

global13R2 = gdistsamp(lambdaformula = ~Int+WaterP+Short+Hab_Type-1, 
                       phiformula = ~1,
                       pformula = ~1, 
                       data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

A13R2 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)


B13R2 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)


C13R2 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

D13R2 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

E13R2 = gdistsamp(lambdaformula = ~Short-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

G13R2 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

H13R2 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

I13R2 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

J13R2 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

K13R2 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R2, keyfun = "uniform", mixture="NB",se = TRUE)

list13R2 = fitList(null13R2, global13R2,A13R2,B13R2,C13R2,D13R2,E13R2,G13R2,H13R2,I13R2,J13R2,K13R2)
model13R2 = modSel(list13R2)
model13R2




IntConstant <- data.frame(Int = 0,
                          Hab_Type=factor(c("PE", "MS", "UP")))



Elambda <- predict(H13R2, type="lambda", newdata=IntConstant, appendData=TRUE)

par(mfrow=c(1, 1))
with(Elambda, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 60),cex.names=0.7,
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

EsigmaInt13R2 <- predict(J13R2, type="lambda", newdata=IntVarMS, appendData=TRUE)
EsigmaWater13R2 <- predict(J13R2, type="lambda", newdata=IntVarMSW, appendData=TRUE)

EsigmaIntAll[,7] <- EsigmaInt13R2[,1]
EsigmaIntAll[,8] <- EsigmaInt13R2[,2]

EsigmaWaterAll[,7] <- EsigmaWater13R2[,1]
EsigmaWaterAll[,8] <- EsigmaWater13R2[,2]

EsigmaIntAll
EsigmaWaterAll
