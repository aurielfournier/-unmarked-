#gdistsamp for 2012 Round 2

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora12R2 <- as.matrix(read.csv('Sora_2012_Round2.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov12R2 <- read.csv('Cov_2012_ROund2.csv', header=TRUE)
#read in effort matrices
# Effort12R2 <- read.csv('YCov_2012_R2_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in water depth matrices
# Water_Depth12R2 <- read.csv('YCov_2012_R2_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in water presence matrices
# Water_Pres12R2 <- read.csv('YCov_2012_R2_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Water % Cover matrices
# Water12R2 <- read.csv('YCov_2012_R2_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in wood % cover matrices
# Wood12R2 <- read.csv('YCov_2012_R2_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Wood presence matrices
# Wood_Pres12R2 <- read.csv('YCov_2012_R2_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Interspersion matrices
# Int12R2 <- read.csv('YCov_2012_R2_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in Short % cover 
# Short12R2 <- read.csv('YCov_2012_R2_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in tall % cover
# Tall12R2 <- read.csv('YCov_2012_R2_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in PE % cover
# PE12R2 <- read.csv('YCov_2012_R2_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
# #read in PE Presence matrices
# PEP12R2 <- read.csv('YCov_2012_R2_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA))
#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12

#Unmarked Data Frame
umf12R2 = unmarkedFrameGDS(y=sora12R2, 
                           numPrimary=6,
                           siteCovs = cov12R2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12R2$effort_m,
                           #yearlySiteCovs=list(PE=PE12R2, PEP=PEP12R2, Short=Short12R2, tall=Tall12R2, Int=Int12R2, Water=Water12R2, WaterD=Water_Depth12R2, WaterP=Water_Pres12R2, Wood=Wood12R2, WoodP=Wood_Pres12R2)
                           )

#MODELS FOR 2012 ROUND 2
null12R2 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE
)

global12R2 = gdistsamp(lambdaformula = ~Int+WaterP+Short+Hab_Type-1, 
                       phiformula = ~1,
                       pformula = ~Int+WaterP+Short+Hab_Type-1, 
                       data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

A12R2 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodP-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD+WaterP+WoodP-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)


B12R2 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD+WaterP-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)


C12R2 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

D12R2 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~Int-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

E12R2 = gdistsamp(lambdaformula = ~Short-1, 
                  phiformula = ~1, 
                  pformula = ~Short-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

G12R2 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~WaterP-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

H12R2 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

I12R2 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type+Int-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

J12R2 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type+Int+WaterD-1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

K12R2 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf12R2, keyfun = "uniform", mixture="NB",se = TRUE)

list12R2 = fitList(null12R2, global12R2,A12R2,B12R2,C12R2,D12R2,E12R2,G12R2,H12R2,I12R2,J12R2,K12R2)
modSel(list12R2)




IntConstant <- data.frame(Int = 0,
                          Hab_Type=factor(c("PE", "MS")))



Elambda <- predict(H12R2, type="lambda", newdata=IntConstant, appendData=TRUE)

par(mfrow=c(1, 1))
with(Elambda, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 80),cex.names=0.7,
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

EsigmaInt12R2<- predict(J13R1, type="lambda", newdata=IntVarMS, appendData=TRUE)
EsigmaWater12R2 <- predict(J13R1, type="lambda", newdata=IntVarMSW, appendData=TRUE)

EsigmaIntAll[,1] <- EsigmaInt12R2[,1]
EsigmaIntAll[,2] <- EsigmaInt12R2[,2]

EsigmaWaterAll[,1] <- EsigmaWater12R2[,1]
EsigmaWaterAll[,2] <- EsigmaWater12R2[,2]

EsigmaIntAll
EsigmaWaterAll
