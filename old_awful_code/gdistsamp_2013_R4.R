#gdistsamp for 2013 Round 4

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")


library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora13R4 <- as.matrix(read.csv('Sora_2013_Round4.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov13R4 <- read.csv('Cov_2013_ROund4.csv', header=TRUE)
#read in effort matrices
# Effort13R4 <- read.csv('YCov_2013_R4_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in water depth matrices
# Water_Depth13R4 <- read.csv('YCov_2013_R4_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in water presence matrices
# Water_Pres13R4 <- read.csv('YCov_2013_R4_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Water % Cover matrices
# WateR43R4 <- read.csv('YCov_2013_R4_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in wood % cover matrices
# Wood13R4 <- read.csv('YCov_2013_R4_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Wood presence matrices
# Wood_Pres13R4 <- read.csv('YCov_2013_R4_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Interspersion matrices
# Int13R4 <- read.csv('YCov_2013_R4_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in short % cover 
# Short13R4 <- read.csv('YCov_2013_R4_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in tall % cover
# Tall13R4 <- read.csv('YCov_2013_R4_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in PE % cover
# PE13R4 <- read.csv('YCov_2013_R4_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in PE Presence matrices
# PEP13R4 <- read.csv('YCov_2013_R4_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf13R4 = unmarkedFrameGDS(y=sora13R4, 
                           numPrimary=6,
                           siteCovs = cov13R4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13R4$effort_m,
                           #yearlySiteCovs=list(PE=PE13R4, PEP=PEP13R4, short=Short13R4, tall=Tall13R4, Int=Int13R4, Water=WateR43R4, WaterD=Water_Depth13R4, WaterP=Water_Pres13R4, Wood=Wood13R4, WoodP=Wood_Pres13R4))
)
#MODELS FOR 2013 ROUND 4

null13R4 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

global13R4 = gdistsamp(lambdaformula = ~Int+WaterP+Short+Hab_Type-1, 
                       phiformula = ~1,
                       pformula = ~1, 
                       data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

A13R4 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodyP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)


B13R4 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)


C13R4 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

D13R4 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

E13R4 = gdistsamp(lambdaformula = ~Short-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

G13R4 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

H13R4 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

I13R4 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

J13R4 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

K13R4 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R4, keyfun = "uniform", mixture="NB",se = TRUE)

list13R4 = fitList(null13R4, global13R4,A13R4,B13R4,C13R4,D13R4,E13R4,G13R4,H13R4,I13R4,J13R4,K13R4)
model13R4 = modSel(list13R4)


IntConstant0 <- data.frame(Int = 0,
                           Hab_Type=factor(c("PE", "MS")))


IntConstant3 <- data.frame(Int = 0.03,
                           Hab_Type=factor(c("PE", "MS")))

IntConstant7 <- data.frame(Int = 0.07,
                           Hab_Type=factor(c("PE", "MS")))


Elambda0 <- predict(J13R4, type="lambda", newdata=IntConstant0, appendData=TRUE)

Elambda3 <- predict(J13R4, type="lambda", newdata=IntConstant3,appendData=TRUE)

Elambda7 <- predict(J13R4, type="lambda", newdata=IntConstant7,appendData=TRUE)


par(mfrow=c(1, 3))
with(Elambda0, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 300), cex.names=0.7,
               cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  box()
})

with(Elambda3, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 300), cex.names=0.7,
               cex.lab=0.7, cex.axis=0.7)
  arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
  box()
})
with(Elambda7, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 300), cex.names=0.7,
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

EsigmaInt13R4<- predict(J13R4, type="lambda", newdata=IntVarMS, appendData=TRUE)
EsigmaWater13R4 <- predict(J13R4, type="lambda", newdata=IntVarMSW, appendData=TRUE)

EsigmaIntAll[,11] <- EsigmaInt13R4[,1]
EsigmaIntAll[,12] <- EsigmaInt13R4[,2]

EsigmaWaterAll[,11] <- EsigmaWater13R4[,1]
EsigmaWaterAll[,12] <- EsigmaWater13R4[,2]

EsigmaIntAll
EsigmaWaterAll


