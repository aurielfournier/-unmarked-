#gdistsamp for 2013 Round 3

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora13R3 <- as.matrix(read.csv('Sora_2013_Round3.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov13R3 <- read.csv('Cov_2013_Round3.csv', header=TRUE)
#read in effort matrices
# Effort13R3 <- read.csv('YCov_2013_R3_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in water depth matrices
# Water_Depth13R3 <- read.csv('YCov_2013_R3_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in water presence matrices
# Water_Pres13R3 <- read.csv('YCov_2013_R3_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Water % Cover matrices
# Water13R3 <- read.csv('YCov_2013_R3_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in wood % cover matrices
# Wood13R3 <- read.csv('YCov_2013_R3_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Wood presence matrices
# Wood_Pres13R3 <- read.csv('YCov_2013_R3_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Interspersion matrices
# Int13R3 <- read.csv('YCov_2013_R3_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in short % cover 
# Short13R3 <- read.csv('YCov_2013_R3_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in tall % cover
# Tall13R3 <- read.csv('YCov_2013_R3_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in PE % cover
# PE13R3 <- read.csv('YCov_2013_R3_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in PE Presence matrices
# PEP13R3 <- read.csv('YCov_2013_R3_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf13R3 = unmarkedFrameGDS(y=sora13R3, 
                           numPrimary=6,
                           siteCovs = cov13R3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13R3$effort_m,
                           #yearlySiteCovs=list(PE=PE13R3, PEP=PEP13R3, short=Short13R3, tall=Tall13R3, Int=Int13R3, Water=Water13R3, WaterD=Water_Depth13R3, WaterP=Water_Pres13R3, Wood=Wood13R3, WoodP=Wood_Pres13R3))
)
#MODELS FOR 2013 ROUND 3

null13R3 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE
)

global13R3 = gdistsamp(lambdaformula = ~Int+WaterP+ShortVeg+Hab_Type-1, 
                       phiformula = ~1,
                       pformula = ~1, 
                       data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

A13R3 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodyP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)


B13R3 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)


C13R3 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

D13R3 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

E13R3 = gdistsamp(lambdaformula = ~ShortVeg-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

G13R3 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

H13R3 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

I13R3 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

J13R3 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

K13R3 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf13R3, keyfun = "uniform", mixture="NB",se = TRUE)

list13R3 = fitList(null13R3, global13R3,A13R3,B13R3,C13R3,D13R3,E13R3,G13R3,H13R3,I13R3,J13R3,K13R3)
model13R3 = modSel(list13R3)
model13R3



IntConstant <- data.frame(Int = 0,
                          Hab_Type=factor(c("PE", "MS", "UP")))



Elambda <- predict(H13R3, type="lambda", newdata=IntConstant, appendData=TRUE)

par(mfrow=c(1, 1))
with(Elambda, {
  x <- barplot(Predicted, names=Hab_Type, xlab="Habitat",
               ylab="Density (animals / ha)", ylim=c(0, 120),cex.names=0.7,
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

EsigmaInt13R3<- predict(J13R3, type="lambda", newdata=IntVarMS, appendData=TRUE)
EsigmaWater13R3 <- predict(J13R3, type="lambda", newdata=IntVarMSW, appendData=TRUE)

EsigmaIntAll[,9] <- EsigmaInt13R3[,1]
EsigmaIntAll[,10] <- EsigmaInt13R3[,2]

EsigmaWaterAll[,9] <- EsigmaWater13R3[,1]
EsigmaWaterAll[,10] <- EsigmaWater13R3[,2]

EsigmaIntAll
EsigmaWaterAll
