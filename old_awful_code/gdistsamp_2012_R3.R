#gdistsamp for 2012 Round 3

#mac
#setwd("~/Dropbox/R/Distance")
#windows
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)
#read in the sora observations
sora12R3 <- as.matrix(read.csv('Sora_2012_Round3.csv', header=FALSE))
#read in the covariate data #organized by impoundment.
cov12R3 <- read.csv('Cov_2012_Round3.csv', header=TRUE)
#read in effort matrices
# Effort12R3 <- read.csv('YCov_2012_R3_Effort.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in water depth matrices
# Water_Depth12R3 <- read.csv('YCov_2012_R3_WaterD.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in water presence matrices
# Water_Pres12R3 <- read.csv('YCov_2012_R3_WaterP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Water % Cover matrices
# Water12R3 <- read.csv('YCov_2012_R3_Water.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in wood % cover matrices
# Wood12R3 <- read.csv('YCov_2012_R3_Wood.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Wood presence matrices
# Wood_Pres12R3 <- read.csv('YCov_2012_R3_WoodP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Interspersion matrices
# Int12R3 <- read.csv('YCov_2012_R3_Int.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in Short % cover 
# Short12R3 <- read.csv('YCov_2012_R3_Short.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in tall % cover
# Tall12R3 <- read.csv('YCov_2012_R3_Tall.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in PE % cover
# PE12R3 <- read.csv('YCov_2012_R3_PE.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
# #read in PE Presence matrices
# PEP12R3 <- read.csv('YCov_2012_R3_PEP.csv', header=FALSE, colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL"))
#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf12R3 = unmarkedFrameGDS(y=sora12R3, 
                           numPrimary=6,
                           siteCovs = cov12R3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12R3$effort_m,
                           #yearlySiteCovs=list(PE=PE12R3, PEP=PEP12R3, Short=Short12R3, tall=Tall12R3, Int=Int12R3, Water=Water12R3, WaterD=Water_Depth12R3, WaterP=Water_Pres12R3, Wood=Wood12R3, WoodP=Wood_Pres12R3)
                           )

#MODELS FOR 2012 ROUND 3

null12R3 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE
)

global12R3 = gdistsamp(lambdaformula = ~Int+WaterP+Short+Hab_Type-1, 
                       phiformula = ~1,
                       pformula = ~Int+WaterP+Short+Hab_Type-1, 
                       data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

A12R3 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP+WoodP-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD+WaterP+WoodP-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)


B12R3 = gdistsamp(lambdaformula = ~Int+WaterD+WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD+WaterP-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)


C12R3 = gdistsamp(lambdaformula = ~Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~Int+WaterD-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

D12R3 = gdistsamp(lambdaformula = ~Int-1, 
                  phiformula = ~1, 
                  pformula = ~Int-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

E12R3 = gdistsamp(lambdaformula = ~Short-1, 
                  phiformula = ~1, 
                  pformula = ~Short-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

G12R3 = gdistsamp(lambdaformula = ~WaterP-1, 
                  phiformula = ~1, 
                  pformula = ~WaterP-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

H12R3 = gdistsamp(lambdaformula = ~Hab_Type-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

I12R3 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type+Int-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

J12R3 = gdistsamp(lambdaformula = ~Hab_Type+Int+WaterD-1, 
                  phiformula = ~1, 
                  pformula = ~Hab_Type+Int+WaterD-1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

K12R3 = gdistsamp(lambdaformula = ~Hab_Type+Int-1, 
                  phiformula = ~1, 
                  pformula = ~1,
                  data = umf12R3, keyfun = "uniform", mixture="NB",se = TRUE)

list12R3 = fitList(null12R3, global12R3,A12R3,B12R3,C12R3,D12R3,E12R3,G12R3,H12R3,I12R3,J12R3,K12R3)
modSel(list12R3)

IntConstant <- data.frame(Int = 0,
                           Hab_Type=factor(c("PE", "MS")))

Elambda <- predict(H12R3, type="lambda", newdata=IntConstant, appendData=TRUE)

IntVarMS <- data.frame(Int = seq(0, .11, length=10),
                       Hab_Type=factor("MS", levels=c("PE", "MS")),
                       WaterD =12)

IntVarMSW <- data.frame(Int = .06,
                        Hab_Type=factor("MS", levels=c("PE", "MS")),
                        WaterD = seq(0,27,length=10))

EsigmaInt12R3 <- predict(J12R3, type="lambda", newdata=IntVarMS, appendData=TRUE)
EsigmaWater12R3 <- predict(J12R3, type="lambda", newdata=IntVarMSW, appendData=TRUE)

EsigmaIntAll <- data.frame(Predicted12R2 = seq(0,0, length=10), SE12R2= seq(0,0, length=10),
                           Predicted12R3 = seq(0,0, length=10), SE12R3= seq(0,0, length=10),
                           Predicted13R1 = seq(0,0, length=10), SE13R1 = seq(0,0, length=10),
                           Predicted13R2 = seq(0,0, length=10), SE13R2 = seq(0,0, length=10),
                           Predicted13R3 = seq(0,0, length=10), SE13R3= seq(0,0, length=10),
                           Predicted13R4= seq(0,0, length=10), SE13R4 = seq(0,0, length=10))

EsigmaWaterAll <- data.frame(Predicted12R2 = seq(0,0, length=10), SE12R2= seq(0,0, length=10),
                             Predicted12R3 = seq(0,0, length=10), SE12R3= seq(0,0, length=10),
                             Predicted13R1 = seq(0,0, length=10), SE13R1 = seq(0,0, length=10),
                             Predicted13R2 = seq(0,0, length=10), SE13R2 = seq(0,0, length=10),
                             Predicted13R3 = seq(0,0, length=10), SE13R3= seq(0,0, length=10),
                             Predicted13R4= seq(0,0, length=10), SE13R4 = seq(0,0, length=10))

EsigmaIntAll[,3] <- EsigmaInt12R3[,1]
EsigmaIntAll[,4] <- EsigmaInt12R3[,2]

EsigmaWaterAll[,3] <- EsigmaWater12R3[,1]
EsigmaWaterAll[,4] <- EsigmaWater12R3[,2]

EsigmaIntAll[,13] <- EsigmaInt12R3[,5]
EsigmaWaterAll[,13] <- EsigmaWater12R3[,7]

EsigmaIntAll
EsigmaWaterAll

ggplot(EsigmaInt, aes(x=Int)) +
    geom_line(aes(y=Predicted), colour="red") +  # first layer
    geom_line(aes(y=SE), colour="green")


