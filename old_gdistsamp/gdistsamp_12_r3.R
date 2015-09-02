# predictions from GDistsamp 2012

#read in the sora observations
sora12r3 <- read.csv('2012r3_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r3 <- read.csv('2012r3_cov.csv', header=T)
#subset covaraites we need
cov12r3 <- cov12r3[,c("region","length_3","impound","jdate_3","hectares","area", "int","short","water")]
# #the distance bins

sora12r3 <- sora12r3[order(sora12r3$impound),]
cov12r3 <- cov12r3[order(cov12r3$impound),]

sora12r3 <- sora12r3[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r3 = unmarkedFrameGDS(y=sora12r3, 
                           numPrimary=3,
                           siteCovs = cov12r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r3$length_3,
)


null12r3 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg12r3 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area12r3 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w12r3 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short12r3 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w12r3 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global12r3 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list12r3 = fitList(null12r3, global12r3, short_w12r3, short12r3,reg_w12r3,area12r3,reg12r3)
model12r3 =modSel(list12r3)
model12r3