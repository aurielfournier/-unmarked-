# predictions from GDistsamp 2012

#read in the sora observations
sora12r1 <- read.csv('2012r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r1 <- read.csv('2012r1_cov.csv', header=T)
#subset covaraites we need
cov12r1 <- cov12r1[,c("region","length_1","impound","jdate_1","hectares","area", "int","short","water")]
# #the distance bins

sora12r1 <- sora12r1[order(sora12r1$impound),]
cov12r1 <- cov12r1[order(cov12r1$impound),]

sora12r1 <- sora12r1[,3:41]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r1 = unmarkedFrameGDS(y=sora12r1, 
                           numPrimary=3,
                           siteCovs = cov12r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r1$length_1,
)


null12r1 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg12r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area12r1 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w12r1 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short12r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w12r1 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global12r1 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list12r1 = fitList(null12r1, global12r1, short_w12r1, short12r1,reg_w12r1,area12r1,reg12r1)
model12r1 =modSel(list12r1)
model12r1