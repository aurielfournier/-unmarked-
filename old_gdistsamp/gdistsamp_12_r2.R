# predictions from GDistsamp 2012

#read in the sora observations
sora12r2 <- read.csv('2012r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r2 <- read.csv('2012r2_cov.csv', header=T)
#subset covaraites we need
cov12r2 <- cov12r2[,c("region","length_2","impound","jdate_2","hectares","area", "int","short","water")]
# #the distance bins

sora12r2 <- sora12r2[order(sora12r2$impound),]
cov12r2 <- cov12r2[order(cov12r2$impound),]

sora12r2 <- sora12r2[,3:41]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r2 = unmarkedFrameGDS(y=sora12r2, 
                           numPrimary=3,
                           siteCovs = cov12r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r2$length_2,
)


null12r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg12r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area12r2 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w12r2 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short12r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w12r2 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global12r2 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list12r2 = fitList(null12r2, global12r2, short_w12r2, short12r2,reg_w12r2,area12r2,reg12r2)
model12r2 =modSel(list12r2)
model12r2