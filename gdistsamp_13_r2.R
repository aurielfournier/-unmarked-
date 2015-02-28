# predictions from GDistsamp 2012

#read in the sora observations
sora13r2 <- read.csv('2013r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r2 <- read.csv('2013r2_cov.csv', header=T)
#subset covaraites we need
cov13r2 <- cov13r2[,c("region","length_2","impound","jdate_2","hectares","area", "int","short","water")]
# #the distance bins

sora13r2 <- sora13r2[order(sora13r2$impound),]
cov13r2 <- cov13r2[order(cov13r2$impound),]

sora13r2 <- sora13r2[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r2 = unmarkedFrameGDS(y=sora13r2, 
                           numPrimary=3,
                           siteCovs = cov13r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r2$length_2,
)


null13r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg13r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area13r2 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w13r2 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short13r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w13r2 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global13r2 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list13r2 = fitList(null13r2, global13r2, short_w13r2, short13r2,reg_w13r2,area13r2,reg13r2)
model13r2 =modSel(list13r2)
model13r2