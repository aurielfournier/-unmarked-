# predictions from GDistsamp 2012

#read in the sora observations
sora13r1 <- read.csv('2013r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r1 <- read.csv('2013r1_cov.csv', header=T)
#subset covaraites we need
cov13r1 <- cov13r1[,c("region","length_1","impound","jdate_1","hectares","area", "int","short","water")]
# #the distance bins

sora13r1 <- sora13r1[order(sora13r1$impound),]
cov13r1 <- cov13r1[order(cov13r1$impound),]

sora13r1 <- sora13r1[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r1 = unmarkedFrameGDS(y=sora13r1, 
                           numPrimary=3,
                           siteCovs = cov13r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r1$length_1,
)


null13r1 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg13r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area13r1 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w13r1 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short13r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w13r1 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global13r1 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list13r1 = fitList(null13r1, global13r1, short_w13r1, short13r1,reg_w13r1,area13r1,reg13r1)
model13r1 =modSel(list13r1)
model13r1