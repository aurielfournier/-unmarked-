# predictions from GDistsamp 2012

#read in the sora observations
sora13r4 <- read.csv('2013r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r4 <- read.csv('2013r4_cov.csv', header=T)
#subset covaraites we need
cov13r4 <- cov13r4[,c("region","length_4","impound","jdate_4","hectares","area", "int","short","water")]
# #the distance bins

sora13r4 <- sora13r4[order(sora13r4$impound),]
cov13r4 <- cov13r4[order(cov13r4$impound),]

sora13r4 <- sora13r4[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r4 = unmarkedFrameGDS(y=sora13r4, 
                           numPrimary=3,
                           siteCovs = cov13r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r4$length_4,
)


null13r4 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg13r4 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area13r4 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w13r4 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short13r4 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w13r4 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

global13r4 =gdistsamp(lambdaformula = ~region+water+area+short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list13r4 = fitList(null13r4, global13r4, short_w13r4, short13r4,reg_w13r4,area13r4,reg13r4)
model13r4 =modSel(list13r4)
model13r4