# predictions from GDistsamp 2012

#read in the sora observations
sora14r1 <- read.csv('2014r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r1 <- read.csv('2014r1_cov.csv', header=T)
#subset covaraites we need
cov14r1 <- cov14r1[,c("region","length_1","impound","jdate_1","hectares","area", "treat","int","short","averagewater_1")]
# #the distance bins

sora14r1 <- sora14r1[order(sora14r1$impound),]
cov14r1 <- cov14r1[order(cov14r1$impound),]

cov14r1 <- cov14r1[cov14r1$impound!="dc18",]
sora14r1 <- sora14r1[sora14r1$impound!="dc18",]

sora14r1 <- sora14r1[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r1 = unmarkedFrameGDS(y=sora14r1, 
                           numPrimary=3,
                           siteCovs = cov14r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r1$length_1,
)



null14r1 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg14r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_treat14r1 = gdistsamp(lambdaformula = ~region+treat-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area14r1 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w14r1 =gdistsamp(lambdaformula = ~region+averagewater_1-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short14r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w14r1 =gdistsamp(lambdaformula = ~short+averagewater_1-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

treat14r1 =gdistsamp(lambdaformula = ~treat-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


global14r1 =gdistsamp(lambdaformula = ~region+averagewater_1+area+short+treat-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list14r1 = fitList(null14r1, global14r1, reg_treat14r1, treat14r1, short_w14r1, short14r1,reg_w14r1,area14r1,reg14r1)
model14r1 =modSel(list14r1)
model14r1