# predictions from GDistsamp 2012

#read in the sora observations
sora14r4 <- read.csv('2014r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r4 <- read.csv('2014r4_cov.csv', header=T)
#subset covaraites we need
cov14r4 <- cov14r4[,c("region","length_4","impound","jdate_4","hectares","area", "treat","int","short","averagewater_4")]
# #the distance bins

sora14r4 <- sora14r4[order(sora14r4$impound),]
cov14r4 <- cov14r4[order(cov14r4$impound),]

sora14r4 <- sora14r4[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r4 = unmarkedFrameGDS(y=sora14r4, 
                           numPrimary=3,
                           siteCovs = cov14r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r4$length_4,
)


null14r4 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg14r4 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_treat14r4 = gdistsamp(lambdaformula = ~region+treat-1, 
                          phiformula = ~1, 
                          pformula = ~ 1,
                          data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area14r4 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w14r4 =gdistsamp(lambdaformula = ~region+averagewater_4-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short14r4 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w14r4 =gdistsamp(lambdaformula = ~short+averagewater_4-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

treat14r4 =gdistsamp(lambdaformula = ~treat-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


global14r4 =gdistsamp(lambdaformula = ~region+averagewater_4+area+short+treat-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list14r4 = fitList(null14r4, global14r4, reg_treat14r4, treat14r4, short_w14r4, short14r4,reg_w14r4,area14r4,reg14r4)
model14r4 =modSel(list14r4)
model14r4