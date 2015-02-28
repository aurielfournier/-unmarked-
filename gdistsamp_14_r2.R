# predictions from GDistsamp 2012

#read in the sora observations
sora14r2 <- read.csv('2014r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r2 <- read.csv('2014r2_cov.csv', header=T)
#subset covaraites we need
cov14r2 <- cov14r2[,c("region","length_2","impound","jdate_2","hectares","area", "treat","int","short","averagewater_2")]
# #the distance bins

sora14r2 <- sora14r2[order(sora14r2$impound),]
cov14r2 <- cov14r2[order(cov14r2$impound),]

sora14r2 <- sora14r2[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r2 = unmarkedFrameGDS(y=sora14r2, 
                           numPrimary=3,
                           siteCovs = cov14r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r2$length_2,
)


null14r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg14r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_treat14r2 = gdistsamp(lambdaformula = ~region+treat-1, 
                          phiformula = ~1, 
                          pformula = ~ 1,
                          data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

area14r2 = gdistsamp(lambdaformula = ~area-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

reg_w14r2 =gdistsamp(lambdaformula = ~region+averagewater_2-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short14r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

short_w14r2 =gdistsamp(lambdaformula = ~short+averagewater_2-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

treat14r2 =gdistsamp(lambdaformula = ~treat-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


global14r2 =gdistsamp(lambdaformula = ~region+averagewater_2+area+short+treat-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


list14r2 = fitList(null14r2, global14r2, reg_treat14r2, treat14r2, short_w14r2, short14r2,reg_w14r2,area14r2,reg14r2)
model14r2 =modSel(list14r2)
model14r2