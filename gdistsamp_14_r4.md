
```r
# predictions from GDistsamp 2014 round 4
setwd("~/Documents/data")
library(unmarked)
```

```
## Loading required package: methods
## Loading required package: reshape
```

```
## Warning: package 'reshape' was built under R version 3.1.2
```

```
## Loading required package: lattice
## Loading required package: Rcpp
```

```r
#read in the sora observations
#read in the sora observations
sora14r4 <- read.csv('2014r4_sora.csv', header=T)
sora14r4 <- sora14r4[!(sora14r4$impound=="ccmsu12"|sora14r4$impound=="ccmsu2"|sora14r4$impound=="ccmsu1"|sora14r4$impound=="ts2a"|sora14r4$impound=="ts4a"|sora14r4$impound=="ts6a"|sora14r4$impound=="ts8a"|sora14r4$impound=="kt2"|sora14r4$impound=="kt5"|sora14r4$impound=="kt5"|sora14r4$impound=="kt6"|sora14r4$impound=="kt9"|sora14r4$impound=="pool2"|sora14r4$impound=="pool2w"|sora14r4$impound=="pool3w"|sora14r4$impound=="m10"|sora14r4$impound=="m11"|sora14r4$impound=="m13"),]
#read in the covariate data #organized by impoundment.
cov14r4 <- read.csv('2014r4_cov.csv', header=T)
cov14r4 <- cov14r4[!(cov14r4$impound=="ccmsu12"|cov14r4$impound=="ccmsu2"|cov14r4$impound=="ccmsu1"|cov14r4$impound=="ts2a"|cov14r4$impound=="ts4a"|cov14r4$impound=="ts6a"|cov14r4$impound=="ts8a"|cov14r4$impound=="kt2"|cov14r4$impound=="kt5"|cov14r4$impound=="kt5"|cov14r4$impound=="kt6"|cov14r4$impound=="kt9"|cov14r4$impound=="pool2"|cov14r4$impound=="pool2w"|cov14r4$impound=="pool3w"|cov14r4$impound=="m10"|cov14r4$impound=="m11"|cov14r4$impound=="m13"),]
#subset covaraites we need
cov14r4 <- cov14r4[,c("region","length_4","impound","jdate_4","hectares","area", "treat","pe","int","short","averagewater_4")]
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
```

```r
model14r4 <- list()
model14r4$null14r4 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r4$reg14r4 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r4$reg_w14r4 =gdistsamp(lambdaformula = ~region+averagewater_4-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r4$reg_w_i_14r4 =gdistsamp(lambdaformula = ~region+averagewater_4+region*averagewater_4-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r4$short_r14r4 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r4$short_r_i_14r4 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r4$short14r4 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r4$short_w14r4 =gdistsamp(lambdaformula = ~short+averagewater_4-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
model14r4$short_w_i_14r4 =gdistsamp(lambdaformula = ~short+averagewater_4+short*averagewater_4-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model14r4$global14r4=gdistsamp(lambdaformula = ~region+averagewater_4+short+region*averagewater_4+region*short-1, phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
list14r4  = fitList(fits=model14r4)
(models14r4 = modSel(list14r4))
```

```
## Hessian is singular.
## Hessian is singular.
## Hessian is singular.
## Hessian is singular.
## Hessian is singular.
## Hessian is singular.
```

```
##                nPars    AIC  delta    AICwt cumltvWt
## global14r4        15 101.09   0.00  9.9e-01     0.99
## short_r_i_14r4    11 110.49   9.41  9.0e-03     1.00
## reg_w_i_14r4      11 121.59  20.50  3.5e-05     1.00
## short_r14r4        8 125.19  24.10  5.8e-06     1.00
## reg_w14r4          8 126.71  25.62  2.7e-06     1.00
## reg14r4            7 127.10  26.01  2.2e-06     1.00
## null14r4           4 157.79  56.70  4.8e-13     1.00
## short_w_i_14r4     6 192.82  91.73  1.2e-20     1.00
## short_w14r4        5 247.22 146.13  1.8e-32     1.00
## short14r4          4 579.98 478.89 1.0e-104     1.00
```
