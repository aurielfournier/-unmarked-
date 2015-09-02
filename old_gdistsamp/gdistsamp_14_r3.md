
```r
# predictions from GDistsamp 2014 round 3
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
sora14r3 <- read.csv('2014r3_sora.csv', header=T)
sora14r3 <- sora14r3[!(sora14r3$impound=="ccmsu12"|sora14r3$impound=="ccmsu2"|sora14r3$impound=="ccmsu1"|sora14r3$impound=="ts2a"|sora14r3$impound=="ts4a"|sora14r3$impound=="ts6a"|sora14r3$impound=="ts8a"|sora14r3$impound=="kt2"|sora14r3$impound=="kt5"|sora14r3$impound=="kt5"|sora14r3$impound=="kt6"|sora14r3$impound=="kt9"|sora14r3$impound=="pool2"|sora14r3$impound=="pool2w"|sora14r3$impound=="pool3w"|sora14r3$impound=="m10"|sora14r3$impound=="m11"|sora14r3$impound=="m13"),]
#read in the covariate data #organized by impoundment.
cov14r3 <- read.csv('2014r3_cov.csv', header=T)
cov14r3 <- cov14r3[!(cov14r3$impound=="ccmsu12"|cov14r3$impound=="ccmsu2"|cov14r3$impound=="ccmsu1"|cov14r3$impound=="ts2a"|cov14r3$impound=="ts4a"|cov14r3$impound=="ts6a"|cov14r3$impound=="ts8a"|cov14r3$impound=="kt2"|cov14r3$impound=="kt5"|cov14r3$impound=="kt5"|cov14r3$impound=="kt6"|cov14r3$impound=="kt9"|cov14r3$impound=="pool2"|cov14r3$impound=="pool2w"|cov14r3$impound=="pool3w"|cov14r3$impound=="m10"|cov14r3$impound=="m11"|cov14r3$impound=="m13"),]
cov14r3 <- cov14r3[order(cov14r3$impound),]
cov14r3 <- cov14r3[order(cov14r3$impound),]

sora14r3 <- sora14r3[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r3 = unmarkedFrameGDS(y=sora14r3, 
                           numPrimary=3,
                           siteCovs = cov14r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r3$length_3,
)
```


```r
model14r3 <- list()
model14r3$null14r3 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r3$reg14r3 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r3$reg_w14r3 =gdistsamp(lambdaformula = ~region+averagewater_3-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r3$reg_w_i_14r3 =gdistsamp(lambdaformula = ~region+averagewater_3+region*averagewater_3-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r3$short_r14r3 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r3$short_r_i_14r3 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r3$short14r3 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r3$short_w14r3 =gdistsamp(lambdaformula = ~short+averagewater_3-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
model14r3$short_w_i_14r3 =gdistsamp(lambdaformula = ~short+averagewater_3+short*averagewater_3-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model14r3$global14r3 =gdistsamp(lambdaformula = ~region+averagewater_3+short+region*averagewater_3+region*short-1, 
                       phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
list14r3  = fitList(fits=model14r3)
(model14r3 = modSel(list14r3))
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
## short_r_i_14r3    11 -26.89   0.00  9.6e-01     0.96
## global14r3        15 -20.56   6.33  4.1e-02     1.00
## short_r14r3        8  29.22  56.11  6.3e-13     1.00
## reg14r3            7  29.37  56.27  5.8e-13     1.00
## null14r3           4  30.30  57.20  3.6e-13     1.00
## reg_w14r3          8  31.20  58.09  2.3e-13     1.00
## reg_w_i_14r3      11  37.06  63.95  1.2e-14     1.00
## short_w_i_14r3     6 632.36 659.25 6.7e-144     1.00
## short_w14r3        5 637.17 664.06 6.1e-145     1.00
## short14r3          4 831.78 858.67 3.3e-187     1.00
```
