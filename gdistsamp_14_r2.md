
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
sora14r2 <- read.csv('2014r2_sora.csv', header=T)
sora14r2 <- sora14r2[!(sora14r2$impound=="ccmsu12"|sora14r2$impound=="ccmsu2"|sora14r2$impound=="ccmsu1"|sora14r2$impound=="ts2a"|sora14r2$impound=="ts4a"|sora14r2$impound=="ts6a"|sora14r2$impound=="ts8a"|sora14r2$impound=="kt2"|sora14r2$impound=="kt5"|sora14r2$impound=="kt5"|sora14r2$impound=="kt6"|sora14r2$impound=="kt9"),]
#read in the covariate data #organized by impoundment.
cov14r2 <- read.csv('2014r2_cov.csv', header=T)
cov14r2 <- cov14r2[!(cov14r2$impound=="ccmsu12"|cov14r2$impound=="ccmsu2"|cov14r2$impound=="ccmsu1"|cov14r2$impound=="ts2a"|cov14r2$impound=="ts4a"|cov14r2$impound=="ts6a"|cov14r2$impound=="ts8a"|cov14r2$impound=="kt2"|cov14r2$impound=="kt5"|cov14r2$impound=="kt5"|cov14r2$impound=="kt6"|cov14r2$impound=="kt9"),]

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
```

```r
model14r2 <- list()
model14r2$null14r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r2$reg14r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r2$reg_w14r2 =gdistsamp(lambdaformula = ~region+averagewater_2-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r2$reg_w_i_14r2 =gdistsamp(lambdaformula = ~region+averagewater_2+region*averagewater_2-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r2$short_r14r2 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r2$short_r_i_14r2 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r2$short14r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r2$short_w14r2 =gdistsamp(lambdaformula = ~short+averagewater_2-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
model14r2$short_w_i_14r2 =gdistsamp(lambdaformula = ~short+averagewater_2+short*averagewater_2-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model14r2$global14r2 =gdistsamp(lambdaformula = ~region+averagewater_2+short+region*averagewater_2+averagewater_2*short+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```



```r
list14r2  = fitList(fits=model14r2)
(model14r2 = modSel(list14r2))
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
## global14r2        16  90.77   0.00  7.8e-01     0.78
## reg_w_i_14r2      11  93.83   3.06  1.7e-01     0.95
## reg_w14r2          8  96.60   5.83  4.2e-02     0.99
## short_r14r2        8 100.81  10.04  5.2e-03     1.00
## short_r_i_14r2    11 102.14  11.38  2.6e-03     1.00
## reg14r2            7 105.51  14.74  4.9e-04     1.00
## null14r2           4 230.55 139.79  3.4e-31     1.00
## short_w_i_14r2     6 601.61 510.84 9.2e-112     1.00
## short_w14r2        5 635.03 544.26 5.1e-119     1.00
## short14r2          4 663.00 572.23 4.3e-125     1.00
```
