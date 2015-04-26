
```r
# predictions from GDistsamp 2014 round 1
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
sora14r1 <- read.csv('2014r1_sora.csv', header=T)
sora14r1 <- sora14r1[!(sora14r1$impound=="ccmsu12"|sora14r1$impound=="ccmsu2"|sora14r1$impound=="ccmsu1"|sora14r1$impound=="ts2a"|sora14r1$impound=="ts4a"|sora14r1$impound=="ts6a"|sora14r1$impound=="ts8a"|sora14r1$impound=="kt2"|sora14r1$impound=="kt5"|sora14r1$impound=="kt5"|sora14r1$impound=="kt6"|sora14r1$impound=="kt9"),]
#read in the covariate data #organized by impoundment.
cov14r1 <- read.csv('2014r1_cov.csv', header=T)
cov14r1 <- cov14r1[!(cov14r1$impound=="ccmsu12"|cov14r1$impound=="ccmsu2"|cov14r1$impound=="ccmsu1"|cov14r1$impound=="ts2a"|cov14r1$impound=="ts4a"|cov14r1$impound=="ts6a"|cov14r1$impound=="ts8a"|cov14r1$impound=="kt2"|cov14r1$impound=="kt5"|cov14r1$impound=="kt5"|cov14r1$impound=="kt6"|cov14r1$impound=="kt9"),]
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
```


```r
model14r1 <- list()
model14r1$null14r1 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r1$reg14r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r1$reg_w14r1 =gdistsamp(lambdaformula = ~region+averagewater_1-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r1$reg_w_i_14r1 =gdistsamp(lambdaformula = ~region+averagewater_1+region*averagewater_1-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r1$short_r14r1 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r1$short_r_i_14r1 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model14r1$short14r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model14r1$short_w14r1 =gdistsamp(lambdaformula = ~short+averagewater_1-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
model14r1$short_w_i_14r1 =gdistsamp(lambdaformula = ~short+averagewater_1+short*averagewater_1-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model14r1$global14r1 =gdistsamp(lambdaformula = ~region+averagewater_1+short+region*averagewater_1+averagewater_1*short+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
list14r1  = fitList(fits=model14r1)
(model14r1 = modSel(list14r1))
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
##                nPars    AIC delta   AICwt cumltvWt
## reg_w_i_14r1      11  84.05  0.00 4.2e-01     0.42
## short_r_i_14r1    11  84.41  0.35 3.5e-01     0.77
## global14r1        16  85.42  1.37 2.1e-01     0.98
## short_r14r1        8  91.76  7.71 8.9e-03     0.99
## reg14r1            7  91.99  7.93 7.9e-03     1.00
## reg_w14r1          8  93.97  9.92 2.9e-03     1.00
## null14r1           4 111.13 27.07 5.5e-07     1.00
## short_w14r1        5 114.54 30.49 1.0e-07     1.00
## short_w_i_14r1     6 116.38 32.32 4.0e-08     1.00
## short14r1          4 117.06 33.01 2.8e-08     1.00
```
