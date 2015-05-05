
```r
# predictions from GDistsamp 2012
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
sora12 <- read.csv('2012_vira_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12 <- read.csv('2012_cov_vira.csv', header=T)
#subset covaraites we need
cov12 <- cov12[,c("region","length","impound","jdate","area", "int","short","water")]
# #the distance bins

sora12 <- sora12[order(sora12$impound),]
cov12 <- cov12[order(cov12$impound),]

sora12 <- sora12[,2:22]
cutpt = as.numeric(c(0,2,4,6,8,10,12,14)) 
#Unmarked Data Frame
umf12 = unmarkedFrameGDS(y=sora12, 
                           numPrimary=3,
                           siteCovs = cov12,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12$length,
)
```


```r
model12 <- list()
model12$null12 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")

model12$reg12 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model12$reg_w12 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")

model12$reg_w_i_12 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model12$short_r12 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")
model12$short_r_i_12 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model12$short12 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")

model12$short_w12 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")

model12$short_w_i_12 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model12$global12 =gdistsamp(lambdaformula = ~region+water+short+region*water+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
list12  = fitList(model12)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model12 = modSel(list12)
```

```
## Hessian is singular.
```

```r
model12
```

```
##              nPars    AIC delta  AICwt cumltvWt
## short12          5 128.75  0.00 0.4662     0.47
## short_w12        6 130.72  1.97 0.1741     0.64
## null12           5 130.82  2.07 0.1657     0.81
## short_w_i_12     7 131.90  3.15 0.0965     0.90
## reg12            8 133.21  4.46 0.0501     0.95
## short_r12        9 134.80  6.05 0.0226     0.98
## reg_w12          9 135.16  6.41 0.0189     0.99
## short_r_i_12    12 139.44 10.69 0.0022     1.00
## reg_w_i_12      12 139.87 11.12 0.0018     1.00
## global12        15 139.90 11.15 0.0018     1.00
```
