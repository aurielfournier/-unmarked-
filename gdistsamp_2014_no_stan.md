
```r
# predictions from GDistsamp 2014 round 4

library(unmarked)
```

```
## Loading required package: methods
## Loading required package: reshape
## Loading required package: lattice
## Loading required package: Rcpp
```

```r
#read in the sora observations
#read in the sora observations

sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_sora.csv', header=T)
sora <- sora[!(sora$impound=="ccmsu12"|sora$impound=="ccmsu2"|sora$impound=="ccmsu1"|sora$impound=="ts2a"|sora$impound=="ts4a"|sora$impound=="ts6a"|sora$impound=="ts8a"|sora$impound=="kt2"|sora$impound=="kt5"|sora$impound=="kt5"|sora$impound=="kt6"|sora$impound=="kt9"|sora$impound=="pool2"|sora$impound=="pool2w"|sora$impound=="pool3w"|sora$impound=="m10"|sora$impound=="m11"|sora$impound=="m13"),]

sora <- sora[!(sora$impound=="dc18"&sora$round==1),]

#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_cov.csv', header=T)
cov <- cov[!(cov$impound=="ccmsu12"|cov$impound=="ccmsu2"|cov$impound=="ccmsu1"|cov$impound=="ts2a"|cov$impound=="ts4a"|cov$impound=="ts6a"|cov$impound=="ts8a"|cov$impound=="kt2"|cov$impound=="kt5"|cov$impound=="kt5"|cov$impound=="kt6"|cov$impound=="kt9"|cov$impound=="pool2"|cov$impound=="pool2w"|cov$impound=="pool3w"|cov$impound=="m10"|cov$impound=="m11"|cov$impound=="m13"),]

cov <- cov[!(cov$impound=="dc18"&cov$round==1),]
#subset covaraites we need

cov <- cov[,c("region","length","impound","jdate","hectares","area", "treat","short","awater")]

# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,3:80]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                           numPrimary=6,
                           siteCovs = cov,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov$length,
)
```

```r
model <- list()
model$null = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")

model$r = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region - 1, phiformula = ~1, pformula
## = ~1, : Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model$r_w =gdistsamp(lambdaformula = ~region+awater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region + awater - 1, phiformula
## = ~1, : Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model$r_w_i =gdistsamp(lambdaformula = ~region+awater+region*awater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region + awater + region * awater
## - : Hessian is singular. Try using fewer covariates and supplying starting
## values.
```

```r
model$s_r =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~short + region - 1, phiformula
## = ~1, : Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model$s_r_i =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~short + region + short * region - :
## Hessian is singular. Try using fewer covariates and supplying starting
## values.
```


```r
model$s =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")

model$s_w =gdistsamp(lambdaformula = ~short+awater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
model$s_w_i =gdistsamp(lambdaformula = ~short+awater+short*awater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error in optim(starts, nll, method = method, hessian = se, ...): non-finite finite-difference value [3]
```

```r
model$global=gdistsamp(lambdaformula = ~region+awater+short+region*awater+region*short-1, phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region + awater + short + region
## * : Hessian is singular. Try using fewer covariates and supplying starting
## values.
```

```r
list  = fitList(fits=model)
(models = modSel(list))
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
##        nPars     AIC  delta    AICwt cumltvWt
## global    15  835.20   0.00  1.0e+00     1.00
## s_r_i     11  950.22 115.01  1.1e-25     1.00
## r_w        8  965.16 129.96  6.0e-29     1.00
## r_w_i     11  968.50 133.29  1.1e-29     1.00
## s_r        8 1093.42 258.22  8.5e-57     1.00
## r          7 1098.80 263.60  5.8e-58     1.00
## null       4 1131.76 296.56  4.0e-65     1.00
## s_w        5 1158.64 323.44  5.8e-71     1.00
## s          4 1730.99 895.78 3.0e-195     1.00
```
