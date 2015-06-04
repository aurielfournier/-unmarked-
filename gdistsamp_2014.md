
```r
# predictions from GDistsamp 2014

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

sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_sora.csv', header=T)

## removing impoundments which were confounded in the experiment or were not the appropriate habtiat type
sora <- sora[!(sora$impound=="ccmsu12"|sora$impound=="ccmsu2"|sora$impound=="ccmsu1"|sora$impound=="ts2a"|sora$impound=="ts4a"|sora$impound=="ts6a"|sora$impound=="ts8a"|sora$impound=="kt2"|sora$impound=="kt5"|sora$impound=="kt5"|sora$impound=="kt6"|sora$impound=="kt9"|sora$impound=="pool2"|sora$impound=="pool2w"|sora$impound=="pool3w"|sora$impound=="m10"|sora$impound=="m11"|sora$impound=="m13"),]

#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_cov.csv', header=T)
## removing impoundments which were confounded in the experiment or were not the appropriate habtiat type
cov <- cov[!(cov$impound=="ccmsu12"|cov$impound=="ccmsu2"|cov$impound=="ccmsu1"|cov$impound=="ts2a"|cov$impound=="ts4a"|cov$impound=="ts6a"|cov$impound=="ts8a"|cov$impound=="kt2"|cov$impound=="kt5"|cov$impound=="kt5"|cov$impound=="kt6"|cov$impound=="kt9"|cov$impound=="pool2"|cov$impound=="pool2w"|cov$impound=="pool3w"|cov$impound=="m10"|cov$impound=="m11"|cov$impound=="m13"),]

#subset covaraites we need

cov <- cov[,c("region","length","impound","jdate","area", "treat","scale_short","scale_averagewater")]

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
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$r = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region - 1, phiformula = ~1, pformula
## = ~1, : Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model$r_w =gdistsamp(lambdaformula = ~region+scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region + scale_averagewater - 1,
## phiformula = ~1, : Hessian is singular. Try using fewer covariates and
## supplying starting values.
```

```r
model$r_w_i =gdistsamp(lambdaformula = ~region+scale_averagewater+region*scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region + scale_averagewater + region
## * : Hessian is singular. Try using fewer covariates and supplying starting
## values.
```

```r
model$s_r =gdistsamp(lambdaformula = ~scale_short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~scale_short + region - 1, phiformula
## = ~1, : Hessian is singular. Try using fewer covariates and supplying
## starting values.
```

```r
model$s_r_i =gdistsamp(lambdaformula = ~scale_short+region+scale_short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~scale_short + region + scale_short
## * : Hessian is singular. Try using fewer covariates and supplying starting
## values.
```


```r
model$s =gdistsamp(lambdaformula = ~scale_short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$s_w =gdistsamp(lambdaformula = ~scale_short+scale_averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
model$s_w_i =gdistsamp(lambdaformula = ~scale_short+awater+scale_short*averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error in eval(expr, envir, enclos): object 'awater' not found
```

```r
model$global=gdistsamp(lambdaformula = ~region+scale_averagewater+scale_short+region*scale_averagewater+region*scale_short-1, phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in gdistsamp(lambdaformula = ~region + scale_averagewater +
## scale_short + : Hessian is singular. Try using fewer covariates and
## supplying starting values.
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
```

```
## Warning in sqrt(diag(vcov(x, altNames = TRUE))): NaNs produced
```

```
## Hessian is singular.
```

```
##        nPars     AIC  delta   AICwt cumltvWt
## r_w        9 2582.40   0.00 8.1e-01     0.81
## r_w_i     12 2586.97   4.57 8.2e-02     0.89
## s_r        9 2588.88   6.49 3.2e-02     0.92
## r          8 2588.89   6.49 3.1e-02     0.95
## global    16 2589.12   6.72 2.8e-02     0.98
## s_r_i     12 2589.76   7.36 2.0e-02     1.00
## null       5 2599.42  17.02 1.6e-04     1.00
## s_w        6 2781.89 199.49 3.9e-44     1.00
## s          5 2823.37 240.97 3.8e-53     1.00
```
