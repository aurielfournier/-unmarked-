
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
#sora <- sora[!(sora$impound=="ccmsu12"|sora$impound=="ccmsu2"|sora$impound=="ccmsu1"|sora$impound=="ts2a"|sora$impound=="ts4a"|sora$impound=="ts6a"|sora$impound=="ts8a"|sora$impound=="kt2"|sora$impound=="kt5"|sora$impound=="kt5"|sora$impound=="kt6"|sora$impound=="kt9"|sora$impound=="pool2"|sora$impound=="pool2w"|sora$impound=="pool3w"|sora$impound=="m10"|sora$impound=="m11"|sora$impound=="m13"),]

#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_cov.csv', header=T)
## removing impoundments which were confounded in the experiment or were not the appropriate habtiat type
#cov <- cov[!(cov$impound=="ccmsu12"|cov$impound=="ccmsu2"|cov$impound=="ccmsu1"|cov$impound=="ts2a"|cov$impound=="ts4a"|cov$impound=="ts6a"|cov$impound=="ts8a"|cov$impound=="kt2"|cov$impound=="kt5"|cov$impound=="kt5"|cov$impound=="kt6"|cov$impound=="kt9"|cov$impound=="pool2"|cov$impound=="pool2w"|cov$impound=="pool3w"|cov$impound=="m10"|cov$impound=="m11"|cov$impound=="m13"),]

#subset covaraites we need

cov <- cov[,c("region","length","impound","jdate","area", "treat","scale_short","scale_averagewater")]

# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:31]
cutpt = as.numeric(c(0,1,2,3,4,5)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                           numPrimary=6,
                           siteCovs = cov,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov$length
)
```

```r
model <- list()
model$null = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$r = gdistsamp(lambdaformula = ~region, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$r_w =gdistsamp(lambdaformula = ~region+scale_averagewater, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$r_w_i =gdistsamp(lambdaformula = ~region+scale_averagewater+region*scale_averagewater, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$s_r =gdistsamp(lambdaformula = ~scale_short+region, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
model$s_r_i =gdistsamp(lambdaformula = ~scale_short+region+scale_short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$s =gdistsamp(lambdaformula = ~scale_short, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$s_w =gdistsamp(lambdaformula = ~scale_short+scale_averagewater, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
model$s_w_i =gdistsamp(lambdaformula = ~scale_short+scale_averagewater+scale_short*scale_averagewater, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$global=gdistsamp(lambdaformula = ~region+scale_averagewater+scale_short+region*scale_averagewater+region*scale_short, phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
save(model, file="2014_models.Rdata")
list  = fitList(fits=model)
(models = modSel(list))
```

```
##        nPars     AIC delta   AICwt cumltvWt
## r_w_i     12 1148.24  0.00 7.7e-01     0.77
## global    16 1152.28  4.04 1.0e-01     0.87
## s_w        7 1153.88  5.64 4.6e-02     0.91
## r_w        9 1154.03  5.78 4.3e-02     0.96
## s_w_i      8 1154.07  5.82 4.2e-02     1.00
## null       5 1161.96 13.71 8.1e-04     1.00
## s          6 1163.50 15.25 3.7e-04     1.00
## r          8 1165.33 17.08 1.5e-04     1.00
## s_r        9 1166.92 18.67 6.8e-05     1.00
## s_r_i     12 1167.33 19.09 5.5e-05     1.00
```
