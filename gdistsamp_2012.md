
```r
# predictions from GDistsamp 2012

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
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
#subset covaraites we need
cov <- cov[,c("region","length","impound","jdate","area", "scale_int","scale_short","scale_averagewater")]
# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                           numPrimary=3,
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

```r
model$r_w =gdistsamp(lambdaformula = ~region+scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$r_w_i =gdistsamp(lambdaformula = ~region+scale_averagewater+region*scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$s_r =gdistsamp(lambdaformula = ~scale_short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
model$s_r_i =gdistsamp(lambdaformula = ~scale_short+region+scale_short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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

model$s_w_i =gdistsamp(lambdaformula = ~scale_short+scale_averagewater+scale_short*scale_averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$global =gdistsamp(lambdaformula = ~region+scale_averagewater+scale_short+region*scale_averagewater+region*scale_short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
list  = fitList(model)
```

```
## Warning in fitList(model): If supplying a list of fits, use fits = 'mylist'
```

```r
model = modSel(list)
model
```

```
##        nPars     AIC   delta    AICwt cumltvWt
## r          8 -495.90   0.000  3.9e-01     0.39
## r_w        9 -495.86   0.033  3.8e-01     0.77
## s_r        9 -494.25   1.642  1.7e-01     0.94
## r_w_i     12 -490.60   5.300  2.7e-02     0.96
## null       5 -490.24   5.651  2.3e-02     0.99
## s_r_i     12 -489.11   6.782  1.3e-02     1.00
## s_w        6 -194.28 301.613  1.2e-66     1.00
## s          5 -193.20 302.699  7.2e-67     1.00
## s_w_i      7 -192.29 303.605  4.6e-67     1.00
## global    15  -34.50 461.391 2.5e-101     1.00
```
