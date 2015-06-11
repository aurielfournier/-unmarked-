
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
## r          8 -433.34   0.000  3.6e-01     0.36
## r_w        9 -433.25   0.092  3.4e-01     0.70
## s_r        9 -431.82   1.515  1.7e-01     0.87
## s_r_i     12 -429.54   3.803  5.4e-02     0.92
## null       5 -429.38   3.962  5.0e-02     0.97
## r_w_i     12 -428.11   5.226  2.6e-02     1.00
## s          5 -114.26 319.083  1.9e-70     1.00
## s_w        6 -114.25 319.086  1.8e-70     1.00
## s_w_i      7 -112.29 321.048  6.9e-71     1.00
## global    15   86.16 519.496 5.6e-114     1.00
```
