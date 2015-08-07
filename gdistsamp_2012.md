
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

sora <- sora[,2:16]
cutpt = as.numeric(c(0,1,2,3,4,5)) 
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
model$s_r =gdistsamp(lambdaformula = ~scale_short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
model$s_r_i =gdistsamp(lambdaformula = ~scale_short+region+scale_short*region, 
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
model$global =gdistsamp(lambdaformula = ~region+scale_averagewater+scale_short+region*scale_averagewater+region*scale_short, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
save(model, file="2012_models.Rdata")
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
##        nPars     AIC  delta    AICwt cumltvWt
## s_w        7 -957.18   0.00  2.4e-01     0.24
## r_w        9 -956.67   0.51  1.9e-01     0.43
## r          8 -956.62   0.56  1.8e-01     0.61
## s          6 -956.15   1.03  1.4e-01     0.76
## s_w_i      8 -955.30   1.88  9.5e-02     0.85
## s_r        9 -955.20   1.98  9.0e-02     0.94
## s_r_i     12 -952.64   4.54  2.5e-02     0.97
## null       5 -952.00   5.17  1.8e-02     0.99
## r_w_i     12 -951.57   5.60  1.5e-02     1.00
## global    15 -452.87 504.31 7.5e-111     1.00
```
