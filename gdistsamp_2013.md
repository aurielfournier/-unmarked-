
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
sora13 <- read.csv('2013_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13 <- read.csv('2013_cov.csv', header=T)
#subset covaraites we need
cov13 <- cov13[,c("region","length","impound","jdate","area", "int","short","water")]
# #the distance bins

sora13 <- sora13[order(sora13$impound),]
cov13 <- cov13[order(cov13$impound),]

sora13 <- sora13[,2:79]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13 = unmarkedFrameGDS(y=sora13, 
                           numPrimary=6,
                           siteCovs = cov13,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13$length,
)
```


```r
model13 <- list()
model13$null13 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")

model13$reg13 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model13$reg_w13 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")

model13$reg_w_i_13 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model13$short_r13 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
model13$short_r_i_13 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model13$short13 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")

model13$short_w13 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
## Warning: NaNs produced
```

```r
model13$short_w_i_13 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model13$global13 =gdistsamp(lambdaformula = ~region+water+short+region*water+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
list13  = fitList(model13)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model13 = modSel(list13)
model13
```

```
##              nPars     AIC delta   AICwt cumltvWt
## reg_w_i_13      12 1013.77  0.00 9.5e-01     0.95
## global13        16 1020.84  7.07 2.8e-02     0.97
## null13           5 1021.65  7.88 1.8e-02     0.99
## reg13            8 1025.08 11.32 3.3e-03     1.00
## reg_w13          9 1026.47 12.70 1.7e-03     1.00
## short_r13        9 1026.73 12.97 1.4e-03     1.00
## short_r_i_13    12 1030.14 16.37 2.6e-04     1.00
## short_w_i_13     7 1044.23 30.47 2.3e-07     1.00
## short_w13        6 1051.20 37.43 7.1e-09     1.00
## short13          5 1078.30 64.53 9.2e-15     1.00
```
