
```r
setwd("~/Documents/data")
# predictions from GDistsamp 2012
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
sora12r1 <- read.csv('2012r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r1 <- read.csv('2012r1_cov.csv', header=T)
#subset covaraites we need
cov12r1 <- cov12r1[,c("region","length_1","impound","jdate_1","hectares","area", "int","short","water")]
# #the distance bins

sora12r1 <- sora12r1[order(sora12r1$impound),]
cov12r1 <- cov12r1[order(cov12r1$impound),]

sora12r1 <- sora12r1[,3:15]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r1 = unmarkedFrameGDS(y=sora12r1, 
                           numPrimary=1,
                           siteCovs = cov12r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r1$length_1,
)
```

```r
model12r1 <- list()
model12r1$null12r1 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r1$reg12r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r1$reg_w12r1 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r1$reg_w_i_12r1 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r1$short_r12r1 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
model12r1$short_r_i_12r1 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r1$short12r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r1$short_w12r1 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
model12r1$short_w_i_12r1 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r1$global12r1 =gdistsamp(lambdaformula = ~region+water+short+region*water+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
list12r1  = fitList(model12r1)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model12r1 = modSel(list12r1)
model12r1
```

```
##                nPars    AIC  delta   AICwt cumltvWt
## reg_w12r1          7 -79.02   0.00 7.9e-01     0.79
## reg_w_i_12r1      10 -74.37   4.65 7.8e-02     0.87
## reg12r1            6 -74.35   4.68 7.7e-02     0.95
## short_r12r1        7 -72.78   6.24 3.5e-02     0.98
## global12r1        14 -70.33   8.69 1.0e-02     0.99
## short_r_i_12r1    10 -69.05   9.97 5.4e-03     1.00
## short_w_i_12r1     5 -39.35  39.67 1.9e-09     1.00
## null12r1           3 -32.34  46.68 5.8e-11     1.00
## short_w12r1        4  18.81  97.83 4.5e-22     1.00
## short12r1          3 124.92 203.94 4.1e-45     1.00
```
