
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
sora12r3 <- read.csv('2012r3_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r3 <- read.csv('2012r3_cov.csv', header=T)
#subset covaraites we need
cov12r3 <- cov12r3[,c("region","length_3","impound","jdate_3","hectares","area", "int","short","water")]
# #the distance bins

sora12r3 <- sora12r3[order(sora12r3$impound),]
cov12r3 <- cov12r3[order(cov12r3$impound),]

sora12r3 <- sora12r3[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r3 = unmarkedFrameGDS(y=sora12r3, 
                           numPrimary=3,
                           siteCovs = cov12r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r3$length_3,
)
```

```r
model12r3 <- list()
model12r3$null12r3 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r3$reg12r3 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r3$reg_w12r3 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r3$reg_w_i_12r3 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r3$short_r12r3 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
model12r3$short_r_i_12r3 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r3$short12r3 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r3$short_w12r3 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
model12r3$short_w_i_12r3 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r3$global12r3 =gdistsamp(lambdaformula = ~region+water+short+region*water+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```


```r
list12r3  = fitList(model12r3)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model12r3 = modSel(list12r3)
```

```
## Warning: NaNs produced
```

```r
model12r3
```

```
##                nPars      AIC   delta    AICwt cumltvWt
## global12r3        15 -1356.56    0.00  1.0e+00     1.00
## reg_w_i_12r3      11 -1308.15   48.41  3.1e-11     1.00
## short_r_i_12r3    11 -1232.00  124.55  9.0e-28     1.00
## reg_w12r3          8 -1194.70  161.86  7.1e-36     1.00
## reg12r3            7 -1119.10  237.46  2.7e-52     1.00
## short_r12r3        8 -1117.21  239.35  1.1e-52     1.00
## null12r3           4  -780.01  576.55 6.4e-126     1.00
## short_w12r3        5  4509.57 5866.13  0.0e+00     1.00
## short12r3          4  4613.10 5969.66  0.0e+00     1.00
## short_w_i_12r3     6  5383.29 6739.85  0.0e+00     1.00
```
