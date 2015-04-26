
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
sora12r2 <- read.csv('2012r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r2 <- read.csv('2012r2_cov.csv', header=T)
#subset covaraites we need
cov12r2 <- cov12r2[,c("region","length_2","impound","jdate_2","hectares","area", "int","short","water")]
# #the distance bins

sora12r2 <- sora12r2[order(sora12r2$impound),]
cov12r2 <- cov12r2[order(cov12r2$impound),]

sora12r2 <- sora12r2[,3:41]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf12r2 = unmarkedFrameGDS(y=sora12r2, 
                           numPrimary=3,
                           siteCovs = cov12r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r2$length_2,
)
```


```r
model12r2 <- list()
model12r2$null12r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r2$reg12r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r2$reg_w12r2 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r2$reg_w_i_12r2 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r2$short_r12r2 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
model12r2$short_r_i_12r2 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r2$short12r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model12r2$short_w12r2 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
model12r2$short_w_i_12r2 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model12r2$global12r2 =gdistsamp(lambdaformula = ~region+water+short+region*water+water*short+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf12r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```


```r
list12r2  = fitList(model12r2)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model12r2 = modSel(list12r2)
model12r2
```

```
##                nPars     AIC   delta   AICwt cumltvWt
## global12r2        16  626.35    0.00 1.0e+00     1.00
## short_r_i_12r2    11  680.48   54.13 1.8e-12     1.00
## short_r12r2        8  717.86   91.51 1.3e-20     1.00
## reg_w12r2          8  720.08   93.74 4.4e-21     1.00
## reg_w_i_12r2      11  722.13   95.79 1.6e-21     1.00
## reg12r2            7  723.62   97.27 7.5e-22     1.00
## null12r2           4  780.36  154.02 3.6e-34     1.00
## short_w_i_12r2     6 2263.01 1636.66 0.0e+00     1.00
## short_w12r2        5 2539.07 1912.73 0.0e+00     1.00
## short12r2          4 2906.59 2280.25 0.0e+00     1.00
```
