
```r
# predictions from GDistsamp 2013 Round 2
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
sora13r2 <- read.csv('2013r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r2 <- read.csv('2013r2_cov.csv', header=T)
#subset covaraites we need
cov13r2 <- cov13r2[,c("region","length_2","impound","jdate_2","hectares","area", "int","short","water")]
# #the distance bins

sora13r2 <- sora13r2[order(sora13r2$impound),]
cov13r2 <- cov13r2[order(cov13r2$impound),]

sora13r2 <- sora13r2[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r2 = unmarkedFrameGDS(y=sora13r2, 
                           numPrimary=3,
                           siteCovs = cov13r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r2$length_2,
)
```


```r
model13r2 <- list()
model13r2$null13r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r2$reg13r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r2$reg_w13r2 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r2$reg_w_i_13r2 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r2$short_r13r2 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
model13r2$short_r_i_13r2 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r2$short13r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r2$short_w13r2 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
model13r2$short_w_i_13r2 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r2$global13r2 =gdistsamp(lambdaformula = ~region+water+short+region*water+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
list13r2  = fitList(model13r2)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model13r2 = modSel(list13r2)
model13r2
```

```
##                nPars     AIC  delta    AICwt cumltvWt
## global13r2        15  399.35   0.00  1.0e+00     1.00
## reg_w_i_13r2      11  444.14  44.79  1.9e-10     1.00
## short_r_i_13r2    11  473.46  74.11  8.1e-17     1.00
## reg13r2            7  537.57 138.22  9.7e-31     1.00
## reg_w13r2          8  537.93 138.58  8.1e-31     1.00
## short_r13r2        8  539.45 140.10  3.8e-31     1.00
## null13r2           4  647.63 248.28  1.2e-54     1.00
## short_w_i_13r2     6 1093.61 694.26 1.8e-151     1.00
## short_w13r2        5 1174.14 774.79 5.7e-169     1.00
## short13r2          4 1319.34 919.99 1.7e-200     1.00
```
