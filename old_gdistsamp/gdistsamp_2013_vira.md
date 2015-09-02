
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
sora13 <- read.csv('2013_vira.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13 <- read.csv('2013_cov_vira.csv', header=T)
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
##              nPars    AIC delta  AICwt cumltvWt
## short13          5 141.96  0.00 0.2383     0.24
## reg13            8 142.20  0.24 0.2114     0.45
## reg_w_i_13      12 143.21  1.25 0.1278     0.58
## short_r13        9 143.55  1.59 0.1076     0.69
## reg_w13          9 143.82  1.86 0.0943     0.78
## short_w13        6 143.91  1.95 0.0898     0.87
## null13           5 144.10  2.14 0.0819     0.95
## short_w_i_13     7 145.88  3.92 0.0336     0.98
## short_r_i_13    12 148.28  6.32 0.0101     0.99
## global13        16 149.62  7.65 0.0052     1.00
```
