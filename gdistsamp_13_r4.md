
```r
# predictions from GDistsamp 2013 Round 4
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
sora13r4 <- read.csv('2013r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r4 <- read.csv('2013r4_cov.csv', header=T)
#subset covaraites we need
cov13r4 <- cov13r4[,c("region","length_4","impound","jdate_4","hectares","area", "int","short","water")]
# #the distance bins

sora13r4 <- sora13r4[order(sora13r4$impound),]
cov13r4 <- cov13r4[order(cov13r4$impound),]

sora13r4 <- sora13r4[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r4 = unmarkedFrameGDS(y=sora13r4, 
                           numPrimary=3,
                           siteCovs = cov13r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r4$length_4,
)
```


```r
model13r4 <- list()
model13r4$null13r4 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r4$reg13r4 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r4$reg_w13r4 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r4$reg_w_i_13r4 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r4$short_r13r4 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
model13r4$short_r_i_13r4 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r4$short13r4 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r4$short_w13r4 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
model13r4$short_w_i_13r4 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r4$global13r4 =gdistsamp(lambdaformula = ~region+water+short+region*water+water*short+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r4, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Warning: Hessian is singular. Try using fewer covariates and supplying
## starting values.
```



```r
list13r4  = fitList(model13r4)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model13r4 = modSel(list13r4)
```

```
## Hessian is singular.
```

```r
model13r4
```

```
##                nPars   AIC delta   AICwt cumltvWt
## reg_w13r4          6 71.79  0.00 7.1e-01     0.71
## reg_w_i_13r4       7 73.79  2.00 2.6e-01     0.98
## global13r4        10 79.22  7.44 1.7e-02     0.99
## short_r13r4        6 82.67 10.88 3.1e-03     1.00
## reg13r4            5 83.01 11.22 2.6e-03     1.00
## short_r_i_13r4     7 84.67 12.88 1.1e-03     1.00
## short_w_i_13r4     6 86.00 14.21 5.8e-04     1.00
## short_w13r4        5 96.42 24.63 3.2e-06     1.00
## short13r4          4 97.14 25.35 2.2e-06     1.00
## null13r4           4 98.06 26.27 1.4e-06     1.00
```
