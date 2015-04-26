
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
sora13r1 <- read.csv('2013r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13r1 <- read.csv('2013r1_cov.csv', header=T)
#subset covaraites we need
cov13r1 <- cov13r1[,c("region","length_1","impound","jdate_1","hectares","area", "int","short","water")]
# #the distance bins

sora13r1 <- sora13r1[order(sora13r1$impound),]
cov13r1 <- cov13r1[order(cov13r1$impound),]

sora13r1 <- sora13r1[,2:40]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r1 = unmarkedFrameGDS(y=sora13r1, 
                           numPrimary=3,
                           siteCovs = cov13r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r1$length_1,
)
```


```r
model13r1 <- list()
model13r1$null13r1 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r1$reg13r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r1$reg_w13r1 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r1$reg_w_i_13r1 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r1$short_r13r1 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
model13r1$short_r_i_13r1 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r1$short13r1 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")

model13r1$short_w13r1 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
model13r1$short_w_i_13r1 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
model13r1$global13r1 =gdistsamp(lambdaformula = ~region+water+short+region*water+water*short+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r1, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```r
list13r1  = fitList(model13r1)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model13r1 = modSel(list13r1)
model13r1
```

```
##                nPars    AIC delta   AICwt cumltvWt
## reg_w_i_13r1      11 154.22  0.00 8.3e-01     0.83
## reg_w13r1          8 157.93  3.71 1.3e-01     0.96
## global13r1        16 160.34  6.11 3.9e-02     1.00
## reg13r1            7 167.64 13.42 1.0e-03     1.00
## short_r13r1        8 168.96 14.74 5.2e-04     1.00
## short_r_i_13r1    11 173.84 19.62 4.6e-05     1.00
## short13r1          4 181.20 26.98 1.1e-06     1.00
## short_w13r1        5 182.05 27.83 7.5e-07     1.00
## null13r1           4 182.79 28.57 5.2e-07     1.00
## short_w_i_13r1     6 183.97 29.75 2.9e-07     1.00
```
