
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
sora12 <- read.csv('2012_vira_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12 <- read.csv('2012_cov_vira.csv', header=T)
#subset covaraites we need
cov12 <- cov12[,c("region","length","impound","jdate","area", "int","short","water")]
# #the distance bins

sora12 <- sora12[order(sora12$impound),]
cov12 <- cov12[order(cov12$impound),]

sora12 <- sora12[,2:22]
cutpt = as.numeric(c(0,2,4,6,8,10,12,14)) 
#Unmarked Data Frame
umf12 = unmarkedMultFrame(y=sora12, 
                           numPrimary=3,
                           siteCovs = cov12,
)
```


```r
model12 <- list()
model12$null12 = colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")


model12$reg12 = colext(psiformula= ~region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")
```

```r
model12$reg_w12 =colext(psiformula= ~region+water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")

model12$reg_w_i_12 =colext(psiformula= ~region+water+region*water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")
```

```r
model12$short_r12 =colext(psiformula= ~short+region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")

model12$short_r_i_12 =colext(psiformula= ~short+region+short*region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")
```

```r
model12$short12 =colext(psiformula= ~short-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")

model12$short_w12 =colext(psiformula= ~short+water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")

model12$short_w_i_12 =colext(psiformula= ~short+water+short*water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")
```

```r
model12$global12 =colext(psiformula= ~region+water+short+short*region+region*water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf12, method="BFGS")
```

```r
list12  = fitList(model12)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model12 = modSel(list12)
model12
```

```
##              nPars    AIC delta   AICwt cumltvWt
## null12           4 146.61  0.00 5.8e-01     0.58
## reg12            7 149.54  2.93 1.4e-01     0.72
## short12          4 149.71  3.10 1.2e-01     0.84
## short_w12        5 150.74  4.13 7.4e-02     0.92
## short_r12        8 150.74  4.13 7.4e-02     0.99
## short_r_i_12    11 157.52 10.91 2.5e-03     1.00
## reg_w_i_12      11 157.58 10.97 2.4e-03     1.00
## short_w_i_12     6 158.22 11.61 1.8e-03     1.00
## reg_w12          8 161.08 14.47 4.2e-04     1.00
## global12        15 173.98 27.37 6.7e-07     1.00
```
