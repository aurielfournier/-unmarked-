
```r
# predictions from GDistsamp 2013
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
sora13 <- read.csv('2013_vira_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov13 <- read.csv('2013_cov_vira.csv', header=T)
#subset covaraites we need
cov13 <- cov13[,c("region","length","impound","jdate","area", "int","short","water")]
# #the distance bins

sora13 <- sora13[order(sora13$impound),]
cov13 <- cov13[order(cov13$impound),]

sora13 <- sora13[,2:22]
cutpt = as.numeric(c(0,2,4,6,8,10,13,14)) 
#Unmarked Data Frame
umf13 = unmarkedMultFrame(y=sora13, 
                           numPrimary=3,
                           siteCovs = cov13,
)
```


```r
model13 <- list()
model13$null13 = colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")


model13$reg13 = colext(psiformula= ~region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")
```

```r
model13$reg_w13 =colext(psiformula= ~region+water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")

model13$reg_w_i_13 =colext(psiformula= ~region+water+region*water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")
```

```r
model13$short_r13 =colext(psiformula= ~short+region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")

model13$short_r_i_13 =colext(psiformula= ~short+region+short*region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")
```

```r
model13$short13 =colext(psiformula= ~short-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")

model13$short_w13 =colext(psiformula= ~short+water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")

model13$short_w_i_13 =colext(psiformula= ~short+water+short*water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")
```

```r
model13$global13 =colext(psiformula= ~region+water+short+short*region+region*water-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf13, method="BFGS")
```

```r
list13  = fitList(model13)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model13 = modSel(list13)
```

```
## Warning: NaNs produced
## Warning: NaNs produced
```

```r
model13
```

```
##              nPars    AIC delta   AICwt cumltvWt
## null13           4  87.33  0.00 2.6e-01     0.26
## reg13            7  87.45  0.12 2.4e-01     0.50
## short13          4  88.24  0.92 1.6e-01     0.66
## reg_w13          8  88.44  1.12 1.5e-01     0.80
## short_r13        8  89.37  2.04 9.2e-02     0.89
## short_w13        5  89.85  2.52 7.2e-02     0.97
## short_w_i_13     6  92.16  4.84 2.3e-02     0.99
## short_r_i_13    11  94.04  6.71 8.9e-03     1.00
## reg_w_i_13      11  97.32  9.99 1.7e-03     1.00
## global13        15 106.67 19.34 1.6e-05     1.00
```
