
```r
# predictions from GDistsamp 2014
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
sora14 <- read.csv('2014_vira_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14 <- read.csv('2014_cov_vira.csv', header=T)
#subset covaraites we need
cov14 <- cov14[,c("region","length","impound","jdate","area", "int","short","averagewater")]
# #the distance bins

sora14 <- sora14[order(sora14$impound),]
cov14 <- cov14[order(cov14$impound),]

sora14 <- sora14[,2:22]
cutpt = as.numeric(c(0,2,4,6,8,10,14,14)) 
#Unmarked Data Frame
umf14 = unmarkedMultFrame(y=sora14, 
                           numPrimary=3,
                           siteCovs = cov14,
)
```


```r
model14 <- list()
model14$null14 = colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")


model14$reg14 = colext(psiformula= ~region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")
```

```r
model14$reg_w14 =colext(psiformula= ~region+averagewater-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")

model14$reg_w_i_14 =colext(psiformula= ~region+averagewater+region*averagewater-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")
```

```r
model14$short_r14 =colext(psiformula= ~short+region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")

model14$short_r_i_14 =colext(psiformula= ~short+region+short*region-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")
```

```r
model14$short14 =colext(psiformula= ~short-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")

model14$short_w14 =colext(psiformula= ~short+averagewater-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")

model14$short_w_i_14 =colext(psiformula= ~short+averagewater+short*averagewater-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")
```

```r
model14$global14 =colext(psiformula= ~region+averagewater+short+short*region+region*averagewater-1, gammaformula = ~ 1, epsilonformula = ~ 1, pformula = ~ 1, data = umf14, method="BFGS")
```

```r
list14  = fitList(model14)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```r
model14 = modSel(list14)
```

```
## Warning: NaNs produced
```

```r
model14
```

```
##              nPars   AIC delta   AICwt cumltvWt
## null14           4 66.42  0.00 3.2e-01     0.32
## reg14            7 66.89  0.47 2.5e-01     0.57
## short_w_i_14     6 68.42  2.00 1.2e-01     0.69
## reg_w14          8 68.58  2.16 1.1e-01     0.80
## short_r14        8 68.90  2.49 9.2e-02     0.89
## short14          4 70.12  3.70 5.0e-02     0.94
## short_w14        5 70.39  3.97 4.4e-02     0.98
## reg_w_i_14      11 72.33  5.91 1.7e-02     1.00
## short_r_i_14    11 80.90 14.48 2.3e-04     1.00
## global14        15 88.77 22.35 4.5e-06     1.00
```
