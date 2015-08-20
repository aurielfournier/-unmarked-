
```r
# predictions from GDistsamp 2012

library(unmarked)
```

```
## Loading required package: methods
## Loading required package: reshape
## Loading required package: lattice
## Loading required package: Rcpp
```

```r
#read in the sora observations
#sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
sora <- read.csv('~/Documents/data/2013_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
cov <- read.csv('~/Documents/data/2013_cov.csv', header=T)

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:31]
cutpt = as.numeric(c(0,1,2,3,4,5)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                           numPrimary=6,
                           siteCovs = cov,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov$length
)
```


```r
model <- list()

model$null = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$region = gdistsamp(lambdaformula = ~region, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_short = gdistsamp(lambdaformula = ~scale_pe+scale_short, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_openwater = gdistsamp(lambdaformula = ~scale_pe+scale_water, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_averagewater = gdistsamp(lambdaformula = ~scale_pe+scale_averagewater, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_int = gdistsamp(lambdaformula = ~scale_pe+scale_int, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_region = gdistsamp(lambdaformula = ~scale_pe+region, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe = gdistsamp(lambdaformula = ~scale_pe, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater = gdistsamp(lambdaformula = ~scale_averagewater, 
                    phiformula = ~1, 
                    pformula = ~1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int = gdistsamp(lambdaformula = ~scale_int, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$short = gdistsamp(lambdaformula = ~scale_short, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$openwater = gdistsamp(lambdaformula = ~scale_water, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$region_averagewater =gdistsamp(lambdaformula = ~region+scale_averagewater, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$region_int =gdistsamp(lambdaformula = ~region+scale_int, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$region_short =gdistsamp(lambdaformula = ~region+scale_short, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$region_openwater =gdistsamp(lambdaformula = ~region+scale_water, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater_int =gdistsamp(lambdaformula = ~scale_averagewater+scale_int, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$averagewater_short =gdistsamp(lambdaformula = ~scale_averagewater+scale_short, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater_openwater =gdistsamp(lambdaformula = ~scale_averagewater+scale_water, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int_fedstate =gdistsamp(lambdaformula = ~scale_int+fs, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error in eval(expr, envir, enclos): object 'fs' not found
```


```r
model$int_short =gdistsamp(lambdaformula = ~scale_int+scale_short, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int_openwater =gdistsamp(lambdaformula = ~scale_int+scale_water, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$short_openwater =gdistsamp(lambdaformula = ~scale_short+scale_water, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$global <- gdistsamp(lambdaformula = ~scale_short+scale_water+scale_averagewater+region+scale_int+scale_pe, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
save(model, file="2013_models.Rdata")
list  = fitList(model)
```

```
## Warning in fitList(model): If supplying a list of fits, use fits = 'mylist'
```

```r
model = modSel(list)
model
```

```
##                        nPars    AIC delta  AICwt cumltvWt
## averagewater_int           7 529.12 0.000 0.2024     0.20
## int                        6 529.17 0.051 0.1973     0.40
## pe_int                     7 529.77 0.653 0.1460     0.55
## global                    13 531.05 1.938 0.0768     0.62
## int_openwater              7 531.16 2.040 0.0730     0.70
## int_short                  7 531.16 2.047 0.0727     0.77
## region_int                 9 532.32 3.207 0.0407     0.81
## averagewater               6 532.88 3.763 0.0308     0.84
## region_averagewater        9 533.07 3.950 0.0281     0.87
## averagewater_openwater     7 533.46 4.343 0.0231     0.89
## region                     8 533.82 4.702 0.0193     0.91
## null                       5 534.53 5.413 0.0135     0.92
## pe_region                  9 534.59 5.471 0.0131     0.94
## averagewater_short         7 534.72 5.607 0.0123     0.95
## pe_averagewater            7 534.88 5.762 0.0113     0.96
## region_short               9 535.21 6.097 0.0096     0.97
## region_openwater           9 535.30 6.178 0.0092     0.98
## short                      6 536.50 7.388 0.0050     0.98
## pe                         6 536.50 7.388 0.0050     0.99
## openwater                  6 536.53 7.413 0.0050     0.99
## pe_short                   7 538.47 9.356 0.0019     1.00
## short_openwater            7 538.49 9.378 0.0019     1.00
## pe_openwater               7 538.50 9.386 0.0019     1.00
```
