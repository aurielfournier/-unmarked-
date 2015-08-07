
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
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora_multi_year_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov_multi_year_occ.csv', header=T)

rounds <- cov[,grepl("averagewater_",colnames(cov))]
rounds <- rounds[,c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)]
length <- cov[,grepl("length",colnames(cov))]
cov <- cov[,!grepl("averagewater_",colnames(cov))]
cov <- cov[,!grepl("length",colnames(cov))]
#subset covaraites we need
#cov <- cov[,c("region","length","impound","jdate","area", "scale_int","scale_short","scale_averagewater")]
# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:ncol(sora)]
cutpt = as.numeric(c(0,1,2,3,4,5,6)) 

#Unmarked Data Frame

umf = unmarkedFrameGDS(y=sora, 
                           numPrimary=9,
                           siteCovs = cov,
                           yearlySiteCovs=list(scale_w=rounds[,grepl("scale",colnames(rounds))],w=rounds[,grepl("^averagewater",colnames(rounds))]),
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=rowMeans(length,na.rm=TRUE)
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
model$fedstate = gdistsamp(lambdaformula = ~fs, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$perenialemerg = gdistsamp(lambdaformula = ~scale_pe, 
#                     phiformula = ~1, 
#                     pformula = ~ 1,
#                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
model$openwater = gdistsamp(lambdaformula = ~scale_millet_, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$openwater = gdistsamp(lambdaformula = ~scale_smartweed_, 
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
model$region_fedstate =gdistsamp(lambdaformula = ~region+fs-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$region_perennialemerg =gdistsamp(lambdaformula = ~region+scale_pe, 
#                      phiformula = ~1, 
#                      pformula = ~ 1,
#                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
model$averagewater_fedstate =gdistsamp(lambdaformula = ~scale_averagewater+fs, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$averagewater_perennialemerg =gdistsamp(lambdaformula = ~scale_averagewater+scale_pe, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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

```r
# model$int_perennialemerg =gdistsamp(lambdaformula = ~scale_int+scale_pe, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$fedstate_perrenialemerg=gdistsamp(lambdaformula = ~fs+scale_pe, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fedstate_short =gdistsamp(lambdaformula = ~fs+scale_short, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```
## Warning in dnbinom(x, mu = lambda, size = exp(pars[nP])): NaNs produced
```

```r
model$fedstate_openwater =gdistsamp(lambdaformula = ~fs+scale_water, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$perennialemerg_short =gdistsamp(lambdaformula = ~scale_pe+scale_short, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$perennialemerg_openwater =gdistsamp(lambdaformula = ~scale_pe+scale_water, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$short_openwater =gdistsamp(lambdaformula = ~scale_short+scale_water, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$region_millet =gdistsamp(lambdaformula = ~region+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$region_smart =gdistsamp(lambdaformula = ~region+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int_millet =gdistsamp(lambdaformula = ~scale_int+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int_smart =gdistsamp(lambdaformula = ~scale_int+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$openwater_millet =gdistsamp(lambdaformula = ~scale_water+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$openwater_smart =gdistsamp(lambdaformula = ~scale_water+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater_millet =gdistsamp(lambdaformula = ~scale_averagewater+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater_smart =gdistsamp(lambdaformula = ~scale_averagewater+scale_smart_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error in eval(expr, envir, enclos): object 'scale_smart_' not found
```

```r
model$short_millet =gdistsamp(lambdaformula = ~scale_short+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$short_smart =gdistsamp(lambdaformula = ~scale_short+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$pe_millet =gdistsamp(lambdaformula = ~scale_pe+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$pe_millet =gdistsamp(lambdaformula = ~scale_pe+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fs_millet =gdistsamp(lambdaformula = ~fs+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fs_smart =gdistsamp(lambdaformula = ~fs+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$millet_smart =gdistsamp(lambdaformula = ~scale_millet_+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$global <- gdistsamp(lambdaformula = ~scale_short+scale_water+scale_averagewater+fs+region+scale_int+scale_millet_+scale_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
save(model, file="2012_models.Rdata")
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
##                        nPars    AIC delta   AICwt cumltvWt
## int_millet                 7 158.91  0.00 0.12494     0.12
## openwater_millet           7 159.61  0.70 0.08808     0.21
## region_millet              9 159.90  0.99 0.07618     0.29
## fs_millet                  7 160.68  1.77 0.05167     0.34
## region                     8 160.76  1.86 0.04942     0.39
## short_millet               7 160.78  1.88 0.04892     0.44
## millet_smart               7 161.06  2.15 0.04264     0.48
## region_short               9 161.12  2.21 0.04132     0.52
## short                      6 161.36  2.45 0.03675     0.56
## short_smart                7 161.43  2.52 0.03551     0.60
## int_short                  7 161.43  2.52 0.03543     0.63
## region_smart               9 161.54  2.63 0.03346     0.66
## averagewater_millet        7 161.68  2.77 0.03124     0.70
## region_fedstate            9 161.74  2.83 0.03035     0.73
## fs_smart                   7 161.79  2.88 0.02960     0.76
## region_averagewater        9 161.80  2.89 0.02946     0.78
## short_openwater            7 162.09  3.18 0.02554     0.81
## region_openwater           9 162.39  3.48 0.02188     0.83
## fedstate                   6 162.43  3.52 0.02146     0.85
## region_int                 9 162.76  3.85 0.01822     0.87
## averagewater_short         7 162.96  4.05 0.01647     0.89
## fedstate_short             7 163.12  4.21 0.01523     0.90
## int_smart                  7 163.45  4.54 0.01293     0.92
## int_fedstate               7 163.64  4.73 0.01173     0.93
## averagewater_int           7 163.73  4.82 0.01121     0.94
## averagewater_fedstate      7 163.96  5.05 0.01002     0.95
## openwater                  6 164.07  5.16 0.00945     0.96
## int                        6 164.20  5.29 0.00886     0.97
## fedstate_openwater         7 164.22  5.31 0.00879     0.98
## openwater_smart            7 165.07  6.16 0.00574     0.98
## averagewater_openwater     7 165.38  6.47 0.00492     0.99
## null                       5 165.55  6.64 0.00451     0.99
## averagewater               6 165.81  6.90 0.00396     1.00
## int_openwater              7 165.87  6.96 0.00385     1.00
## global                    15 171.15 12.24 0.00027     1.00
```
