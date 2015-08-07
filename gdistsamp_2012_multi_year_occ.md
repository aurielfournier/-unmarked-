
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
# model$fedstate = gdistsamp(lambdaformula = ~fs, 
#                     phiformula = ~1, 
#                     pformula = ~ 1,
#                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$openwater = gdistsamp(lambdaformula = ~scale_millet_, 
#                     phiformula = ~1, 
#                     pformula = ~ 1,
#                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwater = gdistsamp(lambdaformula = ~scale_smartweed_, 
#                     phiformula = ~1, 
#                     pformula = ~ 1,
#                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$region_fedstate =gdistsamp(lambdaformula = ~region+fs-1, 
#                      phiformula = ~1, 
#                      pformula = ~ 1,
#                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$averagewater_fedstate =gdistsamp(lambdaformula = ~scale_averagewater+fs, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$fedstate_short =gdistsamp(lambdaformula = ~fs+scale_short, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$fedstate_openwater =gdistsamp(lambdaformula = ~fs+scale_water, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$region_millet =gdistsamp(lambdaformula = ~region+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$region_smart =gdistsamp(lambdaformula = ~region+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$int_millet =gdistsamp(lambdaformula = ~scale_int+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$int_smart =gdistsamp(lambdaformula = ~scale_int+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$openwater_millet =gdistsamp(lambdaformula = ~scale_water+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$openwater_smart =gdistsamp(lambdaformula = ~scale_water+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$averagewater_millet =gdistsamp(lambdaformula = ~scale_averagewater+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$averagewater_smart =gdistsamp(lambdaformula = ~scale_averagewater+scale_smart_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$short_millet =gdistsamp(lambdaformula = ~scale_short+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$short_smart =gdistsamp(lambdaformula = ~scale_short+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# model$fs_millet =gdistsamp(lambdaformula = ~fs+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$fs_smart =gdistsamp(lambdaformula = ~fs+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$millet_smart =gdistsamp(lambdaformula = ~scale_millet_+scale_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$global <- gdistsamp(lambdaformula = ~scale_short+scale_water+scale_averagewater+region+scale_int, 
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
##                        nPars    AIC delta  AICwt cumltvWt
## region                     8 160.76  0.00 0.1501     0.15
## region_short               9 161.12  0.36 0.1255     0.28
## short                      6 161.36  0.59 0.1117     0.39
## int_short                  7 161.43  0.67 0.1076     0.50
## region_averagewater        9 161.80  1.03 0.0895     0.58
## short_openwater            7 162.09  1.32 0.0776     0.66
## region_openwater           9 162.39  1.63 0.0665     0.73
## region_int                 9 162.76  2.00 0.0554     0.78
## averagewater_short         7 162.96  2.20 0.0500     0.83
## int_fedstate               7 163.64  2.88 0.0356     0.87
## averagewater_int           7 163.73  2.97 0.0341     0.90
## int                        6 164.20  3.44 0.0269     0.93
## averagewater_openwater     7 165.38  4.61 0.0149     0.95
## null                       5 165.55  4.79 0.0137     0.96
## averagewater               6 165.81  5.05 0.0120     0.97
## int_openwater              7 165.87  5.10 0.0117     0.98
## openwater                  6 166.25  5.48 0.0097     0.99
## global                    12 166.81  6.05 0.0073     1.00
```
