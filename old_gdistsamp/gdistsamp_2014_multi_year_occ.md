
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
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_sora_multi_year_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2014_cov_multi_year_occ.csv', header=T)

#rounds <- cov[,grepl("averagewater_",colnames(cov))]
#rounds <- rounds[,c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)]
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
                           numPrimary=18,
                           siteCovs = cov,
                           #yearlySiteCovs=list(scale_w=rounds[,grepl("scale",colnames(rounds))],w=rounds[,grepl("^averagewater",colnames(rounds))]),
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
model$perenialemerg = gdistsamp(lambdaformula = ~scale_pe, 
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
# model$openwater = gdistsamp(lambdaformula = ~scale_millet_, 
#                     phiformula = ~1, 
#                     pformula = ~ 1,
#                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwater = gdistsamp(lambdaformula = ~scale_annual_smartweed_, 
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
model$region_perennialemerg =gdistsamp(lambdaformula = ~region+scale_pe, 
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
# model$averagewater_fedstate =gdistsamp(lambdaformula = ~scale_averagewater+fs, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater_perennialemerg =gdistsamp(lambdaformula = ~scale_averagewater+scale_pe, 
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
# model$int_fedstate =gdistsamp(lambdaformula = ~scale_int+fs, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int_perennialemerg =gdistsamp(lambdaformula = ~scale_int+scale_pe, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
# ```
# ```{r}
# model$fedstate_short =gdistsamp(lambdaformula = ~fs+scale_short, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$fedstate_openwater =gdistsamp(lambdaformula = ~fs+scale_water, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$perennialemerg_short =gdistsamp(lambdaformula = ~scale_pe+scale_short, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$perennialemerg_openwater =gdistsamp(lambdaformula = ~scale_pe+scale_water, 
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
# model$region_millet =gdistsamp(lambdaformula = ~region+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$region_annual_smart =gdistsamp(lambdaformula = ~region+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$int_millet =gdistsamp(lambdaformula = ~scale_int+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$int_annual_smart =gdistsamp(lambdaformula = ~scale_int+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwater_millet =gdistsamp(lambdaformula = ~scale_water+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwater_annual_smart =gdistsamp(lambdaformula = ~scale_water+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$averagewater_millet =gdistsamp(lambdaformula = ~scale_averagewater+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$averagewater_annual_smart =gdistsamp(lambdaformula = ~scale_averagewater+scale_annual_smart_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$short_millet =gdistsamp(lambdaformula = ~scale_short+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$short_annual_smart =gdistsamp(lambdaformula = ~scale_short+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$pe_millet =gdistsamp(lambdaformula = ~scale_pe+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$pe_millet =gdistsamp(lambdaformula = ~scale_pe+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$fs_millet =gdistsamp(lambdaformula = ~fs+scale_millet_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$fs_annual_smart =gdistsamp(lambdaformula = ~fs+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$millet_annual_smart =gdistsamp(lambdaformula = ~scale_millet_+scale_annual_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$milletperennial_smartweed =gdistsamp(lambdaformula = ~scale_millet_+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$annual_smartperennial_smartweed =gdistsamp(lambdaformula = ~scale_annual_smart_+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$fsperennial_smartweed =gdistsamp(lambdaformula = ~fs+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$perennialemergperennial_smartweed =gdistsamp(lambdaformula = ~scale_pe+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$shortperennial_smartweed =gdistsamp(lambdaformula = ~scale_short+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$averagewaterperennial_smartweed =gdistsamp(lambdaformula = ~scale_averagewater+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwaterperennial_smartweed =gdistsamp(lambdaformula = ~scale_water+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$intperennial_smartweed =gdistsamp(lambdaformula = ~scale_int+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$regionperennial_smartweed =gdistsamp(lambdaformula = ~region+scale_perennial_smartweed_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$region_upland =gdistsamp(lambdaformula = ~region+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$int_upland =gdistsamp(lambdaformula = ~scale_int+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwater_upland =gdistsamp(lambdaformula = ~scale_water+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$averagewater_upland =gdistsamp(lambdaformula = ~scale_averagawater+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$short_upland =gdistsamp(lambdaformula = ~scale_short+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$perennialemerg_upland =gdistsamp(lambdaformula = ~scale_pe+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$fs_upland =gdistsamp(lambdaformula = ~fs+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$millet_upland =gdistsamp(lambdaformula = ~scale_millet_+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$annual_smartweed_upland =gdistsamp(lambdaformula = ~scale_annual_smartweed_+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$perennial_smartweed_upland =gdistsamp(lambdaformula = ~scale_perennial_smartweed_+scale_upland_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$global <- gdistsamp(lambdaformula = ~scale_pe+scale_short+scale_water+scale_averagewater+region+scale_int, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
save(model, file="2014_models.Rdata")
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
##                             nPars     AIC delta  AICwt cumltvWt
## region_short                    9 1479.43  0.00 0.1232     0.12
## null                            5 1479.61  0.18 0.1126     0.24
## averagewater                    6 1479.80  0.37 0.1023     0.34
## short                           6 1480.43  1.00 0.0746     0.41
## averagewater_short              7 1480.44  1.01 0.0744     0.49
## region                          8 1481.51  2.08 0.0435     0.53
## int                             6 1481.55  2.12 0.0427     0.57
## perenialemerg                   6 1481.55  2.12 0.0427     0.62
## averagewater_perennialemerg     7 1481.58  2.15 0.0420     0.66
## openwater                       6 1481.61  2.18 0.0414     0.70
## averagewater_openwater          7 1481.78  2.36 0.0379     0.74
## averagewater_int                7 1481.80  2.37 0.0376     0.77
## int_short                       7 1482.24  2.81 0.0303     0.81
## perennialemerg_short            7 1482.32  2.89 0.0290     0.83
## region_averagewater             9 1482.37  2.94 0.0283     0.86
## short_openwater                 7 1482.37  2.94 0.0283     0.89
## int_openwater                   7 1483.00  3.57 0.0207     0.91
## region_int                      9 1483.29  3.86 0.0178     0.93
## int_perennialemerg              7 1483.47  4.04 0.0163     0.95
## region_openwater                9 1483.48  4.05 0.0163     0.96
## region_perennialemerg           9 1483.49  4.06 0.0162     0.98
## perennialemerg_openwater        7 1483.55  4.12 0.0157     0.99
## global                         13 1485.37  5.94 0.0063     1.00
```
