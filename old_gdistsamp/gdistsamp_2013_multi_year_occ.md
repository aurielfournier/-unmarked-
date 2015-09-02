
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
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_sora_multi_year_occ.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_cov_multi_year_occ.csv', header=T)

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
```

```r
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
```

```r
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
```

```r
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
```

```r
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
```

```r
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
```

```r
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
```

```r
# model$millet_bulrush =gdistsamp(lambdaformula = ~scale_millet_+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$annual_smart_bulrush =gdistsamp(lambdaformula = ~scale_annual_smart_+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$fs_bulrush =gdistsamp(lambdaformula = ~fs+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$perennialemerg_bulrush =gdistsamp(lambdaformula = ~scale_pe+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$short_bulrush =gdistsamp(lambdaformula = ~scale_short+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
# model$averagewater_bulrush =gdistsamp(lambdaformula = ~scale_averagewater+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$openwater_bulrush =gdistsamp(lambdaformula = ~scale_water+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$int_bulrush =gdistsamp(lambdaformula = ~scale_int+scale_bulrush_, 
#                       phiformula = ~1, 
#                       pformula = ~ 1,
#                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
# ```
# ```{r}
# model$region_bulrush =gdistsamp(lambdaformula = ~region+scale_bulrush_, 
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
##                             nPars     AIC delta  AICwt cumltvWt
## null                            5 1870.09  0.00 0.1334     0.13
## region                          8 1870.55  0.47 0.1057     0.24
## perenialemerg                   6 1871.43  1.34 0.0682     0.31
## region_openwater                9 1871.47  1.38 0.0669     0.37
## openwater                       6 1871.91  1.82 0.0536     0.43
## short                           6 1871.97  1.88 0.0521     0.48
## int                             6 1872.01  1.92 0.0511     0.53
## averagewater                    6 1872.09  2.00 0.0491     0.58
## region_averagewater             9 1872.42  2.33 0.0415     0.62
## perennialemerg_short            7 1872.50  2.41 0.0400     0.66
## region_int                      9 1872.50  2.41 0.0399     0.70
## region_perennialemerg           9 1872.52  2.43 0.0396     0.74
## region_short                    9 1872.53  2.44 0.0393     0.78
## int_perennialemerg              7 1872.75  2.66 0.0353     0.82
## averagewater_perennialemerg     7 1872.95  2.86 0.0319     0.85
## perennialemerg_openwater        7 1873.05  2.96 0.0303     0.88
## averagewater_openwater          7 1873.72  3.63 0.0217     0.90
## short_openwater                 7 1873.86  3.77 0.0203     0.92
## int_openwater                   7 1873.88  3.79 0.0200     0.94
## averagewater_short              7 1873.91  3.82 0.0197     0.96
## int_short                       7 1873.94  3.85 0.0195     0.98
## averagewater_int                7 1873.98  3.89 0.0191     1.00
## global                         13 1878.73  8.65 0.0018     1.00
```
