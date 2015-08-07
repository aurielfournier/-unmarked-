
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
model$fedstate = gdistsamp(lambdaformula = ~fs, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
model$openwater = gdistsamp(lambdaformula = ~scale_millet_, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$openwater = gdistsamp(lambdaformula = ~scale_annual_smartweed_, 
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
model$averagewater_fedstate =gdistsamp(lambdaformula = ~scale_averagewater+fs, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
model$int_fedstate =gdistsamp(lambdaformula = ~scale_int+fs, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
model$fedstate_perrenialemerg=gdistsamp(lambdaformula = ~fs+scale_pe, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fedstate_short =gdistsamp(lambdaformula = ~fs+scale_short, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fedstate_openwater =gdistsamp(lambdaformula = ~fs+scale_water, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
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
model$region_millet =gdistsamp(lambdaformula = ~region+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$region_annual_smart =gdistsamp(lambdaformula = ~region+scale_annual_smartweed_, 
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
model$int_annual_smart =gdistsamp(lambdaformula = ~scale_int+scale_annual_smartweed_, 
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
model$openwater_annual_smart =gdistsamp(lambdaformula = ~scale_water+scale_annual_smartweed_, 
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
model$averagewater_annual_smart =gdistsamp(lambdaformula = ~scale_averagewater+scale_annual_smart_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error in eval(expr, envir, enclos): object 'scale_annual_smart_' not found
```

```r
model$short_millet =gdistsamp(lambdaformula = ~scale_short+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$short_annual_smart =gdistsamp(lambdaformula = ~scale_short+scale_annual_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_millet =gdistsamp(lambdaformula = ~scale_pe+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$pe_millet =gdistsamp(lambdaformula = ~scale_pe+scale_annual_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fs_millet =gdistsamp(lambdaformula = ~fs+scale_millet_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$fs_annual_smart =gdistsamp(lambdaformula = ~fs+scale_annual_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$millet_annual_smart =gdistsamp(lambdaformula = ~scale_millet_+scale_annual_smartweed_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$millet_bulrush =gdistsamp(lambdaformula = ~scale_millet_+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$annual_smart_bulrush =gdistsamp(lambdaformula = ~scale_annual_smart_+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error in eval(expr, envir, enclos): object 'scale_annual_smart_' not found
```

```r
model$fs_bulrush =gdistsamp(lambdaformula = ~fs+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$perennialemerg_bulrush =gdistsamp(lambdaformula = ~scale_pe+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$short_bulrush =gdistsamp(lambdaformula = ~scale_short+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$averagewater_bulrush =gdistsamp(lambdaformula = ~scale_averagewater+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$openwater_bulrush =gdistsamp(lambdaformula = ~scale_water+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$int_bulrush =gdistsamp(lambdaformula = ~scale_int+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$region_bulrush =gdistsamp(lambdaformula = ~region+scale_bulrush_, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$global <- gdistsamp(lambdaformula = ~scale_pe+scale_short+scale_water+scale_averagewater+fs+region+scale_int+scale_millet_+scale_annual_smartweed_+scale_bulrush_, 
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
##                             nPars     AIC delta   AICwt cumltvWt
## openwater_annual_smart          7 1861.66  0.00 0.39811     0.40
## openwater                       6 1864.22  2.56 0.11059     0.51
## short_annual_smart              7 1864.72  3.06 0.08600     0.59
## int_annual_smart                7 1864.73  3.07 0.08598     0.68
## millet_annual_smart             7 1865.23  3.57 0.06693     0.75
## fs_annual_smart                 7 1865.40  3.74 0.06137     0.81
## pe_millet                       7 1866.20  4.54 0.04121     0.85
## region_annual_smart             9 1867.01  5.35 0.02748     0.88
## short_millet                    7 1868.92  7.26 0.01053     0.89
## averagewater_millet             7 1869.39  7.73 0.00833     0.90
## fs_millet                       7 1869.46  7.80 0.00805     0.90
## region_fedstate                 9 1870.02  8.36 0.00609     0.91
## openwater_millet                7 1870.03  8.37 0.00607     0.92
## millet_bulrush                  7 1870.07  8.41 0.00594     0.92
## null                            5 1870.09  8.43 0.00589     0.93
## int_millet                      7 1870.11  8.45 0.00581     0.93
## region_millet                   9 1870.19  8.53 0.00558     0.94
## region                          8 1870.55  8.89 0.00467     0.94
## short_bulrush                   7 1871.42  9.76 0.00302     0.95
## perenialemerg                   6 1871.43  9.77 0.00301     0.95
## region_openwater                9 1871.47  9.81 0.00295     0.95
## region_bulrush                  9 1871.66 10.00 0.00268     0.96
## global                         17 1871.71 10.05 0.00262     0.96
## short                           6 1871.97 10.31 0.00230     0.96
## int                             6 1872.01 10.35 0.00225     0.96
## fedstate                        6 1872.09 10.43 0.00217     0.97
## averagewater                    6 1872.09 10.43 0.00217     0.97
## fs_bulrush                      7 1872.15 10.49 0.00210     0.97
## fedstate_perrenialemerg         7 1872.32 10.66 0.00193     0.97
## region_averagewater             9 1872.42 10.76 0.00183     0.97
## int_bulrush                     7 1872.48 10.82 0.00178     0.98
## perennialemerg_short            7 1872.50 10.84 0.00176     0.98
## region_int                      9 1872.50 10.84 0.00176     0.98
## region_perennialemerg           9 1872.52 10.86 0.00175     0.98
## region_short                    9 1872.53 10.87 0.00174     0.98
## openwater_bulrush               7 1872.71 11.05 0.00158     0.98
## averagewater_bulrush            7 1872.71 11.05 0.00158     0.99
## int_perennialemerg              7 1872.75 11.09 0.00156     0.99
## averagewater_perennialemerg     7 1872.95 11.29 0.00141     0.99
## perennialemerg_bulrush          7 1873.03 11.37 0.00135     0.99
## perennialemerg_openwater        7 1873.05 11.39 0.00134     0.99
## averagewater_openwater          7 1873.72 12.06 0.00096     0.99
## fedstate_openwater              7 1873.85 12.19 0.00090     0.99
## short_openwater                 7 1873.86 12.20 0.00089     0.99
## int_openwater                   7 1873.88 12.22 0.00088     0.99
## fedstate_short                  7 1873.89 12.23 0.00088     1.00
## averagewater_short              7 1873.91 12.25 0.00087     1.00
## int_short                       7 1873.94 12.28 0.00086     1.00
## int_fedstate                    7 1873.96 12.30 0.00085     1.00
## averagewater_int                7 1873.98 12.32 0.00084     1.00
## averagewater_fedstate           7 1874.09 12.43 0.00080     1.00
```
