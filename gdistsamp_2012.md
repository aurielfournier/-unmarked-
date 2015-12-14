
```r
models <- c("~1","~region","~scale_averagewater","~scale_averagewater+scale_averagewater2","~scale_short","~scale_short+scale_short2","~scale_int","~scale_int+scale_int2","~region+scale_averagewater-1","~region+scale_short","~region+scale_int","~scale_averagewater+scale_short","~scale_averagewater+scale_averagewater2+scale_short","~scale_averagewater+scale_short+scale_short2","~scale_averagewater+scale_int","~scale_averagewater+scale_averagewater2+scale_int","~scale_averagewater+scale_int+scale_int2","~scale_short+scale_int","~scale_short+scale_short2+scale_int","~scale_short+scale_int+scale_int2","~region+scale_averagewater+scale_short+scale_int+scale_scale2+scale_int2+scale_averagewater2-1")


library(unmarked)
```

```
## Loading required package: methods
## Loading required package: reshape
## Loading required package: lattice
## Loading required package: Rcpp
```

```r
sora <- read.csv('~/data/2012_sora.csv', header=T)
```

```
## Warning in file(file, "rt"): cannot open file 'c:/Users/avanderlaar/data/
## 2012_sora.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
cov <- read.csv('~/data/2012_cov.csv', header=T)
```

```
## Warning in file(file, "rt"): cannot open file 'c:/Users/avanderlaar/data/
## 2012_cov.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
sora <- sora[order(sora$impound),]
```

```
## Error in eval(expr, envir, enclos): object 'sora' not found
```

```r
cov <- cov[order(cov$impound),]
```

```
## Error in cov$impound: object of type 'closure' is not subsettable
```

```r
cov$scale_short2 <- cov$scale_short^2
```

```
## Error in cov$scale_short: object of type 'closure' is not subsettable
```

```r
cov$scale_int2 <- cov$scale_int^2
```

```
## Error in cov$scale_int: object of type 'closure' is not subsettable
```

```r
cov$scale_averagewater2 <- cov$scale_averagewater^2
```

```
## Error in cov$scale_averagewater: object of type 'closure' is not subsettable
```

```r
sora <- sora[,2:16]
```

```
## Error in eval(expr, envir, enclos): object 'sora' not found
```

```r
cutpt = as.numeric(c(0,1,2,3,4,5)) 

umf = unmarkedFrameGDS(y=sora, 
                       numPrimary=3,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length
)
```

```
## Error in ncol(y): object 'sora' not found
```


```r
model <- list()



for(i in 1:length(models)){
  print(i)
model[[models[[i]]]] = gdistsamp(lambdaformula = as.formula(models[i]), 
                       phiformula = ~1, 
                       pformula = ~1,
                       data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
}
```

```
## [1] 1
```

```
## Error in is(data, "unmarkedFrameGDS"): object 'umf' not found
```

```r
model$global <- gdistsamp(lambdaformula = ~scale_short+scale_water+scale_averagewater+region+scale_int, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error in is(data, "unmarkedFrameGDS"): object 'umf' not found
```

```r
save(model, file="2012_models.Rdata")
list  = fitList(model)
```

```
## Warning in fitList(model): If supplying a list of fits, use fits = 'mylist'
```

```
## Error in names(fits) <- as.character(c[[2]]): 'names' attribute [1] must be the same length as the vector [0]
```

```r
model = modSel(list)
```

```
## Error in (function (classes, fdef, mtable) : unable to find an inherited method for function 'modSel' for signature '"function"'
```

```r
model
```

```
## list()
```
