
```r
m <- c("region","scale_averagewater","scale_pe","scale_short","scale_int")

tab <- table(m,m)

for(i in 1:length(m)){
  tab[(1:which(tab[,i]==1)),i] <- 1
}

models <- list()
models[[paste0("~",1)]] <- as.formula(paste0("~",1))

for(i in 1:length(m)){
  models[[paste0(m[i])]] <- as.formula(paste0("~",m[i]))
}

index <- which(tab==0, arr.ind=TRUE)

for(i in 1 : nrow(index)){
  models[[paste0(rownames(tab)[index[i,1]],"+",colnames(tab)[index[i,2]])]] <- as.formula(paste0("~",rownames(tab)[index[i,1]],"+",colnames(tab)[index[i,2]]))
  }


library(unmarked)
```

```
## Loading required package: methods
## Loading required package: reshape
## Loading required package: lattice
## Loading required package: Rcpp
```

```r
#sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_sora.csv', header=T)
sora <- read.csv('~/Documents/data/2013_sora.csv', header=T)

#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_cov.csv', header=T)
cov <- read.csv('~/Documents/data/2013_cov.csv', header=T)

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:31]
cutpt = as.numeric(c(0,1,2,3,4,5)) 

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

modelsnames <- names(models)

for(i in 1:length(models)){
model[[modelsnames[[i]]]] = gdistsamp(lambdaformula = models[[i]], 
                       phiformula = ~1, 
                       pformula = ~1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
}
```


```r
model$global <- gdistsamp(lambdaformula = ~scale_short+scale_averagewater+region+scale_int+scale_pe, 
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
##                                nPars    AIC delta  AICwt cumltvWt
## scale_int+scale_averagewater       7 529.12 0.000 0.2435     0.24
## scale_int                          6 529.17 0.051 0.2374     0.48
## scale_pe+scale_int                 7 529.77 0.653 0.1757     0.66
## scale_short+scale_int              7 531.16 2.047 0.0875     0.74
## scale_int+region                   9 532.32 3.207 0.0490     0.79
## scale_averagewater                 6 532.88 3.763 0.0371     0.83
## scale_averagewater+region          9 533.07 3.950 0.0338     0.86
## global                            12 533.55 4.429 0.0266     0.89
## region                             8 533.82 4.702 0.0232     0.91
## ~1                                 5 534.53 5.413 0.0163     0.93
## scale_pe+region                    9 534.59 5.471 0.0158     0.95
## scale_short+scale_averagewater     7 534.72 5.607 0.0148     0.96
## scale_pe+scale_averagewater        7 534.88 5.762 0.0137     0.97
## scale_short+region                 9 535.21 6.097 0.0115     0.99
## scale_short                        6 536.50 7.388 0.0061     0.99
## scale_pe                           6 536.50 7.388 0.0061     1.00
## scale_short+scale_pe               7 538.47 9.356 0.0023     1.00
```
