
```r
m <- c( "region","scale_averagewater","scale_short","scale_water","scale_int")

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
#sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
sora <- read.csv('~/Documents/data/2012_sora.csv', header=T)

#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
cov <- read.csv('~/Documents/data/2012_cov.csv', header=T)

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:16]
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
##                                nPars     AIC delta  AICwt cumltvWt
## scale_short+scale_averagewater     7 -957.65  0.00 0.2084     0.21
## scale_averagewater+region          9 -957.29  0.36 0.1740     0.38
## region                             8 -956.62  1.03 0.1244     0.51
## scale_short                        6 -956.06  1.58 0.0945     0.60
## scale_short+region                 9 -955.17  2.47 0.0606     0.66
## scale_int+region                   9 -954.89  2.75 0.0526     0.71
## scale_water+region                 9 -954.78  2.86 0.0498     0.76
## scale_short+scale_int              7 -954.67  2.98 0.0470     0.81
## scale_water+scale_short            7 -954.41  3.24 0.0412     0.85
## scale_int+scale_averagewater       7 -954.10  3.55 0.0353     0.89
## global                            12 -954.06  3.58 0.0347     0.92
## scale_averagewater                 6 -952.92  4.73 0.0196     0.94
## scale_water+scale_averagewater     7 -952.70  4.94 0.0176     0.96
## scale_int                          6 -952.22  5.42 0.0139     0.97
## ~1                                 5 -952.00  5.64 0.0124     0.99
## scale_water                        6 -951.33  6.32 0.0089     0.99
## scale_water+scale_int              7 -950.23  7.42 0.0051     1.00
```
