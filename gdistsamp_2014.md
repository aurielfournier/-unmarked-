
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
#sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
sora <- read.csv('~/Documents/data/2014_sora.csv', header=T)

#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
cov <- read.csv('~/Documents/data/2014_cov.csv', header=T)

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
##                                nPars     AIC delta   AICwt cumltvWt
## scale_averagewater                 6 1152.49  0.00 0.26253     0.26
## scale_int+scale_averagewater       7 1152.67  0.18 0.23942     0.50
## scale_pe+scale_averagewater        7 1152.89  0.40 0.21463     0.72
## scale_short+scale_averagewater     7 1153.87  1.39 0.13130     0.85
## scale_averagewater+region          9 1153.97  1.48 0.12536     0.97
## global                            12 1157.70  5.21 0.01937     0.99
## ~1                                 5 1161.96  9.47 0.00231     0.99
## scale_short                        6 1163.50 11.01 0.00107     1.00
## scale_pe                           6 1163.78 11.29 0.00093     1.00
## scale_int                          6 1163.80 11.31 0.00092     1.00
## region                             8 1165.33 12.84 0.00043     1.00
## scale_short+scale_pe               7 1165.36 12.88 0.00042     1.00
## scale_short+scale_int              7 1165.40 12.91 0.00041     1.00
## scale_pe+scale_int                 7 1165.53 13.04 0.00039     1.00
## scale_short+region                 9 1166.92 14.43 0.00019     1.00
## scale_int+region                   9 1167.23 14.74 0.00017     1.00
## scale_pe+region                    9 1167.32 14.83 0.00016     1.00
```
