
```r
m <- c("region","scale_averagewater","scale_pe","scale_short")

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




# predictions from GDistsamp 2015

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
sora <- read.csv('~/Documents/data/2015_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
cov <- read.csv('~/Documents/data/2015_cov.csv', header=T)

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

covariates being used

scale_short+scale_water+scale_averagewater+region+scale_int+scale_pe


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
model$global <- gdistsamp(lambdaformula = ~scale_short+scale_averagewater+region+scale_pe, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
save(model, file="2015_models.Rdata")
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
##                                nPars    AIC delta AICwt cumltvWt
## ~1                                 5 161.17 0.000 0.202     0.20
## scale_short                        6 161.25 0.083 0.194     0.40
## region                             6 162.77 1.604 0.091     0.49
## scale_averagewater                 6 162.80 1.631 0.089     0.58
## scale_short+scale_averagewater     7 163.10 1.932 0.077     0.65
## scale_short+region                 7 163.14 1.969 0.076     0.73
## scale_pe                           6 163.16 1.994 0.075     0.80
## scale_short+scale_pe               7 163.22 2.053 0.072     0.88
## scale_pe+region                    7 164.42 3.249 0.040     0.92
## scale_pe+scale_averagewater        7 164.63 3.459 0.036     0.95
## scale_averagewater+region          7 164.69 3.521 0.035     0.99
## global                             9 166.53 5.358 0.014     1.00
```
