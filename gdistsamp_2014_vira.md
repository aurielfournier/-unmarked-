
```r
# predictions from GDistsamp 2014
setwd("~/Documents/data")
library(unmarked)
```

```
## Loading required package: methods
## Loading required package: reshape
```

```
## Warning: package 'reshape' was built under R version 3.1.2
```

```
## Loading required package: lattice
## Loading required package: Rcpp
```

```r
#read in the sora observations
sora14 <- read.csv('2014_vira.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14 <- read.csv('2014_cov_vira.csv', header=T)
#subset covaraites we need
cov14 <- cov14[,c("region","length","impound","jdate","area", "int","short","averagewater")]
# #the distance bins

sora14 <- sora14[order(sora14$impound),]
cov14 <- cov14[order(cov14$impound),]

sora14 <- sora14[,2:79]
cutpt = as.numeric(c(0,2,4,6,8,10,14,14)) 
#Unmarked Data Frame
umf14 = unmarkedFrameGDS(y=sora14, 
                           numPrimary=6,
                           siteCovs = cov14,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14$length,
)
```


```r
model14 <- list()
model14$null14 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$reg14 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$reg_w14 =gdistsamp(lambdaformula = ~region+averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$reg_w_i_14 =gdistsamp(lambdaformula = ~region+averagewater+region*averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$short_r14 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$short_r_i_14 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$short14 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$short_w14 =gdistsamp(lambdaformula = ~short+averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$short_w_i_14 =gdistsamp(lambdaformula = ~short+averagewater+short*averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf14, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
model14$global14 =gdistsamp(lambdaformula = ~region+averagewater+short+region*averagewater+region*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: number of items to replace is not a multiple of replacement length
```

```r
list14  = fitList(model14)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```
## Error: 'names' attribute [1] must be the same length as the vector [0]
```

```r
model14 = modSel(list14)
```

```
## Error: error in evaluating the argument 'object' in selecting a method for function 'modSel': Error: object 'list14' not found
```

```r
model14
```

```
## list()
```
