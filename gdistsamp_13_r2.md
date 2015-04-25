
```r
# predictions from GDistsamp 2013 Round 2
setwd("~/GitHub/data")
```

```
## Error: cannot change working directory
```

```r
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
sora13r2 <- read.csv('2013r2_sora.csv', header=T)
```

```
## Warning: cannot open file '2013r2_sora.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
#read in the covariate data #organized by impoundment.
cov13r2 <- read.csv('2013r2_cov.csv', header=T)
```

```
## Warning: cannot open file '2013r2_cov.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
#subset covaraites we need
cov13r2 <- cov13r2[,c("region","length_2","impound","jdate_2","hectares","area", "int","short","water")]
```

```
## Error: object 'cov13r2' not found
```

```r
# #the distance bins

sora13r2 <- sora13r2[order(sora13r2$impound),]
```

```
## Error: object 'sora13r2' not found
```

```r
cov13r2 <- cov13r2[order(cov13r2$impound),]
```

```
## Error: object 'cov13r2' not found
```

```r
sora13r2 <- sora13r2[,2:40]
```

```
## Error: object 'sora13r2' not found
```

```r
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r2 = unmarkedFrameGDS(y=sora13r2, 
                           numPrimary=3,
                           siteCovs = cov13r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r2$length_2,
)
```

```
## Error: object 'sora13r2' not found
```


```r
model13r2 <- list()
model13r2$null13r2 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$reg13r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$reg_w13r2 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$reg_w_i_13r2 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$short_r13r2 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$short_r_i_13r2 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$short13r2 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$short_w13r2 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$short_w_i_13r2 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
model13r2$global13r2 =gdistsamp(lambdaformula = ~region+water+short+region*water+water*short+region*short+region*water*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r2, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r2' not found
```

```r
list13r2  = fitList(model13r2)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```
## Error: 'names' attribute [1] must be the same length as the vector [0]
```

```r
model13r2 = modSel(list13r2)
```

```
## Error: error in evaluating the argument 'object' in selecting a method for function 'modSel': Error: object 'list13r2' not found
```

```r
model13r2
```

```
## list()
```
