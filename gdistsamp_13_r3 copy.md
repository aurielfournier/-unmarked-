
```r
# predictions from GDistsamp 2013 Round 3
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
sora13r3 <- read.csv('2013r3_sora.csv', header=T)
```

```
## Warning: cannot open file '2013r3_sora.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
#read in the covariate data #organized by impoundment.
cov13r3 <- read.csv('2013r3_cov.csv', header=T)
```

```
## Warning: cannot open file '2013r3_cov.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
#subset covaraites we need
cov13r3 <- cov13r3[,c("region","length_3","impound","jdate_3","hectares","area", "int","short","water")]
```

```
## Error: object 'cov13r3' not found
```

```r
# #the distance bins

sora13r3 <- sora13r3[order(sora13r3$impound),]
```

```
## Error: object 'sora13r3' not found
```

```r
cov13r3 <- cov13r3[order(cov13r3$impound),]
```

```
## Error: object 'cov13r3' not found
```

```r
sora13r3 <- sora13r3[,2:40]
```

```
## Error: object 'sora13r3' not found
```

```r
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf13r3 = unmarkedFrameGDS(y=sora13r3, 
                           numPrimary=3,
                           siteCovs = cov13r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r3$length_3,
)
```

```
## Error: object 'sora13r3' not found
```


```r
model13r3 <- list()
model13r3$null13r3 = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$reg13r3 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$reg_w13r3 =gdistsamp(lambdaformula = ~region+water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$reg_w_i_13r3 =gdistsamp(lambdaformula = ~region+water+region*water-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$short_r13r3 =gdistsamp(lambdaformula = ~short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$short_r_i_13r3 =gdistsamp(lambdaformula = ~short+region+short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$short13r3 =gdistsamp(lambdaformula = ~short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$short_w13r3 =gdistsamp(lambdaformula = ~short+water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$short_w_i_13r3 =gdistsamp(lambdaformula = ~short+water+short*water-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
model13r3$global13r3 =gdistsamp(lambdaformula = ~region+water+short+region*water+water*short+region*short+region*water*short-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf13r3, keyfun = "hazard", mixture="P",se = T, output="abund")
```

```
## Error: object 'umf13r3' not found
```

```r
list13r3  = fitList(model13r3)
```

```
## Warning: If supplying a list of fits, use fits = 'mylist'
```

```
## Error: 'names' attribute [1] must be the same length as the vector [0]
```

```r
model13r3 = modSel(list13r3)
```

```
## Error: error in evaluating the argument 'object' in selecting a method for function 'modSel': Error: object 'list13r3' not found
```

```r
model13r3
```

```
## list()
```
