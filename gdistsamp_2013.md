
```r
# predictions from GDistsamp 2013
library(unmarked)
```

```r
#read in the sora observations
sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2013_cov.csv', header=T)
#subset covaraites we need
#subset covaraites we need
#cov <- cov[,c("region","length","impound","jdate","area", "int","short","water")]
# #the distance bins

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,3:80]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                           numPrimary=6,
                           siteCovs = cov,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov$length,
)
```


```r
model <- list()
model$null = gdistsamp(lambdaformula = ~1, 
                     phiformula = ~1, 
                     pformula = ~1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$r = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$r_w =gdistsamp(lambdaformula = ~region+scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$r_w_i =gdistsamp(lambdaformula = ~region+water+region*scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$w =gdistsamp(lambdaformula = ~scale_averagewater-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$s_r =gdistsamp(lambdaformula = ~scale_short+region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
model$s_r_i =gdistsamp(lambdaformula = ~scale_short+region+scale_short*region-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```


```r
model$s =gdistsamp(lambdaformula = ~scale_short-1, 
                     phiformula = ~1, 
                     pformula = ~ 1,
                     data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$s_w =gdistsamp(lambdaformula = ~scale_short+scale_averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

model$s_w_i =gdistsamp(lambdaformula = ~scale_short+scale_averagewater+scale_short*scale_averagewater-1, 
                       phiformula = ~1, 
                       pformula = ~ 1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
model$global =gdistsamp(lambdaformula = ~region+scale_averagewater+scale_short+region*scale_averagewater+region*scale_short+scale_short*scale_averagewater-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
```

```r
list  = fitList(model)
```



```r
model = modSel(list)
```


```r
model
```

```
##        nPars     AIC  delta   AICwt cumltvWt
## r_w_i     13 4302.88   0.00 9.4e-01     0.94
## global    17 4309.52   6.64 3.4e-02     0.98
## r_w        9 4310.54   7.66 2.0e-02     1.00
## r          8 4315.10  12.23 2.1e-03     1.00
## s_r        9 4316.49  13.61 1.0e-03     1.00
## s_r_i     12 4319.64  16.76 2.2e-04     1.00
## null       5 4321.37  18.50 9.1e-05     1.00
## s_w_i      7 4634.36 331.48 9.9e-73     1.00
## w          5 4634.62 331.75 8.6e-73     1.00
## s_w        6 4636.59 333.71 3.2e-73     1.00
## s          5 4640.25 337.37 5.2e-74     1.00
```
