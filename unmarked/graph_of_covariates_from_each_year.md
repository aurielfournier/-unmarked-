# graphs of covariates from each year


```r
library(ggplot2)
```

```
## Error in library(ggplot2): there is no package called 'ggplot2'
```

```r
a12 <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/abundances_2012.csv")
a13 <- read.csv("C:/Users/avanderlaar/Documents/GitHub/data/abundances_2013.csv")
a14 <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/abundances_2014.csv')
load("C:/Users/avanderlaar/Documents/GitHub/unmarked/top_models.RData")
```

# 2012


```r
ggplot()+geom_boxplot(data=a12, aes(x=region, y=mean))+
  theme(axis.text.x = element_text(ang=90,color="black", size=15),
        axis.text.y = element_text(ang=90,size=15,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        panel.background = element_rect(fill = "white"),
        legend.position="none",
        axis.line=element_line(colour="black"))
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

# 2013


```r
grid <- with(a13, expand.grid(
  scale_averagewater = seq(min(scale_averagewater), max(scale_averagewater), length = 20),
  region = levels(factor(region))
))

grid[,3:6]<- predict(r_w_i, newdata=grid, type="lambda")
```

```
## Error in UseMethod("predict"): no applicable method for 'predict' applied to an object of class "unmarkedFitGDS"
```

```r
ggplot()+geom_line(data=grid, aes(x=scale_averagewater, y=Predicted))+facet_wrap(~region)
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
ggplot()+geom_boxplot(data=grid, aes(x=region, y=Predicted))+
  theme(axis.text.x = element_text(ang=90,color="black", size=15),
        axis.text.y = element_text(ang=90,size=15,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        panel.background = element_rect(fill = "white"),
        legend.position="none",
        axis.line=element_line(colour="black"))
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

# 2014


```r
grid <- with(a14, expand.grid(
  scale_averagewater = seq(min(scale_averagewater), max(scale_averagewater), length = 20),
  region = levels(factor(region))
))

grid[,3:6]<- predict(r_w14, newdata=grid, type="lambda")
```

```
## Error in UseMethod("predict"): no applicable method for 'predict' applied to an object of class "unmarkedFitGDS"
```

```r
grid$uci <- grid$Predicted + 1.96 * grid$SE
```

```
## Error in `$<-.data.frame`(`*tmp*`, "uci", value = numeric(0)): replacement has 0 rows, data has 80
```

```r
grid$lci <- grid$Predicted - 1.96 * grid$SE
```

```
## Error in `$<-.data.frame`(`*tmp*`, "lci", value = numeric(0)): replacement has 0 rows, data has 80
```

```r
ggplot(data=grid, aes(x=scale_averagewater, y=Predicted, color=region))+geom_smooth(aes(ymin=lci, ymax=uci),stat="identity")+facet_wrap(~region)+
  theme(axis.text.x = element_text(ang=90,color="black", size=15),
        axis.text.y = element_text(ang=90,size=15,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        panel.background = element_rect(fill = "white"),
        legend.position="none",
        axis.line=element_line(colour="black"))
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
ggplot()+geom_boxplot(data=grid, aes(x=region, y=Predicted))+
  theme(axis.text.x = element_text(ang=90,color="black", size=15),
        axis.text.y = element_text(ang=90,size=15,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        panel.background = element_rect(fill = "white"),
        legend.position="none",
        axis.line=element_line(colour="black"))
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```
