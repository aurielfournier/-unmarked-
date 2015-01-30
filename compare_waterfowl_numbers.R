setwd("C:/Users/avanderlaar/Documents/SourceTree/data")
dat <- read.csv('2014_waterfowl.csv')
library(gridExtra)

cb2 <- c("#a6611a","#018571")

a <-ggplot()+
  geom_boxplot(data=dat, aes(region, waterfowl, fill=treat))+
  ylim(0,10000)+
  scale_fill_manual(values=cb2)

b <- ggplot()+
  geom_boxplot(data=dat, aes(region, dabblers, fill=treat))+
  scale_fill_manual(values=cb2)

d <- ggplot()+
  geom_boxplot(data=dat, aes(region, mall, fill=treat))+
  ylim(0,10000)+
  scale_fill_manual(values=cb2)

grid.arrange(a,b,d,ncol=1)

ttest <- lm(waterfowl ~ treat*region, data=dat)
