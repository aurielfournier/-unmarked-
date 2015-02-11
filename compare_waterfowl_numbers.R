setwd("C:/Users/avanderlaar/Documents/SourceTree/data")
dat <- read.csv('2014_waterfowl.csv')
#dat <- dat[dat$region!="ne",]
library(gridExtra)
library(ggplot2)

cb2 <- c("#a6611a","#018571")

a <-ggplot()+
  geom_boxplot(data=dat, aes(region, waterfowl, fill=treat))+
  ylim(0,10000)+
  scale_fill_manual(values=cb2)+
  theme(text=element_text(size=20),
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "right",
        legend.direction= "vertical",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA))

b <- ggplot()+
  geom_boxplot(data=dat, aes(region, dabblers, fill=treat))+
  scale_fill_manual(values=cb2)+
  theme(text=element_text(size=20),
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "right",
        legend.direction= "vertical",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA))


d <- ggplot()+
  geom_boxplot(data=dat, aes(region, mall, fill=treat))+
  ylim(0,10000)+
  scale_fill_manual(values=cb2)+
  theme(text=element_text(size=20),
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "right",
        legend.direction= "vertical",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA))


grid.arrange(a,b,d,ncol=1)

ttest <- lm(waterfowl ~ treat*region, data=dat)
