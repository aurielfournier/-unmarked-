
dat <- read.csv("2014_waterfowl_tidy_hec.csv")
#dat <- dat[dat$region!="ne",]
library(gridExtra)
library(ggplot2)
dat <- dat[dat$variable=="waterfowl",]
cb2 <- c("#018571","#a6611a")

a <-ggplot()+
  geom_boxplot(data=dat, aes(region, birds_ha, fill=treat))+
  ylim(0,200)+
  scale_fill_manual(values=cb2)+
  ylab("Ducks per hectare")+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=40), #x axis labels
        axis.text.y = element_text(colour="black",size=40), #y axis labels
        axis.title.x = element_blank(), #x axis title
        axis.title.y = element_text(colour="black",size=40), #y axis title
        legend.text = element_text(colour="black", size=40), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_rect(colour="black", fill="white"),
        legend.key.width=unit(3,"cm"),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        panel.background = element_rect(fill = "white"),
        axis.line=element_line(colour="black"))


ttest <- lm(birds_ha ~ treat, data=dat)
ttest1 <- lm(birds_ha ~ treat+region, data=dat)
