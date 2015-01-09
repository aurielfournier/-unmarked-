# smooth spline of abundance estimates for 2014


a14r1 <- read.csv("abundance_14r1.csv")
a14r1 <- a14r1[,c("mean","jdate")]
a14r2 <- read.csv("abundance_14r2.csv")
a14r2 <- a14r2[,c("mean","jdate")]
a14r3 <- read.csv("abundance_14r3.csv")
a14r3 <- a14r3[,c("mean","jdate")]
a14r4 <- read.csv("abundance_14r4.csv")
a14r4 <- a14r4[,c("mean","jdate")]

c14r1 <- melt(a14r1, id=c("jdate"))
c14r1 <- cast(jdate ~ variable, data=c14r1, sum, fill=NA_real_)

c14r2 <- melt(a14r2, id=c("jdate"))
c14r2 <- cast(jdate ~ variable, data=c14r2, sum, fill=NA_real_)

c14r3 <- melt(a14r3, id=c("jdate"))
c14r3 <- cast(jdate ~ variable, data=c14r3, sum, fill=NA_real_)

c14r4 <- melt(a14r4, id=c("jdate"))
c14r4 <- cast(jdate ~ variable, data=c14r4, sum, fill=NA_real_)

c14r12 <- rbind(c14r1, c14r2)
c14r34 <- rbind(c14r3, c14r4)
c14 <- rbind(c14r12, c14r34)
spline14 = smooth.spline(c14$jdate, c14$mean, spar=.7)


smoothdf14 = data.frame(x=spline14$x, y=spline14$y)

xaxis <- data.frame(jdate=c(min(smoothdf14$x):max(smoothdf14$x)), value=rep(0))


ggplot() + 
  geom_bar(data=xaxis, aes(x=jdate, y=value), position=position_dodge(), stat="identity", colour="black",size=.5) +    
  geom_line(data=smoothdf14, aes(x=x, y=y), size=2)+
  xlab("Date") +
  ylab("Sora per hour") +
  ggtitle("Sora") +
  scale_fill_manual(values=c("2012"="#80cdc1", "2013"="#dfc27d", "2014"="#018571"))+
  scale_colour_manual(values=c("2012"="#80cdc1", "2013"="#dfc27d", "2014"="#018571"))+
  theme(plot.title = element_text(colour="black",size=20), #plot title
        axis.text.x = element_text(ang=90, colour="black", size=10), #x axis labels
        axis.text.y = element_text(colour="black",size=10), #y axis labels
        axis.title.x = element_blank(), #x axis title
        axis.title.y = element_text(colour="black",size=15), #y axis title
        legend.text = element_text(colour="black", size=15), #legend text
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


