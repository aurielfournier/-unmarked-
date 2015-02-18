library(ggplot2)
dat = data.frame(x=1,y=1:25)

dat$distance = c(0,0,1,1,2,2,3,3,4,4,4,4,5,5,7,7,8,8,11,11,12,12,16,16,16)
dat$bin = c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,4,4,4)
ggplot() + geom_histogram(data=dat, aes(x=distance), stat="bin", binwidth=5,colour="black", fill="#FFCC00")

ggplot(dat, aes(x=distance)) + 
  geom_histogram(stat="bin",binwidth=5,colour="black", fill="#FFCC00")+
  xlab("Distance From Transect Line") +
  scale_x_continuous(limit=c(0,20))+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=30), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA))+ #x axis grid line color
  guides(fill=guide_legend(title="Habitat Type")) #retitles the legend
