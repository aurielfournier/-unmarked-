library(unmarked)
library(ggplot2)
library(reshape)
library(gridExtra)
#read in sora data
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")
sora14r1 <- read.csv('2014r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r1 <- read.csv('2014r1_cov.csv', header=T)
#subset the covariates we need
cov14r1 <- cov14r1[,c("region","length_1","averagewater_1","impound","treat","jdate_1","hectares")]

sora14r1 <- sora14r1[order(sora14r1$impound),]
cov14r1 <- cov14r1[order(cov14r1$impound),]

sora14r1 <- sora14r1[,2:79]


#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) #the fartherest distance is 12
#Unmarked Data Frame
umf14r1 = unmarkedFrameGDS(y=sora14r1, 
                           numPrimary=6,
                           siteCovs = cov14r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r1$length_1,
)


reg14r1 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r1, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

#read in the sora observations
sora14r2 <- read.csv('2014r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r2 <- read.csv('2014r2_cov.csv', header=T)
#subset covaraites we need
cov14r2 <- cov14r2[,c("region","length_2","averagewater_2","impound","treat","jdate_2","hectares")]
# #the distance bins

sora14r2 <- sora14r2[order(sora14r2$impound),]
cov14r2 <- cov14r2[order(cov14r2$impound),]

sora14r2 <- sora14r2[,2:79]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r2 = unmarkedFrameGDS(y=sora14r2, 
                           numPrimary=6,
                           siteCovs = cov14r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r2$length_2,
)


reg14r2 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r2, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")



#read in the sora observations
sora14r3 <- read.csv("2014r3_sora.csv", header=T)
#read in the covariate data #organized by impoundment.
cov14r3 <- read.csv('2014r3_cov.csv', header=T)
#subset the covariates
cov14r3 <- cov14r3[,c("region","length_3","averagewater_3","impound","treat","jdate_3","hectares")]
# #the distance bins

sora14r3 <- sora14r3[order(sora14r3$impound),]
cov14r3 <- cov14r3[order(cov14r3$impound),]
sora14r3 <- sora14r3[,2:79]

cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r3 = unmarkedFrameGDS(y=sora14r3, 
                           numPrimary=6,
                           siteCovs = cov14r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r3$length_3,
)



treat14r3 = gdistsamp(lambdaformula = ~treat-1, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf14r3, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")


#sora 
sora14r4 <- read.csv('2014r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r4 <- read.csv('2014r4_cov.csv', header=T)
#subset the covariates
cov14r4 <- cov14r4[,c("region","length_4","averagewater_4","impound","treat","jdate_4","hectares")]
# the distance bins

sora14r4 <- sora14r4[order(sora14r4$impound),]
cov14r4 <- cov14r4[order(cov14r4$impound),]
sora14r4 <- sora14r4[,2:79]

cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) 
#Unmarked Data Frame
umf14r4 = unmarkedFrameGDS(y=sora14r4, 
                           numPrimary=6,
                           siteCovs = cov14r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov14r4$length_4,
)


treat14r4 = gdistsamp(lambdaformula = ~treat-1, 
                          phiformula = ~1, 
                          pformula = ~ 1,
                          data = umf14r4, keyfun = "hazard", mixture="NB",se = T, output="density",unitsOut="ha")

setwd("C:/Users/avanderlaar/Dropbox/data")

options(scipen=999) #disables scientific notation

ab14r1 <- ranef(reg14r1)
abund14r1 <- data.frame(matrix(ncol=4, nrow=33))
abund14r1$X1 <- bup(ab14r1, stat="mean")
abund14r1$X2 <- bup(ab14r1, stat="mode")
abund14r1[,3:4] <- confint(ab14r1, level=0.9) # 90% CI
abund14r1$impound <- cov14r1$impound
abund14r1$jdate <- cov14r1$jdate_1
abund14r1$region <- cov14r1$region
abund14r1$treat <- cov14r1$treat
abund14r1$hectares <- cov14r1$hectares
colnames(abund14r1) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares")
write.csv(abund14r1, "abundance_14r1.csv")
abund14r1 

ab14r2 <- ranef(reg14r2)
abund14r2 <- data.frame(matrix(ncol=4, nrow=33))
abund14r2$X1 <- bup(ab14r2, stat="mean")
abund14r2$X2 <- bup(ab14r2, stat="mode")
abund14r2[,3:4] <- confint(ab14r2, level=0.9) # 90% CI
abund14r2$impound <- cov14r2$impound
abund14r2$jdate <- cov14r2$jdate_2
abund14r2$region <- cov14r2$region
abund14r2$treat <- cov14r2$treat
abund14r2$hectares <- cov14r2$hectares
colnames(abund14r2) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares")
write.csv(abund14r2, "abundance_14r2.csv")
abund14r2

ab14r3 <- ranef(treat14r3)
abund14r3 <- data.frame(matrix(ncol=4, nrow=32))
abund14r3$X1 <- bup(ab14r3, stat="mean")
abund14r3$X2 <- bup(ab14r3, stat="mode")
abund14r3[,3:4] <- confint(ab14r3, level=0.9) # 90% CI
abund14r3$impound <- cov14r3$impound
abund14r3$jdate <- cov14r3$jdate_3
abund14r3$region <- cov14r3$region
abund14r3$treat <- cov14r3$treat
abund14r3$hectares <- cov14r3$hectares
colnames(abund14r3) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares")
write.csv(abund14r3, "abundance_14r3.csv")
abund14r3

ab14r4 <- ranef(treat14r4)
abund14r4 <- data.frame(matrix(ncol=4, nrow=32))
abund14r4$X1 <- bup(ab14r4, stat="mean")
abund14r4$X2 <- bup(ab14r4, stat="mode")
abund14r4[,3:4] <- confint(ab14r4, level=0.9) # 90% CI
abund14r4$impound <- cov14r4$impound
abund14r4$jdate <- cov14r4$jdate_4
abund14r4$region <- cov14r4$region
abund14r4$treat <- cov14r4$treat
abund14r4$hectares <- cov14r4$hectares
colnames(abund14r4) <- c("mean","mode","CI1","CI2","impound","jdate","region","treat","hectares")
write.csv(abund14r4, "abundance_14r4.csv")
abund14r4



#predictions
df14r1 <- data.frame(region = cov14r1$region,
                     impound = cov14r1$impound)

df14r2 <- data.frame(region = cov14r2$region,
                     impound = cov14r2$impound)


df14r3 <- data.frame(treat = cov14r3$treat,
                     impound = cov14r3$impound)


df14r4 <- data.frame(treat = cov14r4$treat,
                     impound = cov14r4$impound)


pred14r1 <- predict(reg14r1, type="lambda", newdata=df14r1, appendData=T)
mpred14r1 <- melt(pred14r1)

pred14r2 <- predict(reg14r2, type="lambda", newdata=df14r2, appendData=T)
mpred14r2 <- melt(pred14r2)

pred14r3 <- predict(treat14r3, type="lambda", newdata=df14r3, appendData=T)
mpred14r3 <- melt(pred14r3)

pred14r4 <- predict(treat14r4, type="lambda", newdata=df14r4, appendData=T)
mpred14r4 <- melt(pred14r4)

r1 <- ggplot() +
  geom_boxplot(data=mpred14r1, aes(x=region, y=value))+
  #xlab("Interspersion (%)") +
  ylab("Sora per Hectare") +
  scale_y_continuous(limits=c(0,100))+
  #ggtitle("The Impact of Interspersion\n on Sora Density in 2012 Round 2") +
  #scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))

r2 <- ggplot() +
  geom_boxplot(data=mpred14r2, aes(x=region, y=value))+
  #xlab("Interspersion (%)") +
  ylab("Sora per Hectare") +
  scale_y_continuous(limits=c(0,100))+
  #ggtitle("The Impact of Interspersion\n on Sora Density in 2012 Round 2") +
  #scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))

r3 <- ggplot() +
  geom_boxplot(data=mpred14r3, aes(x=treat, y=value))+
  #xlab("Interspersion (%)") +
  ylab("Sora per Hectare") +
  scale_y_continuous(limits=c(0,100))+
  #ggtitle("The Impact of Interspersion\n on Sora Density in 2012 Round 2") +
  #scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))



r4 <- ggplot() +
  geom_boxplot(data=mpred14r4, aes(x=treat, y=value))+
  #xlab("Interspersion (%)") +
  ylab("Sora per Hectare") +
  scale_y_continuous(limits=c(0,100))+
  #ggtitle("The Impact of Interspersion\n on Sora Density in 2012 Round 2") +
  #scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))

grid.arrange(r1, r2,r3,r4,ncol=2)
