library(unmarked)
library(ggplot2)
library(reshape)

#read in sora data
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")
sora14r1 <- read.csv('2014r1_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r1 <- read.csv('2014r1_cov.csv', header=T)
#subset the covariates we need
cov14r1 <- cov14r1[,c("region","length_1","averagewater_1","impound","treat")]

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


reg14r1 = gdistsamp(lambdaformula = ~1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r1)

#read in the sora observations
sora14r2 <- read.csv('2014r2_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r2 <- read.csv('2014r2_cov.csv', header=T)
#subset covaraites we need
cov14r2 <- cov14r2[,c("region","length_2","averagewater_2","impound","treat")]
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
                    data = umf14r2, keyfun = "hazard", mixture="NB",se = T)



#read in the sora observations
sora14r3 <- read.csv("2014r3_sora.csv", header=T)
#read in the covariate data #organized by impoundment.
cov14r3 <- read.csv('2014r3_cov.csv', header=T)
#subset the covariates
cov14r3 <- cov14r3[,c("region","length_3","averagewater_3","impound","treat")]
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



water_reg14r3 = gdistsamp(lambdaformula = ~region+averagewater_3-1, 
                          phiformula = ~1, 
                          pformula = ~ 1,
                          data = umf14r3, keyfun = "hazard", mixture="NB",se = T)


#sora 
sora14r4 <- read.csv('2014r4_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov14r4 <- read.csv('2014r4_cov.csv', header=T)
#subset the covariates
cov14r4 <- cov14r4[,c("region","length_4","averagewater_4","impound","treat")]
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




reg14r4 = gdistsamp(lambdaformula = ~region-1, 
                    phiformula = ~1, 
                    pformula = ~ 1,
                    data = umf14r4, keyfun = "hazard", mixture="NB",se = T)



df14r1 <- data.frame(region = cov14r1$region,
                     impound = cov14r1$impound)


pred14r1 <- predict(reg14r1, type="lambda", newdata=df14r1, appendData=T)

library(ggplot2)

# ggplot() +
#   geom_boxplot(data=pred14r1, aes(x=region, y=Predicted))+
#   xlab("Interspersion (%)") +
#   ylab("Sora per Hectare") +
#   scale_y_continuous(limits=c(0,100))+
#   #ggtitle("The Impact of Interspersion\n on Sora Density in 2012 Round 2") +
#   #scale_colour_manual(values=cbPalette)+
#   theme(plot.title = element_text(colour="black",size=40), #plot title
#         axis.text.x = element_text(colour="black", size=20), #x axis labels
#         axis.text.y = element_text(colour="black",size=20), #y axis labels
#         axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
#         axis.title.y = element_text(colour="black",size=30), #y axis title
#         legend.text = element_text(colour="black", size=20), #legend text
#         legend.title = element_blank(),#legend title
#         legend.background = element_rect(fill="white"), #legend background color
#         legend.position = "top",
#         legend.direction= "horizontal",
#         legend.key = element_blank(),
#         plot.background = element_rect(fill = "white" ), #plot background color
#         panel.background = element_rect(fill = "white"), #panel background color
#         panel.grid.major.y= element_line(colour="black"), #y axis grid line color
#         panel.grid.major.x = element_line(colour=NA),
#         panel.grid.minor = element_line(colour=NA),
#         plot.margin = unit(c(3,3,3,3), "line"))