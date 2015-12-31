library(unmarked)
library(ggplot2)
library(ggthemes)
library(AICcmodavg)

############
# Reading in the Data
############

sora <- read.csv('~/data/2012_sora.csv', header=T)
cov <- read.csv('~/data/2012_cov.csv', header=T)

# the input files should have the rows ordered so they match already, but this makes sure that is true
sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

# trims the sora input file so it only has the columns of distance bins, nothing else
sora <- sora[,2:16]
## Lumps all of the 3 survey data together
sora2<-cbind(sora[,1]+sora[,6]+sora[,11],
             sora[,2]+sora[,7]+sora[,12],
             sora[,3]+sora[,8]+sora[,13],
             sora[,4]+sora[,9]+sora[,14],
             sora[,5]+sora[,10]+sora[,15])

# define's the distances of the bins
cutpt = as.numeric(c(0,1,2,3,4,5)) 

# brings the two files together into the ummarkedFrameGDS
umf = unmarkedFrameGDS(y=sora2, 
                       numPrimary=1,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length*1000*3
)


##############################
## Best key function ########
#############################

basemodels <- list() 

basemodels$NB.hazard <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

basemodels$P.hazard <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, output="density", keyfun="hazard", mixture="P")

basemodels$P.half.normal <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, starts=as.numeric(coef(basemodels$P.hazard)[1:5]))

basemodels$NB.half.normal <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, output="density", rel.tol=0.001, mixture="NB")

basemodels$NB.exp <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, output="density", rel.tol=0.001, keyfun="exp", mixture="NB")

basemodels$P.exp <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, output="density",  keyfun="exp", mixture="P")

## Assemble the various model fits into a "fitList" and do model selection
fits.basemodels <- fitList(fits=basemodels)
(ms.basemodels <- modSel(fits.basemodels))
 
summary(basemodels$NB.hazard)

## stuff needed for the table
(export.basemodels <- cbind(coef(ms.basemodels), ms.basemodels@Full$AIC)) # Coefs and AIC

###############################
### Best model for detection ##
###############################

## Best model for detection, using best supported key function from above ####
## these are just a few for the sake of example, you can probably think of more 
## biologically sensible models

detect.models<-list()

detect.models$wood <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_wood, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

detect.models$tall <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_tall, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

detect.models$water <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

detect.models$null <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

## Assemble the various model fits into a "fitList" and do model selection
fits.detect <- fitList(fits=detect.models)
(ms.detect <- modSel(fits.detect))

summary(detect.models$water)

################################
### Best model for density #####
################################

# use best model for detection from above

density.models<-list()

density.models$water_water2 <- gdistsamp(~scale_averagewater+I(scale_averagewater^2), ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

density.models$tall <- gdistsamp(~scale_tall, ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

density.models$round <- gdistsamp(~round, ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

density.models$short <- gdistsamp(~scale_short, ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

density.models$int <- gdistsamp(~scale_int, ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

density.models$null <- gdistsamp(~1, ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

density.models$water_water2_round <- gdistsamp(~round+scale_averagewater+I(scale_averagewater^2), ~1, ~scale_water, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

## Assemble the various model fits into a "fitList" and do model selection
fits.density <- fitList(fits=density.models)
(ms.density <- modSel(fits.density))

summary(density.models$water_water2_round)


########################################
## Average water depth density plot ####
########################################

scale_ave.water <- data.frame(scale_short=mean(cov$scale_short),scale_averagewater=seq(min(cov$scale_averagewater), max(cov$scale_averagewater), length.out=50), 
                            scale_int=mean(cov$scale_int), scale_tall=mean(cov$scale_tall), round=2)
water.predict<-predict(fits.density, type="lambda", newdata=scale_ave.water)

water.pre <- cbind(scale_ave.water, water.predict)
water.pre$averagewater<-water.pre$scale_averagewater*sd(cov$averagewater)+mean(cov$averagewater)

(s12 <- ggplot(data=water.pre, aes(x=averagewater, y=Predicted)))+ylab("Sora density (birds/ha)")+xlab("Average water depth (cm)")+
  geom_ribbon(aes(ymin = lower,  ymax = upper), alpha = 0.6) + geom_line( size=1.3) +
  theme_few(base_size = 16)+
  ggtitle("2012")  +theme(axis.line = element_line(color = 'black'))+
  theme(plot.background = element_blank()
        ,panel.border = element_blank())+
  guides(fill=FALSE)

########################################
## Effect of water on detection plot ####
########################################

scale_water <- data.frame(water=c(min(cov$water), max(cov$water)),
                            scale_water=c(min(cov$scale_water), max(cov$scale_water)))
water.predict<-predict(fits.density, type="det", newdata=scale_water)

water.pre <- cbind(scale_water, water.predict)

#plot the function
pred.l<-gxhn(seq(0, 5, by=0.1), water.pre$Predicted[1])
pred.h<-gxhn(seq(0, 5, by=0.1),  water.pre$Predicted[2])
pred.min.l<-gxhn(seq(0, 5, by=0.1), water.pre$lower[1])
pred.min.h<-gxhn(seq(0, 5, by=0.1), water.pre$upper[1])
pred.max.l<-gxhn(seq(0, 5, by=0.1), water.pre$lower[2])
pred.max.h<-gxhn(seq(0, 5, by=0.1), water.pre$upper[2])

water<-data.frame(pred=c(pred.l, pred.h),  low=c(pred.min.l, pred.max.l), x=rep(seq(0, 5, by=0.1), 2),
                   high=c(pred.min.h,pred.max.h), level=c(rep("Min water", 51),rep("Max water", 51) ))


ggplot(water, aes(x=x, y = pred, fill=level, colour=level)) + 
  geom_ribbon(aes(ymin = low,  ymax = high, fill=level), alpha = 0.6, color=NA) +
  geom_line(aes(colour=level), size=1.3) +
  scale_colour_manual(values = c("orange", "darkorange3")) +
  scale_fill_manual(values = c("orange", "darkorange3"))+
  xlab(expression("Distance from observer (m)"))+
  ylab(expression(paste("Detection probability")))+
  theme_few(base_size = 12)+
  scale_y_continuous(breaks=seq(0, 1.16, by=0.2), limits=c(0, 1.16))+
  theme(legend.position=c(0.7, 0.9), legend.title=element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(plot.background = element_blank()
        ,panel.border = element_blank())+
  guides(fill=FALSE)

