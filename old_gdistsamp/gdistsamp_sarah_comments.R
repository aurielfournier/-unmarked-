# new analysis based on Sarah's Comments

library(unmarked)
library(ggplot2)
library(ggthemes)
library(AICcmodavg)

############
# Reading in the Data
############

sora12 <- read.csv('~/data/2012_sora.csv', header=T)
cov12 <- read.csv('~/data/2012_cov.csv', header=T)
sora13 <- read.csv('~/data/2013_sora.csv', header=T)
cov13 <- read.csv('~/data/2013_cov.csv', header=T)
sora14 <- read.csv('~/data/2014_sora.csv', header=T)
cov14 <- read.csv('~/data/2014_cov.csv', header=T)
cov14$year <- 2014
sora15 <- read.csv('~/data/2015_sora.csv', header=T)
cov15 <- read.csv('~/data/2015_cov.csv', header=T)
cov15$year <- 2015
# the input files should have the rows ordered so they match already, but this makes sure that is true
sora12 <- sora12[order(sora12$impound),]
cov13 <- cov13[order(cov13$impound),]
sora13 <- sora13[order(sora13$impound),]
cov13 <- cov13[order(cov13$impound),]
sora14 <- sora14[order(sora14$impound),]
cov14 <- cov14[order(cov14$impound),]
sora15 <- sora15[order(sora15$impound),]
cov15 <- cov15[order(cov15$impound),]

# trims the sora input file so it only has the columns of distance bins, nothing else
sora12 <- sora12[,2:16]
sora13 <- sora13[,2:31]
sora14 <- sora14[,2:11]
sora15 <- sora15[,2:11]
## Lumps all of the 3 survey data together
sora12<-data.frame(cbind(sora12[,1]+sora12[,6]+sora12[,11],
             sora12[,2]+sora12[,7]+sora12[,12],
             sora12[,3]+sora12[,8]+sora12[,13],
             sora12[,4]+sora12[,9]+sora12[,14],
             sora12[,5]+sora12[,10]+sora12[,15]))


sora13 <- data.frame(cbind(sora13[,1]+sora13[,6]+sora13[,11]+sora13[,16]+sora13[,21]+sora13[,26],
                sora13[,2]+sora13[,7]+sora13[,12]+sora13[,17]+sora13[,22]+sora13[,27],
                sora13[,3]+sora13[,8]+sora13[,13]+sora13[,18]+sora13[,23]+sora13[,28],
                sora13[,4]+sora13[,9]+sora13[,14]+sora13[,19]+sora13[,24]+sora13[,29],
                sora13[,5]+sora13[,10]+sora13[,15]+sora13[,20]+sora13[,25]+sora13[,30]))


sora14 <- data.frame(cbind(sora14[,1]+sora14[,6],
                sora14[,2]+sora14[,7],
                sora14[,3]+sora14[,8],
                sora14[,4]+sora14[,9],
                sora14[,5]+sora14[,10]))

sora15 <- data.frame(cbind(sora15[,1]+sora15[,6],
                sora15[,2]+sora15[,7],
                sora15[,3]+sora15[,8],
                sora15[,4]+sora15[,9],
                sora15[,5]+sora15[,10]))

  
sora <- rbind(sora12, sora13, sora14, sora15)


cov <- rbind(cov12[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region","int","short")],
             cov13[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region","int","short")],
             cov14[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region","int","short")],
             cov15[,c("length","scale_averagewater","averagewater","scale_short","scale_int","year","round","jdate","region","int","short")])

cov$scale_averagewater2 <- cov$scale_averagewater^2

# define's the distances of the bins
cutpt = as.numeric(c(0,1,2,3,4,5)) 

cov$lengthm <- cov$length*1000*3

cov$year <- as.factor(cov$year)

# brings the two files together into the ummarkedFrameGDS
umf = unmarkedFrameGDS(y=sora, 
                       numPrimary=1,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$lengthm
)


##############################
## Best key function ########
#############################

basemodels <- list() 

basemodels$NB.hazard <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

basemodels$P.hazard <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, output="density", keyfun="hazard", mixture="P")

basemodels$P.half.normal <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, starts=as.numeric(coef(basemodels$P.hazard)[1:5]))

basemodels$NB.half.normal <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, output="density", rel.tol=0.001, mixture="NB")

basemodels$NB.exp <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="exp", mixture="NB")

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

detect.models$awater <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~scale_averagewater, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

detect.models$null <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

## Assemble the various model fits into a "fitList" and do model selection
fits.detect <- fitList(fits=detect.models)
(ms.detect <- modSel(fits.detect))

summary(detect.models$water)

################################
### Best model for density #####
################################

# use best model for detection from above

density.modelsP<-list()

density.modelsP$int_year <- gdistsamp(~scale_int+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$int <- gdistsamp(~scale_int, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$short_year <- gdistsamp(~scale_short+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$water_year <- gdistsamp(~scale_averagewater+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$water2_year <- gdistsamp(~scale_averagewater+scale_averagewater2+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$water <- gdistsamp(~scale_averagewater, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$water2 <- gdistsamp(~scale_averagewater+scale_averagewater2, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$global <- gdistsamp(~scale_int+year+scale_averagewater+scale_averagewater2+scale_short, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$null <- gdistsamp(~1, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$short_int <- gdistsamp(~scale_short+scale_int, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

density.modelsP$short_int_year <- gdistsamp(~scale_short+scale_int+year, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="P")

## Assemble the various model fits into a "fitList" and do model selection
fits.density <- fitList(fits=density.modelsP)
(ms.density <- modSel(fits.density))

save(density.modelsP, file="~/manuscripts/Dissertation_Chapter_2_Habitat_Sora/poisson_models.Rdata")

(par <- parboot(density.modelsP$globalP, statistic=fitstats, nsim=3))

summary(density.modelsP$global)


########################################
## Average water depth density plot ####
########################################


scale_ave.water <- data.frame(scale_averagewater=seq(min(cov$scale_averagewater), max(cov$scale_averagewater), length.out=50), scale_int=mean(cov$scale_int), scale_short=mean(cov$scale_short), year=2015, round=2)
water.predict<-predict(fits.density, type="lambda", newdata=scale_ave.water)

water.pre <- cbind(scale_ave.water, water.predict)
water.pre$averagewater<-water.pre$scale_averagewater*sd(cov$averagewater)+mean(cov$averagewater)

(s15 <- ggplot(data=water.pre, aes(x=averagewater, y=Predicted)))+ylab("Sora density (birds/ha)")+xlab("Average water depth (cm)")+
  geom_ribbon(aes(ymin = lower,  ymax = upper), alpha = 0.6) + geom_line( size=1.3) +
  theme_few(base_size = 16)+theme(axis.line = element_line(color = 'black'))+
  theme(plot.background = element_blank()
        ,panel.border = element_blank())+
  guides(fill=FALSE)

save(density.modelsP, file="~/manuscripts/Dissertation_Chapter_1_MO_Phenology/sarah_models.Rdata")
