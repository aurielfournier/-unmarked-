# new analysis based on Sarah's Comments

library(unmarked)
library(ggplot2)
library(ggthemes)
library(AICcmodavg)
library(tidyr)
library(raildata)
############
# Reading in the Data
############

#data(soraDONE)
sora <- read.csv("~/data/sora_DONE.csv")
cov <- read.csv("~/data/veg_DONE.csv")

sora <- sora[order(sora$iry),]
cov <- cov[order(cov$iry),]

cov <- cov[!is.na(cov$Exp),]

sora <- sora[sora$iry %in% cov$iry,]

nrow(sora) == nrow(cov)

cutpt = as.numeric(c(0,1,2,3,4,5)) 

cov$year <- as.factor(cov$year)

cov$scale_averagewater2 <- cov$scale_averagewater^2

# brings the two files together into the ummarkedFrameGDS
umf = unmarkedFrameGDS(y=sora[,1:5], 
                       numPrimary=1,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length
)


##############################
## Best key function ########
#############################

basemodels <- list() 

basemodels$NB.hazard <- gdistsamp(~1, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

basemodels$P.hazard <- gdistsamp(~1, ~1, ~1, umf, output="density", keyfun="hazard", mixture="P")

basemodels$P.half.normal <- gdistsamp(~1, ~1, ~1, umf, starts=as.numeric(coef(basemodels$P.hazard)[1:5]))

basemodels$NB.half.normal <- gdistsamp(~1, ~1, ~1, umf, output="density", rel.tol=0.001, mixture="NB")

basemodels$NB.exp <- gdistsamp(~1, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="exp", mixture="NB")

basemodels$P.exp <- gdistsamp(~1, ~1, ~1, umf, output="density",  keyfun="exp", mixture="P")

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

detect.models$awater <- gdistsamp(~1, ~1, ~Exp, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

detect.models$null <- gdistsamp(~1, ~1, ~1, umf, output="density", rel.tol=0.001, keyfun="hazard", mixture="NB")

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

save(density.modelsP, file="./data/poisson_models.Rdata")

(par <- parboot(density.modelsP$global,  nsim=3))

summary(density.modelsP$global)

