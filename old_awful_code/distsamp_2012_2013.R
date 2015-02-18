#this correctly runs the 2012 and 2013 data with distsamp
#or atleast it did

setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)

sora <- as.matrix(read.csv('Dist_Sora_12_13.csv')) #this is a file with each transect as a seperate row and detections binned into five groups

covs <- read.csv('Dist_Cov_12_13_old.csv', header=TRUE) #covariates at the transect level

cutpt = c(0,3,6,9,12,15) #this is the breakdown of distance bins in meters, the actual farthest distance is 12

umf = unmarkedFrameDS(y=sora, 
                      survey="line", 
                      dist.breaks=cutpt, 
                      siteCovs=covs, 
                      unitsIn="m", 
                      tlength=covs$Effort_M) 

summary(umf)
#should have 338 sites 
# five distance classes
#185 sites with atleast one detection

null = distsamp(~1~1, umf) #this will return the Hessian is singular error, even though there are no covs and there are start values
nullhalf = distsamp(~1~1, umf, keyfun="halfnorm", output='density', unitsOut='ha')
nullhaz = distsamp(~1~1, umf, keyfun="hazard")
null = distsamp(~1~1, umf)
#these should run without any errors
round = distsamp(~1~Round, umf, keyfun="hazard")
region = distsamp(~1~Region, umf)
impound = distsamp(~1~Impoundment, umf)
year = distsamp(~1~Year, umf)
habitat = distsamp(~1~Habitat, umf)
int = distsamp(~1~Interspersion, umf)
tran = distsamp(~1~Transect, umf)


Cand.mod = list()
Cand.mod[[1]] = distsamp(~1~1, umf)
Cand.mod[[2]] = distsamp(~1~1, umf, keyfun="hazard")
Cand.mod[[3]] = distsamp(~1~1, umf, keyfun="halfnorm")

Modnames = c("null", "hazard", "halfnorm")

detect.table = aictab(cand.set=Cand.mod, modnames=Modnames)
detect.table


round
region 
impound
year
habitat
int


hist(umf) #shows the distribution of observations within each distance class. 


#backstransform to get answers o nthe same scale


#backtransformation for a null model
backTransform(round, type="state") #returns population density
backTransform(round, type="det") #returns hazard-rate shape parameter
backTransform(round, type="scale") #hazard-rate scale parameter

#backtransformation for a model with covariates
backTransform(linearComb(round['state'], c(1,2)))
backTransform(linearComb(round['det'], c(1)))
backTransform(linearComb(round['scale'], c(1)))

backTransform(linearComb(region['state'], c(1,2)))
backTransform(linearComb(region['det'], c(1)))
backTransform(linearComb(region['scale'], c(1)))

backTransform(linearComb(impound['state'], c(1,2)))
backTransform(linearComb(impound['det'], c(1)))
backTransform(linearComb(impound['scale'], c(1)))

backTransform(linearComb(tran['state'], c(1:318)))
backTransform(linearComb(tran['det'], c(1)))
backTransform(linearComb(tran['scale'], c(1,2)))


#predicting
site.level.density <- predict(round, type="state")$Predicted
plotArea.inHectares <- 100 * 40 / 10000
site.level.abundance <- site.level.density * plotArea.inHectares
(N.hat <- sum(site.level.abundance))

#describe the uncertainty of N.hat or other derived parameters with a paramteric bootstrap approach
getN.hat <- function(fit) {
  d <- predict(fit, type="state")$Predicted
  a <- d * (100 * 40 / 10000)
  N.hat <- c(N.hat = sum(a))
  return(N.hat)
}
pb <- parboot(round, statistic=getN.hat, nsim=25)
pb



(rndConstant <- data.frame(Round =factor(c("1", "2", "3", "4"))))

Elambda <- predict(round, type="state", newdata=rndConstant, appendData=TRUE)
Esigma <- predict(round, type="det", newdata=rndConstant, appendData=TRUE)



