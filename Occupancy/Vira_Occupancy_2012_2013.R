

#Virginia Rail Occupancy 2013

setwd("C:/Users/avanderlaar/Dropbox/R/Occupancy")

library(unmarked)

vira <- as.matrix(read.csv('VIRA_Occ_2013.csv', header=FALSE))
#csv file with each row representing a impoundment that was surveyed. 
#there are 16 columns. there were four rounds, each with four columns


sites <- 37 #number of rows in sora file, each is an impoundment
T <- 4 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 4 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
round <- matrix(c('13_01', '13_02', '13_03', "13_04"), nrow(sora), T, byrow=TRUE)

umf<- unmarkedMultFrame(
  y = sora,
  yearlySiteCovs = list(round=round),
  numPrimary=T)

summary(umf)

#model with constant parameters
m0 <- colext(psiformula= ~1, #first year occupancy
             gammaformula = ~ 1,#conolinzation
             epsilonformula = ~ 1, #extinction
             pformula = ~ 1, #detection
             data = umf)

#backtransform m0
backTransform(m0, type="psi")
confint(backTransform(m0, type="psi"))


#this is a model that looks at the impact of round on colonization, extinction and detectability
m1 <- colext(psiformula = ~round-1, # First-year occupancy
             gammaformula = ~ round-1, # Colonization
             epsilonformula = ~ round-1, # Extinction
             pformula = ~ round-1, # Detection
             data = umf)

m1


#backtransform m2

nd <- data.frame(round=c('13_01', '13_02', '13_03', '13_04'))
E.ext <- predict(m1, type='ext', newdata=nd) #extinction
E.col <- predict(m1, type='col', newdata=nd) #colonization
nd2 <- data.frame(round=c('13_01', '13_02', '13_03', '13_04'))
E.det <- predict(m1, type='det', newdata=nd2) #detectability

backTransform(m1, type="psi") #first year occupancy
confint(backTransform(m1, type="psi"))
E.ext #extinction
E.col #colonization
E.det #detectability


############
###########
# collapse 1 and 0 down to regions
# so every four rows becomes 1 row
###########
##########
#create a matrix
viraRegion = matrix(,nrow=37, ncol=4)

#get rid of the NAs in the exisiting data 
virad = as.data.frame(vira)
virad[is.na(virad)] = 0

viraRegion[,1] = virad[,1]+virad[,2]+virad[,3]+virad[,4]
viraRegion[,2] = virad[,5]+virad[,6]+virad[,7]+virad[,8]
viraRegion[,2] = virad[,9]+virad[,10]+virad[,11]+virad[,12]
viraRegion[,4] = virad[,13]+virad[,14]+virad[,15]+virad[,16]



sites <- 37 #number of rows in sora file, each is an impoundment
T <- 4 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 1 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
round <- matrix(c('13_01', '13_02', '13_03', "13_04"), nrow(viraRound), T, byrow=TRUE)

umfReg<- unmarkedMultFrame(
  y = viraRegion,
  yearlySiteCovs = list(round=round),
  numPrimary=T)

summary(umfReg)

#model with constant parameters
m0Region <- colext(psiformula= ~1, #first year occupancy
             gammaformula = ~ 1,#conolinzation
             epsilonformula = ~ 1, #extinction
             pformula = ~ 1, #detection
             data = umfReg)

#backtransform m0
backTransform(m0Region, type="psi")
confint(backTransform(m0Region, type="psi"))



############
###########
# collapse 1 and 0 down to rounds
# so 1,5,9,13 become round 1, etc
###########
##########
#create a matrix
viraRound = matrix(,nrow=37, ncol=4)

#get rid of the NAs in the exisiting data 
virad = as.data.frame(vira)
virad[is.na(virad)] = 0 

viraRound[,1] = virad[,1]+virad[,5]+virad[,9]+virad[,13]
viraRound[,2] = virad[,2]+virad[,6]+virad[,10]+virad[,14]
viraRound[,3] = virad[,3]+virad[,7]+virad[,11]+virad[,15]
viraRound[,4] = virad[,4]+virad[,8]+virad[,12]+virad[,16]



sites <- 37 #number of rows in sora file, each is an impoundment
T <- 4 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 1 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
round <- matrix(c('13_01', '13_02', '13_03', "13_04"), nrow(viraRound), T, byrow=TRUE)

umfRound<- unmarkedMultFrame(
  y = viraRound,
  yearlySiteCovs = list(round=round),
  numPrimary=T)

summary(umfRound)

#model with constant parameters
m0Round <- colext(psiformula= ~1, #first year occupancy
             gammaformula = ~ 1,#conolinzation
             epsilonformula = ~ 1, #extinction
             pformula = ~ 1, #detection
             data = umfRound)

#backtransform m0
backTransform(m0Round, type="psi")
confint(backTransform(m0Round, type="psi"))

