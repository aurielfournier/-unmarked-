#Yellow Rail Occupancy 2013

setwd("C:/Users/avanderlaar/Dropbox/R/Occupancy")

library(unmarked)

yera <- as.matrix(read.csv('YERA_Occ_2013.csv', header=FALSE))
#csv file with each row representing a impoundment that was surveyed. 
#there are 28 columns. In 2012 there were 3 rounds, each with four columns
#in 2013 there were four rounds, each with four columns
#yes in 2012 the third and fourth columns are entirely NA since sampling was different that year
#this is Ok


sites <- 37 #number of rows in sora file, each is an impoundment
T <- 4 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 4 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
yera.round <- matrix(c('13_01', '13_02', '13_03', "13_04"), nrow(yera), T, byrow=TRUE)

umf<- unmarkedMultFrame(
  y = yera,
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
m1 <- colext(psiformula = ~1, # First-year occupancy
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
yeraRegion = matrix(,nrow=37, ncol=4)
yera.data.frame = as.data.frame(yera)

n <- 4
grp <- seq(1, ncol(yera), by=n)
sapply(grp, function(x) rowSums(na.omit(yera[, x:(x+n-1)])))


yeraRegion[,1] = rowSums(yera.data.frame[,c(1,2,3,4)], na.rm=TRUE)
yeraRegion[,2] = rowSums(yera.data.frame[,c(5,6,7,8)], na.rm=FALSE)
yeraRegion[,3] = rowSums(yera,c(9,10,11,12))
yeraRegion[,4] = rowSums(yera,c(13,14,15,16))
yeraRegion[,2] = sum(na.omit(yera[,5]+yera[,6]+yera[,7]+yera[,8]))
yeraRegion[,3] = sum(na.omit(yera[,9]+yera[,10]+yera[,11]+yera[,12]))
yeraRegion[,4] = sum(na.omit(yera[,13]+yera[,14]+yera[,15]+yera[,16]))



sites <- 37 #number of rows in sora file, each is an impoundment
T <- 4 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 1 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
round <- matrix(c('13_01', '13_02', '13_03', "13_04"), nrow(viraRound), T, byrow=TRUE)

umfReg<- unmarkedMultFrame(
  y = yeraRegion,
  yearlySiteCovs = list(round=round),
  numPrimary=T)

summary(umfReg)

#model with constant parameters
m0Reg <- colext(psiformula= ~1, #first year occupancy
             gammaformula = ~ 1,#conolinzation
             epsilonformula = ~ 1, #extinction
             pformula = ~ 1, #detection
             data = umfReg)

#backtransform m0
backTransform(m0Reg, type="psi")
confint(backTransform(m0Reg, type="psi"))


m1Reg <- colext(psiformula = ~1, # First-year occupancy
             gammaformula = ~ round-1, # Colonization
             epsilonformula = ~ round-1, # Extinction
             pformula = ~ round-1, # Detection
             data = umfReg)



nd <- data.frame(round=c('13_02', '13_03', '13_04'))
E.ext <- predict(m1Reg, type='ext', newdata=nd) #extinction
E.col <- predict(m1Reg, type='col', newdata=nd) #colonization
nd2 <- data.frame(round=c('13_01', '13_02', '13_03', '13_04'))
E.det <- predict(m1Reg, type='det', newdata=nd2) #detectability

backTransform(m1Reg, type="psi") #first year occupancy
E.ext #extinction
E.col #colonization
E.det #detectability


############
###########
# collapse 1 and 0 down to rounds
# so 1,5,9,13 become round 1, etc
###########
##########
#create a matrix
yeraRound = matrix(,nrow=37, ncol=4)

#get rid of the NAs in the exisiting data 

yeraRound[,1] = sum(yera[,1]+yera[,5]+yera[,9]+yera[,13])
yeraRound[,2] = sum(na.omit(yera[,2]+yera[,6]+yera[,10]+yera[,14]))
yeraRound[,3] = sum(na.omit(yera[,3]+yera[,7]+yera[,11]+yera[,15]))
yeraRound[,4] = sum(na.omit(yera[,4]+yera[,8]+yera[,12]+yera[,16]))



sites <- 37 #number of rows in sora file, each is an impoundment
T <- 4 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 1 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
round <- matrix(c('13_01', '13_02', '13_03', "13_04"), nrow(yeraRound), T, byrow=TRUE)

umfRoundY<- unmarkedMultFrame(
  y = yeraRound,
  yearlySiteCovs = list(round=round),
  numPrimary=T)

summary(umfRoundY)

#model with constant parameters
m0RoundY <- colext(psiformula= ~1, #first year occupancy
                  gammaformula = ~ 1,#conolinzation
                  epsilonformula = ~ 1, #extinction
                  pformula = ~ 1, #detection
                  data = umfRoundY)

#backtransform m0
backTransform(m0RoundY, type="psi")
confint(backTransform(m0RoundY, type="psi"))

