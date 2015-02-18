#Rail Occupancy 2012 & 2013

setwd("C:/Users/avanderlaar/Dropbox/R/Occupancy")

library(unmarked)

sora <- as.matrix(read.csv('Sora1213Input.csv', header=FALSE))
#csv file with each row representing a impoundment that was surveyed. 
#there are 28 columns. In 2012 there were 3 rounds, each with four columns
#in 2013 there were four rounds, each with four columns
#yes in 2012 the third and fourth columns are entirely NA since sampling was different that year
#this is Ok


sites <- 55 #number of rows in sora file, each is an impoundment
T <- 7 #primary sampling periods (3 rounds in 2012 + 4 rounds in 2013)
J <- 4 #secondary sampling periods

#this lets UMF understand whatis what with each year. 
round <- matrix(c('12_01','12_02','12_03','13_01', '13_02', '13_03', "13_04"), nrow(sora), T, byrow=TRUE)

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
m1 <- colext(psiformula = ~1, # First-year occupancy
             gammaformula = ~ round-1, # Colonization
             epsilonformula = ~ round-1, # Extinction
             pformula = ~ round-1, # Detection
             data = umf)

m1


#backtransform m2

nd <- data.frame(round=c('12_02', '12_03', '13_01', '13_02', '13_03', '13_04'))
E.ext <- predict(m1, type='ext', newdata=nd) #extinction
E.col <- predict(m1, type='col', newdata=nd) #colonization
nd2 <- data.frame(round=c('12_01', '12_02', '12_03', '13_01', '13_02', '13_03', '13_04'))
E.det <- predict(m1, type='det', newdata=nd2) #detectability

backTransform(m1, type="psi") #first year occupancy
confint(backTransform(m1, type="psi")) #confidence interval
E.ext #extinction
E.col #colonization
E.det #detectability

