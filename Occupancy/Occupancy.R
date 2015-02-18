M <- 250 # Number of sites
J <- 3 # num secondary sample periods
T <- 10 # num primary sample periods
psi <- rep(NA, T) # Occupancy probability
muZ <- z <- array(dim = c(M, T)) # Expected and realized occurrence
y <- array(NA, dim = c(M, J, T)) # Detection histories
set.seed(13973)
psi[1] <- 0.4 # Initial occupancy probability
p <- c(0.3,0.4,0.5,0.5,0.1,0.3,0.5,0.5,0.6,0.2)
phi <- runif(n=T-1, min=0.6, max=0.8) # Survival probability (1-epsilon)
gamma <- runif(n=T-1, min=0.1, max=0.2) # Colonization probability
# Generate latent states of occurrence
# First year
  z[,1] <- rbinom(M, 1, psi[1]) # Initial occupancy state
# Later years
  for(i in 1:M){ # Loop over sites
  for(k in 2:T){ # Loop over years
      muZ[k] <- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1]
      z[i,k] <- rbinom(1, 1, muZ[k])
    }
  }
# Generate detection/non-detection data
  for(i in 1:M){
    for(k in 1:T){
      prob <- z[i,k] * p[k]
      for(j in 1:J){
        y[i,j,k] <- rbinom(1, 1, prob)
      }
    }
  }
# Compute annual population occupancy
 for (k in 2:T){
    psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
  }


#plots proportion of sites occupied
plot(1:T, colMeans(z), type = "b", xlab = "Year",
     ylab = "Proportion of sites occupied",
     col = "black", xlim=c(0.5, 10.5), xaxp=c(1,10,9),
     ylim = c(0,0.6), lwd = 2, lty = 1,
     frame.plot = FALSE, las = 1, pch=16)
psi.app <- colMeans(apply(y, c(1,3), max))
lines(1:T, psi.app, type = "b", col = "blue", lty=3, lwd = 2)
legend(1, 0.6, c("truth", "observed"),
         col=c("black", "blue"), lty=c(1,3), pch=c(16,1))


library(unmarked)

yy <- matrix(y, M, J*T)

year <- matrix(c('01','02','03','04','05','06','07','08','09','10'),
               nrow(yy), T, byrow=TRUE)


simUMF <- unmarkedMultFrame(
  y = yy,
  yearlySiteCovs = list(year = year),
  numPrimary=T)
summary(simUMF)


# Model with all constant parameters
m0 <- colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1,
               pformula = ~ 1, data = simUMF, method="BFGS")

#back transform values to the original scale (currenty on logit scale)
#Initial
plogis(-0.813)
#Colonization
plogis(-1.77)
#Extinction
plogis(-0.59)
#Detection
plogis(-0.837)

or
#back transforms
backTransform(m0, type="psi")
#confidence interval
confint(backTransform(m0, type="psi"))

#dynamic occupancy model
m1 <- colext(psiformula = ~1, # First-year occupancy
             gammaformula = ~ year-1, # Colonization
             epsilonformula = ~ year-1, # Extinction
             pformula = ~ year-1, # Detection
             data = simUMF)
#backtransform again

nd <- data.frame(year=c('01','02','03','04','05','06','07','08','09'))
E.ext <- predict(m1, type='ext', newdata=nd)
E.col <- predict(m1, type='col', newdata=nd)
nd <- data.frame(year=c('01','02','03','04','05','06','07','08','09','10'))
E.det <- predict(m1, type='det', newdata=nd)


op <- par(mfrow=c(3,1), mai=c(0.6, 0.6, 0.1, 0.1))
with(E.ext, { # Plot for extinction probability
  plot(1:9, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Extinction probability ( ', epsilon, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:9, labels=nd$year[1:9])
  arrows(1:9, lower, 1:9, upper, code=3, angle=90, length=0.03, col=4)
  points((1:9)-0.1, 1-phi, col=1, lwd = 1, pch=16)
  legend(7, 1, c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
         cex=0.8)
})
with(E.col, { # Plot for colonization probability
  plot(1:9, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Colonization probability ( ', gamma, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:9, labels=nd$year[1:9])
  arrows(1:9, lower, 1:9, upper, code=3, angle=90, length=0.03, col=4)
  points((1:9)-0.1, gamma, col=1, lwd = 1, pch=16)
  legend(7, 1, c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
         cex=0.8)
})
with(E.det, { # Plot for detection probability: note 10 years
  plot(1:10, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Detection probability ( ', p, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:10, labels=nd$year)
  arrows(1:10, lower, 1:10, upper, code=3, angle=90, length=0.03, col=4)
  points((1:10)-0.1, p, col=1, lwd = 1, pch=16)
  legend(7.5, 1, c('Parameter','Estimate'), col=c(1,4), pch=c(16, 1),
         cex=0.8)
})
par(op)

#B is the # of bootstraps, so it should be a big number (1000)
m1 <- nonparboot(m1, B = 10)
cbind(psi=psi, smoothed=smoothed(m1)[2,], SE=m1@smoothed.mean.bsse[2,])


#turnover probability

turnover <- function(fm) {
  psi.hat <- plogis(coef(fm, type="psi"))
  if(length(psi.hat) > 1)
    stop("this function only works if psi is scalar")
  T <- getData(fm)@numPrimary
  tau.hat <- numeric(T-1)
  gamma.hat <- plogis(coef(fm, type="col"))
  14phi.hat <- 1 - plogis(coef(fm, type="ext"))
  if(length(gamma.hat) != T-1 | length(phi.hat) != T-1)
    stop("this function only works if gamma and phi T-1 vectors")
  for(t in 2:T) {
    psi.hat[t] <- psi.hat[t-1]*phi.hat[t-1] +
      (1-psi.hat[t-1])*gamma.hat[t-1]
    tau.hat[t-1] <- gamma.hat[t-1]*(1-psi.hat[t-1]) / psi.hat[t-1]
  }
  return(tau.hat)
}


#95% Confidence Intervals for Turnover Rates

pb <- parboot(m1, statistic=turnover, nsim=2)
turnCI <- cbind(pb@t0,
                  t(apply(pb@t.star, 2, quantile, probs=c(0.025, 0.975))))
colnames(turnCI) <- c("tau", "lower", "upper")
turnCI

#uses a parmetric boostrap to assess goodness of fit

chisq <- function(fm) {
  umf <- getData(fm)
  y <- getY(umf)
  sr <- fm@sitesRemoved
  if(length(sr)>0)
    y <- y[-sr,,drop=FALSE]
  fv <- fitted(fm, na.rm=TRUE)
  y[is.na(fv)] <- NA
  sum((y-fv)^2/(fv*(1-fv)))
}
set.seed(344)
pb.gof <- parboot(m0, statistic=chisq, nsim=100)
