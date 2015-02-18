library(unmarked)

#crossbill occupancy

data(crossbill)
colnames(crossbill)

#removes missing data so that each model uses the same data set and we can compare AIC

DATE <- as.matrix(crossbill[,32:58])
y.cross <- as.matrix(crossbill[,5:31])
y.cross[is.na(DATE) != is.na(y.cross)] <- NA

#scaling covariates

sd.DATE <- sd(c(DATE), na.rm=TRUE)
mean.DATE <- mean(DATE, na.rm=TRUE)
DATE <- (DATE - mean.DATE) / sd.DATE

#format everything appropriatly

years <- as.character(1999:2007)
years <- matrix(years, nrow(crossbill), 9, byrow=TRUE)
umf <- unmarkedMultFrame(y=y.cross,
                           siteCovs=crossbill[,2:3], yearlySiteCovs=list(year=years),
                           obsCovs=list(date=DATE),
                           numPrimary=9)

#models

# A model with constant parameters
 fm0 <- colext(~1, ~1, ~1, ~1, umf)
# Like fm0, but with year-dependent detection
   fm1 <- colext(~1, ~1, ~1, ~year, umf)
 # Like fm0, but with year-dependent colonization and extinction
   fm2 <- colext(~1, ~year-1, ~year-1, ~1, umf)
 # A fully time-dependent model
   fm3 <- colext(~1, ~year-1, ~year-1, ~year, umf)
 # Like fm3 with forest-dependence of 1st-year occupancy
   fm4 <- colext(~forest, ~year-1, ~year-1, ~year, umf)
 # Like fm4 with date- and year-dependence of detection
   fm5 <- colext(~forest, ~year-1, ~year-1, ~year + date + I(date^2),
                  umf, starts=c(coef(fm4), 0, 0))
 # Same as fm5, but with detection in addition depending on forest cover
   fm6 <- colext(~forest, ~year-1, ~year-1, ~year + date + I(date^2) +
                    forest, umf)


#rank models

models <- fitList('psi(.)gam(.)eps(.)p(.)' = fm0,
                    'psi(.)gam(.)eps(.)p(Y)' = fm1,
                    'psi(.)gam(Y)eps(Y)p(.)' = fm2,
                    'psi(.)gam(Y)eps(Y)p(Y)' = fm3,
                    'psi(F)gam(Y)eps(Y)p(Y)' = fm4,
                    'psi(F)gam(Y)eps(Y)p(YD2)' = fm5,
                    'psi(F)gam(Y)eps(Y)p(YD2F)' = fm6)
ms <- modSel(models)
ms

#exporting from R

coef(ms) # Estimates only
SE(ms) # Standard errors only
toExport <- as(ms, "data.frame") # Everything


#prediction and plotting

op <- par(mfrow=c(1,2), mai=c(0.8,0.8,0.1,0.1))
nd <- data.frame(forest=seq(0, 100, length=50))
E.psi <- predict(fm6, type="psi", newdata=nd, appendData=TRUE)
with(E.psi, {
  plot(forest, Predicted, ylim=c(0,1), type="l",
       xlab="Percent cover of forest",
       ylab=expression(hat(psi)), cex.lab=0.8, cex.axis=0.8)
  lines(forest, Predicted+1.96*SE, col=gray(0.7))
  lines(forest, Predicted-1.96*SE, col=gray(0.7))
})
nd <- data.frame(date=seq(-2, 2, length=50),
                   year=factor("2005", levels=c(unique(years))),
                   forest=50)
E.p <- predict(fm6, type="det", newdata=nd, appendData=TRUE)
E.p$dateOrig <- E.p$date*sd.DATE + mean.DATE
with(E.p, {
  plot(dateOrig, Predicted, ylim=c(0,1), type="l",
       xlab="Julian date", ylab=expression( italic(p) ),
       cex.lab=0.8, cex.axis=0.8)
  lines(dateOrig, Predicted+1.96*SE, col=gray(0.7))
  lines(dateOrig, Predicted-1.96*SE, col=gray(0.7))
})
par(op)

