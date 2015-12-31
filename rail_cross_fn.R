rail.cross.fn <- function(sites=30 ,rounds= 4, mean.lambdae=20 , mean.lambdal=10,beta1= 3, beta2 = -2, beta1sd=10, beta2sd=5, mean.detection= 0.05,alpha1= 1, alpha2 = 1){

early <- rbinom(n=sites, 1,0.5)
late <- ifelse(early==0,1,0)

earlyrail <- rnorm(n=sites, mean=mean.lambdae, sd=beta1sd)*early
laterail <- rnorm(n=sites, mean=mean.lambdal, sd=beta2sd)*late

beta0 <- log(mean.lambda)

for(i in 1:sites){
lambda <- exp(beta0 + (beta1 * earlyrail) + (beta2 * laterail))
}
N <- rpois(n=sites, lambda=lambda)
Ntotal <- sum(N)
psi.true <- mean(N>0)
  
alpha0 <- qlogis(mean.detection)
p <- plogis(alpha0 + alpha1 * earlyrail + alpha2 * laterail)
C <- matrix(NA, nrow=sites, ncol=rounds)
for(i in 1:rounds){
  C[,i] <- rbinom(n=sites, size=N, prob=p)
}  
summaxC <- sum(apply(C,1,max))
psi.obs <- mean(apply(C,1,max)>0)

return(list(sites=sites, j=j, mean.lambda=mean.lambda, beta0=beta0,beta1=beta1,beta2=beta2,mean.detection=mean.detection, alpha0=alpha0, alpha1=alpha1, alpha2=alpha2, early=early, late=late, lambda=lambda, N=N, p=p, C=C, Ntotal=Ntotal, psi.true=psi.true, summaxC=summaxC, psi.obs=psi.obs))
}

sims <- 10000

sigs <- seq(1,1,length=sims)

for(i in 1:sims){
raildat <- rail.cross.fn()
rdat <- data.frame(cbind(raildat$lambda, raildat$C, raildat$early))

rdat$X6 <- as.factor(ifelse(rdat$X6==1,"early","late"))
rdat$meand <- rowMeans(rdat[,2:5])

mod <- glm(data=rdat, meand ~ X6)
summary(mod)

sigs[i] <- coef(summary(mod))[,4][2]

}


dat <- data.frame(sigs=sigs, num=NA)

nrow(dat[dat$sigs<=0.05,])

