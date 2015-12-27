# simulate treatment differences for rails and waterfowl

library(pwr)
library(compute.es)

M <- 31
simlength <- 10000


e_size <- seq(1,1,length=simlength)
power <- seq(1,1,length=simlength)
pvalue <- seq(1,1,length=simlength)
for(i in 1:simlength){
e4 <- data.frame(railha = abs(rnorm(n=M, mean=29,sd=40)), year=2014, treatment="e")
l4 <- data.frame(railha = abs(rnorm(n=M, mean=15, sd=20)), year=2014, treatment="l")
e5 <- data.frame(railha = abs(rnorm(n=M, mean=15,sd=30)), year=2015, treatment="e")
l5 <- data.frame(railha = abs(rnorm(n=M, mean=10, sd=20)), year=2015, treatment="l")
e6 <- data.frame(railha = abs(rnorm(n=M, mean=30, sd=40)), year=2016, treatment="e")
l6 <- data.frame(railha = abs(rnorm(n=M, mean=25, sd=30)), year=2016, treatment="l")
                 
dat <- rbind(e4, l4, e5, l5, e6, l6)

e_size[i] <- abs(mean(dat[dat$treat=="e","railha"]) - mean(dat[dat$treat=="l","railha"]))
  
pwr <- pwr.f2.test(u=M-1, v=M-1, f2=e_size[i], sig.level=0.05, power=NULL)

power[i] <- pwr$power

mod <- glm(data=dat, railha ~ treatment)

pvalue[i] <- coef(summary(mod))[,4][2]

}


esize <- data.frame(e_size=e_size, num=seq(1,simlength,by=1))
ppower <- data.frame(power=power, num=seq(1,simlength,by=1))
ppvalue <- data.frame(pvalue=pvalue, num=seq(1,simlength, by=1))

dat <- data.frame(num=esize$num, e_size=esize$e_size, power=ppower$power, pvalue=ppvalue$pvalue)

a <- ggplot()+geom_point(data=dat[dat$e_size>=3,], aes(x=num, y=pvalue))+theme_krementz()+geom_hline(y=0.05, lwd=3)
b <- ggplot()+geom_point(data=dat[dat$e_size>=3,], aes(x=num, y=power))+theme_krementz()
c <- ggplot()+geom_point(data=dat[dat$e_size>=3,], aes(x=num, y=e_size))+theme_krementz()

grid.arrange(a,b,c,ncol=1)
