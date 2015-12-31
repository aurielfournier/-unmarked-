# simulate differences in treatments
library(ggplot2)
library(gridExtra)
#we'd expect counts of bird abundance to follow a poison distribution....I suppose anyway

raillist <- list()
region <- c("nw","nc","ne","se")

for(i in region){
    lambdae <- runif(min=30, max=70, n=2)
    lambdal <- rpois(lambda=, n=2)
    dat <- as.data.frame(rnorm(18*2, mean=lambdae[[1]],sd=lambdae[[2]]*.5))  
      dat$region <- i
      dat$treat <- "early"
      datl <- as.data.frame(rnorm(15*2,mean=lambdal[[1]], sd=lambdal[[2]]*.5))
      datl$region <- i
      datl$treat <- "late"
      colnames(datl) <- colnames(dat)
      r <- rbind(dat, datl)
      colnames(r) <- c("count","region","treat")
    raillist[[i]] <- r
}

railbf <-do.call(rbind,raillist) 

ducklist <- list()
region <- c("nw","nc","ne","se")

for(i in region){
  lambdae <- runif(min=150, max=500, n=2)
  lambdal <- runif(min=140, max=480, n=2)
  dat <- as.data.frame(rnorm(20*2, mean=lambdae[[1]], sd=.25*lambdae[[2]]))  
  dat$region <- i
  dat$treat <- "early"
  datl <- as.data.frame(rnorm(42*2,mean=lambdal[[1]], sd=.25*lambdal[[2]]))
  datl$region <- i
  datl$treat <- "late"
  colnames(datl) <- colnames(dat)
  r <- rbind(dat, datl)
  colnames(r) <- c("count","region","treat")
  ducklist[[i]] <- r
}

duckbf <-do.call(rbind,ducklist) 


rlm <- lm(count ~ treat, data=railbf)
dlm <- lm(count ~ treat , data=duckbf)

rl <- ggplot(railbf[railbf$treat=="late",])+
  geom_histogram(aes(x=count),stat="bin",position="dodge")
re <- ggplot(railbf[railbf$treat=="early",])+
  geom_histogram(aes(x=count, group=region, fill=region),stat="bin",position="dodge")
r <- ggplot(railbf)+geom_histogram(aes(x=count, group=treat, fill=treat), stat="bin",position="dodge")
d <- ggplot(duckbf)+
  geom_histogram(aes(x=count, group=treat, fill=treat),stat="bin", position="dodge")

grid.arrange(re,rl, ncol=1)
summary(rlm)
summary(dlm)