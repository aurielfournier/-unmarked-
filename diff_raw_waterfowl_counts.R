#comparing raw waterfowl counts
#

setwd('~/data')
dat <- read.csv('2014_waterfowl.csv')

dat$jdate_new <- ifelse(dat$jdate<200, dat$jdate+365, dat$jdate)


ggplot()+
  geom_boxplot(data=dat, aes(x=treat, y=waterfowl))


ttest <- lm(waterfowl ~ treat + jdate, data=dat)

ggplot(data=dat, aes(x=jdate_new, y=waterfowl, colour=treat, group=treat))+
  geom_point()+
  geom_smooth(method=lm)

