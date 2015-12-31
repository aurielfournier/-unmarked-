
setwd("C:/Users/avanderlaar/Dropbox/data")

library(ggplot2)
library(reshape)

abund14r1 <- read.csv("abundance_14r1.csv", header=T)
abund14r1 <- abund14r1[abund14r1$region!="se",]
abund14r1 <- abund14r1[abund14r1$impound!="kt5",]
abund14r1 <- abund14r1[abund14r1$impound!="ts6a",]
abund14r1 <- abund14r1[abund14r1$impound!="ts8a",]
abund14r1 <- abund14r1[abund14r1$impound!="ccmsu2",]
abund14r2 <- read.csv("abundance_14r2.csv", header=T)
abund14r2 <- abund14r2[abund14r2$impound!="kt5",]
abund14r3 <- read.csv("abundance_14r3.csv", header=T)
abund14r3 <- abund14r3[abund14r3$region!="nc",]
abund14r4 <- read.csv("abundance_14r4.csv", header=T)
abund14r4 <- abund14r4[abund14r4$region!="nc",]

abund14r1$round <- 1
abund14r2$round <- 2
abund14r3$round <- 3
abund14r4$round <- 4

abundr1r2 <- rbind(abund14r1, abund14r2)
abundr3r4 <- rbind(abund14r3, abund14r4)
abund <- rbind(abundr1r2, abundr3r4)

aov_treat_reg <- aov(data=abund, mean ~ treat + region)
aov_treat <- aov(data=abund, mean ~ treat)
aov_treat_round <- aov(data=abund, mean ~ treat + round)

aov_r1 <- aov(data=abund14r1, mean ~ treat)
aov_r1_r <- aov(data=abund14r1, mean ~ treat+region)

aov_r2 <- aov(data=abund14r2, mean ~ treat)
aov_r2_r <- aov(data=abund14r2, mean ~ treat+region)

aov_r3 <- aov(data=abund14r3, mean ~ treat)
aov_r3_r <- aov(data=abund14r3, mean ~ treat+region)

aov_r4 <- aov(data=abund14r4, mean ~ treat)
aov_r4_r <- aov(data=abund14r4, mean ~ treat+region)

aov_nw  <- aov(data=abund[abund$region=="nw",], mean ~ treat)
aov_nw_round <- aov(data=abund[abund$region=="nw",], mean ~ treat + round)
aov_nc <- aov(data=abund[abund$region=="nc",], mean ~ treat)
aov_nc_round <- aov(data=abund[abund$region=="nc",], mean ~ treat + round)
aov_ne <- aov(data=abund[abund$region=="ne",], mean ~ treat)
aov_ne_round <- aov(data=abund[abund$region=="ne",], mean ~ treat + round)
aov_se <- aov(data=abund[abund$region=="se",],mean~treat )
aov_se_round <- aov(data=abund[abund$region=="se",], mean ~ treat + round)

ggplot()+
  geom_boxplot(data=abund, aes(x=region, y=mean, fill=treat), group="treat")+
  facet_wrap(~round)

ggplot()+
  geom_boxplot(data=abund, aes(x=treat, y=mean, fill=treat))+
  facet_wrap(~round)

ggplot()+
  geom_boxplot(data=abund, aes(x=treat, y=mean, fill=treat))
