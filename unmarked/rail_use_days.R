
setwd("~/SourceTree/data")

library(ggplot2)
library(reshape)
library(gridExtra)

options(scipen=999)

abund14r1 <- read.csv("abundance_14r1.csv", header=T)
abund14r2 <- read.csv("abundance_14r2.csv", header=T)
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
abund <- abund[abund$impound!="r3",]
abund <- abund[abund$impound!="r7",]
abund <- abund[,c("mean",'impound',"treat","region","round","jdate","hectares")]
jdate <- abund[,c("impound","round","jdate")]
mdat <- melt(abund, id=c("impound","treat","region","round","jdate","hectares"))
mjdate <- melt(jdate, id=c('round',"impound"))
cdat <- cast(data=mdat, impound + hectares ~ round)
jdate <- cast(data=mjdate, impound ~ round)

cdat[is.na(cdat)] <- 0

jdate21 <- (jdate$"2"-jdate$"1")
jdate21[is.na(jdate21)] <- 0
jdate32 <- (jdate$"3"-jdate$"2")
jdate32[is.na(jdate32)] <- 0
jdate43 <-(jdate$"4"-jdate$"3")
jdate43[is.na(jdate43)] <- 0

cdat$jdate21 <- as.integer(jdate21)
cdat$jdate32 <- as.integer(jdate32)
cdat$jdate43 <- as.integer(jdate43)
#number of surveys
cdat$surv <- apply(cdat[,c("jdate21","jdate32","jdate43")],1,function(x) sum(x>=1, na.rm=T))

cdat$jsum <- (jdate21 + jdate32 + jdate43)

cdat$rounds <- (jdate21 + jdate32 + jdate43)/cdat$surv

cdat$rud_jdate <- ((cdat$"1" + cdat$"2")/2)+((cdat$"2"+cdat$"3")/2)+((cdat$"3"+cdat$"4")/2)*(cdat$rounds)

cdat$rud <- ((cdat$"1" + cdat$"2")/2)+((cdat$"2"+cdat$"3")/2)+((cdat$"3"+cdat$"4")/2)

cdat$rudh <- cdat$rud / cdat$hectares

cdat$rud_jdateh <- cdat$rud_jdate / cdat$hectares

a <- ggplot()+
  geom_bar(data=cdat, aes(x=impound, y=rud),
           position=position_dodge(), 
           stat="identity",
           colour="black")

b <- ggplot()+
  geom_bar(data=cdat, aes(x=impound, y=rud_jdate),
           position=position_dodge(), 
           stat="identity",
           colour="black")

c <- ggplot()+
  geom_bar(data=cdat, aes(x=impound, y=rudh),
           position=position_dodge(), 
           stat="identity",
           colour="black")

d <- ggplot()+
  geom_bar(data=cdat, aes(x=impound, y=rud_jdateh),
           position=position_dodge(), 
           stat="identity",
           colour="black")
grid.arrange(a,b,c,d,ncol=1)

