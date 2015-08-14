library(ggplot2)



theme_krementz <- function(){
  theme(axis.text.x = element_text(size=12,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        plot.background = element_blank(),
        panel.border=element_blank(),
        panel.grid.major= element_line(colour=NA), 
        panel.grid.minor=element_line(colour=NA),
        title=element_text(size=20),
        panel.background = element_rect(fill = "white"),
        axis.line=element_line(colour="black"))
}


setwd("~/Documents/unmarked")
load("2012_models.Rdata")
m12 <- model
names12 <- names(m12)
short12 <- modavg(cand.set=m12, modnames=names12, parm.type="lambda", parm="scale_short")
water12 <- modavg(cand.set=m12, modnames=names12, parm.type="lambda", parm="scale_averagewater")

reg12 <- data.frame(region=c("nw","nc","ne","se"))

region12 <- predict(m12$region, type="lambda", newdata=reg12, appendData=TRUE)


parameters <- data.frame(variable=c("non persistent moist soil vegetation","average water","region nw","region nc","region ne","region se"),beta=c(short12$Mod.avg.beta, water12$Mod.avg.beta,  region12$Predicted),se=c(short12$Uncond.SE,water12$Uncond.SE,region12$SE))


a12 <- ggplot(data=parameters[parameters$variable!="non persistent moist soil vegetation"&parameters$variable!="average water",], aes(x=variable, y=beta))+geom_point()+geom_errorbar(aes(ymin=c(beta-se), ymax=c(beta+se)))+theme_krementz()+theme(axis.text=element_text(ang=90))



b12 <- ggplot(data=parameters[parameters$variable=="non persistent moist soil vegetation"|parameters$variable=="average water",], aes(x=variable, y=beta))+geom_point()+geom_errorbar(aes(ymin=c(beta-se), ymax=c(beta+se)))+theme_krementz()+theme(axis.text=element_text(ang=90))

grid.arrange(a12, b12, ncol=2, heights=c(4,2))


######


load("2013_models.Rdata")
m13 <- model
names12 <- names(m13)
int13 <- modavg(cand.set=m13, modnames=names12, parm.type="lambda", parm="scale_int")
water13 <- modavg(cand.set=m13, modnames=names12, parm.type="lambda", parm="scale_averagewater")


parameters <- data.frame(variable=c("interspersion","water"),beta=c(int13$Mod.avg.beta, water13$Mod.avg.beta),se=c(int13$Uncond.SE,water13$Uncond.SE))

a12 <- ggplot(data=parameters, aes(x=variable, y=beta))+geom_point()+geom_errorbar(aes(ymin=c(beta-se), ymax=c(beta+se)))+theme_krementz()
a12


###



load("2014_models.Rdata")
m14 <- model
names14 <- names(m14)

int14 <- modavg(cand.set=m14, modnames=names14, parm.type="lambda", parm="scale_int")
water14 <- modavg(cand.set=m14, modnames=names14, parm.type="lambda", parm="scale_averagewater")
short14 <- modavg(cand.set=m14, modnames=names14, parm.type="lambda", parm="scale_short")

reg14 <- data.frame(region=c("nw","nc","ne","se"))

region14 <- predict(m14$region, type="lambda", newdata=reg14, appendData=TRUE)



parameters <- data.frame(variable=c("non persistent moist soil vegetation","average water","interspersion","region nw","region nc","region ne","region se"),beta=c(short14$Mod.avg.beta, water14$Mod.avg.beta, int14$Mod.avg.beta , region14$Predicted),se=c(short14$Uncond.SE,water14$Uncond.SE,int14$Uncond.SE,region14$SE))



a14 <- ggplot(data=parameters[parameters$variable!="non persistent moist soil vegetation"&parameters$variable!="average water"&parameters$variable!="interspersion",], aes(x=variable, y=beta))+geom_point()+geom_errorbar(aes(ymin=c(beta-se), ymax=c(beta+se)))+theme_krementz()+theme(axis.text=element_text(ang=90))



b14 <- ggplot(data=parameters[parameters$variable=="non persistent moist soil vegetation"|parameters$variable=="average water"|parameters$variable=="interspersion",], aes(x=variable, y=beta))+geom_point()+geom_errorbar(aes(ymin=c(beta-se), ymax=c(beta+se)))+theme_krementz()+theme(axis.text=element_text(ang=90))

grid.arrange(a14, b14, ncol=2, heights=c(4,2))
