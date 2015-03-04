#adding julian date to the waterfowl data

dat <- read.csv('2014_waterfowl.csv')

dates <- dat[,c("month","day","year")]

varmonth<-cbind(c(1:12),c(0,31,59,90,120,151,181,212,243,273,304,334))
juldates<-data.frame(date=as.numeric())
for(i in 1:nrow(dates)) {juldates[i,1]<-varmonth[which(dates[i,1]==varmonth[,1]),2]+dates[i,2]
                         
}

dat$jdate <- juldates$date


write.csv(dat,"2014_waterfowl.csv", row.names=F)
