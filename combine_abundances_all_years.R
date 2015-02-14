
d12 <- read.csv("abundances_2012.csv")
d13 <- read.csv("abundances_2013.csv")
d14 <- read.csv("abundances_2014.csv"))

d <- rbind(rbind(d12, d13),d14)

write.csv(d, "rail_abundances_all_years.csv", row.names=F)
