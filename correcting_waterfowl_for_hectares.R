#correcting waterfowl for hectares
library(dplyr)
dat <- read.csv("2014_waterfowl_tidy.csv")
cov <- read.csv("hectares.csv")
new_ <- left_join(dat, cov) 
new_$birds_ha <- new_$value/new_$hectares
write.csv(new_,"2014_waterfowl_tidy_hec.csv")


