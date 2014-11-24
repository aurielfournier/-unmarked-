
setwd("C:/Users/avanderlaar/Dropbox/data")

library(ggplot2)
library(reshape)

abund14r1 <- read.csv("abundance_14r1.csv", header=T)
abund14r2 <- read.csv("abundance_14r2.csv", header=T)
abund14r3 <- read.csv("abundance_14r3.csv", header=T)
abund14r4 <- read.csv("abundance_14r4.csv", header=T)

abund14r1$round <- 1
abund14r2$round <- 2
abund14r3$round <- 3
abund14r4$round <- 4

abundr1r2 <- rbind(abund14r1, abund14r2)
abundr3r4 <- rbind(abund14r3, abund14r4)
abund <- rbind(abundr1r2, abundr3r4)
