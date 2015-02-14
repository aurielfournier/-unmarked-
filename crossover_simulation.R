# simulate differences in treatments

#we'd expect counts of bird abundance to follow a poison distribution....I suppose anyway
rail_e <- as.data.frame(rpois(200, lambda=50))
rail_l <- as.data.frame(rpois(200, lambda=75))
duck_e <- as.data.frame(rpois(200, lambda=200))
duck_l <- as.data.frame(rpois(200, lambda=175))
# n <- for the rails this should be 200
# n <- for the waterfowl, 200 as well (20 surveys, 10 areas?)

duck_l$treat <- "late"
duck_e$treat <- "early"
rail_e$treat <- "early"
rail_l$treat <- "late"

names <- c("count","treat")
colnames(rail_e) <- names
colnames(rail_l) <- names
colnames(duck_e) <- names
colnames(duck_l) <- names

duck <- rbind(duck_e, duck_l)
rail <- rbind(rail_e, rail_l)

rlm <- lm(count ~ treat, data=rail)
dlm <- lm(count ~ treat, data=duck)