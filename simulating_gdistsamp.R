# data simulation 

sites  <- 31
visits <- 3
mean.lambda <- 10 #rails per hectare
beta0 <- log(mean.lambda)
beta1 <- 2    # water depth
beta2 <- 1    # short percent cover
#beta3 <- 0.5  # interspersion
#beta4 <- -2   # perennial emergent percent cover

waterdepth <- array(runif(n = sites*visits, -1, 1), dim = c(sites, visits)) # Scaled wind speed
short <- runif(n = sites, -1, 1)           # Scaled forest cover at each site
#int <- runif(n = sites, -1, 1)
#pe <- runif(n = sites, -1, 1)

log.lambda <- beta0 + beta1 * waterdepth + beta2 * short #+ beta3 * int + beta4 * pe
lambda <- exp(log.lambda) 

par(mfrow = c(2, 2), cex.main = 1)
curve(exp(beta0 + beta1*x), -1, 1, col = "red", frame.plot = FALSE)
matplot(waterdepth, lambda,pch = "*",  frame.plot = FALSE, xlab = "water depth", ylab = "")

curve(exp(beta0 + beta2*x), -1, 1, col = "red", frame.plot = FALSE)
matplot(short, lambda,pch = "*",  frame.plot = FALSE, xlab = "short", ylab = "")


