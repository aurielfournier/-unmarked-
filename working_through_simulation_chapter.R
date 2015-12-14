

simrep <- 10000
NTOTAL <- SUMMAXC <- numeric(simrep)
for(i in 1:simrep){
  data <- data.fn(show.plot = FALSE, J=1, alpha1 = 0.5,alpha2 = 0.5,alpha3 = 0.5,M=100)
  NTOTAL[i] <- data$Ntotal
  SUMMAXC[i] <- data$summaxC
}
plot(sort(NTOTAL), ylim = c(min(SUMMAXC), max(NTOTAL)), ylab = "", xlab = "Simulation", col = "red", frame = FALSE)
points(SUMMAXC[order(NTOTAL)], col = "blue")
 