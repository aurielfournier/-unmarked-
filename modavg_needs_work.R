load("2012_models.Rdata")
model12 <- model
model12 <- model12[names(model12)!=c("r_w_i","s_r_i","s_w_i","global","s_r")]
load("2013_models.Rdata")
model13 <- model
load("2014_models.Rdata")
model14 <- model
model14 <- model14[names(model14)!=c("s_r_i")]

mw12 <- modavg(cand.set=model12, parm="scale_averagewater", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global","s_r"))
mnw12 <- modavg(cand.set=model12, parm="regionnw", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mnc12 <- modavg(cand.set=model12, parm="regionnc", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mne12 <- modavg(cand.set=model12, parm="regionne", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mse12 <- modavg(cand.set=model12, parm="regionse", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))

mw13 <- modavg(cand.set=model13, parm="scale_averagewater", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mnw13 <- modavg(cand.set=model13, parm="regionnw", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mnc13 <- modavg(cand.set=model13, parm="regionnc", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mne13 <- modavg(cand.set=model13, parm="regionne", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mse13 <- modavg(cand.set=model13, parm="regionse", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))

mw14 <- modavg(cand.set=model14, parm="scale_averagewater", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mnw14 <- modavg(cand.set=model14, parm="regionnw", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mnc14 <- modavg(cand.set=model14, parm="regionnc", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mne14 <- modavg(cand.set=model14, parm="regionne", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))
mse14 <- modavg(cand.set=model14, parm="regionse", parm.type="lambda", exclude=list("r_w_i","s_r_i","s_w_i","global"))

fds <- fitList(model12)
newdat <- data.frame(region=c("nw","nc","ne","se"), scale_averagewater=-0.2650385, scale_short=-0.05644074)
pred <- predict(fds, type="lambda", newdata=newdat)
