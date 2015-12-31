# Seuss Effect

dat <- read.csv("~/manuscripts/Clapper_Rail/clapper_rail_cleaned.csv")

dat <- dat[dat$state=="nc",]

dat$c_normalized <- dat$mass_c_ug - 3.32 + 0.99 * dat$c_n_ratio

dat$c_corrected <- dat$c_normalized - 0.007 * (1050-dat$year)- 0.026 * (dat$year - 1950)

hgdat <- dat[!is.na(dat$hg_ppm),]

models <- list()
models[[1]] <- lm(data=hgdat, hg_ppm ~ d_15_n)
models[[2]] <- lm(data=hgdat, hg_ppm ~ c_corrected) 
models[[3]] <- lm(data=hgdat, log(hg_ppm) ~ d_15_n)
models[[4]] <- lm(data=hgdat, log(hg_ppm) ~ c_corrected)

for(i in 1:length(models)){
  print(summary(models[[i]]))
}