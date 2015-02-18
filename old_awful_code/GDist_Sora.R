#gdistsamp for 2013 sORA data, 

setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

library(unmarked)
library(AICcmodavg)

#read in the sora observations
#3 surveys, 12 distance brackets, each 1 meter wide
#10 transects
sora <- as.matrix(read.csv('Gdist_Clipped.csv', header=FALSE))

transect.length = 5000

cutpt = as.numeric(c(0:12)) #the fartherest distance is 12m

umf = unmarkedFrameGDS(y=sora, 
                       numPrimary=3,
                       #siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=rep(transect.length,10))

null = gdistsamp(lambdaformula = ~1, 
                 phiformula = ~1, 
                 pformula = ~1, 
                 data = umf, 
                 unitsOut = "kmsq",
                 starts=c(log(30),log(30),log(30)),
                 keyfun = "exp",
                 output = "density",
                 mixture="NB",
                 se = TRUE,
)

