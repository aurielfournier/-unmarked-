setwd("~/Dropbox/R/Distance")

#read in the sora observations
sora12r3 <- as.matrix(read.csv('2012r3_sora.csv', header=T))
# this file has 36 columns, because I have three visits and each of them takes up 12 columns, one column for each distance bin
# each row is a place, in my case a wetland impoundment, that is visited

#read in the covariate data #organized by impoundment.
cov12r3 <- read.csv('2012r3_cov.csv', header=T)
#this file has the covariates in it, summarized by site, with each site being a differnt row. They are in the same order as the bird input file
# the important one here for transect based surveys is the effort, because you have to tell unmarked how long each transect was, and mine aren't all the same length
# this is how you input covariates that do not change across visits, if things do change you have to input them differently
# and I honestly don't remember how to do that at the moment, if you want to know how, ask me and I'll try to dig up taht code in the next few days


#this is just a list of my distance bins, I dont' detect birds very far out (farthest is 12) so break them into 1 meter bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) 

#Unmarked Data Frame
umf12r3 = unmarkedFrameGDS(y=sora12r3, 
                           numPrimary=3,
                           siteCovs = cov12r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r3$effortm,
)
