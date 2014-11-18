library(unmarked)

setwd("C:/Users/avanderlaar/Dropbox/Field 2014")
birds <- read.csv("allbirds.csv",header=T) #this is a csv with a row for each unique bird observation, 
#important columns are the impoundment where the bird was observed
# the distance from the line the bird was observed at
# and the night (in unmarked terms 'occasion') the survey occured on. 
# I have other crap in this file too, but it's not needed for this analysis

birds <- birds[birds$species=="sora",] #we don't have enough observations of the other rail spp, so we're just focusing on Sora
birds12 <- birds[birds$year==2012,]
birds12$night <- as.factor(birds12$night) #in 2012 we only did one survey at a night per night, so all the nights are "1.1 or 2.1 etc)
#birds12r1 doesn't exist, for a wide variety of reasons we won't get into here
birds12r2 <- birds12[birds12$round==2,] #each round is a two week period during which all the sites were surveyed, 
birds12r3 <- birds12[birds12$round==3,]
#birds12r4 doesn't exist, because we only visited sites three times in 2012
birds13 <- birds[birds$year==2013,]
birds13$night <- as.factor(birds13$night) #starting in 2013 we did two surveys at a night, so now we have 1.1, 1.2, 2.1. 2.2, etc. 
birds13r1 <- birds13[birds13$round==1,]
birds13r2 <- birds13[birds13$round==2,]
birds13r3 <- birds13[birds13$round==3,]
birds13r4 <- birds13[birds13$round==4,]
birds14 <- birds[birds$year==2014,]
birds14$night <- as.factor(birds14$night)
birds14r1 <- birds14[birds14$round==1,]
birds14r2 <- birds14[birds14$round==2,]
birds14r3 <- birds14[birds14$round==3,]
birds14r4 <- birds14[birds14$round==4,]

dist.breaks <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13) #our greatest distance is 13  

#remember 2012 round 1 doesn't exisit
gd12r2 <- formatDistData(birds12r2, "distance", "impound", dist.breaks, "night" )
#so the file is the subset we just did above
# "distance" is the column name where the distance of the bird from the line is stored
# "impound" is my unique identifer for the transect ("impound" means wetland impoundment)
# dist.breaks is the vector we created above that has my distance breaks in it
# "night" is my occasion marker, which delineates the repeat surveys for each impound, 
gd12r3 <- formatDistData(birds12r3, "distance", "impound", dist.breaks, "night" )
#also 2012 round 4 doesn't exisit

gd13r1 <- formatDistData(birds13r1, "distance", "impound", dist.breaks, "night" )
gd13r2 <- formatDistData(birds13r2, "distance", "impound", dist.breaks, "night" )
gd13r3 <- formatDistData(birds13r3, "distance", "impound", dist.breaks, "night" )
gd13r4 <- formatDistData(birds13r4, "distance", "impound", dist.breaks, "night" )

gd14r1 <- formatDistData(birds14r1, "distance", "impound", dist.breaks, "night" )
gd14r2 <- formatDistData(birds14r2, "distance", "impound", dist.breaks, "night" )
gd14r3 <- formatDistData(birds14r3, "distance", "impound", dist.breaks, "night" )
gd14r4 <- formatDistData(birds14r4, "distance", "impound", dist.breaks, "night" )


