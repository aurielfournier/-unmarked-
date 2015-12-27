# Rail BBL Data analysis

b <- read.csv("C:/Users/avand/Documents/data/Fournier_rails_BS_201512071317.csv")

d <- read.csv("C:/Users/avand/Documents/data/Fournier_rails_Es_201512071317.csv")

enc <- b[b$BAND_NUM %in% d$BAND_NUM,]

enc <- enc[,c("BAND_NUM","SPECIES_NAME","BANDING_DATE","LAT_10_MIN_BLK","LON_10_MIN_BLK"),]

d <- d[,c("BAND_NUM","B_LAT_10_MIN_BLK","B_LON_10_MIN_BLK","BANDING_DATE","ENCOUNTER_DATE","E_LON_10_MIN_BLK","E_LAT_10_MIN_BLK","B_SPECIES_NAME")]

db <- d[,c("BAND_NUM","B_LAT_10_MIN_BLK","B_LON_10_MIN_BLK","BANDING_DATE","B_SPECIES_NAME")]
de <- d[,c("BAND_NUM","E_LAT_10_MIN_BLK","E_LON_10_MIN_BLK", "ENCOUNTER_DATE","B_SPECIES_NAME")]

cnames <- c("bandnumber","lat","lon","date","spp")

colnames(db) <- cnames
colnames(de) <- cnames

dd <- rbind(db, de)

dd <- dd[order(dd$bandnumber),]

ggplot()+geom_path(data=dd, aes(x=lon,y=lat, group=bandnumber))+geom_point(data=dd, aes(x=lon,y=lat))

dd$latd <- dd$lat/10
dd$lond <- -dd$lon/10
write.csv(dd, file="C:/Users/avand/Documents/data/recap_rails.csv")

