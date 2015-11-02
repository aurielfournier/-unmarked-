# creating buffers around each impoundment

library(rgdal)
library(rgeos)
library(raster)
dsn <- "C:/Users/avanderlaar/Documents/data/gis/ex_impounds"

fnames <- data.frame(names=list.files(path=dsn, pattern=".prj"))

fnames <- fnames %>% separate(names,sep=-5, into=c("names","extension"))

polys <- list()

for(i in fnames$names){
coords <- readOGR(dsn=dsn, layer=i)
polys[[i]] <-  spTransform(coords, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
}

buffs <- list()

for(i in 1:length(polys)){
buffs[[i]] <-  gBuffer(polys[[i]], width=5000)
}

for(i in 1:15){
  plot(buffs[[i]])
  plot(polys[[i]], add=TRUE)
}

nlcd <- raster('C:/Users/avanderlaar/Documents/data/gis/nlcd_2006_landcover_2011_edition_2014_10_10/nlcd_2006_landcover_2011_edition_2014_10_10.img')


lndf1 <- readGDAL(fname='C:/Users/avanderlaar/Documents/data/gis/US_130EVC/Metadata.shp')

lndf2 <- readOGR(dsn='C:/Users/avanderlaar/Documents/data/gis/US_130EVT', layer="Metadata")

clips <- list()

for(i in 1:15){
  clips[[i]] <- crop(nlcd,buffs[[i]])
}

summaries <- list()

for(i in 1:15){
  summaries[[i]] <- as.data.frame(clips[[i]])
}

categories <- levels(unique(summaries[[i]]$NLCD.2006.Land.Cover.Class))

datsum <- data.frame(matrix(ncol=16, nrow=length(categories)))
datsum[,1] <- categories


for(i in 1:15){
  dat <- summaries[[i]]
  row <- nrow(dat)
  sumsum <- summary(dat$NLCD.2006.Land.Cover.Class)
  sumcorrect <- sumsum/row
  datsum[,1+i] <- sumcorrect
}

colnames(datsum) <- c("nlcdvariable",fnames$names)

datsum <- datsum[2:nrow(datsum),]

mdatsum <- melt(datsum, id="nlcdvariable")

keep <- c("Deciduous Forest","Emergent Herbaceuous Wetlands","Developed, Open Space","Open Water","Hay/Pasture","Woody Wetlands")

mdatsum <- mdatsum[mdatsum$nlcdvariable %in% keep,]

ggplot()+geom_point(data=mdatsum, aes(x=variable, y=value))+facet_wrap(~nlcdvariable)+theme(axis.text.x=element_text(ang=90))

dsn <- "C:/Users/avanderlaar/Documents/data/gis/landfire/Grid2/us_130evt/z001001.adf"

file_names <- list.files("C:/Users/avanderlaar/Documents/data/gis/landfire/Grid2/us_130evt/", patter=".adf")

landfire <- list()

for(i in 1:length(file_names)){
x <- new("GDALReadOnlyDataset",paste0("C:/Users/avanderlaar/Documents/data/gis/landfire/Grid2/us_130evt/","",file_names[i]))
getDriver(x)
getDriverLongName(getDriver(x))
landfire[[i]] <- asSGDF_GROD(x, output.dim=c(200,200))
}


for(i in 1:length(file_names)){
spplot(landfire[[i]], "band1")
}
