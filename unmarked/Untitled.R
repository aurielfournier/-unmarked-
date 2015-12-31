m <- c("round", "region","scale_averagwater","scale_pe","scale_short","scale_water","scale_int")

tab <- table(m,m)

for(i in 1:7){
  tab[(1:which(tab[,i]==1)),i] <- 1
}

models <- list()
models[[paste0("~",1)]] <- as.formula(paste0("~",1))

for(i in 1:7){
  models[[paste0(m[i])]] <- as.formula(paste0("~",m[i]))
}

index <- which(tab==0, arr.ind=TRUE)

for(i in 1 : nrow(index)){
  models[[paste0(rownames(tab)[index[i,1]],"+",colnames(tab)[index[i,2]])]] <- as.formula(paste0("~",rownames(tab)[index[i,1]],"+",colnames(tab)[index[i,2]]))
  }




library(unmarked)

#read in the sora observations
#sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
sora <- read.csv('~/Documents/data/2014_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
cov <- read.csv('~/Documents/data/2014_cov.csv', header=T)

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

sora <- sora[,2:31]
cutpt = as.numeric(c(0,1,2,3,4,5)) 
#Unmarked Data Frame
umf = unmarkedFrameGDS(y=sora, 
                       numPrimary=6,
                       siteCovs = cov,
                       survey="line", 
                       dist.breaks=cutpt,  
                       unitsIn="m", 
                       tlength=cov$length
)



model <- list()

for(i in 1:length(models)){
  
  modelsnames <- names(models)
model[[modelsnames[[i]]]] = gdistsamp(lambdaformula = models[[i]], 
                       phiformula = ~1, 
                       pformula = ~1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
}
