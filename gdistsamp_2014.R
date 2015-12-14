# m <- c("region","scale_averagewater","scale_pe","scale_short","scale_int")
# 
# tab <- table(m,m)
# 
# for(i in 1:length(m)){
#   tab[(1:which(tab[,i]==1)),i] <- 1
# }
# 
# models <- list()
# models[[paste0("~",1)]] <- as.formula(paste0("~",1))
# 
# for(i in 1:length(m)){
#   models[[paste0(m[i])]] <- as.formula(paste0("~",m[i]))
# }
# 
# index <- which(tab==0, arr.ind=TRUE)
# 
# for(i in 1 : nrow(index)){
#   models[[paste0(rownames(tab)[index[i,1]],"+",colnames(tab)[index[i,2]])]] <- as.formula(paste0("~",rownames(tab)[index[i,1]],"+",colnames(tab)[index[i,2]]))
#   }

models <- c("~1","~region","~scale_averagewater","~scale_averagewater+scale_averagewater2","~scale_short","~scale_short+scale_short2","~scale_int","~scale_int+scale_int2","~region+scale_averagewater-1","~region+scale_short","~region+scale_int","~scale_averagewater+scale_short","~scale_averagewater+scale_averagewater2+scale_short","~scale_averagewater+scale_short+scale_short2","~scale_averagewater+scale_int","~scale_averagewater+scale_averagewater2+scale_int","~scale_averagewater+scale_int+scale_int2","~scale_short+scale_int","~scale_short+scale_short2+scale_int","~scale_short+scale_int+scale_int2","~region+scale_averagewater+scale_short+scale_int+scale_short2+scale_int2+scale_averagewater2-1")


library(unmarked)


#sora <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_sora.csv', header=T)
sora <- read.csv('~/data/2014_sora.csv', header=T)

#cov <- read.csv('C:/Users/avanderlaar/Documents/GitHub/data/2012_cov.csv', header=T)
cov <- read.csv('~/data/2014_cov.csv', header=T)

sora <- sora[order(sora$impound),]
cov <- cov[order(cov$impound),]

cov$scale_short2 <- cov$scale_short^2
cov$scale_int2 <- cov$scale_int^2
cov$scale_averagewater2 <- cov$scale_averagewater^2

sora <- sora[,2:31]
cutpt = as.numeric(c(0,1,2,3,4,5)) 

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
model[[models[[i]]]] = gdistsamp(lambdaformula = as.formula(models[i]), 
                       phiformula = ~1, 
                       pformula = ~1,
                       data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")
print(models[[i]])
}


model$global <- gdistsamp(lambdaformula = ~scale_short+scale_averagewater+region+scale_int+scale_pe, 
                      phiformula = ~1, 
                      pformula = ~ 1,
                      data = umf, keyfun = "hazard", mixture="NB",se = T, output="abund")

save(model, file="~/data/2014_models.Rdata")
list  = fitList(model)
model = modSel(list)
model