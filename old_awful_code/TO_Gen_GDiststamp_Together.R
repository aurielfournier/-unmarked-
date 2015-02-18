#generating the input files for gdistsamp analyis

library(reshape)

setwd("C:/Users/avanderlaar/Dropbox/r/Distance")

#############################################################
###########                   Sora
############################################################


#2012 birds
birds12all = read.csv("Birds2012.csv", header=T)
birds13all = read.csv("Birds2013.csv", header=T)

birds12 = subset(birds12all, spp=="s")
birds13 = subset(birds13all, spp=="s")
birds13 = subset(birds13, nighta!="g") #taking out these nights becuase they were extra and are f*ing things up
birds13 = subset(birds13, nighta!="h") #these too, stupid extra survey nights

cutpt = seq(0,12,1)

#first the data has to be binned. We use 12 bins 0-1, 1-2, etc etc since our farthest observation is a 12
birds12$bin1 = ifelse(birds12$distance<1,1,0)
birds12$bin2 = ifelse(birds12$distance<2,ifelse(birds12$distance>.9,1,0),0)
birds12$bin3 = ifelse(birds12$distance<3,ifelse(birds12$distance>1.9,1,0),0)
birds12$bin4 = ifelse(birds12$distance<4,ifelse(birds12$distance>2.9,1,0),0)
birds12$bin5 = ifelse(birds12$distance<5,ifelse(birds12$distance>3.9,1,0),0)
birds12$bin6 = ifelse(birds12$distance<6,ifelse(birds12$distance>4.9,1,0),0)
birds12$bin7 = ifelse(birds12$distance<7,ifelse(birds12$distance>5.9,1,0),0)
birds12$bin8 = ifelse(birds12$distance<8,ifelse(birds12$distance>6.9,1,0),0)
birds12$bin9 = ifelse(birds12$distance<9,ifelse(birds12$distance>7.9,1,0),0)
birds12$bin10 = ifelse(birds12$distance<10,ifelse(birds12$distance>8.9,1,0),0)
birds12$bin11 = ifelse(birds12$distance<11,ifelse(birds12$distance>9.9,1,0),0)
birds12$bin12 = ifelse(birds12$distance<12,ifelse(birds12$distance>10.9,1,0),0)

birds13$bin1 = ifelse(birds13$distance<1,1,0)
birds13$bin2 = ifelse(birds13$distance<2,ifelse(birds13$distance>.9,1,0),0)
birds13$bin3 = ifelse(birds13$distance<3,ifelse(birds13$distance>1.9,1,0),0)
birds13$bin4 = ifelse(birds13$distance<4,ifelse(birds13$distance>2.9,1,0),0)
birds13$bin5 = ifelse(birds13$distance<5,ifelse(birds13$distance>3.9,1,0),0)
birds13$bin6 = ifelse(birds13$distance<6,ifelse(birds13$distance>4.9,1,0),0)
birds13$bin7 = ifelse(birds13$distance<7,ifelse(birds13$distance>5.9,1,0),0)
birds13$bin8 = ifelse(birds13$distance<8,ifelse(birds13$distance>6.9,1,0),0)
birds13$bin9 = ifelse(birds13$distance<9,ifelse(birds13$distance>7.9,1,0),0)
birds13$bin10 = ifelse(birds13$distance<10,ifelse(birds13$distance>8.9,1,0),0)
birds13$bin11 = ifelse(birds13$distance<11,ifelse(birds13$distance>9.9,1,0),0)
birds13$bin12 = ifelse(birds13$distance<12,ifelse(birds13$distance>10.9,1,0),0)

#2012 
r22012 = subset(birds12, round==2)
r22012 = r22012[,colnames(r22012)!=("spp")]
r22012 = r22012[,colnames(r22012)!=("id")]
r22012 = r22012[,colnames(r22012)!=("tech")]
r22012 = r22012[,colnames(r22012)!=("round")]
r22012 = r22012[,colnames(r22012)!=("impound")]
r22012 = r22012[,colnames(r22012)!=("lat")]
r22012 = r22012[,colnames(r22012)!=("long")]
r22012 = r22012[,colnames(r22012)!=("night")]
r22012 = r22012[,colnames(r22012)!=("distance")]
r22012 = r22012[,colnames(r22012)!=("daten")]
r22012 = r22012[,colnames(r22012)!=("date")]
r22012 = r22012[,colnames(r22012)!=("nighta")]
meltb122 = melt(r22012)
castb122 = cast(meltb122, impounda ~ variable | night_two, sum)
mergeb122 = merge(castb122$'1a', castb122$'2a', by="impounda", all=T)
mergeb122 = merge(mergeb122, castb122$c, by="impounda", all=T)


r32012 = subset(birds12, round==3)
r32012 = r32012[,colnames(r32012)!=("spp")]
r32012 = r32012[,colnames(r32012)!=("id")]
r32012 = r32012[,colnames(r32012)!=("tech")]
r32012 = r32012[,colnames(r32012)!=("round")]
r32012 = r32012[,colnames(r32012)!=("impound")]
r32012 = r32012[,colnames(r32012)!=("lat")]
r32012 = r32012[,colnames(r32012)!=("long")]
r32012 = r32012[,colnames(r32012)!=("night")]
r32012 = r32012[,colnames(r32012)!=("nighta")]
r32012 = r32012[,colnames(r32012)!=("distance")]
r32012 = r32012[,colnames(r32012)!=("daten")]
r32012 = r32012[,colnames(r32012)!=("date")]
meltb123 = melt(r32012)
castb123 = cast(meltb123, impounda ~ variable | night_two, sum)

mergeb123 = merge(castb123$'1a', castb123$'2a', by="impounda", all=T)
mergeb123 = merge(mergeb123, castb123$c, by="impounda", all=T)

#merge together the 2012 data into ONE GIANT DATA 
birds2012 = merge(mergeb122, mergeb123, by="impounda", all=T)


#2013 
r12013 = subset(birds13, round==1)
r22013 = subset(birds13, round==2)
r32013 = subset(birds13, round==3)
r42013 = subset(birds13, round==4)

###########
# Round 1
#########3
r12013 = r12013[,colnames(r12013)!=("name")]
r12013 = r12013[,colnames(r12013)!=("canwrnum")]
r12013 = r12013[,colnames(r12013)!=("spp")]
r12013 = r12013[,colnames(r12013)!=("id")]
r12013 = r12013[,colnames(r12013)!=("tech")]
r12013 = r12013[,colnames(r12013)!=("round")]
r12013 = r12013[,colnames(r12013)!=("impound")]
r12013 = r12013[,colnames(r12013)!=("lat")]
r12013 = r12013[,colnames(r12013)!=("long")]
r12013 = r12013[,colnames(r12013)!=("night")]
r12013 = r12013[,colnames(r12013)!=("distance")]
r12013 = r12013[,colnames(r12013)!=("daten")]
r12013 = r12013[,colnames(r12013)!=("date")]
r12013 = r12013[,colnames(r12013)!=("obsever")]
r12013 = r12013[,colnames(r12013)!=("year")]
r12013 = r12013[,colnames(r12013)!=("impoundnum")]
r12013 = r12013[,colnames(r12013)!=("regionnum")]
meltb131 = melt(r12013)
castb131 = cast(meltb131, impounda ~ variable | nighta, sum)

merge131a = merge(castb131$a, castb131$b, by="impounda", all=T, suffixes=c("a", "b"))
merge131b = merge(merge131a, castb131$c, by="impounda", all=T, suffixes=c("b", "c"))
merge131c = merge(merge131b, castb131$d, by="impounda", all=T, suffixes=c("c", "d"))
merge131d = merge(merge131c, castb131$e, by="impounda", all=T, suffixes=c("d", "e"))
merge131 = merge(merge131d, castb131$f, by="impounda", all=T, suffixes=c("e", "f"))

##########
# Round 2
###########
r22013 = r22013[,colnames(r22013)!=("name")]
r22013 = r22013[,colnames(r22013)!=("canwrnum")]
r22013 = r22013[,colnames(r22013)!=("spp")]
r22013 = r22013[,colnames(r22013)!=("id")]
r22013 = r22013[,colnames(r22013)!=("tech")]
r22013 = r22013[,colnames(r22013)!=("round")]
r22013 = r22013[,colnames(r22013)!=("impound")]
r22013 = r22013[,colnames(r22013)!=("lat")]
r22013 = r22013[,colnames(r22013)!=("long")]
r22013 = r22013[,colnames(r22013)!=("night")]
r22013 = r22013[,colnames(r22013)!=("distance")]
r22013 = r22013[,colnames(r22013)!=("daten")]
r22013 = r22013[,colnames(r22013)!=("date")]
r22013 = r22013[,colnames(r22013)!=("obsever")]
r22013 = r22013[,colnames(r22013)!=("year")]
r22013 = r22013[,colnames(r22013)!=("impoundnum")]
r22013 = r22013[,colnames(r22013)!=("regionnum")]
meltb132 = melt(r22013)
castb132 = cast(meltb132, impounda ~ variable | nighta, sum)
merge132a = merge(castb132$a, castb132$b, by="impounda", all=T, suffixes=c("a", "b"))
merge132b = merge(merge132a, castb132$c, by="impounda", all=T, suffixes=c("b", "c"))
merge132c = merge(merge132b, castb132$d, by="impounda", all=T, suffixes=c("c", "d"))
merge132d = merge(merge132c, castb132$e, by="impounda", all=T, suffixes=c("d", "e"))
merge132 = merge(merge132d, castb132$f, by="impounda", all=T, suffixes=c("e", "f"))


##########
# Round 3
###########
r32013 = r32013[,colnames(r32013)!=("name")]
r32013 = r32013[,colnames(r32013)!=("canwrnum")]
r32013 = r32013[,colnames(r32013)!=("spp")]
r32013 = r32013[,colnames(r32013)!=("id")]
r32013 = r32013[,colnames(r32013)!=("tech")]
r32013 = r32013[,colnames(r32013)!=("round")]
r32013 = r32013[,colnames(r32013)!=("impound")]
r32013 = r32013[,colnames(r32013)!=("lat")]
r32013 = r32013[,colnames(r32013)!=("long")]
r32013 = r32013[,colnames(r32013)!=("night")]
r32013 = r32013[,colnames(r32013)!=("distance")]
r32013 = r32013[,colnames(r32013)!=("daten")]
r32013 = r32013[,colnames(r32013)!=("date")]
r32013 = r32013[,colnames(r32013)!=("obsever")]
r32013 = r32013[,colnames(r32013)!=("year")]
r32013 = r32013[,colnames(r32013)!=("impoundnum")]
r32013 = r32013[,colnames(r32013)!=("regionnum")]
meltb133 = melt(r32013)
castb133 = cast(meltb133, impounda ~ variable | nighta, sum)
merge133a = merge(castb133$a, castb133$b, by="impounda", all=T, suffixes=c("a", "b"))
merge133b = merge(merge133a, castb133$c, by="impounda", all=T, suffixes=c("b", "c"))
merge133c = merge(merge133b, castb133$d, by="impounda", all=T, suffixes=c("c", "d"))
merge133d = merge(merge133c, castb133$e, by="impounda", all=T, suffixes=c("d", "e"))
merge133 = merge(merge133d, castb133$f, by="impounda", all=T, suffixes=c("e", "f"))

##########
# Round 4
###########

r42013 = r42013[,colnames(r42013)!=("name")]
r42013 = r42013[,colnames(r42013)!=("canwrnum")]
r42013 = r42013[,colnames(r42013)!=("spp")]
r42013 = r42013[,colnames(r42013)!=("id")]
r42013 = r42013[,colnames(r42013)!=("tech")]
r42013 = r42013[,colnames(r42013)!=("round")]
r42013 = r42013[,colnames(r42013)!=("impound")]
r42013 = r42013[,colnames(r42013)!=("lat")]
r42013 = r42013[,colnames(r42013)!=("long")]
r42013 = r42013[,colnames(r42013)!=("night")]
r42013 = r42013[,colnames(r42013)!=("distance")]
r42013 = r42013[,colnames(r42013)!=("daten")]
r42013 = r42013[,colnames(r42013)!=("date")]
r42013 = r42013[,colnames(r42013)!=("obsever")]
r42013 = r42013[,colnames(r42013)!=("year")]
r42013 = r42013[,colnames(r42013)!=("impoundnum")]
r42013 = r42013[,colnames(r42013)!=("regionnum")]

meltb134 = melt(r42013)
castb134 = cast(meltb134, impounda ~ variable | nighta, sum)
merge134a = merge(castb134$a, castb134$c, by="impounda", all=T, suffixes=c("a", "c"))
merge134d = merge(merge134a, castb134$e, by="impounda", all=T, suffixes=c("c", "e"))
merge134 = merge(merge134d, castb134$f, by="impounda", all=T, suffixes=c("e", "f"))

###### 2013 ALL TOGETHER

birds2013a = merge(merge131,   merge132, by="impounda", all=T)
birds2013b = merge(merge133, merge134, by="impounda", all=T)
birds2013 = merge(birds2013a, birds2013b, by="impounda", all=T)




########################################################
##### Site Covariates (Things which are the same across time)
#########################################################

#read in the data
scov12 = read.csv("2012_Veg.csv", header=T)
scov13 = read.csv("2013_veg.csv", header=T) 

### 2012 ###
scov12sub = scov12[,c("region", "round",  "habtype","dua", "dista", "impounda", "canwr", "region", "effortm","waterd","int")]
#drop the upland sites, there aren't enough of them
scov12 = subset(scov12sub, scov12sub$habtype=="ms"|scov12sub$habtype=="pe")
meltscov12 = melt(scov12)
castscov12 = cast(meltscov12, impounda + habtype+dua+dista+region~ variable, mean)

### 2013 ###
scov13sub = scov13[,c("habtype", "effortm", "region","round","impounda", "canwr", "dua", "dista", "effortm", "waterd","int")]
scov13 = subset(scov13sub, scov13sub$habtype=="ms"|scov13sub$habtype=="pe") #takes out upland, because there aren't enough of them
meltscov13 = melt(scov13)
castscov13 = cast(meltscov13, impounda + habtype + dua+ dista+ region ~ variable, mean)

########################################################
##### Obs Covariates (Things which change with each night of survey across time)
#########################################################

#read in the data
ocov12 = read.csv("2012_Veg.csv", header=T)
ocov12 = subset(ocov12, ocov12$habtype=="ms"|ocov12$habtype=="pe")
ocov13 = read.csv("2013_veg.csv", header=T)
ocov13 = subset(ocov13, ocov13$habtype=="ms"|ocov13$habtype=="pe")

#2012
ocov122 = subset(ocov12, round==2)
ocov123 = subset(ocov12, round==3)

meltocov122 = melt(ocov122)
castocov122 = cast(meltocov122, impounda ~ variable, mean)
castocov122 = castocov122[,c("impounda", "effortm", "int", "short", "tall", "up", "water", "waterp", "wood", "bg", "crop","other","waterd")]

meltocov123 = melt(ocov123)
castocov123 = cast(meltocov123, impounda ~ variable, mean)
castocov123 = castocov123[,c("impounda", "effortm", "int", "short", "tall", "up", "water", "waterp", "wood", "bg", "crop","other","waterd")]


#2013
ocov131 = subset(ocov13, round==1)

ocov132 = subset(ocov13, round==2)

ocov133 = subset(ocov13, round==3)

ocov134 = subset(ocov13, round==4)


meltocov131 = melt(ocov131)
castocov131 = cast(meltocov131, impounda ~ variable, mean)
castocov131 = castocov131[,c("impounda", "effortm", "int", "short", "pe", "tall", "up", "water", "waterp", "wood", "bg", "crop","other","waterd")]

meltocov132 = melt(ocov132)
castocov132 = cast(meltocov132, impounda ~ variable, mean)
castocov132 = castocov132[,c("impounda", "effortm", "int", "short", "pe", "tall", "up", "water", "waterp", "wood", "bg", "crop","other","waterd")]

meltocov133 = melt(ocov133)
castocov133 = cast(meltocov133, impounda ~ variable, mean)
castocov133 = castocov133[,c("impounda", "effortm", "int", "short", "pe", "tall", "up", "water", "waterp", "wood", "bg", "crop","other","waterd")]

meltocov134 = melt(ocov134)
castocov134 = cast(meltocov134, impounda ~ variable, mean)
castocov134 = castocov134[,c("impounda", "effortm", "int", "short", "pe", "tall", "up", "water", "waterp", "wood", "bg", "crop","other","waterd")]

##############
# Merging
##############

#2012

merge12 = merge(ocov122, ocov123, by="impounda", all=T, suffixes=c("_2", "_3"))

#2013
merge131_132 = merge(castocov131, castocov132, by="impounda", all=T, suffixes=c("_1", "_2"))
merge133_134 = merge(castocov133, castocov134, by="impounda", all=T, suffixes=c("_3", "_4"))
merge13 = merge(merge131_132, merge133_134, by="impounda", all=T)

#short
short12 = merge12[,c("impounda","short_2", "short_2","short_2", "short_3","short_3", "short_3")]
meltshort12 = melt(short12)
castshort12 = cast(meltshort12, impounda~variable, mean)
short13 = merge13[,c("impounda", "short_1", "short_1", "short_1", "short_1","short_1", "short_1","short_2", "short_2","short_2", "short_2","short_2", "short_2","short_3", "short_3","short_3", "short_3","short_3", "short_3","short_4","short_4","short_4","short_4")]
meltshort13 = melt(short13)
castshort13 = cast(meltshort13, impounda~variable, mean)


#pe
pe13 = merge13[,c("impounda", "pe_1", "pe_1", "pe_1", "pe_1","pe_1", "pe_1","pe_2", "pe_2","pe_2", "pe_2","pe_2", "pe_2","pe_3", "pe_3","pe_3", "pe_3","pe_3", "pe_3","pe_4","pe_4","pe_4","pe_4")]
meltpe13 = melt(pe13)
castpe13 = cast(meltpe13, impounda~variable, mean)


#tall
tall12 = merge12[,c("impounda", "tall_2", "tall_2","tall_2", "tall_3","tall_3", "tall_3")]
melttall12 = melt(tall12)
casttall12 = cast(melttall12, impounda~variable, mean)
tall13 = merge13[,c("impounda", "tall_1", "tall_1", "tall_1", "tall_1","tall_1", "tall_1","tall_2", "tall_2","tall_2", "tall_2","tall_2", "tall_2","tall_3", "tall_3","tall_3", "tall_3","tall_3", "tall_3","tall_4","tall_4","tall_4","tall_4")]
melttall13 = melt(tall13)
casttall13 = cast(melttall13, impounda~variable, mean)


#up
up12 = merge12[,c("impounda", "up_2","up_2", "up_2","up_3","up_3", "up_3")]
meltup12 = melt(up12)
castup12 = cast(meltup12, impounda~variable, mean)
up13 = merge13[,c("impounda", "up_1", "up_1", "up_1", "up_1","up_1", "up_1","up_2", "up_2","up_2", "up_2","up_2", "up_2","up_3", "up_3","up_3", "up_3","up_3", "up_3","up_4","up_4","up_4","up_4")]
meltup13 = melt(up13)
castup13 = cast(meltup13, impounda~variable, mean)


#int
int12 = merge12[,c("impounda", "int_2", "int_2","int_2", "int_3","int_3", "int_3")]
meltint12 = melt(int12)
castint12 = cast(meltint12, impounda~variable, mean)
int13 = merge13[,c("impounda", "int_1", "int_1", "int_1", "int_1","int_1", "int_1","int_2", "int_2","int_2", "int_2","int_2", "int_2","int_3", "int_3","int_3", "int_3","int_3", "int_3","int_4","int_4","int_4","int_4")]
meltint13 = melt(int13)
castint13 = cast(meltint13, impounda~variable, mean)


#waterd
waterd12 = merge12[,c("impounda", "waterd_2","waterd_2", "waterd_2","waterd_3", "waterd_3","waterd_3")]
meltwaterd12 = melt(waterd12)
castwaterd12 = cast(meltwaterd12, impounda~variable, mean)
waterd13 = merge13[,c("impounda", "waterd_1", "waterd_1", "waterd_1", "waterd_1","waterd_1", "waterd_1","waterd_2", "waterd_2","waterd_2", "waterd_2","waterd_2", "waterd_2","waterd_3", "waterd_3","waterd_3", "waterd_3","waterd_3", "waterd_3","waterd_4","waterd_4","waterd_4","waterd_4")]
meltwaterd13 = melt(waterd13)
castwaterd13 = cast(meltwaterd13, impounda~variable, mean)


#waterp
waterp12 = merge12[,c("impounda", "waterp_2","waterp_2", "waterp_2","waterp_3", "waterp_3","waterp_3")]
meltwaterp12 = melt(waterp12)
castwaterp12 = cast(meltwaterp12, impounda~variable, mean)
waterp13 = merge13[,c("impounda", "waterp_1", "waterp_1", "waterp_1", "waterp_1","waterp_1", "waterp_1","waterp_2", "waterp_2","waterp_2", "waterp_2","waterp_2", "waterp_2","waterp_3", "waterp_3","waterp_3", "waterp_3","waterp_3", "waterp_3","waterp_4","waterp_4","waterp_4","waterp_4")]
meltwaterp13 = melt(waterp13)
castwaterp13 = cast(meltwaterp13, impounda~variable, mean)


#wood
wood12 = merge12[,c("impounda", "wood_2","wood_2", "wood_2","wood_3", "wood_3","wood_3" )]
meltwood12 = melt(wood12)
castwood12 = cast(meltwood12, impounda~variable, mean)
wood13 = merge13[,c("impounda", "wood_2","wood_2", "wood_2","wood_3", "wood_3","wood_3")]
meltwood13 = melt(wood13)
castwood13 = cast(meltwood13, impounda~variable, mean)


#crop
crop12 = merge12[,c("impounda", "crop_2","crop_2", "crop_2","crop_3", "crop_3","crop_3")]
meltcrop12 = melt(crop12)
castcrop12 = cast(meltcrop12, impounda~variable, mean)
crop13 = merge13[,c("impounda", "crop_1", "crop_1", "crop_1", "crop_1","crop_1", "crop_1","crop_2", "crop_2","crop_2", "crop_2","crop_2", "crop_2","crop_3", "crop_3","crop_3", "crop_3","crop_3", "crop_3","crop_4","crop_4","crop_4","crop_4")]
meltcrop13 = melt(crop13)
castcrop13 = cast(meltcrop13, impounda~variable, mean)

#date

#############
# 3 nights
############
dates122 = subset(birds12, round=="2")
dates122 = dates122[,c("impounda", "daten", "nighta")]
dates123 = subset(birds12, round=="3")
dates123 = dates123[,c("impounda", "daten", "nighta")]

dates131 = subset(birds13, round=="1")
dates131 = dates131[,c("impounda", "daten", "nighta")]
dates132 = subset(birds13, round=="2")
dates132 = dates132[,c("impounda", "daten", "nighta")]
dates133 = subset(birds13, round=="3")
dates133 = dates133[,c("impounda", "daten", "nighta")]
dates134 = subset(birds13, round=="4")
dates134 = dates134[,c("impounda", "daten", "nighta")]


meltdates122 = melt(dates122)
meltdates123 = melt(dates123)
meltdates131 = melt(dates131)
meltdates132 = melt(dates132)
meltdates133 = melt(dates133)
meltdates134 = melt(dates134)

castdates122 = cast(meltdates122, impounda ~ nighta, max)
castdates122[castdates122<=0]<-NA
castdates123 = cast(meltdates123, impounda ~ nighta, max)
castdates123[castdates123<=0]<-NA

castdates131 = cast(meltdates131, impounda ~ nighta, max)
castdates131[castdates131<=0]<-NA
castdates132 = cast(meltdates132, impounda ~ nighta, max)
castdates132[castdates132<=0]<-NA
castdates133 = cast(meltdates133, impounda ~ nighta, max)
castdates133[castdates133<=0]<-NA
castdates134 = cast(meltdates134, impounda ~ nighta, max)
castdates134[castdates134<=0]<-NA
#this will throw an error about not meaningful for factors, ignore this, it's mad about the impounda

date12 = merge(castdates122, castdates123, by="impounda", all=T)

date13_a = merge(castdates131, castdates132, by="impounda", all=T, suffixes=c("_1","_2"))
date13_b = merge(castdates133, castdates134, by="impounda", all=T, suffixes=c("_3","_4"))
date13 = merge(date13_a, date13_b, by="impounda", all=T)

#########
# 2 Nights
#############

dates122 = subset(birds12, round=="2")
dates122 = dates122[,c("impounda", "daten", "night_two")]
dates123 = subset(birds12, round=="3")
dates123 = dates123[,c("impounda", "daten", "night_two")]

dates131 = subset(birds13, round=="1")
dates131 = dates131[,c("impounda", "daten", "nighta")]
dates132 = subset(birds13, round=="2")
dates132 = dates132[,c("impounda", "daten", "nighta")]
dates133 = subset(birds13, round=="3")
dates133 = dates133[,c("impounda", "daten", "nighta")]
dates134 = subset(birds13, round=="4")
dates134 = dates134[,c("impounda", "daten", "nighta")]

meltdates122 = melt(dates122)
meltdates123 = melt(dates123)
meltdates131 = melt(dates131)
meltdates132 = melt(dates132)
meltdates133 = melt(dates133)
meltdates134 = melt(dates134)

castdates122 = cast(meltdates122, impounda ~ night_two, max)
castdates122[castdates122<=0]<-NA
castdates123 = cast(meltdates123, impounda ~ night_two, max)
castdates123[castdates123<=0]<-NA

castdates131 = cast(meltdates131, impounda ~ nighta, max)
castdates131[castdates131<=0]<-NA
castdates132 = cast(meltdates132, impounda ~ nighta, max)
castdates132[castdates132<=0]<-NA
castdates133 = cast(meltdates133, impounda ~ nighta, max)
castdates133[castdates133<=0]<-NA
castdates134 = cast(meltdates134, impounda ~ nighta, max)
castdates134[castdates134<=0]<-NA
#this will throw an error about not meaningful for factors, ignore this, it's mad about the impounda

date12 = merge(castdates122, castdates123, by="impounda", all=T)

date13_a = merge(castdates131, castdates132, by="impounda", all=T, suffixes=c("_1","_2"))
date13_b = merge(castdates133, castdates134, by="impounda", all=T, suffixes=c("_3","_4"))
date13 = merge(date13_a, date13_b, by="impounda", all=T)


########################################################
##### Condense and Write the Output Files
#########################################################

####Create Lists of Impoundments with birds & covs
#List of what is in the Sora files
vi12 = data.frame("impounda"=birds2012$impounda,1)
vi13 = data.frame("impounda"=birds2013$impounda,1)

#list of what is in the cov files
covi12 = data.frame("impounda"=castscov12$impounda,1)
covi13 = data.frame("impounda"=castscov13$impounda,1)

id12 = merge(vi12, covi12, by="impounda")

id13 = merge(vi13, covi13, by="impounda")

#####################
#create sora files
####################

mmerge12 = merge(id12,birds2012,  by="impounda", all.x=F)
mmerge12 = mmerge12[,colnames(mmerge12)!=("X1.x")]
mmerge12 = mmerge12[,colnames(mmerge12)!=("X1.y")]
write.csv(mmerge12, "2012sora.csv", row.names=F)

mmerge13 = merge(id13,birds2013,  by="impounda", all=F)
mmerge13 = mmerge13[,colnames(mmerge13)!=("X1.x")]
mmerge13 = mmerge13[,colnames(mmerge13)!=("X1.y")]
write.csv(mmerge13, "2013sora.csv", row.names=F)

##########################
#create across survey covariate files
#############################

mscov12 = merge(id12,castscov12,  by="impounda", all.x=F)
mscov12 = mscov12[,colnames(mscov12)!=("X1.x")]
mscov12 = mscov12[,colnames(mscov12)!=("X1.y")]
write.csv(mscov12, "2012cov.csv", row.names=F)

mscov13 = merge(id13,castscov13,  by="impounda",all.x=F)
mscov13 = mscov13[,colnames(mscov13)!=("X1.x")]
mscov13 = mscov13[,colnames(mscov13)!=("X1.y")]
write.csv(mscov13, "2013cov.csv", row.names=F)

######################
# create yearly (changign covariate) files
#########################

########
# 3 nights
#########
date12 = merge(id12, date12, by="impounda", all.x=F)
date12$a.x[which(is.na(date12$a.x))]<- max(date12$a.x, na.rm=T)
date12$b.x[which(is.na(date12$b.x))]<- max(date12$b.x, na.rm=T)
date12$c.x[which(is.na(date12$c.x))]<- max(date12$c.x, na.rm=T)
date12$a.y[which(is.na(date12$a.y))]<- max(date12$a.y, na.rm=T)
date12$b.y[which(is.na(date12$b.y))]<- max(date12$b.y, na.rm=T)
date12$c.y[which(is.na(date12$c.y))]<- max(date12$c.y, na.rm=T)
write.csv(date12, "date12.csv")

date13$a_1[which(is.na(date13$a_1))]<- max(date13$a_1, na.rm=T)
date13$b_1[which(is.na(date13$b_1))]<- max(date13$b_1, na.rm=T)
date13$c_1[which(is.na(date13$c_1))]<- max(date13$c_1, na.rm=T)
date13$d_1[which(is.na(date13$d_1))]<- max(date13$d_1, na.rm=T)
date13$e_1[which(is.na(date13$e_1))]<- max(date13$e_1, na.rm=T)
date13$f_1[which(is.na(date13$f_1))]<- max(date13$f_1, na.rm=T)
date13$a_2[which(is.na(date13$a_2))]<- max(date13$a_2, na.rm=T)
date13$b_2[which(is.na(date13$b_2))]<- max(date13$b_2, na.rm=T)
date13$c_2[which(is.na(date13$c_2))]<- max(date13$c_2, na.rm=T)
date13$d_2[which(is.na(date13$d_2))]<- max(date13$d_2, na.rm=T)
date13$e_2[which(is.na(date13$e_2))]<- max(date13$e_2, na.rm=T)
date13$f_2[which(is.na(date13$f_2))]<- max(date13$f_2, na.rm=T)
date13$a_3[which(is.na(date13$a_3))]<- max(date13$a_3, na.rm=T)
date13$b[which(is.na(date13$b))]<- max(date13$b, na.rm=T)
date13$c_3[which(is.na(date13$c_3))]<- max(date13$c_3, na.rm=T)
date13$d[which(is.na(date13$d))]<- max(date13$d, na.rm=T)
date13$e_3[which(is.na(date13$e_3))]<- max(date13$e_3, na.rm=T)
date13$f_3[which(is.na(date13$f_3))]<- max(date13$f_3, na.rm=T)
date13$a_4[which(is.na(date13$a_4))]<- max(date13$a_4, na.rm=T)
date13$c_4[which(is.na(date13$c_4))]<- max(date13$c_4, na.rm=T)
date13$e_4[which(is.na(date13$e_4))]<- max(date13$e_4, na.rm=T)
date13$f_4[which(is.na(date13$f_4))]<- max(date13$f_4, na.rm=T)
date13 = merge(id13, date13, by="impounda", all.x=F)

write.csv(date13, "date13.csv")


mwaterd12 = merge(id12, castwaterd12, by="impounda", all.x=F)
mwaterd13 = merge(id13, castwaterd13, by="impounda", all.x=F)
write.csv(mwaterd12, "waterd12.csv")
write.csv(mwaterd13, "waterd13.csv")

mcrop12 = merge(id12, castcrop12, by="impounda", all.x=F)
mcrop13 = merge(id13, castcrop13, by="impounda", all.x=F)
write.csv(mcrop12, "crop12.csv")
write.csv(mcrop13, "crop13.csv")

mwood12 = merge(id12, castwood12, by="impounda", all.x=F)
mwood13 = merge(id13, castwood13, by="impounda", all.x=F)
write.csv(mwood12, "woodp12.csv")
write.csv(mwood13, "woodp13.csv")

mwaterp12 = merge(id12, castwaterp13, by="impounda", all.x=F)
mwaterp13 = merge(id13, castwaterp13, by="impounda", all.x=F)
write.csv(mwaterp12, "waterp12.csv")
write.csv(mwaterp13, "waterp13.csv")

mup12 = merge(id12, castup12, by="impounda", all.x=F)
mup13 = merge(id13, castup13, by="impounda", all.x=F)
write.csv(mup12, "up12.csv")
write.csv(mup13, "up13,csv")

mtall12 = merge(id12, casttall12, by="impounda", all.x=F)
mtall13 = merge(id13, casttall13, by="impounda", all.x=F)
write.csv(mtall12, "tall12.csv")
write.csv(mtall13, "tall13.csv")

mint12 = merge(id12, castint12, by="impounda", all.x=F)
mint13 = merge(id13, castint13, by="impounda", all.x=F)
write.csv(mint12, "int12.csv")
write.csv(mint13, "int13.csv")

mshort12 = merge(id12, castshort12, by="impounda", all.x=F)
mshort13 = merge(id13, castshort13, by="impounda", all.x=F)
write.csv(mshort12, "short12.csv")
write.csv(mshort13, "short13.csv")

mpe13 = merge(id13, castpe13, by="impounda", all.x=F)
write.csv(mpe13, "pe13.csv")


###########
# 2 nights
###########

date12 = merge(id12, date12, by="impounda", all.x=F)
date12 = date12[,colnames(date12)!=("X1.x")]
date12 = date12[,colnames(date12)!=("X1.y")]
date12$'1a.x'[which(is.na(date12$'1a.x'))]<- max(date12$'1a.x', na.rm=T)
date12$'2a.x'[which(is.na(date12$'2a.x'))]<- max(date12$'2a.x', na.rm=T)
date12$'1a.y'[which(is.na(date12$'1a.y'))]<- max(date12$'1a.y', na.rm=T)
date12$'2a.y'[which(is.na(date12$'2a.y'))]<- max(date12$'2a.y', na.rm=T)
write.csv(date12, "date12.csv")

date13$a_1[which(is.na(date13$a_1))]<- max(date13$a_1, na.rm=T)
date13$b_1[which(is.na(date13$b_1))]<- max(date13$b_1, na.rm=T)
date13$c_1[which(is.na(date13$c_1))]<- max(date13$c_1, na.rm=T)
date13$d_1[which(is.na(date13$d_1))]<- max(date13$d_1, na.rm=T)
date13$e_1[which(is.na(date13$e_1))]<- max(date13$e_1, na.rm=T)
date13$f_1[which(is.na(date13$f_1))]<- max(date13$f_1, na.rm=T)
date13$a_2[which(is.na(date13$a_2))]<- max(date13$a_2, na.rm=T)
date13$b_2[which(is.na(date13$b_2))]<- max(date13$b_2, na.rm=T)
date13$c_2[which(is.na(date13$c_2))]<- max(date13$c_2, na.rm=T)
date13$d_2[which(is.na(date13$d_2))]<- max(date13$d_2, na.rm=T)
date13$e_2[which(is.na(date13$e_2))]<- max(date13$e_2, na.rm=T)
date13$f_2[which(is.na(date13$f_2))]<- max(date13$f_2, na.rm=T)
date13$a_3[which(is.na(date13$a_3))]<- max(date13$a_3, na.rm=T)
date13$b[which(is.na(date13$b))]<- max(date13$b, na.rm=T)
date13$c_3[which(is.na(date13$c_3))]<- max(date13$c_3, na.rm=T)
date13$d[which(is.na(date13$d))]<- max(date13$d, na.rm=T)
date13$e_3[which(is.na(date13$e_3))]<- max(date13$e_3, na.rm=T)
date13$f_3[which(is.na(date13$f_3))]<- max(date13$f_3, na.rm=T)
date13$a_4[which(is.na(date13$a_4))]<- max(date13$a_4, na.rm=T)
date13$c_4[which(is.na(date13$c_4))]<- max(date13$c_4, na.rm=T)
date13$e_4[which(is.na(date13$e_4))]<- max(date13$e_4, na.rm=T)
date13$f_4[which(is.na(date13$f_4))]<- max(date13$f_4, na.rm=T)
date13 = merge(id13, date13, by="impounda", all.x=F)

write.csv(date13, "date13.csv")
