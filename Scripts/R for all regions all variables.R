library(rgdal)
ALLREGION <- read.csv("C:/Users/Zhong Xiao Leng/Desktop/New folder/Newdata.csv")




#subdata<-subset(ALLREGION, select = c("obj_id","x","y","lnprice","q95_01","q95_02","q95_03","q95_04","q96_01","q96_02","q96_03","q96_04","q97_01","q97_02","q97_03","q97_04","q98_01","q98_02","q98_03","q98_04","q99_01","q99_02","q99_03","q99_04","q00_01","q00_02","q00_03","q00_04","q01_01","q01_02","q01_03","q01_04","q02_01","q02_02","q02_03","q02_04","q03_01","q03_02","q03_03","q03_04","q04_01","q04_02","q04_03","q04_04","q05_01","q05_02","q05_03","q05_04","lnwoonop","nbadk","inpandig","isol","tuin_zow","won_4","won_1","bwpr1905","bwpr6070","bwpr7180","bwpr8190","bwpr9100","bevdicht","pnietact","geminkinko","raildist","highdist","dist_local","buff25","dist_4e", "dist_3a","dist_4d","dist_4f","dist_zwem","chl","doo","ZM","Flev","Geld","Maas","Rijn","Vech"))



subset<-subset(ALLREGION, chl != -999)
subset<-subset(subset, doo != -999)
subset<-subset(subset, ZM != -999)


ALLREGION$Year <- 0
ALLREGION$Year <- ifelse(ALLREGION$q95_01=="1",1995,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q95_02=="1",1995,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q95_03=="1",1995,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q95_04=="1",1995,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q96_01=="1",1996,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q96_02=="1",1996,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q96_03=="1",1996,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q96_04=="1",1996,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q97_01=="1",1997,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q97_02=="1",1997,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q97_03=="1",1997,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q97_04=="1",1997,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q98_01=="1",1998,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q98_02=="1",1998,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q98_03=="1",1998,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q98_04=="1",1998,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q99_01=="1",1999,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q99_02=="1",1999,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q99_03=="1",1999,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q99_04=="1",1999,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q00_01=="1",2000,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q00_02=="1",2000,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q00_03=="1",2000,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q00_04=="1",2000,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q01_01=="1",2001,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q01_02=="1",2001,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q01_03=="1",2001,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q01_04=="1",2001,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q02_01=="1",2002,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q02_02=="1",2002,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q02_03=="1",2002,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q02_04=="1",2002,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q03_01=="1",2003,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q03_02=="1",2003,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q03_03=="1",2003,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q03_04=="1",2003,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q04_01=="1",2004,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q04_02=="1",2004,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q04_03=="1",2004,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q04_04=="1",2004,ALLREGION$Year)

ALLREGION$Year <- ifelse(ALLREGION$q05_01=="1",2005,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q05_02=="1",2005,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q05_03=="1",2005,ALLREGION$Year)
ALLREGION$Year <- ifelse(ALLREGION$q05_04=="1",2005,ALLREGION$Year)

frequencies(ALLREGION$Year)
frequencies(subset$Year)


Maas <- subset(ALLREGION, Maas==1)
Maassubset<-subset(Maas, chl != -999)
Maassubset<-subset(Maassubset, doo != -999)
Maassubset<-subset(Maassubset, ZM != -999)

Vech <- subset(ALLREGION, Vech==1)
Vechsubset<-subset(Vech, chl != -999)
Vechsubset<-subset(Vechsubset, doo != -999)
Vechsubset<-subset(Vechsubset, ZM != -999)

Rijn <- subset(ALLREGION, Rijn==1)
Rijnsubset<-subset(Rijn, chl != -999)
Rijnsubset<-subset(Rijnsubset, doo != -999)
Rijnsubset<-subset(Rijnsubset, ZM != -999)
Flev <- subset(ALLREGION, Flev==1)
Flevsubset<-subset(Flev, chl != -999)
Flevsubset<-subset(Flevsubset, doo != -999)
Flevsubset<-subset(Flevsubset, ZM != -999)

Geld <- subset(ALLREGION, Geld==1)
Geldsubset<-subset(Geld, chl != -999)
Geldsubset<-subset(Geldsubset, doo != -999)
Geldsubset<-subset(Geldsubset, ZM != -999)


frequencies(Maas$Year)
frequencies(Maassubset$Year)
frequencies(Vech$Year)
frequencies(Vechsubset$Year)
frequencies(Rijn$Year)
frequencies(Rijnsubset$Year)
frequencies(Geld$Year)
frequencies(Geldsubset$Year)
frequencies(Flev$Year)
frequencies(Flevsubset$Year)
