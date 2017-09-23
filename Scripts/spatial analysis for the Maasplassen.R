

if(Sys.info()["user"] == "linde069"){
  dataPath <- "C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles"
}


#D:/Analyses/HedonicSpatialWQ/SurveyData


install.packages("rgdal")

library(rgdal)
Maas <- readOGR(dsn = "C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles", layer = "Maas")
Maas <- readOGR(dsn = "dataPath", layer = "Maas")
summarize(Maas$q95_0)
install.packages(c("JGR","Deducer","DeducerExtras"))
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_144') 
library(rJava)
library(JGR)
JGR()
library(Deducer)

descriptive.table(vars = d(q95_02, q95_03,q95_04, q96_01, q96_02, q96_03, q96_04,
                           q97_01, q97_02, q97_03,q97_04, q98_01, q98_02, q98_03, q98_04
                           ),data= Maas, 
                  func.names = c("Valid N"))

Maas$Y1995 <-  as.numeric(Maas$q95_02) + as.numeric(Maas$q95_03) + as.numeric(Maas$q95_04)
frequencies(Maas$Y1995)
Maas$Y1996 <-  as.numeric(Maas$q96_01) + as.numeric(Maas$q96_02) + as.numeric(Maas$q96_03) + as.numeric(Maas$q96_04)
frequencies(Maas$Y1996)
Maas$Y1997 <-  as.numeric(Maas$q97_01) + as.numeric(Maas$q97_02) + as.numeric(Maas$q97_03) + as.numeric(Maas$q97_04)
frequencies(Maas$Y1997)
Maas$Y1998 <-  as.numeric(Maas$q98_01) + as.numeric(Maas$q98_02) + as.numeric(Maas$q98_03) + as.numeric(Maas$q98_04)
frequencies(Maas$Y1998)
Maas$Y1999 <-  as.numeric(Maas$q99_01) + as.numeric(Maas$q99_02) + as.numeric(Maas$q99_03) + as.numeric(Maas$q99_04)
frequencies(Maas$Y1999)
Maas$Y2000 <-  as.numeric(Maas$q00_01) + as.numeric(Maas$q00_02) + as.numeric(Maas$q00_03) + as.numeric(Maas$q00_04)
frequencies(Maas$Y2000)

Maas$Y2001 <-  as.numeric(Maas$q01_01) + as.numeric(Maas$q01_02) + as.numeric(Maas$q01_03) + as.numeric(Maas$q01_04)
frequencies(Maas$Y2001)
Maas$Y2002 <-  as.numeric(Maas$q02_01) + as.numeric(Maas$q02_02) + as.numeric(Maas$q02_03) + as.numeric(Maas$q02_04)
frequencies(Maas$Y2002)
Maas$Y2003 <-  as.numeric(Maas$q03_01) + as.numeric(Maas$q03_02) + as.numeric(Maas$q03_03) + as.numeric(Maas$q03_04)
frequencies(Maas$Y2003)
Maas$Y2004 <-  as.numeric(Maas$q04_01) + as.numeric(Maas$q04_02) + as.numeric(Maas$q04_03) + as.numeric(Maas$q04_04)
frequencies(Maas$Y2004)
Maas$Y2005 <-  as.numeric(Maas$q05_01) + as.numeric(Maas$q05_02) + as.numeric(Maas$q05_03) 
frequencies(Maas$Y2005)

############################## START OF MODULE ###########################################################
## Constructing more simple variables for Year, Quarter and some variables defined as factor but 
## definitely should be mnumerical.
##########################################################################################################


Maas$Year <- 0
Maas$Year <- ifelse(Maas$q95_01=="1",1995,Maas$Year)
Maas$Year <- ifelse(Maas$q95_02=="1",1995,Maas$Year)
Maas$Year <- ifelse(Maas$q95_03=="1",1995,Maas$Year)
Maas$Year <- ifelse(Maas$q95_04=="1",1995,Maas$Year)

Maas$Year <- ifelse(Maas$q96_01=="1",1996,Maas$Year)
Maas$Year <- ifelse(Maas$q96_02=="1",1996,Maas$Year)
Maas$Year <- ifelse(Maas$q96_03=="1",1996,Maas$Year)
Maas$Year <- ifelse(Maas$q96_04=="1",1996,Maas$Year)

Maas$Year <- ifelse(Maas$q97_01=="1",1997,Maas$Year)
Maas$Year <- ifelse(Maas$q97_02=="1",1997,Maas$Year)
Maas$Year <- ifelse(Maas$q97_03=="1",1997,Maas$Year)
Maas$Year <- ifelse(Maas$q97_04=="1",1997,Maas$Year)

Maas$Year <- ifelse(Maas$q98_01=="1",1998,Maas$Year)
Maas$Year <- ifelse(Maas$q98_02=="1",1998,Maas$Year)
Maas$Year <- ifelse(Maas$q98_03=="1",1998,Maas$Year)
Maas$Year <- ifelse(Maas$q98_04=="1",1998,Maas$Year)

Maas$Year <- ifelse(Maas$q99_01=="1",1999,Maas$Year)
Maas$Year <- ifelse(Maas$q99_02=="1",1999,Maas$Year)
Maas$Year <- ifelse(Maas$q99_03=="1",1999,Maas$Year)
Maas$Year <- ifelse(Maas$q99_04=="1",1999,Maas$Year)

Maas$Year <- ifelse(Maas$q00_01=="1",2000,Maas$Year)
Maas$Year <- ifelse(Maas$q00_02=="1",2000,Maas$Year)
Maas$Year <- ifelse(Maas$q00_03=="1",2000,Maas$Year)
Maas$Year <- ifelse(Maas$q00_04=="1",2000,Maas$Year)

Maas$Year <- ifelse(Maas$q01_01=="1",2001,Maas$Year)
Maas$Year <- ifelse(Maas$q01_02=="1",2001,Maas$Year)
Maas$Year <- ifelse(Maas$q01_03=="1",2001,Maas$Year)
Maas$Year <- ifelse(Maas$q01_04=="1",2001,Maas$Year)

Maas$Year <- ifelse(Maas$q02_01=="1",2002,Maas$Year)
Maas$Year <- ifelse(Maas$q02_02=="1",2002,Maas$Year)
Maas$Year <- ifelse(Maas$q02_03=="1",2002,Maas$Year)
Maas$Year <- ifelse(Maas$q02_04=="1",2002,Maas$Year)

Maas$Year <- ifelse(Maas$q03_01=="1",2003,Maas$Year)
Maas$Year <- ifelse(Maas$q03_02=="1",2003,Maas$Year)
Maas$Year <- ifelse(Maas$q03_03=="1",2003,Maas$Year)
Maas$Year <- ifelse(Maas$q03_04=="1",2003,Maas$Year)

Maas$Year <- ifelse(Maas$q04_01=="1",2004,Maas$Year)
Maas$Year <- ifelse(Maas$q04_02=="1",2004,Maas$Year)
Maas$Year <- ifelse(Maas$q04_03=="1",2004,Maas$Year)
Maas$Year <- ifelse(Maas$q04_04=="1",2004,Maas$Year)

Maas$Year <- ifelse(Maas$q05_01=="1",2005,Maas$Year)
Maas$Year <- ifelse(Maas$q05_02=="1",2005,Maas$Year)
Maas$Year <- ifelse(Maas$q05_03=="1",2005,Maas$Year)
Maas$Year <- ifelse(Maas$q05_04=="1",2005,Maas$Year)

frequencies(Maas$Year)
hist(Maas$Year)


Maas$Quarter <- 200504
Maas$Quarter <- ifelse(Maas$q95_02=="1",199501,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q95_03=="1",199502,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q95_04=="1",199503,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q96_01=="1",199601,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q96_02=="1",199602,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q96_03=="1",199603,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q96_04=="1",199604,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q97_01=="1",199701,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q97_02=="1",199702,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q97_03=="1",199703,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q97_04=="1",199704,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q98_01=="1",199801,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q98_02=="1",199802,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q98_03=="1",199803,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q98_04=="1",199804,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q99_01=="1",199901,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q99_02=="1",199902,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q99_03=="1",199903,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q99_04=="1",199904,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q00_01=="1",200001,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q00_02=="1",200002,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q00_03=="1",200003,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q00_04=="1",200004,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q01_01=="1",200101,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q01_02=="1",200102,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q01_03=="1",200103,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q01_04=="1",200104,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q02_01=="1",200201,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q02_02=="1",200202,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q02_03=="1",200203,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q02_04=="1",200204,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q03_01=="1",200301,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q03_02=="1",200302,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q03_03=="1",200303,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q03_04=="1",200304,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q04_01=="1",200401,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q04_02=="1",200402,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q04_03=="1",200403,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q04_04=="1",200404,Maas$Quarter)

Maas$Quarter <- ifelse(Maas$q05_01=="1",200501,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q05_02=="1",200502,Maas$Quarter)
Maas$Quarter <- ifelse(Maas$q05_03=="1",200503,Maas$Quarter)
frequencies(Maas$Quarter)
hist(Maas$Quarter)

Maas$Quarter <- as.factor(Maas$Quarter)


frequencies(Maas$q95_02)
frequencies(Maas$q95_03)
frequencies(Maas$q95_04)

frequencies(Maas$q96_01)
frequencies(Maas$q96_02)
frequencies(Maas$q96_03)
frequencies(Maas$q96_04)

frequencies(Maas$q97_01)
frequencies(Maas$q97_02)
frequencies(Maas$q97_03)
frequencies(Maas$q97_04)

frequencies(Maas$q98_01)
frequencies(Maas$q98_02)
frequencies(Maas$q98_03)
frequencies(Maas$q98_04)

frequencies(Maas$q99_01)
frequencies(Maas$q99_02)
frequencies(Maas$q99_03)
frequencies(Maas$q99_04)

frequencies(Maas$q00_01)
frequencies(Maas$q00_02)
frequencies(Maas$q00_03)
frequencies(Maas$q00_04)

frequencies(Maas$q01_01)
frequencies(Maas$q01_02)
frequencies(Maas$q01_03)
frequencies(Maas$q01_04)

frequencies(Maas$q02_01)
frequencies(Maas$q02_02)
frequencies(Maas$q02_03)
frequencies(Maas$q02_04)

frequencies(Maas$q03_01)
frequencies(Maas$q03_02)
frequencies(Maas$q03_03)
frequencies(Maas$q03_04)

frequencies(Maas$q04_01)
frequencies(Maas$q04_02)
frequencies(Maas$q04_03)
frequencies(Maas$q04_04)

frequencies(Maas$q05_01)
frequencies(Maas$q05_02)
frequencies(Maas$q05_03)

summary(Maas$q95_02)

#OLS regression

Maas$bevdicht   <- as.numeric(Maas$bevdicht)
Maas$pnietact   <- as.numeric(Maas$pnietact)
Maas$geminkinko <- as.numeric(Maas$geminkinko)

Maas$raildist   <- as.numeric(Maas$raildist)
Maas$highdist   <- as.numeric(Maas$highdist)
Maas$dist_local <- as.numeric(Maas$dist_local)
Maas$dist_3a    <- as.numeric(Maas$dist_3a)
Maas$dist_4a    <- as.numeric(Maas$dist_4a)
Maas$dist_4d    <- as.numeric(Maas$dist_4d)
Maas$dist_4e    <- as.numeric(Maas$dist_4e)
Maas$dist_zwem  <- as.numeric(Maas$dist_zwem)

Maas$nbadk      <- as.numeric(Maas$nbadk)
Maas$isol       <- as.numeric(Maas$isol)

#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Maasplassen
## ro the limited degrees of freedom. 
##########################################################################################################

library(Deducer)
descriptive.table(vars = d(Maas$lnprice, Maas$lnwoonop, Maas$nbadk, Maas$isol,
                           Maas$bwpr1905, Maas$bwpr6070, Maas$bwpr7180, Maas$bwpr8190, 
                           Maas$bwpr9100, Maas$bevdicht, 
                           Maas$pnietact, Maas$geminkinko, Maas$raildist, Maas$highdist, Maas$dist_local,  
                           Maas$dist_4e, Maas$dist_3a , Maas$dist_4d, Maas$dist_zwem, Maas$chl, Maas$doo, Maas$ZM),
                  data= Maas, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))

frequencies(Maas$Quarter)
frequencies(Maas$inpandig)
frequencies(Maas$tuin_zow)
frequencies(Maas$buff25)

#run regressions in 2000 and 2005
#
#
subset<-subset(Maas, chl != -999)
subset<-subset(subset, doo != -999)
subset<-subset(subset, ZM != -999)
dist4e<-subset$dist_4e/1000000
subset$dist_4e <-dist4e
dist4d<-subset$dist_4d/1000000
subset$dist_4d <-dist4d
dist3a<-subset$dist_3a/1000000
subset$dist_3a <-dist3a
distzw<-subset$dist_zwem/1000000
subset$dist_zwem <-distzw
zm<-subset$ZM/1000000
subset$ZM <-zm
distlocal<-subset$dist_local/1000000
subset$dist_local <-distlocal
disthigh<-subset$highdist/1000000
subset$highdist <-disthigh
distrail<-subset$raildist/1000000
subset$raildist <-distrail


#subset1<- subset(subset,Year==2000)
#subset2<- subset(subset,Year==2005)
subset3<- subset(subset,Quarter==200001 | Quarter==200002|Quarter==200003|Quarter==200004)
subset4<- subset(subset,Quarter==200501 | Quarter==200502|Quarter==200503|Quarter==200504)

subset3$dist_3a
frequencies(subset$Year)

frequencies(subset$Quarter)
frequencies(subset1$buff25) 
frequencies(subset1$inpandig) 
frequencies(subset1$tuin_zow) 
frequencies(subset1$bwpr1905) 

frequencies(subset1$bwpr6070)
frequencies(subset1$bwpr7180)
frequencies(subset1$bwpr8190)
frequencies(subset1$bwpr9100)


  
OLSMaas1 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
                  data=subset3)

OLSMaas2 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist + highdist,
              data=subset3)
OLSMaas3 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist + highdist + dist_local ,
              data=subset3)
OLSMaas4 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                dist_4e + dist_3a + dist_4d + dist_zwem ,
              data=subset3)
OLSMaas5 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
              data=subset3)

stargazer(OLSMaas1, OLSMaas2, OLSMaas3, OLSMaas4,OLSMaas5, out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_OLS_2000.htm")

OLSMaas6 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
               data=subset4)

OLSMaas7 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko + raildist + highdist,
               data=subset4)
OLSMaas8 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko + raildist + highdist + dist_local ,
               data=subset4)
OLSMaas9 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                 dist_4e + dist_3a + dist_4d + dist_zwem ,
               data=subset4)
OLSMaas10 <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                 dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM,
               data=subset4)

stargazer(OLSMaas6, OLSMaas7, OLSMaas8, OLSMaas9,OLSMaas10, out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_OLS_2005.htm")
stargazer(OLSMaas1, OLSMaas6, OLSMaas2, OLSMaas7,OLSMaas3,OLSMaas8, OLSMaas4, OLSMaas9, OLSMaas5,OLSMaas10, out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_OLS_2001&2005.htm")

writeOGR(subset3, "C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles", "Maas2000", driver="ESRI Shapefile")
frequencies(subset$Quarter)
subset$Quarter   <- as.numeric(subset$Quarter)
hist(subset$Quarter)


#Spatial Analysis for Maas

centroids2000 <- coordinates(subset3)
#IDs<-row.names(as(centroids2000))
#plot(centroids2000,xlab="longitude",ylab="latitude")
#plot(subset3,add=TRUE)
#title("Maas2000 with centroids")
#plot(subset3,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Maas with number label")

#install.packages("spdep")
library(spdep)
#queen1 <- poly2nb(Maas,queen=FALSE)
#summary(queen1)
#wq1 <- nb2listw(queen1) # Not working because there are more than 5,000 observations without any neighbour!
#summary(wq1)
#plot.listw(wq1,centroids)
#title("Links in the queen matrix")

knearneigh1 <- knearneigh(centroids2000,k=4)
knn1 <- knn2nb(knearneigh1)
#plot(Maas, border="black")
#plot(knn1, centroids2000, add=TRUE)
#title(main="K nearest neighbours, k = 4")

knn1_W  <-nb2listw(knn1)
#linmod <- lm(form_base_3,nuts.dat)
#summary(linmod)
#install.packages(c("car","systemfit"),repo="http://cran.stat.ucla.edu",dep=TRUE)

#install.packages("car")

library("lmtest")


#lm.morantest(OLSMaas5, knn1_W)

lm <- lm.LMtests(OLSMaas5, knn1_W, zero.policy=TRUE, test="all")
Maaslag<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset3, knn1_W)


Maaserror<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset3, knn1_W)
#summary(Maaslag)

#try 10 neighbours

knearneigh10 <- knearneigh(centroids2000,k=10)
knn10 <- knn2nb(knearneigh10)
knn10_W  <-nb2listw(knn10)
lm10 <- lm.LMtests(OLSMaas5, knn10_W, zero.policy=TRUE, test="all")

Maas10nlag<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset3, knn10_W)


Maas10nerror<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                      + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                        pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                        dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset3, knn10_W)







centroids2005 <- coordinates(subset4)
#IDs<-row.names(as(centroids2005))
#plot(centroids2005,xlab="longitude",ylab="latitude")
#plot(subset4,add=TRUE)
#title("Maas2005 with centroids")
#plot(subset4,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Maas with number label")

#install.packages("spdep")
library(spdep)
#queen1 <- poly2nb(Maas,queen=FALSE)
#summary(queen1)
#wq1 <- nb2listw(queen1) # Not working because there are more than 5,000 observations without any neighbour!
#summary(wq1)
#plot.listw(wq1,centroids)
#title("Links in the queen matrix")

knearneigh12005<- knearneigh(centroids2005,k=4)
knn12005 <- knn2nb(knearneigh12005)
#plot(Maas, border="black")
#plot(knn1, centroids2005, add=TRUE)
#title(main="K nearest neighbours, k = 4")

knn1_W2005  <-nb2listw(knn12005)
#linmod <- lm(form_base_3,nuts.dat)
#summary(linmod)
#install.packages(c("car","systemfit"),repo="http://cran.stat.ucla.edu",dep=TRUE)

#install.packages("car")

library("lmtest")


#lm.morantest(OLSMaas5, knn1_W)



lm2005 <- lm.LMtests(OLSMaas10, knn1_W2005, zero.policy=TRUE, test="all")

Maaslag2<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                    dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset4, knn1_W2005)
Maaserror2<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                   + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                     pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                     dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset4, knn1_W2005)

# 10 neighbour for 2005
knearneigh102005 <- knearneigh(centroids2005,k=10)
knn102005 <- knn2nb(knearneigh102005)
knn10_W2005  <-nb2listw(knn102005)

lm200510n <- lm.LMtests(OLSMaas10, knn10_W2005, zero.policy=TRUE, test="all")

Maas10nlag2<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                   + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                     pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                     dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset4, knn10_W2005)
Maas10nerror2<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                       + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                         pnietact + geminkinko + raildist + highdist + dist_local + factor(buff25) + 
                         dist_4e + dist_3a + dist_4d + dist_zwem + chl + doo + ZM, data=subset4, knn10_W2005)


#summary(Maaslag)
library(stargazer)
stargazer(Maaslag,Maaserror,Maaslag2,Maaserror2, Maas10nlag,Maas10nerror,Maas10nlag2,Maas10nerror2,out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_LAG_2000&2005.txt")


lm
lm2005
lm10
lm200510n

frequencies(subset3$ZM)

frequencies(Maas$Year)
frequencies(subset$Year)
