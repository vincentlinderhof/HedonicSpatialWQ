

if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/HedonicSpatialWQ/SurveyData"
  setwd("D:/Analyses/HedonicSpatialWQ")
}

#D:/Analyses/HedonicSpatialWQ/SurveyData

#install.packages("rgdal")
library(rgdal)
library(Deducer)

# Reading the data from a shapefile (all data for Maasplassen)
if(Sys.info()["user"] == "zhongxiaoleng"){
  Maas <- readOGR(dsn = "C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles", layer = "Maas")
}
if(Sys.info()["user"] == "linde069"){
  Maas <- readOGR(dsn = "D:/Analyses/HedonicSpatialWQ/SurveyData", layer = "newMaas")
}

# why do we need this Zhongzxiao?
if(Sys.info()["user"] == "zhongxiaoleng"){
  summary(Maas$q95_0)
  install.packages(c("JGR","Deducer","DeducerExtras"))
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_144') 
  library(rJava)
  library(JGR)
  JGR()
}

#Check on the data: cross-table for the year and the quarters of a year
table(Maas$jaar,Maas$quarter)
frequencies(Maas$jaar)
#VL Identifying the missing values for the water quality variables.
#VL Initially, the variables are factors but we will transform them into numerical variables

frequencies(Maas$chl)
frequencies(Maas$doo)
frequencies(Maas$ZM)
frequencies(Maas$geminkinko)
frequencies(Maas$pnietact)

Maas$Selvar     <- 0
Maas$Selvar     <- ifelse(as.numeric(Maas$chl)        == 98 , 1, Maas$Selvar)
Maas$Selvar     <- ifelse(as.numeric(Maas$doo)        == 110, 1, Maas$Selvar)
Maas$Selvar     <- ifelse(as.numeric(Maas$ZM)         == 169, 1, Maas$Selvar)
Maas$Selvar     <- ifelse(as.numeric(Maas$geminkinko) == 83 , 1, Maas$Selvar)
Maas$Selvar     <- ifelse(as.numeric(Maas$pnietact)   == 36 , 1, Maas$Selvar)

frequencies(Maas$Selvar)
#frequencies(Maas$Flev)


############################## START OF MODULE ###########################################################
## Redefining the type of variables (from factor to numerical). Some variables defined as factor should 
## definitely be mnumerical.
##########################################################################################################

frequencies(Maas$jaar)
frequencies(Maas$quarter)

# A set of variables are read in as factors but should be numerical variables
# Don't worry about the missing variable levels of these variables as they have been identified above
Maas$bevdicht   <- as.numeric(Maas$bevdicht)
Maas$pnietact   <- as.numeric(Maas$pnietact)
Maas$geminkinko <- as.numeric(Maas$geminkinko)

Maas$raildist   <- as.numeric(Maas$raildist)
Maas$highdist   <- as.numeric(Maas$highdist)
Maas$dist_local <- as.numeric(Maas$dist_local)
Maas$dist_3a    <- as.numeric(Maas$dist_3a)
Maas$dist_4f    <- as.numeric(Maas$dist_4f)
Maas$dist_4d    <- as.numeric(Maas$dist_4d)
Maas$dist_4e    <- as.numeric(Maas$dist_4e)
Maas$dist_zwem  <- as.numeric(Maas$dist_zwem)

Maas$nbadk      <- as.numeric(Maas$nbadk)
Maas$isol       <- as.numeric(Maas$isol)

#VL New: Also the water quality variables have to be redefined
Maas$chl       <- as.numeric(Maas$chl)
Maas$doo       <- as.numeric(Maas$doo)
Maas$ZM        <- as.numeric(Maas$ZM)

#New: create the Quarter from the variables "jaar" and "quarter"
Maas$Quarter <- (1994+as.numeric(Maas$jaar))*100+as.numeric(Maas$quarter)
frequencies(Maas$Quarter)
Maas$Quarter <- as.factor(Maas$Quarter)
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Maasplassen
## ro the limited degrees of freedom. 
##########################################################################################################


sink('Results/Analysis-output_Maasplassen.txt')

cat("This file includes the raw output of statistical analyses during the data checking, \n")
cat("testing and regressions\n")
cat(" \n")
cat("=========================================================================\n")
cat("T-test for the means between Selvar=0 (included) and Selvar=1 (excluded)") 
cat("=========================================================================\n")

frequencies(Maas$Selvar)

library(Deducer)
descriptive.table(vars = d(Maas$lnprice,  Maas$lnwoonop, Maas$nbadk,      Maas$isol,
                           Maas$bwpr1905, Maas$bwpr6070, Maas$bwpr7180,   Maas$bwpr8190, 
                           Maas$bwpr9100, Maas$bevdicht, Maas$pnietact,   Maas$geminkinko, 
                           Maas$raildist, Maas$highdist, Maas$dist_local, Maas$dist_3a,  
                           Maas$dist_4d,  Maas$dist_4e,  Maas$dist_4f,    Maas$dist_zwem, 
                           Maas$chl,      Maas$doo,      Maas$ZM        ),
                  strata =  Maas$Selvar,
                  data= Maas, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

t.test(lnprice    ~ Selvar, data=Maas) 
t.test(lnwoonop   ~ Selvar, data=Maas) 
t.test(nbadk      ~ Selvar, data=Maas) 
t.test(isol       ~ Selvar, data=Maas) 

t.test(bevdicht   ~ Selvar, data=Maas) 
t.test(pnietact   ~ Selvar, data=Maas) 
t.test(geminkinko ~ Selvar, data=Maas) 
t.test(raildist   ~ Selvar, data=Maas) 

t.test(highdist   ~ Selvar, data=Maas) 
t.test(dist_local ~ Selvar, data=Maas) 
t.test(dist_3a    ~ Selvar, data=Maas) 
t.test(dist_4d    ~ Selvar, data=Maas) 

t.test(dist_4e    ~ Selvar, data=Maas) 
t.test(dist_4f    ~ Selvar, data=Maas) 
t.test(dist_zwem  ~ Selvar, data=Maas) 
t.test(chl        ~ Selvar, data=Maas) 

t.test(doo        ~ Selvar, data=Maas) 
t.test(ZM         ~ Selvar, data=Maas) 

frequencies(Maas$Quarter)
frequencies(Maas$inpandig)
frequencies(Maas$tuin_zow)
frequencies(Maas$buff25)

sink() # Stop writing to the file

#Some rescaling
Maas$dist_3a_rs      <-Maas$dist_3a/1000
Maas$dist_4d_rs      <-Maas$dist_4d/1000
Maas$dist_4e_rs      <-Maas$dist_4e/1000
Maas$dist_4f_rs      <-Maas$dist_4f/1000
Maas$dist_zwem_rs    <-Maas$dist_zwem/1000
Maas$dist_local_rs   <-Maas$dist_local/1000
Maas$highdist_rs     <-Maas$highdist/1000
Maas$raildist_rs     <-Maas$raildist/1000
Maas$ZM_rs           <-Maas$ZM/1000

sink('Results/Analysis-output_Maasplassen.txt', append=TRUE)
cat("=========================================================================\n")
cat("Checking the rescaling of variables \n")
cat("=========================================================================\n")
descriptive.table(vars = d(Maas$lnprice,  Maas$lnwoonop, Maas$isol, Maas$bevdicht, Maas$pnietact,
                           Maas$geminkinko),
                  data= Maas, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))


descriptive.table(vars = d(Maas$raildist,    Maas$raildist_rs,   Maas$highdist,  Maas$highdist_rs,
                           Maas$dist_local,  Maas$dist_local_rs, Maas$dist_3a,   Maas$dist_3a_rs, 
                           Maas$dist_4e,     Maas$dist_4e_rs,    Maas$dist_4d,   Maas$dist_4d_rs, 
                           Maas$dist_4f,     Maas$dist_4f_rs,    Maas$dist_zwem, Maas$dist_zwem_rs,
                           Maas$chl,         Maas$doo,           Maas$ZM,        Maas$ZM_rs),
                  data= Maas, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))
sink()

Maas$Selvar <- as.factor(Maas$Selvar)

subset0 <- subset(Maas, Selvar==0)
subset3 <- subset(subset0,Quarter==200001 | Quarter==200002 | Quarter==200003 | Quarter==200004)
subset4 <- subset(subset0,Quarter==200501 | Quarter==200502 | Quarter==200503 | Quarter==200504)


## REGRESSIONS 

library(spdep)
library("stargazer")
library("lmtest")

## OLS regressions for 2000, stepwise adding sets of variables (model A-E)
  
Maas2000_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
               data=subset3)
Maas2000_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist_rs + highdist_rs,
              data=subset3)
Maas2000_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs ,
              data=subset3)
Maas2000_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs ,
              data=subset3)
Maas2000_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs + chl + doo + ZM_rs,
              data=subset3)
#stargazer(OLSMaas1, OLSMaas2, OLSMaas3, OLSMaas4,OLSMaas5, out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_OLS_2000.htm")
stargazer(Maas2000_OLS_A, Maas2000_OLS_B, Maas2000_OLS_C, Maas2000_OLS_D, Maas2000_OLS_E, out="Results/Maas_OLS_2000.htm")

## OLS regressions for 2005, stepwise adding sets of variables (model A-E)
Maas2005_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
               data=subset4)
Maas2005_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko +  raildist_rs + highdist_rs,
               data=subset4)
Maas2005_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko +  raildist_rs + highdist_rs + dist_local_rs,
               data=subset4)
Maas2005_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs,
               data=subset4)
Maas2005_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                 pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
               data=subset4)

stargazer(Maas2005_OLS_A, Maas2005_OLS_B, Maas2005_OLS_C, Maas2005_OLS_D, Maas2005_OLS_E, out="Results/Maas_OLS_2005.htm")

#stargazer(OLSMaas6, OLSMaas7, OLSMaas8, OLSMaas9,OLSMaas10, out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_OLS_2005.htm")
#stargazer(OLSMaas1, OLSMaas6, OLSMaas2, OLSMaas7,OLSMaas3,OLSMaas8, OLSMaas4, OLSMaas9, OLSMaas5,OLSMaas10, out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_OLS_2001&2005.htm")
#stargazer(OLSMaas6, OLSMaas7, OLSMaas8, OLSMaas9,OLSMaas10, out="Results/Maas_OLS_2005.htm")

stargazer(Maas2000_OLS_E,Maas2005_OLS_E, out="Results/Maas_OLS_2000&2005_E.htm")

writeOGR(subset3, "C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles", "Maas2000", driver="ESRI Shapefile")
frequencies(subset$Quarter)
subset$Quarter   <- as.numeric(subset$Quarter)
hist(subset$Quarter)


#Spatial Analysis for Maas 2000

#IDs<-row.names(as(centroids2000))
#plot(centroids2000,xlab="longitude",ylab="latitude")
#plot(subset3,add=TRUE)
#title("Maas2000 with centroids")
#plot(subset3,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Maas with number label")

# four nearest neighbours: create weighing matrix for 2000
centroids2000 <- coordinates(subset3)
knearneigh4 <- knearneigh(centroids2000,k=4)
knn4 <- knn2nb(knearneigh4)
#plot(subset3, border="grey")
#plot(knn1, centroids2000, add=TRUE)
#title(main="Maasplassen, 4 nearest neighbours")
knn4_W  <-nb2listw(knn4)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm <- lm.LMtests(Maas2000_OLS_E, knn4_W, zero.policy=TRUE, test="all")
lm2000_4NN <-lm

Maas2000_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                    dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                  data=subset3, knn4_W)


Maas2000_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                    dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                  data=subset3, knn4_W)
#summary(Maaslag)

# ten nearest neighbours: create weighing matrix for 2000
knearneigh10 <- knearneigh(centroids2000,k=10)
knn10 <- knn2nb(knearneigh10)
#plot(subset3, border="grey")
#plot(knn10, centroids2000, add=TRUE)
#title(main="Maasplassen, 10 nearest neighbours")
knn10_W  <-nb2listw(knn10)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm10 <- lm.LMtests(Maas2000_OLS_E, knn10_W, zero.policy=TRUE, test="all")
lm2000_10NN <-lm10

Maas2000_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                    dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                  data=subset3, knn10_W)


Maas2000_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                      + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                        pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                        dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                      data=subset3, knn10_W)

stargazer(Maas2000_OLS_E, Maas2000_SPL_E_NN4, Maas2000_SPE_E_NN4, Maas2000_SPL_E_NN10, Maas2000_SPE_E_NN10, out="Results/SpatialMaas2000.html")


# twentyfive nearest neighbours: create weighing matrix for 2000
knearneigh25 <- knearneigh(centroids2000,k=25)
knn25 <- knn2nb(knearneigh25)
#plot(subset3, border="grey")
#plot(knn25, centroids2000, add=TRUE)
#title(main="Maasplassen, 25 nearest neighbours")
knn25_W  <-nb2listw(knn25)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm25 <- lm.LMtests(Maas2000_OLS_E, knn25_W, zero.policy=TRUE, test="all")
lm2000_25NN <-lm25

Maas2000_SPL_E_NN25<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                     data=subset3, knn25_W)


Maas2000_SPE_E_NN25<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                         + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                           pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                           dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                         data=subset3, knn25_W)

stargazer(Maas2000_OLS_E, Maas2000_SPL_E_NN4, Maas2000_SPE_E_NN4, Maas2000_SPL_E_NN10, Maas2000_SPE_E_NN10, out="Results/SpatialMaas2000EXTRA.html")

#Spatial Analysis for Maas 2005

#IDs<-row.names(as(centroids2005))
#plot(centroids2005,xlab="longitude",ylab="latitude")
#plot(subset4,add=TRUE)
#title("Maas2005 with centroids")
#plot(subset4,border="black")
#invisible(text(centroids, labels=as.character(Maas$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Maas with number label")

centroids2005 <- coordinates(subset4)
knearneigh4_2005<- knearneigh(centroids2005,k=4)
knn4_2005 <- knn2nb(knearneigh4_2005)
#plot(subset4, border="grey")
#plot(knn12005, centroids2005, add=TRUE)
#title(main="Maasplassen, 4 nearest neighbours")
knn4_W2005  <-nb2listw(knn4_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm2005 <- lm.LMtests(Maas2005_OLS_E, knn4_W2005, zero.policy=TRUE, test="all")
lm2005_4NN <-lm2005

Maas2005_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                  + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                    pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                    dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                  data=subset4, knn4_W2005)
Maas2005_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                   + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                     pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                     dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                   data=subset4, knn4_W2005)

# 10 neighbour for 2005
knearneigh102005 <- knearneigh(centroids2005,k=10)
knn10_2005 <- knn2nb(knearneigh102005)
#plot(subset4, border="grey")
#plot(knn102005, centroids2005, add=TRUE)
#title(main="Maasplassen, 10 nearest neighbours")
knn10_W2005  <-nb2listw(knn10_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm200510n <- lm.LMtests(Maas2005_OLS_E, knn10_W2005, zero.policy=TRUE, test="all")
lm2005_10NN <-lm200510n
Maas2005_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                   + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                     pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                     dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                   data=subset4, knn10_W2005)
Maas2005_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                       + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                         pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                         dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                       data=subset4, knn10_W2005)

#stargazer(Maaslag,Maaserror,Maaslag2,Maaserror2, Maas10nlag,Maas10nerror,Maas10nlag2,Maas10nerror2,out="C:/Users/Zhong Xiao Leng/Desktop/Maas-shapefiles/Maas_LAG_2000&2005.txt")

#Spatial analysis results comparison
stargazer(Maas2005_OLS_E, Maas2005_SPL_E_NN4, Maas2005_SPE_E_NN4, Maas2005_SPL_E_NN10, Maas2005_SPE_E_NN10, out="Results/SpatialMaas2005.html")

#Comparison between years for model E for three regressions OLS, Spatial Error and Spatial Lag (10 Nearest Neighbours) 
stargazer(Maas2000_OLS_E, Maas2000_SPL_E_NN10, Maas2000_SPE_E_NN10, Maas2005_OLS_E, Maas2005_SPL_E_NN10, Maas2005_SPE_E_NN10, out="Results/SpatialMaas2000&2005.html")

rm(Maas2004, Maas2005_OLS_b, Maas2005_OLS_c, Maas2005_OLS_d)
rm(Maas25nerror, Maas25nlag, Maaserror, Maaserror2, Maaslag, Maaslag2)
rm(MaasSubset, MaasSubSet, MaasSubsetln)
rm(OLSMaas, OLSMaas_A, OLSMaas1, OLSMaas10)
rm(OLSMaas1996, OLSMaas2, OLSMaas2000, OLSMaas2004)
rm(OLSMaas3, OLSMaas4, OLSMaas5, OLSMaas6)
rm(OLSMaas7, OLSMaas8, OLSMaas9, OLSMaas_B, OLSMaas_C, OLSMaas_D, OLSMaas_E)
rm(SPEMaas_E, SPLMaas_E, queen1)
rm(Maas10nerror, Maas10nerror2, Maas10nlag, Maas10nlag2)
rm(subset)
rm(knnSubSet, knnSubSet1, knnSubSet1_w)

rm(DataHedonic)
rm(centroidsSubSet)

sink('Results/Analysis-output_Maasplassen.txt', append=TRUE)
cat("=========================================================================\n")
cat("===RESULTS FOR 2000 =====================================================\n")
cat("=========================================================================\n")
cat("Regression results 2000 - OLS regression (stepwise) the rescaling of \n")
cat("variables \n")
cat("=========================================================================\n")
cat("===Maasplassen, 2000, model A============================================\n")
summary(Maas2000_OLS_A)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, model B============================================\n")
summary(Maas2000_OLS_B)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, model C============================================\n")
summary(Maas2000_OLS_C)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, model D============================================\n")
summary(Maas2000_OLS_D)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, model E============================================\n")
summary(Maas2000_OLS_E)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, spatial dependence test on model E, 4NN ===========\n")
lm2000_4NN
cat("=========================================================================\n")
cat("===Maasplassen, 2000, spatial lag model E, 4NN ==========================\n")
summary(Maas2000_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, spatial error model E, 4NN ========================\n")
summary(Maas2000_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, spatial dependence test on model E, 10NN ==========\n")
lm2000_10NN
cat("=========================================================================\n")
cat("===Maasplassen, 2000, spatial lag model E, 10NN =========================\n")
summary(Maas2000_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Maasplassen, 2000, spatial error model E, 10NN =======================\n")
summary(Maas2000_SPE_E_NN10)
cat("=========================================================================\n")
cat("===RESULTS FOR 2005 =====================================================\n")
cat("=========================================================================\n")
cat("===Maasplassen, 2005, model A============================================\n")
summary(Maas2005_OLS_A)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, model B============================================\n")
summary(Maas2005_OLS_B)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, model C============================================\n")
summary(Maas2005_OLS_C)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, model D============================================\n")
summary(Maas2005_OLS_D)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, model E============================================\n")
summary(Maas2005_OLS_E)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, spatial dependence test on model E, 4NN ===========\n")
lm2005_4NN
cat("=========================================================================\n")
cat("===Maasplassen, 2005, spatial lag model E, 4NN ==========================\n")
sumamry(Maas2005_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, spatial error model E, 4NN ========================\n")
summary(Maas2005_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, spatial dependence test on model E, 10NN ==========\n")
lm2005_10NN
cat("=========================================================================\n")
cat("===Maasplassen, 2005, spatial lag model E, 10NN =========================\n")
summary(Maas2005_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Maasplassen, 2005, spatial error model E, 10NN========================\n")
summary(Maas2005_SPE_E_NN10)
cat("=========================================================================\n")


sink()

Maas2005_SPE_E_NN10
summary(Maas2005_SPE_E_NN10)
Maas2005_SPE_E_NN10

