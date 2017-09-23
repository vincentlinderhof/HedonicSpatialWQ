

if(Sys.info()["user"] == "Zhong Xiao Leng"){
  dataPath <- "C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles"
  setwd("C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles")
}

if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/HedonicSpatialWQ/SurveyData"
  setwd("D:/Analyses/HedonicSpatialWQ")
}

#install.packages("rgdal")
library(rgdal)
library(Deducer)

# Reading the data from a shapefile (all data for Flevopolder)


if(Sys.info()["user"] == "Zhong Xiao Leng"){
  Flev <- readOGR(dsn = "C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles", layer = "newFlev")
}
if(Sys.info()["user"] == "linde069"){
  Flev <- readOGR(dsn = "D:/Analyses/HedonicSpatialWQ/SurveyData", layer = "newFlev")
}

#Check on the data: cross-table for the year and the quarters of a year
table(Flev$jaar,Flev$quarter)

#VL Identifying the missing values for the water quality variables.
#VL Initially, the variables are factors but we will transform them into numerical variables

frequencies(Flev$chl)
#133        NA      19299    72.2        100.0
frequencies(Flev$doo)
#175        NA      15396    57.6        100.0
frequencies(Flev$ZM)
#126      NA      19213    71.8        100.0
frequencies(Flev$geminkinko)
#58    NA        316     1.2        100.0
frequencies(Flev$bevdicht)
#100    NA        106     0.4        100.0
frequencies(Flev$pnietact)
#27    NA        819     3.1        100.0
frequencies(Flev$raildist)
frequencies(Flev$highdist)
frequencies(Flev$dist_local)
frequencies(Flev$dist_3a)
frequencies(Flev$dist_4f)
frequencies(Flev$dist_4d)
frequencies(Flev$dist_4e)
frequencies(Flev$dist_zwem)
frequencies(Flev$nbadk)
frequencies(Flev$isol)
frequencies(Flev$jaar)
frequencies(Flev$quarter)



# A set of variables are read in as factors but should be numerical variables
# Don't worry about the missing variable levels of these variables as they have been identified above
Flev$bevdicht   <- as.numeric(Flev$bevdicht)
Flev$pnietact   <- as.numeric(Flev$pnietact)
Flev$geminkinko <- as.numeric(Flev$geminkinko)
Flev$raildist   <- as.numeric(Flev$raildist)
Flev$highdist   <- as.numeric(Flev$highdist)
Flev$dist_local <- as.numeric(Flev$dist_local)
Flev$dist_3a    <- as.numeric(Flev$dist_3a)
Flev$dist_4f    <- as.numeric(Flev$dist_4f)
Flev$dist_4d    <- as.numeric(Flev$dist_4d)
Flev$dist_4e    <- as.numeric(Flev$dist_4e)
Flev$dist_zwem  <- as.numeric(Flev$dist_zwem)
Flev$nbadk      <- as.numeric(Flev$nbadk)
Flev$isol       <- as.numeric(Flev$isol)

#VL New: Also the water quality variables have to be redefined
Flev$chl       <- as.numeric(Flev$chl)
Flev$doo       <- as.numeric(Flev$doo)
Flev$ZM        <- as.numeric(Flev$ZM)


Flev$Selvar     <- 0
Flev$Selvar     <- ifelse(Flev$chl        == 133 , 1, Flev$Selvar)
Flev$Selvar     <- ifelse(Flev$doo        == 175, 1, Flev$Selvar)
Flev$Selvar     <- ifelse(Flev$ZM         == 126, 1, Flev$Selvar)
Flev$Selvar     <- ifelse(Flev$geminkinko == 58 , 1, Flev$Selvar)
Flev$Selvar     <- ifelse(Flev$bevdicht == 100 , 1, Flev$Selvar)
Flev$Selvar     <- ifelse(Flev$pnietact   == 27 , 1, Flev$Selvar)
frequencies(Flev$Selvar)



#New: create the Quarter from the variables "jaar" and "quarter"
Flev$Quarter <- (1994+as.numeric(Flev$jaar))*100+as.numeric(Flev$quarter)
frequencies(Flev$Quarter)
Flev$Quarter <- as.factor(Flev$Quarter)
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Flevopolder
## ro the limited degrees of freedom. 
##########################################################################################################
frequencies(Flev$Selvar)

sink('Results/Analysis-output_Flevopolder.txt')
cat("This file includes the raw output of statistical analyses during the data checking, \n")
cat("testing and regressions\n")
cat(" \n")
cat("=========================================================================\n")
cat("T-test for the means between Selvar=0 (included) and Selvar=1 (excluded)") 
cat("=========================================================================\n")

library(Deducer)
descriptive.table(vars = d(Flev$lnprice,  Flev$lnwoonop, Flev$nbadk,      Flev$isol,
                           Flev$bwpr1905, Flev$bwpr6070, Flev$bwpr7180,   Flev$bwpr8190, 
                           Flev$bwpr9100, Flev$bevdicht, Flev$pnietact,   Flev$geminkinko, 
                           Flev$raildist, Flev$highdist, Flev$dist_local, Flev$dist_3a,  
                           Flev$dist_4d,  Flev$dist_4e,  Flev$dist_4f,    Flev$dist_zwem, 
                           Flev$chl,      Flev$doo,      Flev$ZM        ),
                  strata =  Flev$Selvar,
                  data= Flev, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

t.test(lnprice    ~ Selvar, data=Flev) 
t.test(lnwoonop   ~ Selvar, data=Flev) 
t.test(nbadk      ~ Selvar, data=Flev) 
t.test(isol       ~ Selvar, data=Flev) 

t.test(bevdicht   ~ Selvar, data=Flev) 
t.test(pnietact   ~ Selvar, data=Flev) 
t.test(geminkinko ~ Selvar, data=Flev) 
t.test(raildist   ~ Selvar, data=Flev) 

t.test(highdist   ~ Selvar, data=Flev) 
t.test(dist_local ~ Selvar, data=Flev) 
t.test(dist_3a    ~ Selvar, data=Flev) 
t.test(dist_4d    ~ Selvar, data=Flev) 

t.test(dist_4e    ~ Selvar, data=Flev) 
t.test(dist_4f    ~ Selvar, data=Flev) 
t.test(dist_zwem  ~ Selvar, data=Flev) 
t.test(chl        ~ Selvar, data=Flev) 

t.test(doo        ~ Selvar, data=Flev) 
t.test(ZM         ~ Selvar, data=Flev) 

frequencies(Flev$Quarter)
frequencies(Flev$inpandig)
frequencies(Flev$tuin_zow)
frequencies(Flev$buff25)
#Flev$dist_3a_rs
#Flev$dist_4d_rs 
#frequencies(Flev$dist_4d_rs )
sink()

#Some rescaling
Flev$dist_3a_rs      <-Flev$dist_3a/1000
Flev$dist_4d_rs      <-Flev$dist_4d/1000
Flev$dist_4e_rs     <-Flev$dist_4e/1000
Flev$dist_4f_rs      <-Flev$dist_4f/1000
Flev$dist_zwem_rs    <-Flev$dist_zwem/1000
Flev$dist_local_rs   <-Flev$dist_local/1000
Flev$highdist_rs     <-Flev$highdist/1000
Flev$raildist_rs     <-Flev$raildist/1000
Flev$ZM_rs          <-Flev$ZM/1000

sink('Results/Analysis-output_Flevopolder.txt', append=TRUE)
cat("=========================================================================\n")
cat("Checking the rescaling of variables \n")
cat("=========================================================================\n")
descriptive.table(vars = d(Flev$lnprice,  Flev$lnwoonop, Flev$isol, Flev$bevdicht, Flev$pnietact,
                           Flev$geminkinko),
                  data= Flev, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))


descriptive.table(vars = d(Flev$raildist,    Flev$raildist_rs,   Flev$highdist,  Flev$highdist_rs,
                           Flev$dist_local,  Flev$dist_local_rs, Flev$dist_3a,   Flev$dist_3a_rs, 
                           Flev$dist_4e,     Flev$dist_4e_rs,    Flev$dist_4d,   Flev$dist_4d_rs, 
                           Flev$dist_4f,     Flev$dist_4f_rs,    Flev$dist_zwem, Flev$dist_zwem_rs,
                           Flev$chl,         Flev$doo,           Flev$ZM,        Flev$ZM_rs),
                  data= Flev, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))
sink()

#run regressions in 2000 and 2005

Flev$Selvar <- as.factor(Flev$Selvar)

subset0 <- subset(Flev, Selvar==0)
subset3 <- subset(subset0,Quarter==200001 | Quarter==200002 | Quarter==200003 | Quarter==200004)
subset4 <- subset(subset0,Quarter==200501 | Quarter==200502 | Quarter==200503 | Quarter==200504)


#check the variation of other variables

frequencies(subset3$bwpr1905)
#all 0
frequencies(subset3$bwpr6070)
#too little variation
#delete 1905 and 6070 in the model
frequencies(subset3$bwpr7180)
frequencies(subset3$bwpr8190)
frequencies(subset3$bwpr9100)



## OLS regressions for 2000, stepwise adding sets of variables (model A-E)

Flev2000_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset3)
Flev2000_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs,
                     data=subset3)
Flev2000_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs ,
                     data=subset3)
Flev2000_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs ,
                     data=subset3)
Flev2000_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs + chl + doo + ZM_rs,
                     data=subset3)
#stargazer(OLSFlev1, OLSFlev2, OLSFlev3, OLSFlev4,OLSFlev5, out="C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles/Flev_OLS_2000.htm")
stargazer(Flev2000_OLS_A, Flev2000_OLS_B, Flev2000_OLS_C, Flev2000_OLS_D, Flev2000_OLS_E, out="Results/Flev_OLS_2000.htm")

## OLS regressions for 2005, stepwise adding sets of variables (model A-E)
Flev2005_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset4)
Flev2005_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs,
                     data=subset4)
Flev2005_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs + dist_local_rs,
                     data=subset4)
Flev2005_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs,
                     data=subset4)
Flev2005_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                     data=subset4)

stargazer(Flev2005_OLS_A, Flev2005_OLS_B, Flev2005_OLS_C, Flev2005_OLS_D, Flev2005_OLS_E, out="Results/Flev_OLS_2005.htm")

#stargazer(OLSFlev6, OLSFlev7, OLSFlev8, OLSFlev9,OLSFlev10, out="C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles/Flev_OLS_2005.htm")
#stargazer(OLSFlev1, OLSFlev6, OLSFlev2, OLSFlev7,OLSFlev3,OLSFlev8, OLSFlev4, OLSFlev9, OLSFlev5,OLSFlev10, out="C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles/Flev_OLS_2001&2005.htm")
#stargazer(OLSFlev6, OLSFlev7, OLSFlev8, OLSFlev9,OLSFlev10, out="Results/Flev_OLS_2005.htm")

stargazer(Flev2000_OLS_E,Flev2005_OLS_E, out="Results/Flev_OLS_2000&2005_E.htm")

writeOGR(subset3, "C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles", "Flev2000", driver="ESRI Shapefile")
frequencies(subset$Quarter)
subset$Quarter   <- as.numeric(subset$Quarter)
hist(subset$Quarter)


#Spatial Analysis for Flev 2000

#IDs<-row.names(as(centroids2000))
#plot(centroids2000,xlab="longitude",ylab="latitude")
#plot(subset3,add=TRUE)
#title("Flev2000 with centroids")
#plot(subset3,border="black")
#invisible(text(centroids, labels=as.character(Flev$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Flev with number label")

# four nearest neighbours: create weighing matrix for 2000
centroids2000 <- coordinates(subset3)
knearneigh4 <- knearneigh(centroids2000,k=4)
knn4 <- knn2nb(knearneigh4)
#plot(subset3, border="grey")
#plot(knn1, centroids2000, add=TRUE)
#title(main="Flevplassen, 4 nearest neighbours")
knn4_W  <-nb2listw(knn4)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Flev_2000_4NN <- lm.LMtests(Flev2000_OLS_E, knn4_W, zero.policy=TRUE, test="all")

Flev2000_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset3, knn4_W)


Flev2000_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset3, knn4_W)
#summary(Flevlag)

# ten nearest neighbours: create weighing matrix for 2000
knearneigh10 <- knearneigh(centroids2000,k=10)
knn10 <- knn2nb(knearneigh10)
#plot(subset3, border="grey")
#plot(knn10, centroids2000, add=TRUE)
#title(main="Flevplassen, 10 nearest neighbours")
knn10_W  <-nb2listw(knn10)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Flev_2000_4NN <- lm.LMtests(Flev2000_OLS_E, knn10_W, zero.policy=TRUE, test="all")
#lm2000_10NN <-lm10

Flev2000_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn10_W)


Flev2000_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn10_W)

stargazer(Flev2000_OLS_E, Flev2000_SPL_E_NN4, Flev2000_SPE_E_NN4, Flev2000_SPL_E_NN10, Flev2000_SPE_E_NN10, out="Results/SpatialFlev2000.html")


# twentyfive nearest neighbours: create weighing matrix for 2000
knearneigh25 <- knearneigh(centroids2000,k=25)
knn25 <- knn2nb(knearneigh25)
#plot(subset3, border="grey")
#plot(knn25, centroids2000, add=TRUE)
#title(main="Flevplassen, 25 nearest neighbours")
knn25_W  <-nb2listw(knn25)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm25 <- lm.LMtests(Flev2000_OLS_E, knn25_W, zero.policy=TRUE, test="all")
lm2000_25NN <-lm25

Flev2000_SPL_E_NN25<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn25_W)


Flev2000_SPE_E_NN25<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn25_W)

stargazer(Flev2000_OLS_E, Flev2000_SPL_E_NN4, Flev2000_SPE_E_NN4, Flev2000_SPL_E_NN10, Flev2000_SPE_E_NN10, out="Results/SpatialFlev2000EXTRA.html")

#Spatial Analysis for Flev 2005

#IDs<-row.names(as(centroids2005))
#plot(centroids2005,xlab="longitude",ylab="latitude")
#plot(subset4,add=TRUE)
#title("Flev2005 with centroids")
#plot(subset4,border="black")
#invisible(text(centroids, labels=as.character(Flev$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Flev with number label")

centroids2005 <- coordinates(subset4)
knearneigh4_2005<- knearneigh(centroids2005,k=4)
knn4_2005 <- knn2nb(knearneigh4_2005)
#plot(subset4, border="grey")
#plot(knn12005, centroids2005, add=TRUE)
#title(main="Flevplassen, 4 nearest neighbours")
knn4_W2005  <-nb2listw(knn4_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Flev_2005_4NN <- lm.LMtests(Flev2005_OLS_E, knn4_W2005, zero.policy=TRUE, test="all")

Flev2005_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset4, knn4_W2005)
Flev2005_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset4, knn4_W2005)

# 10 neighbour for 2005
knearneigh102005 <- knearneigh(centroids2005,k=10)
knn10_2005 <- knn2nb(knearneigh102005)
#plot(subset4, border="grey")
#plot(knn102005, centroids2005, add=TRUE)
#title(main="Flevplassen, 10 nearest neighbours")
knn10_W2005  <-nb2listw(knn10_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Flev_2005_10NN <- lm.LMtests(Flev2005_OLS_E, knn10_W2005, zero.policy=TRUE, test="all")

Flev2005_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset4, knn10_W2005)
Flev2005_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs  + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset4, knn10_W2005)

#stargazer(Flevlag,Fleverror,Flevlag2,Fleverror2, Flev10nlag,Flev10nerror,Flev10nlag2,Flev10nerror2,out="C:/Users/Zhong Xiao Leng/Desktop/Flev-shapefiles/Flev_LAG_2000&2005.txt")

#Spatial analysis results comparison
stargazer(Flev2005_OLS_E, Flev2005_SPL_E_NN4, Flev2005_SPE_E_NN4, Flev2005_SPL_E_NN10, Flev2005_SPE_E_NN10, out="Results/SpatialFlev2005.html")

#Comparison between years for model E for three regressions OLS, Spatial Error and Spatial Lag (10 Nearest Neighbours) 
stargazer(Flev2000_OLS_E, Flev2000_SPL_E_NN10, Flev2000_SPE_E_NN10, Flev2005_OLS_E, Flev2005_SPL_E_NN10, Flev2005_SPE_E_NN10, out="Results/SpatialFlev2000&2005.html")


sink('Results/Analysis-output_Flevopolder.txt', append=TRUE)
cat("=========================================================================\n")
cat("===RESULTS FOR 2000 =====================================================\n")
cat("=========================================================================\n")
cat("Regression results 2000 - OLS regression (stepwise) the rescaling of \n")
cat("variables \n")
cat("=========================================================================\n")
cat("===Flevplassen, 2000, model A============================================\n")
summary(Flev2000_OLS_A)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, model B============================================\n")
summary(Flev2000_OLS_B)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, model C============================================\n")
summary(Flev2000_OLS_C)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, model D============================================\n")
summary(Flev2000_OLS_D)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, model E============================================\n")
summary(Flev2000_OLS_E)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, spatial dependence test on model E, 4NN ===========\n")
lm_Flev_2000_4NN
cat("=========================================================================\n")
cat("===Flevplassen, 2000, spatial lag model E, 4NN ==========================\n")
summary(Flev2000_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, spatial error model E, 4NN ========================\n")
summary(Flev2000_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, spatial dependence test on model E, 10NN ==========\n")
lm_Flev_2000_10NN
cat("=========================================================================\n")
cat("===Flevplassen, 2000, spatial lag model E, 10NN =========================\n")
summary(Flev2000_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Flevplassen, 2000, spatial error model E, 10NN =======================\n")
summary(Flev2000_SPE_E_NN10)
cat("=========================================================================\n")
cat("===RESULTS FOR 2005 =====================================================\n")
cat("=========================================================================\n")
cat("===Flevplassen, 2005, model A============================================\n")
summary(Flev2005_OLS_A)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, model B============================================\n")
summary(Flev2005_OLS_B)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, model C============================================\n")
summary(Flev2005_OLS_C)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, model D============================================\n")
summary(Flev2005_OLS_D)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, model E============================================\n")
summary(Flev2005_OLS_E)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, spatial dependence test on model E, 4NN ===========\n")
lm_Flev_2005_4NN
cat("=========================================================================\n")
cat("===Flevplassen, 2005, spatial lag model E, 4NN ==========================\n")
summary(Flev2005_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, spatial error model E, 4NN ========================\n")
summary(Flev2005_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, spatial dependence test on model E, 10NN ==========\n")
lm_Flev_2005_10NN
cat("=========================================================================\n")
cat("===Flevplassen, 2005, spatial lag model E, 10NN =========================\n")
summary(Flev2005_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Flevplassen, 2005, spatial error model E, 10NN========================\n")
summary(Flev2005_SPE_E_NN10)
cat("=========================================================================\n")


sink()



