

if(Sys.info()["user"] == "Zhong Xiao Leng"){
  dataPath <- "C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles"
  setwd("C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles")
}
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/HedonicSpatialWQ/SurveyData"
  setwd("D:/Analyses/HedonicSpatialWQ")
}

#D:/Analyses/HedonicSpatialWQ/SurveyData

#install.packages("rgdal")
library(rgdal)
library(Deducer)

# Reading the data from a shapefile (all data for Geldplassen)



if(Sys.info()["user"] == "Zhong Xiao Leng"){
  Geld <- readOGR(dsn = "C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles", layer = "newGeld")
}
if(Sys.info()["user"] == "linde069"){
  Geld <- readOGR(dsn = "SurveyData", layer = "newGeld")
}

#Check on the data: cross-table for the year and the quarters of a year
table(Geld$jaar,Geld$quarter)

#VL Identifying the missing values for the water quality variables.
#VL Initially, the variables are factors but we will transform them into numerical variables

frequencies(Geld$chl)
#47        NA      21809    75.8        100.0
frequencies(Geld$doo)
#63       NA      19791    68.8        100.0
frequencies(Geld$ZM)
#56      NA      21365    74.2        100.0
frequencies(Geld$geminkinko)
#85    NA        162     0.6        100.0
frequencies(Geld$bevdicht)

frequencies(Geld$pnietact)
#36    NA       1980     6.9        100.0
frequencies(Geld$raildist)
frequencies(Geld$highdist)
frequencies(Geld$dist_local)
frequencies(Geld$dist_3a)
frequencies(Geld$dist_4f)
frequencies(Geld$dist_4d)
frequencies(Geld$dist_4e)
frequencies(Geld$dist_zwem)
frequencies(Geld$nbadk)
frequencies(Geld$isol)
frequencies(Geld$jaar)
frequencies(Geld$quarter)



# A set of variables are read in as factors but should be numerical variables
# Don't worry about the missing variable levels of these variables as they have been identified above
Geld$bevdicht   <- as.numeric(Geld$bevdicht)
Geld$pnietact   <- as.numeric(Geld$pnietact)
Geld$geminkinko <- as.numeric(Geld$geminkinko)
Geld$raildist   <- as.numeric(Geld$raildist)
Geld$highdist   <- as.numeric(Geld$highdist)
Geld$dist_local <- as.numeric(Geld$dist_local)
Geld$dist_3a    <- as.numeric(Geld$dist_3a)
Geld$dist_4f    <- as.numeric(Geld$dist_4f)
Geld$dist_4d    <- as.numeric(Geld$dist_4d)
Geld$dist_4e    <- as.numeric(Geld$dist_4e)
Geld$dist_zwem  <- as.numeric(Geld$dist_zwem)
Geld$nbadk      <- as.numeric(Geld$nbadk)
Geld$isol       <- as.numeric(Geld$isol)

#VL New: Also the water quality variables have to be redefined
Geld$chl       <- as.numeric(Geld$chl)
Geld$doo       <- as.numeric(Geld$doo)
Geld$ZM        <- as.numeric(Geld$ZM)


Geld$Selvar     <- 0
Geld$Selvar     <- ifelse(Geld$chl        == 47 , 1, Geld$Selvar)
Geld$Selvar     <- ifelse(Geld$doo        == 63, 1, Geld$Selvar)
Geld$Selvar     <- ifelse(Geld$ZM         == 56, 1, Geld$Selvar)
Geld$Selvar     <- ifelse(Geld$geminkinko == 85 , 1, Geld$Selvar)

Geld$Selvar     <- ifelse(Geld$pnietact   == 36 , 1, Geld$Selvar)
frequencies(Geld$Selvar)



#New: create the Quarter from the variables "jaar" and "quarter"
Geld$Quarter <- (1994+as.numeric(Geld$jaar))*100+as.numeric(Geld$quarter)
frequencies(Geld$Quarter)
Geld$Quarter <- as.factor(Geld$Quarter)
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Geldplassen
## ro the limited degrees of freedom. 
##########################################################################################################
frequencies(Geld$Selvar)

library(Deducer)
sink('Results/Analysis-output_GeldersePoort.txt')

cat("This file includes the raw output of statistical analyses during the data checking, \n")
cat("testing and regressions\n")
cat(" \n")
cat("=========================================================================\n")
cat("T-test for the means between Selvar=0 (included) and Selvar=1 (excluded)") 
cat("=========================================================================\n")

descriptive.table(vars = d(Geld$lnprice,  Geld$lnwoonop, Geld$nbadk,      Geld$isol,
                           Geld$bwpr1905, Geld$bwpr6070, Geld$bwpr7180,   Geld$bwpr8190, 
                           Geld$bwpr9100, Geld$bevdicht, Geld$pnietact,   Geld$geminkinko, 
                           Geld$raildist, Geld$highdist, Geld$dist_local, Geld$dist_3a,  
                           Geld$dist_4d,  Geld$dist_4e,  Geld$dist_4f,    Geld$dist_zwem, 
                           Geld$chl,      Geld$doo,      Geld$ZM        ),
                  strata =  Geld$Selvar,
                  data= Geld, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

t.test(lnprice    ~ Selvar, data=Geld) 
t.test(lnwoonop   ~ Selvar, data=Geld) 
t.test(nbadk      ~ Selvar, data=Geld) 
t.test(isol       ~ Selvar, data=Geld) 

t.test(bevdicht   ~ Selvar, data=Geld) 
t.test(pnietact   ~ Selvar, data=Geld) 
t.test(geminkinko ~ Selvar, data=Geld) 
t.test(raildist   ~ Selvar, data=Geld) 

t.test(highdist   ~ Selvar, data=Geld) 
t.test(dist_local ~ Selvar, data=Geld) 
t.test(dist_3a    ~ Selvar, data=Geld) 
t.test(dist_4d    ~ Selvar, data=Geld) 

t.test(dist_4e    ~ Selvar, data=Geld) 
t.test(dist_4f    ~ Selvar, data=Geld) 
t.test(dist_zwem  ~ Selvar, data=Geld) 
t.test(chl        ~ Selvar, data=Geld) 

t.test(doo        ~ Selvar, data=Geld) 
t.test(ZM         ~ Selvar, data=Geld) 
sink()

frequencies(Geld$Quarter)
frequencies(Geld$inpandig)
frequencies(Geld$tuin_zow)
frequencies(Geld$buff25)
#Geld$dist_3a_rs
#Geld$dist_4d_rs 
#frequencies(Geld$dist_4d_rs )

sink('Results/Analysis-output_GeldersePoort.txt', append=TRUE)
cat("=========================================================================\n")
cat("Checking the rescaling of variables \n")
cat("=========================================================================\n")
#Some rescaling
Geld$dist_3a_rs      <-Geld$dist_3a/1000
Geld$dist_4d_rs      <-Geld$dist_4d/1000
Geld$dist_4e_rs     <-Geld$dist_4e/1000
Geld$dist_4f_rs      <-Geld$dist_4f/1000
Geld$dist_zwem_rs    <-Geld$dist_zwem/1000
Geld$dist_local_rs   <-Geld$dist_local/1000
Geld$highdist_rs     <-Geld$highdist/1000
Geld$raildist_rs     <-Geld$raildist/1000
Geld$ZM_rs          <-Geld$ZM/1000

descriptive.table(vars = d(Geld$lnprice,  Geld$lnwoonop, Geld$isol, Geld$bevdicht, Geld$pnietact,
                           Geld$geminkinko),
                  data= Geld, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))


descriptive.table(vars = d(Geld$raildist,    Geld$raildist_rs,   Geld$highdist,  Geld$highdist_rs,
                           Geld$dist_local,  Geld$dist_local_rs, Geld$dist_3a,   Geld$dist_3a_rs, 
                           Geld$dist_4e,     Geld$dist_4e_rs,    Geld$dist_4d,   Geld$dist_4d_rs, 
                           Geld$dist_4f,     Geld$dist_4f_rs,    Geld$dist_zwem, Geld$dist_zwem_rs,
                           Geld$chl,         Geld$doo,           Geld$ZM,        Geld$ZM_rs),
                  data= Geld, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))
sink()
#run regressions in 2000 and 2005

Geld$Selvar <- as.factor(Geld$Selvar)

subset0 <- subset(Geld, Selvar==0)
subset3 <- subset(subset0,Quarter==200001 | Quarter==200002 | Quarter==200003 | Quarter==200004)
subset4 <- subset(subset0,Quarter==200501 | Quarter==200502 | Quarter==200503 | Quarter==200504)


#check the variation of other variables

frequencies(subset3$bwpr1905)

frequencies(subset3$bwpr6070)

frequencies(subset3$bwpr7180)
frequencies(subset3$bwpr8190)
frequencies(subset3$bwpr9100)



## OLS regressions for 2000, stepwise adding sets of variables (model A-E)

Geld2000_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset3)
Geld2000_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs,
                     data=subset3)
Geld2000_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs ,
                     data=subset3)
Geld2000_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs ,
                     data=subset3)
Geld2000_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs + chl + doo + ZM_rs,
                     data=subset3)
#stargazer(OLSGeld1, OLSGeld2, OLSGeld3, OLSGeld4,OLSGeld5, out="C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles/Geld_OLS_2000.htm")
stargazer(Geld2000_OLS_A, Geld2000_OLS_B, Geld2000_OLS_C, Geld2000_OLS_D, Geld2000_OLS_E, out="Results/Geld_OLS_2000.htm")

## OLS regressions for 2005, stepwise adding sets of variables (model A-E)
Geld2005_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset4)
Geld2005_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs,
                     data=subset4)
Geld2005_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs + dist_local_rs,
                     data=subset4)
Geld2005_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs,
                     data=subset4)
Geld2005_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                     data=subset4)

stargazer(Geld2005_OLS_A, Geld2005_OLS_B, Geld2005_OLS_C, Geld2005_OLS_D, Geld2005_OLS_E, out="Results/Geld_OLS_2005.htm")

#stargazer(OLSGeld6, OLSGeld7, OLSGeld8, OLSGeld9,OLSGeld10, out="C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles/Geld_OLS_2005.htm")
#stargazer(OLSGeld1, OLSGeld6, OLSGeld2, OLSGeld7,OLSGeld3,OLSGeld8, OLSGeld4, OLSGeld9, OLSGeld5,OLSGeld10, out="C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles/Geld_OLS_2001&2005.htm")
#stargazer(OLSGeld6, OLSGeld7, OLSGeld8, OLSGeld9,OLSGeld10, out="Results/Geld_OLS_2005.htm")

stargazer(Geld2000_OLS_E,Geld2005_OLS_E, out="Results/Geld_OLS_2000&2005_E.htm")

writeOGR(subset3, "C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles", "Geld2000", driver="ESRI Shapefile")
frequencies(subset$Quarter)
subset$Quarter   <- as.numeric(subset$Quarter)
hist(subset$Quarter)


#Spatial Analysis for Geld 2000

#IDs<-row.names(as(centroids2000))
#plot(centroids2000,xlab="longitude",ylab="latitude")
#plot(subset3,add=TRUE)
#title("Geld2000 with centroids")
#plot(subset3,border="black")
#invisible(text(centroids, labels=as.character(Geld$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Geld with number label")

# four nearest neighbours: create weighing matrix for 2000
centroids2000 <- coordinates(subset3)
knearneigh4 <- knearneigh(centroids2000,k=4)
knn4 <- knn2nb(knearneigh4)
#plot(subset3, border="grey")
#plot(knn1, centroids2000, add=TRUE)
#title(main="Geldplassen, 4 nearest neighbours")
knn4_W  <-nb2listw(knn4)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Geld_2000_4NN <- lm.LMtests(Geld2000_OLS_E, knn4_W, zero.policy=TRUE, test="all")

Geld2000_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset3, knn4_W)


Geld2000_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset3, knn4_W)
#summary(Geldlag)

# ten nearest neighbours: create weighing matrix for 2000
knearneigh10 <- knearneigh(centroids2000,k=10)
knn10 <- knn2nb(knearneigh10)
#plot(subset3, border="grey")
#plot(knn10, centroids2000, add=TRUE)
#title(main="Geldplassen, 10 nearest neighbours")
knn10_W  <-nb2listw(knn10)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Geld_2000_10NN <- lm.LMtests(Geld2000_OLS_E, knn10_W, zero.policy=TRUE, test="all")

Geld2000_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn10_W)


Geld2000_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn10_W)

stargazer(Geld2000_OLS_E, Geld2000_SPL_E_NN4, Geld2000_SPE_E_NN4, Geld2000_SPL_E_NN10, Geld2000_SPE_E_NN10, out="Results/SpatialGeld2000.html")


# twentyfive nearest neighbours: create weighing matrix for 2000
knearneigh25 <- knearneigh(centroids2000,k=25)
knn25 <- knn2nb(knearneigh25)
#plot(subset3, border="grey")
#plot(knn25, centroids2000, add=TRUE)
#title(main="Geldplassen, 25 nearest neighbours")
knn25_W  <-nb2listw(knn25)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm25 <- lm.LMtests(Geld2000_OLS_E, knn25_W, zero.policy=TRUE, test="all")
lm2000_25NN <-lm25

Geld2000_SPL_E_NN25<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn25_W)


Geld2000_SPE_E_NN25<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn25_W)

stargazer(Geld2000_OLS_E, Geld2000_SPL_E_NN4, Geld2000_SPE_E_NN4, Geld2000_SPL_E_NN10, Geld2000_SPE_E_NN10, out="Results/SpatialGeld2000EXTRA.html")

#Spatial Analysis for Geld 2005

#IDs<-row.names(as(centroids2005))
#plot(centroids2005,xlab="longitude",ylab="latitude")
#plot(subset4,add=TRUE)
#title("Geld2005 with centroids")
#plot(subset4,border="black")
#invisible(text(centroids, labels=as.character(Geld$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Geld with number label")

centroids2005 <- coordinates(subset4)
knearneigh4_2005<- knearneigh(centroids2005,k=4)
knn4_2005 <- knn2nb(knearneigh4_2005)
#plot(subset4, border="grey")
#plot(knn12005, centroids2005, add=TRUE)
#title(main="Geldplassen, 4 nearest neighbours")
knn4_W2005  <-nb2listw(knn4_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Geld_2005_4NN <- lm.LMtests(Geld2005_OLS_E, knn4_W2005, zero.policy=TRUE, test="all")
#lm2005_4NN <-lm2005

Geld2005_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset4, knn4_W2005)
Geld2005_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset4, knn4_W2005)

# 10 neighbour for 2005
knearneigh102005 <- knearneigh(centroids2005,k=10)
knn10_2005 <- knn2nb(knearneigh102005)
#plot(subset4, border="grey")
#plot(knn102005, centroids2005, add=TRUE)
#title(main="Geldplassen, 10 nearest neighbours")
knn10_W2005  <-nb2listw(knn10_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Geld_2005_10NN <- lm.LMtests(Geld2005_OLS_E, knn10_W2005, zero.policy=TRUE, test="all")
#lm2005_10NN <-lm200510n
Geld2005_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset4, knn10_W2005)
Geld2005_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset4, knn10_W2005)

#stargazer(Geldlag,Gelderror,Geldlag2,Gelderror2, Geld10nlag,Geld10nerror,Geld10nlag2,Geld10nerror2,out="C:/Users/Zhong Xiao Leng/Desktop/Geld-shapefiles/Geld_LAG_2000&2005.txt")

#Spatial analysis results comparison
stargazer(Geld2005_OLS_E, Geld2005_SPL_E_NN4, Geld2005_SPE_E_NN4, Geld2005_SPL_E_NN10, Geld2005_SPE_E_NN10, out="Results/SpatialGeld2005.html")

#Comparison between years for model E for three regressions OLS, Spatial Error and Spatial Lag (10 Nearest Neighbours) 
stargazer(Geld2000_OLS_E, Geld2000_SPL_E_NN10, Geld2000_SPE_E_NN10, Geld2005_OLS_E, Geld2005_SPL_E_NN10, Geld2005_SPE_E_NN10, out="Results/SpatialGeld2000&2005.html")


sink('Results/Analysis-output_GeldersePoort.txt', append=TRUE)
cat("=========================================================================\n")
cat("===RESULTS FOR 2000 =====================================================\n")
cat("=========================================================================\n")
cat("Regression results 2000 - OLS regression (stepwise) the rescaling of \n")
cat("variables \n")
cat("=========================================================================\n")
cat("===Geldplassen, 2000, model A============================================\n")
summary(Geld2000_OLS_A)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, model B============================================\n")
summary(Geld2000_OLS_B)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, model C============================================\n")
summary(Geld2000_OLS_C)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, model D============================================\n")
summary(Geld2000_OLS_D)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, model E============================================\n")
summary(Geld2000_OLS_E)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, spatial dependence test on model E, 4NN ===========\n")
lm_Geld_2000_4NN
cat("=========================================================================\n")
cat("===Geldplassen, 2000, spatial lag model E, 4NN ==========================\n")
summary(Geld2000_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, spatial error model E, 4NN ========================\n")
summary(Geld2000_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, spatial dependence test on model E, 10NN ==========\n")
lm_Geld_2000_10NN
cat("=========================================================================\n")
cat("===Geldplassen, 2000, spatial lag model E, 10NN =========================\n")
summary(Geld2000_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Geldplassen, 2000, spatial error model E, 10NN =======================\n")
summary(Geld2000_SPE_E_NN10)
cat("=========================================================================\n")
cat("===RESULTS FOR 2005 =====================================================\n")
cat("=========================================================================\n")
cat("===Geldplassen, 2005, model A============================================\n")
summary(Geld2005_OLS_A)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, model B============================================\n")
summary(Geld2005_OLS_B)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, model C============================================\n")
summary(Geld2005_OLS_C)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, model D============================================\n")
summary(Geld2005_OLS_D)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, model E============================================\n")
summary(Geld2005_OLS_E)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, spatial dependence test on model E, 4NN ===========\n")
lm_Geld_2005_4NN
cat("=========================================================================\n")
cat("===Geldplassen, 2005, spatial lag model E, 4NN ==========================\n")
summary(Geld2005_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, spatial error model E, 4NN ========================\n")
summary(Geld2005_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, spatial dependence test on model E, 10NN ==========\n")
lm_Geld_2005_10NN
cat("=========================================================================\n")
cat("===Geldplassen, 2005, spatial lag model E, 10NN =========================\n")
summary(Geld2005_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Geldplassen, 2005, spatial error model E, 10NN========================\n")
summary(Geld2005_SPE_E_NN10)
cat("=========================================================================\n")


sink()




