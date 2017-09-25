

if(Sys.info()["user"] == "Zhong Xiao Leng"){
  dataPath <- "C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles"
  setwd("C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles")
}
if(Sys.info()["user"] == "linde069"){
  dataPath <- "D:/Analyses/HedonicSpatialWQ/SurveyData"
  setwd("D:/Analyses/HedonicSpatialWQ")
}

#install.packages("rgdal")
library(rgdal)
library(Deducer)

# Reading the data from a shapefile (all data for Vechplassen)

if(Sys.info()["user"] == "Zhong Xiao Leng"){
  Vech <- readOGR(dsn = "C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles", layer = "newVech")
}
if(Sys.info()["user"] == "linde069"){
  Vech <- readOGR(dsn = "D:/Analyses/HedonicSpatialWQ/SurveyData", layer = "newVech")
}




#Check on the data: cross-table for the year and the quarters of a year
table(Vech$jaar,Vech$quarter)

#VL Identifying the missing values for the water quality variables.
#VL Initially, the variables are factors but we will transform them into numerical variables
options(max.print=1000000)
frequencies(Vech$chl)
#596        NA      33985    51.3        100.0
frequencies(Vech$doo)
#446        NA      29731    44.9        100.0
frequencies(Vech$ZM)
#446      NA      34301    51.8        100.0
frequencies(Vech$geminkinko)
#118    NA        286     0.4        100.0
frequencies(Vech$bevdicht)
#338    NA         32     0.0        100.0
frequencies(Vech$pnietact)
#35    NA       2277     3.4        100.0
frequencies(Vech$raildist)
frequencies(Vech$highdist)
frequencies(Vech$dist_local)
frequencies(Vech$dist_3a)
frequencies(Vech$dist_4f)


frequencies(Vech$dist_4d)
#4778    NA       3542     5.3        100.0
frequencies(Vech$dist_4e)

#3709    NA          3     0.0        100.0

frequencies(Vech$dist_zwem)
#4621    NA       8191    12.4        100.0
frequencies(Vech$nbadk)
frequencies(Vech$isol)
frequencies(Vech$jaar)
frequencies(Vech$quarter)



# A set of variables are read in as factors but should be numerical variables
# Don't worry about the missing variable levels of these variables as they have been identified above
Vech$bevdicht   <- as.numeric(Vech$bevdicht)
Vech$pnietact   <- as.numeric(Vech$pnietact)
Vech$geminkinko <- as.numeric(Vech$geminkinko)
Vech$raildist   <- as.numeric(Vech$raildist)
Vech$highdist   <- as.numeric(Vech$highdist)
Vech$dist_local <- as.numeric(Vech$dist_local)
Vech$dist_3a    <- as.numeric(Vech$dist_3a)
Vech$dist_4f    <- as.numeric(Vech$dist_4f)
Vech$dist_4d    <- as.numeric(Vech$dist_4d)
Vech$dist_4e    <- as.numeric(Vech$dist_4e)
Vech$dist_zwem  <- as.numeric(Vech$dist_zwem)
Vech$nbadk      <- as.numeric(Vech$nbadk)
Vech$isol       <- as.numeric(Vech$isol)

#VL New: Also the water quality variables have to be redefined
Vech$chl       <- as.numeric(Vech$chl)
Vech$doo       <- as.numeric(Vech$doo)
Vech$ZM        <- as.numeric(Vech$ZM)


Vech$Selvar     <- 0
Vech$Selvar     <- ifelse(Vech$chl        == 47 , 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$doo        == 63, 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$ZM         == 56, 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$geminkinko == 85 , 1, Vech$Selvar)

Vech$Selvar     <- ifelse(Vech$bevdicht   == 338 , 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$pnietact   == 35 , 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$dist_4d   == 4778 , 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$dist_4e   == 3709 , 1, Vech$Selvar)
Vech$Selvar     <- ifelse(Vech$dist_zwem   == 4621 , 1, Vech$Selvar)
frequencies(Vech$Selvar)



#New: create the Quarter from the variables "jaar" and "quarter"
Vech$Quarter <- (1994+as.numeric(Vech$jaar))*100+as.numeric(Vech$quarter)
frequencies(Vech$Quarter)
Vech$Quarter <- as.factor(Vech$Quarter)
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Vechplassen
## ro the limited degrees of freedom. 
##########################################################################################################
frequencies(Vech$Selvar)

#library(Deducer)
sink('Results/Analysis-output_Vechtstreek.txt')

cat("This file includes the raw output of statistical analyses during the data checking, \n")
cat("testing and regressions\n")
cat(" \n")
cat("=========================================================================\n")
cat("T-test for the means between Selvar=0 (included) and Selvar=1 (excluded)") 
cat("=========================================================================\n")

descriptive.table(vars = d(Vech$lnprice,  Vech$lnwoonop, Vech$nbadk,      Vech$isol,
                           Vech$bwpr1905, Vech$bwpr6070, Vech$bwpr7180,   Vech$bwpr8190, 
                           Vech$bwpr9100, Vech$bevdicht, Vech$pnietact,   Vech$geminkinko, 
                           Vech$raildist, Vech$highdist, Vech$dist_local, Vech$dist_3a,  
                           Vech$dist_4d,  Vech$dist_4e,  Vech$dist_4f,    Vech$dist_zwem, 
                           Vech$chl,      Vech$doo,      Vech$ZM        ),
                  strata =  Vech$Selvar,
                  data= Vech, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

t.test(lnprice    ~ Selvar, data=Vech) 
t.test(lnwoonop   ~ Selvar, data=Vech) 
t.test(nbadk      ~ Selvar, data=Vech) 
t.test(isol       ~ Selvar, data=Vech) 

t.test(bevdicht   ~ Selvar, data=Vech) 
t.test(pnietact   ~ Selvar, data=Vech) 
t.test(geminkinko ~ Selvar, data=Vech) 
t.test(raildist   ~ Selvar, data=Vech) 

t.test(highdist   ~ Selvar, data=Vech) 
t.test(dist_local ~ Selvar, data=Vech) 
t.test(dist_3a    ~ Selvar, data=Vech) 
t.test(dist_4d    ~ Selvar, data=Vech) 

t.test(dist_4e    ~ Selvar, data=Vech) 
t.test(dist_4f    ~ Selvar, data=Vech) 
t.test(dist_zwem  ~ Selvar, data=Vech) 
t.test(chl        ~ Selvar, data=Vech) 

t.test(doo        ~ Selvar, data=Vech) 
t.test(ZM         ~ Selvar, data=Vech) 

frequencies(Vech$Quarter)
frequencies(Vech$inpandig)
frequencies(Vech$tuin_zow)
frequencies(Vech$buff25)
Vech$dist_3a_rs
Vech$dist_4d_rs 
frequencies(Vech$dist_4d_rs )

cat("=========================================================================\n")
cat("Checking the rescaling of variables \n")
cat("=========================================================================\n")
#Some rescaling
Vech$dist_3a_rs      <-Vech$dist_3a/1000
Vech$dist_4d_rs      <-Vech$dist_4d/1000
Vech$dist_4e_rs     <-Vech$dist_4e/1000
Vech$dist_4f_rs      <-Vech$dist_4f/1000
Vech$dist_zwem_rs    <-Vech$dist_zwem/1000
Vech$dist_local_rs   <-Vech$dist_local/1000
Vech$highdist_rs     <-Vech$highdist/1000
Vech$raildist_rs     <-Vech$raildist/1000
Vech$ZM_rs          <-Vech$ZM/1000

descriptive.table(vars = d(Vech$lnprice,  Vech$lnwoonop, Vech$isol, Vech$bevdicht, Vech$pnietact,
                           Vech$geminkinko),
                  data= Vech, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))


descriptive.table(vars = d(Vech$raildist,    Vech$raildist_rs,   Vech$highdist,  Vech$highdist_rs,
                           Vech$dist_local,  Vech$dist_local_rs, Vech$dist_3a,   Vech$dist_3a_rs, 
                           Vech$dist_4e,     Vech$dist_4e_rs,    Vech$dist_4d,   Vech$dist_4d_rs, 
                           Vech$dist_4f,     Vech$dist_4f_rs,    Vech$dist_zwem, Vech$dist_zwem_rs,
                           Vech$chl,         Vech$doo,           Vech$ZM,        Vech$ZM_rs),
                  data= Vech, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))

sink()
#run regressions in 2000 and 2005

Vech$Selvar <- as.factor(Vech$Selvar)

subset0 <- subset(Vech, Selvar==0)
subset3 <- subset(subset0,Quarter==200001 | Quarter==200002 | Quarter==200003 | Quarter==200004)
subset4 <- subset(subset0,Quarter==200501 | Quarter==200502 | Quarter==200503 | Quarter==200504)


#check the variation of other variables

frequencies(subset3$bwpr1905)

frequencies(subset3$bwpr6070)

frequencies(subset3$bwpr7180)
frequencies(subset3$bwpr8190)
frequencies(subset3$bwpr9100)



## OLS regressions for 2000, stepwise adding sets of variables (model A-E)

Vech2000_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset3)
Vech2000_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs,
                     data=subset3)
Vech2000_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs ,
                     data=subset3)
Vech2000_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs ,
                     data=subset3)
Vech2000_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs + chl + doo + ZM_rs,
                     data=subset3)
#stargazer(OLSVech1, OLSVech2, OLSVech3, OLSVech4,OLSVech5, out="C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles/Vech_OLS_2000.htm")
stargazer(Vech2000_OLS_A, Vech2000_OLS_B, Vech2000_OLS_C, Vech2000_OLS_D, Vech2000_OLS_E, out="Results/Vech_OLS_2000.htm")

## OLS regressions for 2005, stepwise adding sets of variables (model A-E)
Vech2005_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset4)
Vech2005_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs,
                     data=subset4)
Vech2005_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs + dist_local_rs,
                     data=subset4)
Vech2005_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs,
                     data=subset4)
Vech2005_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                     + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                     data=subset4)

stargazer(Vech2005_OLS_A, Vech2005_OLS_B, Vech2005_OLS_C, Vech2005_OLS_D, Vech2005_OLS_E, out="Results/Vech_OLS_2005.htm")

#stargazer(OLSVech6, OLSVech7, OLSVech8, OLSVech9,OLSVech10, out="C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles/Vech_OLS_2005.htm")
#stargazer(OLSVech1, OLSVech6, OLSVech2, OLSVech7,OLSVech3,OLSVech8, OLSVech4, OLSVech9, OLSVech5,OLSVech10, out="C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles/Vech_OLS_2001&2005.htm")
#stargazer(OLSVech6, OLSVech7, OLSVech8, OLSVech9,OLSVech10, out="Results/Vech_OLS_2005.htm")

stargazer(Vech2000_OLS_E,Vech2005_OLS_E, out="Results/Vech_OLS_2000&2005_E.htm")

writeOGR(subset3, "C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles", "Vech2000", driver="ESRI Shapefile")
frequencies(subset$Quarter)
subset$Quarter   <- as.numeric(subset$Quarter)
hist(subset$Quarter)


#Spatial Analysis for Vech 2000

#IDs<-row.names(as(centroids2000))
#plot(centroids2000,xlab="longitude",ylab="latitude")
#plot(subset3,add=TRUE)
#title("Vech2000 with centroids")
#plot(subset3,border="black")
#invisible(text(centroids, labels=as.character(Vech$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Vech with number label")

# four nearest neighbours: create weighing matrix for 2000
centroids2000 <- coordinates(subset3)
knearneigh4 <- knearneigh(centroids2000,k=4)
knn4 <- knn2nb(knearneigh4)
#plot(subset3, border="grey")
#plot(knn1, centroids2000, add=TRUE)
#title(main="Vechplassen, 4 nearest neighbours")
knn4_W  <-nb2listw(knn4)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Vech_2000_4NN <- lm.LMtests(Vech2000_OLS_E, knn4_W, zero.policy=TRUE, test="all")


Vech2000_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset3, knn4_W)


Vech2000_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset3, knn4_W)
#summary(Vechlag)

# ten nearest neighbours: create weighing matrix for 2000
knearneigh10 <- knearneigh(centroids2000,k=10)
knn10 <- knn2nb(knearneigh10)
#plot(subset3, border="grey")
#plot(knn10, centroids2000, add=TRUE)
#title(main="Vechplassen, 10 nearest neighbours")
knn10_W  <-nb2listw(knn10)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Vech_2000_10NN <- lm.LMtests(Vech2000_OLS_E, knn10_W, zero.policy=TRUE, test="all")

Vech2000_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn10_W)


Vech2000_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn10_W)

stargazer(Vech2000_OLS_E, Vech2000_SPL_E_NN4, Vech2000_SPE_E_NN4, Vech2000_SPL_E_NN10, Vech2000_SPE_E_NN10, out="Results/SpatialVech2000.html")


# twentyfive nearest neighbours: create weighing matrix for 2000
knearneigh25 <- knearneigh(centroids2000,k=25)
knn25 <- knn2nb(knearneigh25)
#plot(subset3, border="grey")
#plot(knn25, centroids2000, add=TRUE)
#title(main="Vechplassen, 25 nearest neighbours")
knn25_W  <-nb2listw(knn25)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm25 <- lm.LMtests(Vech2000_OLS_E, knn25_W, zero.policy=TRUE, test="all")
lm2000_25NN <-lm25

Vech2000_SPL_E_NN25<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn25_W)


Vech2000_SPE_E_NN25<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn25_W)

stargazer(Vech2000_OLS_E, Vech2000_SPL_E_NN4, Vech2000_SPE_E_NN4, Vech2000_SPL_E_NN10, Vech2000_SPE_E_NN10, out="Results/SpatialVech2000EXTRA.html")

#Spatial Analysis for Vech 2005

#IDs<-row.names(as(centroids2005))
#plot(centroids2005,xlab="longitude",ylab="latitude")
#plot(subset4,add=TRUE)
#title("Vech2005 with centroids")
#plot(subset4,border="black")
#invisible(text(centroids, labels=as.character(Vech$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Vech with number label")

centroids2005 <- coordinates(subset4)
knearneigh4_2005<- knearneigh(centroids2005,k=4)
knn4_2005 <- knn2nb(knearneigh4_2005)
#plot(subset4, border="grey")
#plot(knn12005, centroids2005, add=TRUE)
#title(main="Vechplassen, 4 nearest neighbours")
knn4_W2005  <-nb2listw(knn4_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Vech_2005_4NN <- lm.LMtests(Vech2005_OLS_E, knn4_W2005, zero.policy=TRUE, test="all")

Vech2005_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset4, knn4_W2005)
Vech2005_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset4, knn4_W2005)

# 10 neighbour for 2005
knearneigh102005 <- knearneigh(centroids2005,k=10)
knn10_2005 <- knn2nb(knearneigh102005)
#plot(subset4, border="grey")
#plot(knn102005, centroids2005, add=TRUE)
#title(main="Vechplassen, 10 nearest neighbours")
knn10_W2005  <-nb2listw(knn10_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm_Vech_2005_10NN <- lm.LMtests(Vech2005_OLS_E, knn10_W2005, zero.policy=TRUE, test="all")

Vech2005_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset4, knn10_W2005)
Vech2005_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                                + bwpr1905 + bwpr6070 + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset4, knn10_W2005)

#stargazer(Vechlag,Vecherror,Vechlag2,Vecherror2, Vech10nlag,Vech10nerror,Vech10nlag2,Vech10nerror2,out="C:/Users/Zhong Xiao Leng/Desktop/Vech-shapefiles/Vech_LAG_2000&2005.txt")

#Spatial analysis results comparison
stargazer(Vech2005_OLS_E, Vech2005_SPL_E_NN4, Vech2005_SPE_E_NN4, Vech2005_SPL_E_NN10, Vech2005_SPE_E_NN10, out="Results/SpatialVech2005.html")

#Comparison between years for model E for three regressions OLS, Spatial Error and Spatial Lag (10 Nearest Neighbours) 
stargazer(Vech2000_OLS_E, Vech2000_SPL_E_NN10, Vech2000_SPE_E_NN10, Vech2005_OLS_E, Vech2005_SPL_E_NN10, Vech2005_SPE_E_NN10, out="Results/SpatialVech2000&2005.html")


sink('Results/Analysis-output_Vechplassen.txt', append=TRUE)
cat("=========================================================================\n")
cat("===RESULTS FOR 2000 =====================================================\n")
cat("=========================================================================\n")
cat("Regression results 2000 - OLS regression (stepwise) the rescaling of \n")
cat("variables \n")
cat("=========================================================================\n")
cat("===Vechplassen, 2000, model A============================================\n")
summary(Vech2000_OLS_A)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, model B============================================\n")
summary(Vech2000_OLS_B)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, model C============================================\n")
summary(Vech2000_OLS_C)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, model D============================================\n")
summary(Vech2000_OLS_D)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, model E============================================\n")
summary(Vech2000_OLS_E)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, spatial dependence test on model E, 4NN ===========\n")
lm_Vech_2000_4NN
cat("=========================================================================\n")
cat("===Vechplassen, 2000, spatial lag model E, 4NN ==========================\n")
summary(Vech2000_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, spatial error model E, 4NN ========================\n")
summary(Vech2000_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, spatial dependence test on model E, 10NN ==========\n")
lm_Vech_2000_10NN
cat("=========================================================================\n")
cat("===Vechplassen, 2000, spatial lag model E, 10NN =========================\n")
summary(Vech2000_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Vechplassen, 2000, spatial error model E, 10NN =======================\n")
summary(Vech2000_SPE_E_NN10)
cat("=========================================================================\n")
cat("===RESULTS FOR 2005 =====================================================\n")
cat("=========================================================================\n")
cat("===Vechplassen, 2005, model A============================================\n")
summary(Vech2005_OLS_A)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, model B============================================\n")
summary(Vech2005_OLS_B)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, model C============================================\n")
summary(Vech2005_OLS_C)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, model D============================================\n")
summary(Vech2005_OLS_D)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, model E============================================\n")
summary(Vech2005_OLS_E)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, spatial dependence test on model E, 4NN ===========\n")
lm_Vech_2005_4NN
cat("=========================================================================\n")
cat("===Vechplassen, 2005, spatial lag model E, 4NN ==========================\n")
summary(Vech2005_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, spatial error model E, 4NN ========================\n")
summary(Vech2005_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, spatial dependence test on model E, 10NN ==========\n")
lm_Vech_2005_10NN
cat("=========================================================================\n")
cat("===Vechplassen, 2005, spatial lag model E, 10NN =========================\n")
summary(Vech2005_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Vechplassen, 2005, spatial error model E, 10NN========================\n")
summary(Vech2005_SPE_E_NN10)
cat("=========================================================================\n")


sink()




