

if(Sys.info()["user"] == "Zhong Xiao Leng"){
  dataPath <- "C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles"
  setwd("C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles")
}

#D:/Analyses/HedonicSpatialWQ/SurveyData

#install.packages("rgdal")
library(rgdal)
library(Deducer)

# Reading the data from a shapefile (all data for Rijnplassen)




Rijn <- readOGR(dsn = "C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles", layer = "newRijn")

#Check on the data: cross-table for the year and the quarters of a year
table(Rijn$jaar,Rijn$quarter)

#VL Identifying the missing values for the water quality variables.
#VL Initially, the variables are factors but we will transform them into numerical variables
options(max.print=1000000)
frequencies(Rijn$chl)
#239        NA      39127    49.5        100.0
frequencies(Rijn$doo)
#402        NA      29084    36.8        100.0
frequencies(Rijn$ZM)
#315      NA      36382    46.0        100.0
frequencies(Rijn$geminkinko)
#123    NA        511     0.6        100.0
frequencies(Rijn$bevdicht)
#669    NA        266     0.3        100.0
frequencies(Rijn$pnietact)
#47    NA       4784     6.0        100.0
frequencies(Rijn$raildist)
frequencies(Rijn$highdist)
frequencies(Rijn$dist_local)
frequencies(Rijn$dist_3a)
frequencies(Rijn$dist_4f)
#4848    NA       8830    11.2        100.0
frequencies(Rijn$dist_4d)
#4612    NA      52313    66.1        100.0
frequencies(Rijn$dist_4e)
#4849    NA       1900     2.4        100.0
frequencies(Rijn$dist_zwem)
#4778    NA      13475    17.0        100.0
frequencies(Rijn$nbadk)
frequencies(Rijn$isol)
frequencies(Rijn$jaar)
frequencies(Rijn$quarter)



# A set of variables are read in as factors but should be numerical variables
# Don't worry about the missing variable levels of these variables as they have been identified above
Rijn$bevdicht   <- as.numeric(Rijn$bevdicht)
Rijn$pnietact   <- as.numeric(Rijn$pnietact)
Rijn$geminkinko <- as.numeric(Rijn$geminkinko)
Rijn$raildist   <- as.numeric(Rijn$raildist)
Rijn$highdist   <- as.numeric(Rijn$highdist)
Rijn$dist_local <- as.numeric(Rijn$dist_local)
Rijn$dist_3a    <- as.numeric(Rijn$dist_3a)
Rijn$dist_4f    <- as.numeric(Rijn$dist_4f)
Rijn$dist_4d    <- as.numeric(Rijn$dist_4d)
Rijn$dist_4e    <- as.numeric(Rijn$dist_4e)
Rijn$dist_zwem  <- as.numeric(Rijn$dist_zwem)
Rijn$nbadk      <- as.numeric(Rijn$nbadk)
Rijn$isol       <- as.numeric(Rijn$isol)

#VL New: Also the water quality variables have to be redefined
Rijn$chl       <- as.numeric(Rijn$chl)
Rijn$doo       <- as.numeric(Rijn$doo)
Rijn$ZM        <- as.numeric(Rijn$ZM)


Rijn$Selvar     <- 0
Rijn$Selvar     <- ifelse(Rijn$chl        == 239 , 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$doo        == 402, 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$ZM         == 315, 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$geminkinko == 123 , 1, Rijn$Selvar)

Rijn$Selvar     <- ifelse(Rijn$bevdicht   == 669 , 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$pnietact   == 47 , 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$dist_4f   == 4848 , 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$dist_4d   == 4612 , 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$dist_4e   == 4849 , 1, Rijn$Selvar)
Rijn$Selvar     <- ifelse(Rijn$dist_zwem   == 4778 , 1, Rijn$Selvar)
frequencies(Rijn$Selvar)



#New: create the Quarter from the variables "jaar" and "quarter"
Rijn$Quarter <- (1994+as.numeric(Rijn$jaar))*100+as.numeric(Rijn$quarter)
frequencies(Rijn$Quarter)
Rijn$Quarter <- as.factor(Rijn$Quarter)
#***************************** END OF MODULE *************************************************************


############################## START OF MODULE ###########################################################
## Descriptive statistics for Rijnplassen
## ro the limited degrees of freedom. 
##########################################################################################################
frequencies(Rijn$Selvar)

library(Deducer)
descriptive.table(vars = d(Rijn$lnprice,  Rijn$lnwoonop, Rijn$nbadk,      Rijn$isol,
                           Rijn$bwpr1905, Rijn$bwpr6070, Rijn$bwpr7180,   Rijn$bwpr8190, 
                           Rijn$bwpr9100, Rijn$bevdicht, Rijn$pnietact,   Rijn$geminkinko, 
                           Rijn$raildist, Rijn$highdist, Rijn$dist_local, Rijn$dist_3a,  
                           Rijn$dist_4d,  Rijn$dist_4e,  Rijn$dist_4f,    Rijn$dist_zwem, 
                           Rijn$chl,      Rijn$doo,      Rijn$ZM        ),
                  strata =  Rijn$Selvar,
                  data= Rijn, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

t.test(lnprice    ~ Selvar, data=Rijn) 
t.test(lnwoonop   ~ Selvar, data=Rijn) 
t.test(nbadk      ~ Selvar, data=Rijn) 
t.test(isol       ~ Selvar, data=Rijn) 

t.test(bevdicht   ~ Selvar, data=Rijn) 
t.test(pnietact   ~ Selvar, data=Rijn) 
t.test(geminkinko ~ Selvar, data=Rijn) 
t.test(raildist   ~ Selvar, data=Rijn) 

t.test(highdist   ~ Selvar, data=Rijn) 
t.test(dist_local ~ Selvar, data=Rijn) 
t.test(dist_3a    ~ Selvar, data=Rijn) 
t.test(dist_4d    ~ Selvar, data=Rijn) 

t.test(dist_4e    ~ Selvar, data=Rijn) 
t.test(dist_4f    ~ Selvar, data=Rijn) 
t.test(dist_zwem  ~ Selvar, data=Rijn) 
t.test(chl        ~ Selvar, data=Rijn) 

t.test(doo        ~ Selvar, data=Rijn) 
t.test(ZM         ~ Selvar, data=Rijn) 

frequencies(Rijn$Quarter)
frequencies(Rijn$inpandig)
frequencies(Rijn$tuin_zow)
frequencies(Rijn$buff25)
Rijn$dist_3a_rs
Rijn$dist_4d_rs 
frequencies(Rijn$dist_4d_rs )
sink('Results/Analysis-output_Rijnplassen.txt', append=TRUE)
cat("=========================================================================\n")
cat("Checking the rescaling of variables \n")
cat("=========================================================================\n")
#Some rescaling
Rijn$dist_3a_rs      <-Rijn$dist_3a/1000
Rijn$dist_4d_rs      <-Rijn$dist_4d/1000
Rijn$dist_4e_rs     <-Rijn$dist_4e/1000
Rijn$dist_4f_rs      <-Rijn$dist_4f/1000
Rijn$dist_zwem_rs    <-Rijn$dist_zwem/1000
Rijn$dist_local_rs   <-Rijn$dist_local/1000
Rijn$highdist_rs     <-Rijn$highdist/1000
Rijn$raildist_rs     <-Rijn$raildist/1000
Rijn$ZM_rs          <-Rijn$ZM/1000

descriptive.table(vars = d(Rijn$lnprice,  Rijn$lnwoonop, Rijn$isol, Rijn$bevdicht, Rijn$pnietact,
                           Rijn$geminkinko),
                  data= Rijn, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))


descriptive.table(vars = d(Rijn$raildist,    Rijn$raildist_rs,   Rijn$highdist,  Rijn$highdist_rs,
                           Rijn$dist_local,  Rijn$dist_local_rs, Rijn$dist_3a,   Rijn$dist_3a_rs, 
                           Rijn$dist_4e,     Rijn$dist_4e_rs,    Rijn$dist_4d,   Rijn$dist_4d_rs, 
                           Rijn$dist_4f,     Rijn$dist_4f_rs,    Rijn$dist_zwem, Rijn$dist_zwem_rs,
                           Rijn$chl,         Rijn$doo,           Rijn$ZM,        Rijn$ZM_rs),
                  data= Rijn, 
                  func.names = c("Mean","St. Deviation", "Min", "Max"))


#run regressions in 2000 and 2005

Rijn$Selvar <- as.factor(Rijn$Selvar)

subset0 <- subset(Rijn, Selvar==0)
subset3 <- subset(subset0,Quarter==200001 | Quarter==200002 | Quarter==200003 | Quarter==200004)
subset4 <- subset(subset0,Quarter==200501 | Quarter==200502 | Quarter==200503 | Quarter==200504)


#check the variation of other variables

frequencies(subset3$bwpr1905)
frequencies(subset4$bwpr1905)
#too little variation
frequencies(subset3$bwpr6070)
#too little variation
frequencies(subset3$bwpr7180)
frequencies(subset3$bwpr8190)
frequencies(subset3$bwpr9100)



## OLS regressions for 2000, stepwise adding sets of variables (model A-E)

Rijn2000_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset3)
Rijn2000_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs,
                     data=subset3)
Rijn2000_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs ,
                     data=subset3)
Rijn2000_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs ,
                     data=subset3)
Rijn2000_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs + chl + doo + ZM_rs,
                     data=subset3)
#stargazer(OLSRijn1, OLSRijn2, OLSRijn3, OLSRijn4,OLSRijn5, out="C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles/Rijn_OLS_2000.htm")
stargazer(Rijn2000_OLS_A, Rijn2000_OLS_B, Rijn2000_OLS_C, Rijn2000_OLS_D, Rijn2000_OLS_E, out="Results/Rijn_OLS_2000.htm")

## OLS regressions for 2005, stepwise adding sets of variables (model A-E)
Rijn2005_OLS_A <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 ,
                     data=subset4)
Rijn2005_OLS_B <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs,
                     data=subset4)
Rijn2005_OLS_C <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko +  raildist_rs + highdist_rs + dist_local_rs,
                     data=subset4)
Rijn2005_OLS_D <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs,
                     data=subset4)
Rijn2005_OLS_E <- lm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                    + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                       pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                       dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                     data=subset4)

stargazer(Rijn2005_OLS_A, Rijn2005_OLS_B, Rijn2005_OLS_C, Rijn2005_OLS_D, Rijn2005_OLS_E, out="Results/Rijn_OLS_2005.htm")

#stargazer(OLSRijn6, OLSRijn7, OLSRijn8, OLSRijn9,OLSRijn10, out="C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles/Rijn_OLS_2005.htm")
#stargazer(OLSRijn1, OLSRijn6, OLSRijn2, OLSRijn7,OLSRijn3,OLSRijn8, OLSRijn4, OLSRijn9, OLSRijn5,OLSRijn10, out="C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles/Rijn_OLS_2001&2005.htm")
#stargazer(OLSRijn6, OLSRijn7, OLSRijn8, OLSRijn9,OLSRijn10, out="Results/Rijn_OLS_2005.htm")

stargazer(Rijn2000_OLS_E,Rijn2005_OLS_E, out="Results/Rijn_OLS_2000&2005_E.htm")

writeOGR(subset3, "C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles", "Rijn2000", driver="ESRI Shapefile")
frequencies(subset$Quarter)
subset$Quarter   <- as.numeric(subset$Quarter)
hist(subset$Quarter)


#Spatial Analysis for Rijn 2000

#IDs<-row.names(as(centroids2000))
#plot(centroids2000,xlab="longitude",ylab="latitude")
#plot(subset3,add=TRUE)
#title("Rijn2000 with centroids")
#plot(subset3,border="black")
#invisible(text(centroids, labels=as.character(Rijn$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Rijn with number label")

# four nearest neighbours: create weighing matrix for 2000
centroids2000 <- coordinates(subset3)
knearneigh4 <- knearneigh(centroids2000,k=4)
knn4 <- knn2nb(knearneigh4)
#plot(subset3, border="grey")
#plot(knn1, centroids2000, add=TRUE)
#title(main="Rijnplassen, 4 nearest neighbours")
knn4_W  <-nb2listw(knn4)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm <- lm.LMtests(Rijn2000_OLS_E, knn4_W, zero.policy=TRUE, test="all")
lm2000_4NN <-lm

Rijn2000_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                            + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset3, knn4_W)


Rijn2000_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset3, knn4_W)
#summary(Rijnlag)

# ten nearest neighbours: create weighing matrix for 2000
knearneigh10 <- knearneigh(centroids2000,k=10)
knn10 <- knn2nb(knearneigh10)
#plot(subset3, border="grey")
#plot(knn10, centroids2000, add=TRUE)
#title(main="Rijnplassen, 10 nearest neighbours")
knn10_W  <-nb2listw(knn10)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm10 <- lm.LMtests(Rijn2000_OLS_E, knn10_W, zero.policy=TRUE, test="all")
lm2000_10NN <-lm10

Rijn2000_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn10_W)


Rijn2000_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn10_W)

stargazer(Rijn2000_OLS_E, Rijn2000_SPL_E_NN4, Rijn2000_SPE_E_NN4, Rijn2000_SPL_E_NN10, Rijn2000_SPE_E_NN10, out="Results/SpatialRijn2000.html")


# twentyfive nearest neighbours: create weighing matrix for 2000
knearneigh25 <- knearneigh(centroids2000,k=25)
knn25 <- knn2nb(knearneigh25)
#plot(subset3, border="grey")
#plot(knn25, centroids2000, add=TRUE)
#title(main="Rijnplassen, 25 nearest neighbours")
knn25_W  <-nb2listw(knn25)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm25 <- lm.LMtests(Rijn2000_OLS_E, knn25_W, zero.policy=TRUE, test="all")
lm2000_25NN <-lm25

Rijn2000_SPL_E_NN25<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset3, knn25_W)


Rijn2000_SPE_E_NN25<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset3, knn25_W)

stargazer(Rijn2000_OLS_E, Rijn2000_SPL_E_NN4, Rijn2000_SPE_E_NN4, Rijn2000_SPL_E_NN10, Rijn2000_SPE_E_NN10, out="Results/SpatialRijn2000EXTRA.html")

#Spatial Analysis for Rijn 2005

#IDs<-row.names(as(centroids2005))
#plot(centroids2005,xlab="longitude",ylab="latitude")
#plot(subset4,add=TRUE)
#title("Rijn2005 with centroids")
#plot(subset4,border="black")
#invisible(text(centroids, labels=as.character(Rijn$obj_id), cex=0.4))
#invisible(text(centroids, labels=1, cex=0.4))
#title("Rijn with number label")

centroids2005 <- coordinates(subset4)
knearneigh4_2005<- knearneigh(centroids2005,k=4)
knn4_2005 <- knn2nb(knearneigh4_2005)
#plot(subset4, border="grey")
#plot(knn12005, centroids2005, add=TRUE)
#title(main="Rijnplassen, 4 nearest neighbours")
knn4_W2005  <-nb2listw(knn4_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm2005 <- lm.LMtests(Rijn2005_OLS_E, knn4_W2005, zero.policy=TRUE, test="all")
lm2005_4NN <-lm2005

Rijn2005_SPL_E_NN4<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                            + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                               pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                               dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                             data=subset4, knn4_W2005)
Rijn2005_SPE_E_NN4<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                              + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                 pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                 dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                               data=subset4, knn4_W2005)

# 10 neighbour for 2005
knearneigh102005 <- knearneigh(centroids2005,k=10)
knn10_2005 <- knn2nb(knearneigh102005)
#plot(subset4, border="grey")
#plot(knn102005, centroids2005, add=TRUE)
#title(main="Rijnplassen, 10 nearest neighbours")
knn10_W2005  <-nb2listw(knn10_2005)

# test OLS models with weighing matrix and run spatial error and spatial lag model for model E
lm200510n <- lm.LMtests(Rijn2005_OLS_E, knn10_W2005, zero.policy=TRUE, test="all")
lm2005_10NN <-lm200510n
Rijn2005_SPL_E_NN10<-lagsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                             + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                              data=subset4, knn10_W2005)
Rijn2005_SPE_E_NN10<-errorsarlm(lnprice ~ factor(Quarter) + lnwoonop + nbadk + factor(inpandig) + isol + factor(tuin_zow) 
                               + bwpr7180 + bwpr8190 + bwpr9100 + bevdicht + 
                                  pnietact + geminkinko + + raildist_rs + highdist_rs + dist_local_rs + factor(buff25) + 
                                  dist_3a_rs + dist_4d_rs + dist_4e_rs + dist_4f_rs + dist_zwem_rs  + chl + doo + ZM_rs,
                                data=subset4, knn10_W2005)

#stargazer(Rijnlag,Rijnerror,Rijnlag2,Rijnerror2, Rijn10nlag,Rijn10nerror,Rijn10nlag2,Rijn10nerror2,out="C:/Users/Zhong Xiao Leng/Desktop/Rijn-shapefiles/Rijn_LAG_2000&2005.txt")

#Spatial analysis results comparison
stargazer(Rijn2005_OLS_E, Rijn2005_SPL_E_NN4, Rijn2005_SPE_E_NN4, Rijn2005_SPL_E_NN10, Rijn2005_SPE_E_NN10, out="Results/SpatialRijn2005.html")

#Comparison between years for model E for three regressions OLS, Spatial Error and Spatial Lag (10 Nearest Neighbours) 
stargazer(Rijn2000_OLS_E, Rijn2000_SPL_E_NN10, Rijn2000_SPE_E_NN10, Rijn2005_OLS_E, Rijn2005_SPL_E_NN10, Rijn2005_SPE_E_NN10, out="Results/SpatialRijn2000&2005.html")

rm(Rijn2004, Rijn2005_OLS_b, Rijn2005_OLS_c, Rijn2005_OLS_d)
rm(Rijn25nerror, Rijn25nlag, Rijnerror, Rijnerror2, Rijnlag, Rijnlag2)
rm(RijnSubset, RijnSubSet, RijnSubsetln)
rm(OLSRijn, OLSRijn_A, OLSRijn1, OLSRijn10)
rm(OLSRijn1996, OLSRijn2, OLSRijn2000, OLSRijn2004)
rm(OLSRijn3, OLSRijn4, OLSRijn5, OLSRijn6)
rm(OLSRijn7, OLSRijn8, OLSRijn9, OLSRijn_B, OLSRijn_C, OLSRijn_D, OLSRijn_E)
rm(SPERijn_E, SPLRijn_E, queen1)
rm(Rijn10nerror, Rijn10nerror2, Rijn10nlag, Rijn10nlag2)
rm(subset)
rm(knnSubSet, knnSubSet1, knnSubSet1_w)

rm(DataHedonic)
rm(centroidsSubSet)

sink('Results/Analysis-output_Rijnplassen.txt', append=TRUE)
cat("=========================================================================\n")
cat("===RESULTS FOR 2000 =====================================================\n")
cat("=========================================================================\n")
cat("Regression results 2000 - OLS regression (stepwise) the rescaling of \n")
cat("variables \n")
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, model A============================================\n")
summary(Rijn2000_OLS_A)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, model B============================================\n")
summary(Rijn2000_OLS_B)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, model C============================================\n")
summary(Rijn2000_OLS_C)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, model D============================================\n")
summary(Rijn2000_OLS_D)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, model E============================================\n")
summary(Rijn2000_OLS_E)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, spatial dependence test on model E, 4NN ===========\n")
lm2000_4NN
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, spatial lag model E, 4NN ==========================\n")
summary(Rijn2000_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, spatial error model E, 4NN ========================\n")
summary(Rijn2000_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, spatial dependence test on model E, 10NN ==========\n")
lm2000_10NN
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, spatial lag model E, 10NN =========================\n")
summary(Rijn2000_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Rijnplassen, 2000, spatial error model E, 10NN =======================\n")
summary(Rijn2000_SPE_E_NN10)
cat("=========================================================================\n")
cat("===RESULTS FOR 2005 =====================================================\n")
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, model A============================================\n")
summary(Rijn2005_OLS_A)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, model B============================================\n")
summary(Rijn2005_OLS_B)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, model C============================================\n")
summary(Rijn2005_OLS_C)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, model D============================================\n")
summary(Rijn2005_OLS_D)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, model E============================================\n")
summary(Rijn2005_OLS_E)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, spatial dependence test on model E, 4NN ===========\n")
lm2005_4NN
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, spatial lag model E, 4NN ==========================\n")
sumamry(Rijn2005_SPL_E_NN4)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, spatial error model E, 4NN ========================\n")
summary(Rijn2005_SPE_E_NN4)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, spatial dependence test on model E, 10NN ==========\n")
lm2005_10NN
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, spatial lag model E, 10NN =========================\n")
summary(Rijn2005_SPL_E_NN10)
cat("=========================================================================\n")
cat("===Rijnplassen, 2005, spatial error model E, 10NN========================\n")
summary(Rijn2005_SPE_E_NN10)
cat("=========================================================================\n")


sink()

Rijn2005_SPE_E_NN10
summary(Rijn2005_SPE_E_NN10)
Rijn2005_SPE_E_NN10


