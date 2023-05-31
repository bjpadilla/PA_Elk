################################################################
################################################################
##########                                       ###############
##########          MODEL CROSS VALIDATION       ###############
##########                                       ###############
################################################################
################################################################
# Load libraries
library(INLA)
library(ggplot2)
library(tidyverse)

# Load in the model data and sort accordingly ####
mod_data <- read.csv("Seasonal_SSF_Analysis/inla_mod_data_prj2.csv")
# Create animal ID not animal ID year
mod_data$animal_ID <- substr(mod_data$ID_year,1,nchar(mod_data$ID_year)-5)
# we will use animal ID rather than ID year here as random effect
mod_data$anim <- as.numeric(as.factor(mod_data$ID_year))
mod_data$anim_agri <- mod_data$anim
mod_data$anim_canopy <- mod_data$anim
mod_data$animcover <- mod_data$anim
mod_data$anim_deci <- mod_data$anim
mod_data$anim_dev <- mod_data$anim
mod_data$anim_egmi <- mod_data$anim
mod_data$anim_elev <- mod_data$anim
mod_data$anim_gras <- mod_data$anim
mod_data$anim_slope <- mod_data$anim
mod_data$anim_NDVI <- mod_data$anim
mod_data$anim_NDVIsd <- mod_data$anim
mod_data$anim_forest <- mod_data$anim
mod_data$anim_open <- mod_data$anim
mod_data$anim_timber <- mod_data$anim
mod_data$anim_NDVIsd <- mod_data$anim
mod_data$anim_SL <- mod_data$anim
mod_data$anim_TA <- mod_data$anim
mod_data$anim_TOD <- mod_data$anim


# Create a factor for sex and random effect feature
mod_data$SEX <- factor(ifelse(mod_data$sex == 1, "female","male"))
mod_data$elksex <- as.factor(mod_data$sex)
mod_data$anim_sex <- mod_data$anim



mod_data$elevSQ <- mod_data$elev^2
mod_data$anim_elevSQ <- mod_data$anim
mod_data$NDVI_sq <- mod_data$NDVI_end ^2

# convert steps from degrees latitude to meters
mod_data$SL <- (2*pi*6371000*mod_data$sl_)/360
mod_data$logSL <- ifelse(log(mod_data$SL)<0,NA,log(mod_data$SL))

# compute cosine of turn angle
mod_data$cosTA <- cos(mod_data$ta_)

# create individual landcover variables 
mod_data$developed <- ifelse(mod_data$covertype == 1, 1, 0)
mod_data$deciduous <- ifelse(mod_data$covertype == 2, 1, 0)
mod_data$evg.mix <- ifelse(mod_data$covertype == 3, 1, 0)
mod_data$grass.shrub <- ifelse(mod_data$covertype == 4, 1, 0)
mod_data$agriculture <- ifelse(mod_data$covertype == 5, 1, 0)

# Create Forest and Open broader categories ####
mod_data$forest <- mod_data$deciduous + mod_data$evg.mix
mod_data$open <- mod_data$grass.shrub + mod_data$agriculture

# set mean and precision for the strata ID
mean.beta <- 0
prec.beta <- 1e-4

# set the # of individuals
n.indivs <- length(unique(mod_data$ID_year))

##############################
# read in model formula ######
formula.full <-  #########
case_ ~  -1 + SEX * (elev + elevSQ + slope_end + canopy + NDVI_end + NDVI_sq + developed + deciduous + evg.mix + grass.shrub + agriculture+ logSL + cosTA) +
  # random effect for step ID
  f(step_id_, model="iid", hyper=list(theta = list(initial=log(1e-6),fixed=T))) + 
  # effect for elevation
  f(anim_elev, elev, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_elevSQ, elevSQ, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  # effect for slope
  f(anim_slope, slope_end, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  # effect for slope
  f(anim_canopy, canopy, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_NDVI, NDVI_end, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_NDVIsd, NDVI_sq, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_dev, developed, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_egmi, evg.mix, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_gras, grass.shrub, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_agri, agriculture, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_deci, deciduous, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_SL, logSL, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_TA, cosTA, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05))))
######

########################################
##### Season-Sex Model #################
########################################

# create object to store output
df.crossval <- data.frame(matrix(NA, nrow=8,ncol = 5))
# extract predictions and bins
for(i in c(1:8)){
  # extract random subset of 20 individuals 
  elk_ids <- sample(mod_data$ID_year, 20, replace = FALSE)
  training <- mod_data %>% filter(!ID_year %in% elk_ids)
  testing <- mod_data %>% filter(!ID_year %in% elk_ids)
  testing$y.obs <- as.numeric(testing$case_)
  mod_train <- inla(formula.full, 
                    family ="Poisson", 
                    data = training, 
                    control.fixed = list(
                      mean = mean.beta,
                      prec = list(default = prec.beta)
                    ),
                    control.predictor = list(
                      compute = TRUE
                    ),
                    inla.mode = "experimental",
                    #control.compute = list(
                    #waic = TRUE,
                    #config = TRUE
                    #),
                    verbose = FALSE
  ) #####
  
  # build model matrix for testing dataset
  testmat <- matrix(nrow=nrow(testing), ncol = length(mod_train$summary.fixed[,1]))
  testmat[,1] <- ifelse(testing$SEX=="female",1,0)
  testmat[,2] <- ifelse(testing$SEX=="male",1,0)
  testmat[,3] <- testing$elev * ifelse(testing$SEX=="female",1,0) 
  testmat[,4] <- testing$elevSQ * ifelse(testing$SEX=="female",1,0)
  testmat[,5] <- testing$slope_end * ifelse(testing$SEX=="female",1,0)
  testmat[,6] <- testing$canopy * ifelse(testing$SEX=="female",1,0)
  testmat[,7] <- testing$NDVI_end * ifelse(testing$SEX=="female",1,0)
  testmat[,8] <- testing$NDVI_sq * ifelse(testing$SEX=="female",1,0)
  testmat[,9] <- testing$developed * ifelse(testing$SEX=="female",1,0)
  testmat[,10] <- testing$deciduous * ifelse(testing$SEX=="female",1,0)
  testmat[,11] <- testing$evg.mix * ifelse(testing$SEX=="female",1,0)
  testmat[,12] <- testing$grass.shrub * ifelse(testing$SEX=="female",1,0)
  testmat[,13] <- testing$agriculture * ifelse(testing$SEX=="female",1,0)
  testmat[,14] <- testing$logSL * ifelse(testing$SEX=="female",1,0)
  testmat[,15] <- testing$cosTA * ifelse(testing$SEX=="female",1,0)
  testmat[,16] <- testing$elev * ifelse(testing$SEX=="male",1,0)
  testmat[,17] <- testing$elevSQ * ifelse(testing$SEX=="male",1,0)
  testmat[,18] <- testing$slope_end * ifelse(testing$SEX=="male",1,0)
  testmat[,19] <- testing$canopy * ifelse(testing$SEX=="male",1,0)
  testmat[,20] <- testing$NDVI_end * ifelse(testing$SEX=="male",1,0)
  testmat[,21] <- testing$NDVI_sq * ifelse(testing$SEX=="male",1,0)
  testmat[,22] <- testing$developed * ifelse(testing$SEX=="male",1,0)
  testmat[,23] <- testing$deciduous * ifelse(testing$SEX=="male",1,0)
  testmat[,24] <- testing$evg.mix * ifelse(testing$SEX=="male",1,0)
  testmat[,25] <- testing$grass.shrub * ifelse(testing$SEX=="male",1,0)
  testmat[,26] <- testing$agriculture * ifelse(testing$SEX=="male",1,0)
  testmat[,27] <- testing$logSL * ifelse(testing$SEX=="male",1,0)
  testmat[,28] <- testing$cosTA * ifelse(testing$SEX=="male",1,0)
  
  preds <- as.numeric(exp(rowSums(testmat * mod_train$summary.fixed$mean)))
  # create bins of equal size 
  bins <- cut_number(preds, n=10)
  # mean of bins
  wbins <- tapply(preds, bins, mean)
  # area of bins
  abins <- tapply(preds,bins,length)
  ux <- wbins * abins / sum(wbins * abins)
  # expected # of obs in each bin
  Nx <- (sum(testing$y.obs)*ux) / sum(testing$y.obs)
  # actual observations in each bin
  nx <- (tapply(testing$y.obs, bins, sum)) / sum(testing$y.obs)
  cv.df <- data.frame(Nx = Nx, nx = nx)
  
  # regression: obs vs. expected
  lm <- lm(Nx~nx, data = cv.df)
  sp.mcor <- paste("Rank Correlation: ", round(cor(cv.df$Nx, cv.df$nx, method = "spearman"),2), sep = "")
  r2 <- paste("Regression: ", round(coef(lm)[1],3), " + ", round(coef(lm)[2],3), "R2: ", round(summary(lm)$r.square,2))
  
  # visualize regression between number of obs and expected used points
  plotmod<-ggplot(data=cv.df,mapping=aes(nx,Nx))+
    geom_point()+
    geom_smooth(method="lm")+
    labs(y="expected", x="observed", title=paste("Model: ",i,"/8",sep="" ))+
    annotate(geom="text",x=-Inf, y=Inf, label=sp.mcor, hjust=-0.6, vjust=5,size=5)+
    annotate(geom="text",x=-Inf, y=Inf, label=r2, hjust=-0.3, vjust=7,size=5)+
    geom_abline(intercept = 0, slope = 1)
  plot(plotmod)
  
  df.crossval[i,1] <- paste("Fold: ",i, sep = "")
  df.crossval[i,2]<-round(coef(lm)[1],2)
  df.crossval[i,3]<-round(coef(lm)[2],2)
  df.crossval[i,4]<-round(summary(lm)$r.square,2)
  df.crossval[i,5]<-round(cor(cv.df$Nx, cv.df$nx,method="spearman"),2)
}
colnames(df.crossval) <- c("Fold","lm(intercept)","lm(slope)","r2","Spearmans")

#################################################
####### Season - Time-of-Day Model ##############
#################################################
# model formula
formula.TOD <-  #########
case_ ~  -1 + tod_end_ * (elev + elevSQ + slope_end + canopy + NDVI_end + NDVI_sq + developed + deciduous + evg.mix + grass.shrub + agriculture+ logSL + cosTA) +
  # random effect for step ID
  f(step_id_, model="iid", hyper=list(theta = list(initial=log(1e-6),fixed=T))) + 
  # effect for elevation
  f(anim_elev, elev, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_elevSQ, elevSQ, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  # effect for slope
  f(anim_slope, slope_end, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  # effect for slope
  f(anim_canopy, canopy, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_NDVI, NDVI_end, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_NDVIsd, NDVI_sq, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_dev, developed, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_egmi, evg.mix, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_gras, grass.shrub, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_agri, agriculture, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_deci, deciduous, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_SL, logSL, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05)))) +
  f(anim_TA, cosTA, values=1:n.indivs, model="iid",  
    hyper=list(theta=list(initial=log(1), fixed = F,prior = "pc.prec", param = c(3,0.05))))
######

# create object to store output
df.crossval.tod <- data.frame(matrix(NA, nrow=8,ncol = 5))
# extract predictions and bins
for(i in c(1:8)){
  # extract random subset of 20 individuals 
  elk_ids <- sample(tod.DATA$ID_year, 20, replace = FALSE)
  training <- tod.DATA %>% filter(!ID_year %in% elk_ids)
  testing <- tod.DATA %>% filter(!ID_year %in% elk_ids)
  testing$y.obs <- as.numeric(testing$case_)
  mod_train <- inla(formula.TOD, 
                    family ="Poisson", 
                    data = training, 
                    control.fixed = list(
                      mean = mean.beta,
                      prec = list(default = prec.beta)
                    ),
                    control.predictor = list(
                      compute = TRUE
                    ),
                    inla.mode = "experimental",
                    #control.compute = list(
                    #waic = TRUE,
                    #config = TRUE
                    #),
                    verbose = FALSE
  ) #####
  
  # build model matrix for testing dataset
  testmat <- matrix(nrow=nrow(testing), ncol = length(mod_train$summary.fixed[,1]))
  testmat[,1] <- ifelse(testing$SEX=="day",1,0)
  testmat[,2] <- ifelse(testing$SEX=="night",1,0)
  testmat[,3] <- testing$elev * ifelse(testing$SEX=="day",1,0) 
  testmat[,4] <- testing$elevSQ * ifelse(testing$SEX=="day",1,0)
  testmat[,5] <- testing$slope_end * ifelse(testing$SEX=="day",1,0)
  testmat[,6] <- testing$canopy * ifelse(testing$SEX=="day",1,0)
  testmat[,7] <- testing$NDVI_end * ifelse(testing$SEX=="day",1,0)
  testmat[,8] <- testing$NDVI_sq * ifelse(testing$SEX=="day",1,0)
  testmat[,9] <- testing$developed * ifelse(testing$SEX=="day",1,0)
  testmat[,10] <- testing$deciduous * ifelse(testing$SEX=="day",1,0)
  testmat[,11] <- testing$evg.mix * ifelse(testing$SEX=="day",1,0)
  testmat[,12] <- testing$grass.shrub * ifelse(testing$SEX=="day",1,0)
  testmat[,13] <- testing$agriculture * ifelse(testing$SEX=="day",1,0)
  testmat[,14] <- testing$logSL * ifelse(testing$SEX=="day",1,0)
  testmat[,15] <- testing$cosTA * ifelse(testing$SEX=="day",1,0)
  testmat[,16] <- testing$elev * ifelse(testing$SEX=="night",1,0)
  testmat[,17] <- testing$elevSQ * ifelse(testing$SEX=="night",1,0)
  testmat[,18] <- testing$slope_end * ifelse(testing$SEX=="night",1,0)
  testmat[,19] <- testing$canopy * ifelse(testing$SEX=="night",1,0)
  testmat[,20] <- testing$NDVI_end * ifelse(testing$SEX=="night",1,0)
  testmat[,21] <- testing$NDVI_sq * ifelse(testing$SEX=="night",1,0)
  testmat[,22] <- testing$developed * ifelse(testing$SEX=="night",1,0)
  testmat[,23] <- testing$deciduous * ifelse(testing$SEX=="night",1,0)
  testmat[,24] <- testing$evg.mix * ifelse(testing$SEX=="night",1,0)
  testmat[,25] <- testing$grass.shrub * ifelse(testing$SEX=="night",1,0)
  testmat[,26] <- testing$agriculture * ifelse(testing$SEX=="night",1,0)
  testmat[,27] <- testing$logSL * ifelse(testing$SEX=="night",1,0)
  testmat[,28] <- testing$cosTA * ifelse(testing$SEX=="night",1,0)
  
  preds <- as.numeric(exp(rowSums(testmat * mod_train$summary.fixed$mean)))
  # create bins of equal size 
  bins <- cut_number(preds, n=10)
  # mean of bins
  wbins <- tapply(preds, bins, mean)
  # area of bins
  abins <- tapply(preds,bins,length)
  ux <- wbins * abins / sum(wbins * abins)
  # expected # of obs in each bin
  Nx <- (sum(testing$y.obs)*ux) / sum(testing$y.obs)
  # actual observations in each bin
  nx <- (tapply(testing$y.obs, bins, sum)) / sum(testing$y.obs)
  cv.df <- data.frame(Nx = Nx, nx = nx)
  
  # regression: obs vs. expected
  lm <- lm(Nx~nx, data = cv.df)
  sp.mcor <- paste("Rank Correlation: ", round(cor(cv.df$Nx, cv.df$nx, method = "spearman"),2), sep = "")
  r2 <- paste("Regression: ", round(coef(lm)[1],3), " + ", round(coef(lm)[2],3), "R2: ", round(summary(lm)$r.square,2))
  
  # visualize regression between number of obs and expected used points
  plotmod<-ggplot(data=cv.df,mapping=aes(nx,Nx))+
    geom_point()+
    geom_smooth(method="lm")+
    labs(y="expected", x="observed", title=paste("Model: ",i,"/8",sep="" ))+
    annotate(geom="text",x=-Inf, y=Inf, label=sp.mcor, hjust=-0.6, vjust=5,size=5)+
    annotate(geom="text",x=-Inf, y=Inf, label=r2, hjust=-0.3, vjust=7,size=5)+
    geom_abline(intercept = 0, slope = 1)
  plot(plotmod)
  
  df.crossval.tod[i,1] <- paste("Fold: ",i, sep = "")
  df.crossval.tod[i,2]<-round(coef(lm)[1],2)
  df.crossval.tod[i,3]<-round(coef(lm)[2],2)
  df.crossval.tod[i,4]<-round(summary(lm)$r.square,2)
  df.crossval.tod[i,5]<-round(cor(cv.df$Nx, cv.df$nx,method="spearman"),2)
}
colnames(df.crossval.tod) <- c("Fold","lm(intercept)","lm(slope)","r2","Spearmans")
