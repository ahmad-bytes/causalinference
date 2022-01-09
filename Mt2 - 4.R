rm(list = ls())

library("MatchIt")
library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")
library(BayesTree)


dat <- read.csv("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/mt2_df.csv")
head(dat)
dim(dat)

exclude_covs = c("ZIP","ScrubbedFacility","Region","propscore","PM")
include_covs = c("TotPop","PctUrban","PctWhite","PctBlack","PctHisp","PctHighSchool","MedianHHInc","PctPoor","PctOccupied","meanSulfur","totOpTime"," totHeatInput","Phase2")

dat_use_treatment = dat[dat$Z==1,]
xt=as.matrix(dat_use_treatment[,!(names(dat_use_treatment) %in% exclude_covs )])  
xp=as.matrix(dat_use_treatment[dat_use_treatment$Z==1,!(names(dat_use_treatment) %in% exclude_covs )]) 
xp[,14]=0
y=as.numeric(dat_use_treatment[,18])
bart.tot_treatment <- bart(x.train=xt,   y.train=y,  x.test=xp)

dat_use_control = dat[dat$Z==0,]
xt=as.matrix(dat_use_control[,!(names(dat_use_control) %in% exclude_covs )])  
xp=as.matrix(dat_use_control[dat_use_control$Z==0,!(names(dat_use_control) %in% exclude_covs )]) 
xp[,14]=1
y=as.numeric(dat_use_control[,18])
bart.tot_control <- bart(x.train=xt,   y.train=y,  x.test=xp)


bart.tot_treatment$yhat.train
bart.tot_control$yhat.train




diffs = bart.tot_treatment$yhat.train[0] - bart.tot_control$yhat.train[0]
head(diffs) # a matrix with 1000 MCMC samples (rows) for each of 218 treated units (columns)
dim(diffs)
#' Row means correspond to the SATE for each MCMC iteration
mndiffs=apply(diffs,1,mean)
length(mndiffs) #A vector of 1000 simulations of the SATE
ATT_bart = mean(mndiffs) # Posterior mean SATE
ATT_bart
sdATT_bart = sd(mndiffs) # Posterior standard deviation of the SATE
sdATT_bart