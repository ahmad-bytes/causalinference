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

dat_use = dat[dat$Z==1,]

#' Create training data that uses all the covariates (not the outcome)
xt=as.matrix(dat_use[,!(names(dat_use) %in% exclude_covs )])  

head(xt)
#' Create test data that includes the covariates of all the treated units
#' but sets the treatment variable = 0
#' This will be used for predicting the *other* potential outcome for the treated units only
#' i.e., for estimating the ATT
xp=as.matrix(dat_use[dat_use$Z==1,!(names(dat_use) %in% exclude_covs )]) 
xp[,14]=0
y=as.numeric(dat_use[,18])
head(dat_use)
#' Fit the BART model
bart.tot <- bart(x.train=xt,   y.train=y,  x.test=xp)
#' First calculate MCMC simulations of individual treatment effects by subtracting
#' the observed outcome among treated units from the predicted values had they been
#' untreated (i.e., the "test predictions")

mean(bart.tot$sigma)

diffs=bart.tot$yhat.train[,dat_use$Z==1]-bart.tot$yhat.test 

head(diffs) # a matrix with 1000 MCMC samples (rows) for each of 218 treated units (columns)
dim(diffs)
#' Row means correspond to the SATE for each MCMC iteration
mndiffs=apply(diffs,1,mean)
length(mndiffs) #A vector of 1000 simulations of the SATE
ATT_bart = mean(mndiffs) # Posterior mean SATE
ATT_bart
sdATT_bart = sd(mndiffs) # Posterior standard deviation of the SATE
sdATT_bart
