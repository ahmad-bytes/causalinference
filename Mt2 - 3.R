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

dat_use = dat[dat$Z==0,]

#' Create training data that uses all the covariates (not the outcome)
xt=as.matrix(dat_use[,!(names(dat_use) %in% exclude_covs )])  

head(xt)
#' Create test data that includes the covariates of all the treated units
#' but sets the treatment variable = 0
#' This will be used for predicting the *other* potential outcome for the treated units only
#' i.e., for estimating the ATT
xp=as.matrix(dat_use[dat_use$Z==0,!(names(dat_use) %in% exclude_covs )]) 
xp[,14]=1
y=as.numeric(dat_use[,18])
#' Fit the BART model
bart.tot <- bart(x.train=xt,   y.train=y,  x.test=xp)

mean(bart.tot$sigma)
