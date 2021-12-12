rm(list = ls())
library(BayesTree)
library(rstanarm)
library(ggplot2)

#'
#' We'll use the data from the Zigler et al (2018) Impact of Nonattainment Designations 
#' paper.  Note that the example below may not be a defensible way to analyze these data, but is just 
#' being used to illustrate how BART might work in reality
#'
#'

dat <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/bart_sim2.txt")
head(dat)
dim(dat)

dat_use = dat
dim(dat_use)


#' Fit a linear regression model that includes all the covariates as linear terms
mod1 <- stan_glm(Z ~ X1 + X2 + X1:X2, family=binomial(link="logit") , data=dat_use)
pscores_1 = predict(mod1 , type = 'response')

#' Identify whether each unit has an estimated propensity score that 'overlaps' with
#' with the distribution in the other treatment group
minps_a0 = with(dat_use[dat_use$Z==0,], min(p_pred))
maxps_a0 = with(dat_use[dat_use$Z==0,], max(p_pred))
minps_a1 = with(dat_use[dat_use$Z==1,], min(p_pred))
maxps_a1 = with(dat_use[dat_use$Z==1,], max(p_pred))


#dat_use[dat_use$Z==0 & dat_use$p >= minps_a1 & dat_use$p <= maxps_a1,]
subset = dat_use[dat_use$Z==0 | (dat_use$Z==1 & dat_use$p_pred >= minps_a0 & dat_use$p_pred <= maxps_a0),]
ATTmat_new = mean(subset$X2[subset$Z==1]) - mean(subset$X2[subset$Z==0])
sdATT_1 = sd(subset$X2[subset$Z==1])
ATTmat_new/sdATT_1

#' Fit a linear regression model that includes all the covariates as linear terms
mod2 <- stan_glm(Y ~ Z + X1 + X2 + X1:X2 + Z:X1, data=subset)

mod2

#' 
#' Now analyze the data with  BART
#' 

#' Create training data that uses all the covariates (not the outcome)
xt=as.matrix(subset[,!(names(subset) %in% c("X","Y","p","p_pred"))])  

#' Create test data that includes the covariates of all the treated units
#' but sets the treatment variable = 0
#' This will be used for predicting the *other* potential outcome for the treated units only
#' i.e., for estimating the ATT
xp=as.matrix(subset[subset$Z==1,!(names(subset) %in% c("X","Y","p","p_pred"))])  
xp[,3]=0

y=as.numeric(subset[,5])

#' Fit the BART model
bart.tot <- bart(x.train=xt,   y.train=y,  x.test=xp)

# check convergence
library(coda)
plot(as.mcmc(bart.tot$sigma))

#' Use MCMC samples to calculate individual and average treatment effects

#' First calculate MCMC simulations of individual treatment effects by subtracting
#' the observed outcome among treated units from the predicted values had they been
#' untreated (i.e., the "test predictions")
diffs=bart.tot$yhat.train[,subset$Z==1] - bart.tot$yhat.test 
head(diffs) # a matrix with 1000 MCMC samples (rows) for each of 218 treated units (columns)
dim(diffs)

#' Row means correspond to the SATE for each MCMC iteration
mndiffs=apply(diffs,1,mean)
length(mndiffs) #A vector of 1000 simulations of the SATE
ATT_bart = mean(mndiffs) # Posterior mean SATE
ATT_bart

sdATT_bart = sd(mndiffs) # Posterior standard deviation of the SATE
sdATT_bart

