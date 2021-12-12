rm(list = ls())
library(rstanarm)

dat_use <- read.csv("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/Hw5.2_Q2.dat")



#'Estimate the propensity score
dat_use$ps = glm(Z~X1, family= binomial(link = "logit"), data=dat_use)$fitted.values

#' Let's look at the propensity score distribution across treatment groups.
with(dat_use, hist(ps[Z==0], breaks=50, col = rgb(0,0,1, alpha=0.4),
                   main = "Histograms of PS", xlab = "ps"))
with(dat_use, hist(ps[Z==1], breaks=50, col = rgb(1,0,0,alpha=0.4), add=TRUE))
legend("topright", c("Control", "Treated"), 
       fill = c(rgb(0,0,1, alpha=0.4), rgb(1,0,0, alpha=0.4)))

#' Now use the ps to define inverse probability weights
dat_use$wt[dat_use$Z==1] = 1/dat_use$ps[dat_use$Z==1]
dat_use$wt[dat_use$Z==0] = 1/(1-dat_use$ps[dat_use$Z==0])
with(dat_use, tapply(wt, Z, max))

with(dat_use, hist(wt[a==1], breaks=500, col = rgb(1,0,0, alpha=0.4),xlim = c(0,20),
                   main = "Histogram of Weights", xlab = "IPW"))
with(dat_use, hist(wt[a==0], breaks=500, col = rgb(0,0,1,alpha=0.4), add=TRUE))
legend("topright", c("Control", "Treated"), 
       fill = c(rgb(0,0,1, alpha=0.4), rgb(1,0,0, alpha=0.4)))


#' Show that the propensity score distribution is equivalent across the treatment groups in the pseudopopulation
with(dat_use, tapply(p, Z, mean))
with(subset(dat_use, Z==0), weighted.mean(p, wt))
with(subset(dat_use, Z==1), weighted.mean(p, wt))

#' Show that weighted covariate means are more balanced than raw data
with(dat_use, tapply(X1, Z, mean))
with(subset(dat_use, Z==0), weighted.mean(X1, wt))
with(subset(dat_use, Z==1), weighted.mean(X1, wt))

#'
#' Analyze the Outcome
#' 
#' 
#' Unadjusted mean comparison
with(dat_use, tapply(Y, Z, mean))
with(dat_use, mean(Y[Z==1]) - mean(Y[Z==0]))

summary(lm(Y~Z, data=dat_use))

#' Adjusted comparison
#' In this case we know the true model, so simplre regression adjustment should work fine
summary(lm(Y~Z+X1, data=dat_use))

#' Weighted mean comparison

with(subset(dat_use, Z==1 & X1==1), mean(X1)) - with(subset(dat_use, Z==1 & X1==0), mean(X1))

with(subset(dat_use, Z==1 & X1==1), weighted.mean(X1)) - with(subset(dat_use, Z==1 & X1==0), weighted.mean(X1))




with(dat_use, tapply(Y, Z, mean))
with(dat_use, mean(Y[Z==1]) - mean(Y[Z==0]))

with(subset(dat_use, Z==1), weighted.mean(Y1)) - with(subset(dat_use, Z==1), weighted.mean(Y0))

with(subset(dat_use, Z==0), weighted.mean(Y1)) - with(subset(dat_use, Z==0), weighted.mean(Y0))

