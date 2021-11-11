rm(list = ls())
set.seed(51)
library(rstanarm)

#' Simulate data based on Figure 19.10 in the Gelman, Hill and Vehtari
#' textbook, mimicking the hypothetical social services/parenting example
n = 1000
Q0 = rep(NA, n)
Q1 = Q0
Y0 = Q0
Y1 = Q0
parenting_potential = Q0

sim_sd = 1
#' "Poor parenting either way" children
n.pp = n*0.1 # 10% of the sample
parenting_potential[1:n.pp] = 1
Q0[1:n.pp] = 0 # for poor parenting
Q1[1:n.pp] = 0 # for poor parenting
Y0[1:n.pp] = rnorm(n.pp, 60, sim_sd)
Y1[1:n.pp] = Y0[1:n.pp] + 10

#' "Good parenting if treated" children
n.pg = n*0.7 # 70% of the sample
parenting_potential[(n.pp+1):(n.pp+n.pg)] = 2
Q0[(n.pp+1):(n.pp+n.pg)] = 0 # for poor parenting
Q1[(n.pp+1):(n.pp+n.pg)] = 1 # for good parenting
Y0[(n.pp+1):(n.pp+n.pg)] = rnorm(n.pp, 65, sim_sd)
Y1[(n.pp+1):(n.pp+n.pg)] = Y0[(n.pp+1):(n.pp+n.pg)] + 15

#' "Good parenting either way" children
n.gg = n*0.2 # 20% of the sample
parenting_potential[(n.pp+n.pg+1):n] = 3
Q0[(n.pp+n.pg+1):n] = 1 # for poor parenting
Q1[(n.pp+n.pg+1):n] = 1 # for good parenting
Y0[(n.pp+n.pg+1):n] = rnorm(n.pp, 90, sim_sd)
Y1[(n.pp+n.pg+1):n] = Y0[(n.pp+n.pg+1):n] + 10

trueATE = mean(Y1 - Y0)
trueATE

#' Randomly assign units to Z=0 or 1 based on a completely randomized
#' design with 50% of units treated
Z = sample(c(rep(0, n*.5), rep(1, n*0.5)))

#' "Reveal" the observed data
Y = rep(NA, n)
Q = Y
Y[Z==0] = Y0[Z==0]
Q[Z==0] = Q0[Z==0]
Y[Z==1] = Y1[Z==1]
Q[Z==1] = Q1[Z==1]

#' Note, in the below data.frame, the variable "parenting_potential" is not
#' actually observable because it is based on both (Q0, Q1).  We know it 
#' because we simulated the data, and we are including it in the data.frame
#' for illustration
dat <- data.frame(cbind(Y, Z, Q, parenting_potential))


#' Fit a regression model for the "total effect" of treatment on outcome
mod1 <- stan_glm(Y~Z, data = dat)
print(mod1)

#' Now fit a regression model adjusting for observed parenting quality
mod2 <- stan_glm(Y~Z + Q, data = dat)
print(mod2)
#' This result indicates a negative "direct effect" of the social services
#' treatment on child IQ, because it says that, after "adjusting" for parenting
#' quality, the average differences between treated/control groups is negative.
#' But we know that this is an example of posttreatment selection bias!

#' Now pretend that we could somehow actually observe the "parenting_potential"
#' variable - this would be like a "pre-treatment" covariate.  This is like a
#' primitive example of "principal stratification" - but the key practical 
#' challenge is that we would not know the "parenting_potential" covariate because
#' it is based on knowledge of both parenting quality potential outcomes.
mod3 <- stan_glm(Y~Z + parenting_potential, data=dat)
print(mod3)
