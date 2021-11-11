rm(list = ls())
library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")
set.seed(51)

age_data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/age_data.txt")

N = NROW(age_data)

fit_1 <- stan_glm(Y ~ Z + age, family=binomial(link="logit"), data=age_data, refresh=0)
print(fit_1)

fit_1$coefficients

intercept = fit_1$coefficients[1]
treatment_coeff = fit_1$coefficients[2]
age_coeff = fit_1$coefficients[3]

par(mfrow=c(1,1))
plot(age_data$age  + rnorm(N, sd=0.1), age_data$Y + rnorm(N, sd=0.1), main="Jittered x-axis (normal distr.)", xlab="Integer", ylab="Continuous")
curve(invlogit(intercept + treatment_coeff + age_coeff*x), add=TRUE)
curve(invlogit(intercept + age_coeff*x), add=TRUE)


fit_1 <- stan_glm(Y ~ Z + age + Z*age, family=binomial(link="logit"), data=age_data, refresh=0)
print(fit_1)

fit_1$coefficients

intercept = fit_1$coefficients[1]
treatment_coeff = fit_1$coefficients[2]
age_coeff = fit_1$coefficients[3]
interaction_coeff = fit_1$coefficients[4]

par(mfrow=c(1,1))
#plot(age_data$age  + rnorm(N, sd=0.1), age_data$Y + rnorm(N, sd=0.1), main="Jittered x-axis (normal distr.)", xlab="Integer", ylab="Continuous")
plot(age_data$age + rnorm(N, sd=0.1) , age_data$Y + rnorm(N, sd=0.1)  , main="Jittered x-axis (normal distr.)", xlab="Integer", ylab="Continuous")
curve(invlogit(intercept + treatment_coeff + age_coeff*x), add=TRUE)
curve(invlogit(intercept + interaction_coeff*x + age_coeff*x), add=TRUE)

invlogit(intercept + treatment_coeff + age_coeff*mean(age_data$age))
invlogit(intercept + age_coeff*mean(age_data$age))

