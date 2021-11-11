rm(list = ls())
set.seed(51)


N = 100
x = 40
b = 0.7
tau = 10
a = 22
# z is an error of 100 0 and 1
# Generate errors and pre-treatment covariates:

e0 <- rnorm(n = N, mean = 0, sd = 1)

e1 <- rnorm(n = N, mean = 0, sd = 1)

X <- rnorm(n = N, mean = 40, sd = 15)

# Compute potential outcomes based on linear relationship:

set.seed(2021)

Z <- sample(c(1,0), size = N, replace = TRUE) 

Y <- a + b * X + tau * Z + e1

sd(Y[Z==0])

sd(Y[Z==1])

fit = lm(Y  ~ X)

summary(fit)
