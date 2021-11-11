rm(list = ls())
library(MASS)
set.seed(51)


cov <- read.csv("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/cov.txt")
cov_errors <- read.csv("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/cov_errors.txt")


## - simulate data
n = NROW(cov)

#' Create functions of X variables for generating Y
transform_X = function(X){
  Y = -250 * cov$X1 + 2 * (cov$X2)^2 + 0.4 * (cov$X3)^3 - 75 * log(cov$X4) - 100 * cov$Z + cov_errors$x
  return(cbind(Y, cov))
}

no_transform_X = function(){
  Y = -250 * cov$X1 + 2 * cov$X2 + 0.4 * cov$X3 - 75 * cov$X4 - 100 * cov$Z + cov_errors$x
  return(cbind(Y, cov))
}

X.no_transform = no_transform_X() 
fit_lm = lm(X.no_transform$Y ~ X.no_transform$X1 + X.no_transform$X2 + X.no_transform$X3 + X.no_transform$X4 + X.no_transform$Z)
summary(fit_lm)

df = as.data.frame(X.no_transform)
ate = mean(df$Y[df$Z == 1]) - mean(df$Y[df$Z == 0])
ate - fit_lm$coefficients[6]


## Now repeat many times to get sampling distribution of ATE
n.reps = 1000
ATE.wrongmod = rep(NA, n.reps)

for (i in 1:n.reps){
  # - Randomly assing Z
  n0 = round(n/2)
  Z = sample(c(rep(0, n0), rep(1, n-n0)), size = n)
  Y = -250 * cov$X1 + 2 * cov$X2 + 0.4 * cov$X3 - 75 * cov$X4 - 100 * Z + cov_errors$x
  fit_wrong = lm(Y ~ cov$X1 + cov$X2 + cov$X3 + cov$X4 + Z)
  #- Analyze the data using the "wrong" X
  ATE.wrongmod[i] = fit_wrong$coef["Z"]
}

par(mfrow = c(1,1))
hist(ATE.wrongmod,
     main = paste("Mean = ",round(mean(ATE.wrongmod),2),
                  "SE = ", round(sd(ATE.wrongmod), 2)),
     breaks = 10)


#' Now repeat the same, but this time analyze the data with the
#' correct data generating model using X.new
ATE.correctmod = rep(NA, n.reps)

for (i in 1:n.reps){
  # - Randomly assing Z
  n0 = round(n/2)
  Z = sample(c(rep(0, n0), rep(1, n-n0)), size = n)
  Y = -250 * cov$X1 + 2 * (cov$X2)^2 + 0.4 * (cov$X3)^3 - 75 * log(cov$X4) - 100 * Z + cov_errors$x
  fit_corrected = lm(Y ~ I(cov$X1) + I(cov$X2^2) + I(cov$X3^3) + I(log(cov$X4)) + Z)
  #- Analyze the data using the "wrong" X
  ATE.correctmod[i] = fit_corrected$coef["Z"]
}

par(mfrow = c(1,1))
hist(ATE.correctmod, main = paste("Mean = ",round(mean(ATE.correctmod),2), "SE = ", round(sd(ATE.correctmod), 2)),
     breaks = 100)


