rm(list = ls())
library(MASS)
set.seed(51)

## - simulate data
n = 100
p = 4
X = mvrnorm(n, runif(p,-2,2), diag(1, p))  # p uncorrelated covariates

#' Create functions of X variables for generating Y
transform_X = function(X){
  X.new = X
  X.new[,1] = X[,1]^2
  X.new[,2] = sin(X[,2])
  X.new[,3] = exp(X[, 3])
  X.new[,4] = (1-X[,4])*abs(X[,4])
  X.new = cbind(X.new, X.new[,1]*X[,3])
  return(X.new)
}
 
X.new = transform_X(X) 
p.new = ncol(X.new)

# randomly assign treatment from completely randomized design
n0 = round(n/3)
Z = sample(c(rep(0, n0), rep(1, n-n0)), size = n)

# "true" regression coefficients
B = rnorm(p.new+1, runif(p.new+1, -5,5), 1) 

# This is the "true" data generating mechanism for Y
generate_Y = function(trueATE, X.new, Z, B){
  p.new = ncol(X.new)
  n.sim = nrow(X.new)
  Y0 = B[1] + X.new%*%B[2:(p.new+1)]   + rnorm(n.sim, 0, 1)
  Y1 = Y0 + trueATE
  Y = Y0
  Y[Z==1] = Y1[Z==1]

  # Note the above data generation is equivalent to:
  # Y = B[1] +X.new%*%B[2:(p.new+1)] + Z*trueATE + rnorm(n, 0, 1)
  return(Y)
}

Y = generate_Y(trueATE = 5, X.new=X.new, Z=Z, B=B)

# Now, analyze the simulated data with a regression model
# Note that this model specification is "wrong" as it includes
# the original 'X' in the model, not 'X.new' used for data generation
lm.fit = lm(Y~X+Z)
summary(lm.fit)


## Now repeat many times to get sampling distribution of ATE
n.reps = 5000
ATE.wrongmod = rep(NA, n.reps)

for (i in 1:n.reps){
  #- Simulate X
  X = mvrnorm(n, rep(0, p), diag(1, p))  # p uncorrelated covariates
  
  # - Transform X for generating Y
  X.new = transform_X(X)
  p.new = dim(X.new)[[2]]
  
  # - Randomly assing Z
  n0 = round(n/3)
  Z = sample(c(rep(0, n0), rep(1, n-n0)), size = n)
  
  #- Simulate Y0, Y1, Y based on the transformed X.new
  Y = generate_Y(5, X.new=X.new, Z=Z, B=B) #true ATE is 5

  #- Analyze the data using the "wrong" X
  ATE.wrongmod[i] = lm(Y~X+Z)$coef["Z"]
}

par(mfrow = c(1,1))
hist(ATE.wrongmod, 
     main = paste("Mean = ",round(mean(ATE.wrongmod),2),
                  "SE = ", round(sd(ATE.wrongmod), 2)),
     breaks = 100)



#' Now repeat the same, but this time analyze the data with the 
#' correct data generating model using X.new
ATE.correctmod = rep(NA, n.reps)

for (i in 1:n.reps){
  #- Simulate X
  X = mvrnorm(n, rep(0, p), diag(1, p))  # p uncorrelated covariates
  
  # - Transform X for generating Y
  X.new = transform_X(X)
  p.new = dim(X.new)[[2]]
  
  # - Randomly assing Z
  n0 = round(n/3)
  Z = sample(c(rep(0, n0), rep(1, n-n0)), size = n)
  
  #- Simulate Y0, Y1, Y based on the transformed X.new
  Y = generate_Y(5, X.new=X.new, Z=Z, B=B) #true ATE is 5
  
  #- Analyze the data using the correct X.new
  ATE.correctmod[i] = lm(Y~X.new+Z)$coef["Z"]
}

par(mfrow = c(1,1))
hist(ATE.correctmod, main = paste("Mean = ",round(mean(ATE.correctmod),2), "SE = ", round(sd(ATE.correctmod), 2)),
     breaks = 100)


