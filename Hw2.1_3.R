rm(list = ls())

# Set seed for reproducibility (and to get the right answer!):

set.seed(2021)

N <- 1000 # number of individuals

tau <- 7 # homogenous treatment effect

# Generate errors and pre-treatment covariates:

e0 <- rnorm(n = N, mean = 0, sd = 1)

e1 <- rnorm(n = N, mean = 0, sd = 1)

X <- rnorm(n = N, mean = 65, sd = 9)

# Compute potential outcomes based on linear relationship:

#Y0 <- 10 + 0.9 * X + 0 + e0

Y0 <- 10 + 0 * X + 0 + e0

#Y1 <- 10 + 0.9 * X + tau + e1

Y1 <- 10 + 0 * X + tau + e1

# Create omniscient dataframe:

omni_data <- data.frame(X, Y0, Y1)

set.seed(2021)

Zr <- sample(c(1,0), size = N, replace = TRUE) # Generate random treatment assignments

Yobs <- Zr * Y1 + (1 - Zr) * Y0 # Observed potential outcomes Y(Z)


# Create observed dataframe:

obs_data <- data.frame(X, Zr, Yobs)


# ---- Completely Randomized Design
nt = round(N*.5) # number treated units, treat half of all units
# Repeat the completely randomized trial to get the randomization distribution
n_reps = 10000
Z.cr = matrix(NA, n_reps,N) # a matrix to store all of the different Z vectors
set.seed(2021)
for (i in 1:n_reps)
{
  #Z.cr[i,] = sample(c(rep(0,N-nt), rep(1,nt)), N)
  #Z.cr[i,] <- sample(c(1,0), size = N, replace = TRUE)
  Z.cr[i,] <- rbinom(N, 1,.5)
}

# Calculate the mean difference between treated/control for each randomization
tau_sate.mean = rep(NA, n_reps)
tau_sate.ols = rep(NA, n_reps)

for (i in 1:n_reps) {
  #Yobs_y = rep(NA, n_reps)
  tau_sate.mean[i] = mean(Y1[Z.cr[i,]==1]) - mean(Y0[Z.cr[i,]==1])
  Yobs_y = Z.cr[i,] * Y1 + (1 - Z.cr[i,]) * Y0
  test = lm(Yobs_y ~ Z.cr[i,] + X) 
  tau_sate.ols[i] = summary(test)$coefficients[2,1]
}

  
m.mean = round(mean(tau_sate.mean), 2)
sd.mean = sd(tau_sate.mean)
hist(tau_sate.mean, main = paste("CR: Mean = ", m.mean, " SD = ", sd.mean, sep=""))


m.ols = round(mean(tau_sate.ols), 2)
sd.ols = sd(tau_sate.ols)
hist(tau_sate.ols, main = paste("CR: Mean = ", m.ols, " SD = ", sd.ols, sep=""))