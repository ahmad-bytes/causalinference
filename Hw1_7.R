rm(list = ls())
# Set seed for reproducibility

set.seed(2021)

N <- 1000 # number of individuals

tau <- 7 # homogenous treatment effect

# Generate errors and pre-treatment covariates:

e0 <- rnorm(n = N, mean = 0, sd = 1)

e1 <- rnorm(n = N, mean = 0, sd = 1)

X <- rnorm(n = N, mean = 65, sd = 9)

# Compute potential outcomes based on linear relationship:

Y0 <- 10 + 0.9 * X + 0 + e0

Y1 <- 10 + 0.9 * X + tau + e1

set.seed(2021)

Z <- sample(c(1,0), size = N, replace = TRUE) # Generate random treatment assignments

Yobs <- Z * Y1 + (1 - Z) * Y0 # Observed potential outcomes Y(Z)

# Create dataframe:

quiz_obs <- data.frame(X, Z, Yobs)

treated <- mean((quiz_obs$Yobs[quiz_obs$Z == 1] - quiz_obs$X[quiz_obs$Z == 1]))
untreated <- mean((quiz_obs$Yobs[quiz_obs$Z == 0] - quiz_obs$X[quiz_obs$Z == 0]))
diff <- treated - untreated

t <- (tau - diff) / sd(Yobs)


treated1 <- sum((quiz_obs$Yobs - quiz_obs$X)[quiz_obs$Z == 1]) / 513
untreated1 <- sum((quiz_obs$Yobs - quiz_obs$X)[quiz_obs$Z == 0]) / 487
diff1 <- treated1 - untreated1
# do not use gain score
treated2 <- mean(quiz_obs$Yobs[quiz_obs$Z == 1])
untreated2 <- mean(quiz_obs$Yobs[quiz_obs$Z == 0])
diff2 <- treated2 - untreated2

t2 <- (tau - diff2) / sd(Yobs)
