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

Y0 <- 10 + 0.9 * X + 0 + e0

Y1 <- 10 + 0.9 * X + tau + e1

# Create dataframe:

quiz_omni <- data.frame(X, Y0, Y1)

ate <- mean(quiz_omni$Y1 - quiz_omni$Y0)



par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(X, Y0, pch = 16, col = 2, ylim =c(40,100))              # Create first plot

reg_0 <- lm(Y0 ~ X) 
abline(reg_0)

par(new = TRUE)                             # Add new plot
plot(X, Y1, pch = 17, col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "" , ylim =c(40,100))
axis(side = 4, at = pretty(range(Y1)))      # Add second axis
mtext("Y1", side = 4, line = 3) 
reg_1 <- lm(Y1 ~ X) 
abline(reg_1)

