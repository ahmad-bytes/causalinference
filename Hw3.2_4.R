rm(list = ls())
library(MASS)
set.seed(51)

Z = c(1,0,0,0,0,0,1)
Y0 = c(16, 16, 20, 20 ,10, 14, 16)
Y1 = c(14, 22.5, 22.5, 22.5 ,22.5, 22.5, 31)
df = data.frame(cbind(Z, Y1, Y0))
# 
mean(df$Y1 - df$Y0)
mean(df$Y1) - mean(df$Y0)

Y0_prob = c(16, 20, 20 ,10, 14)
Y1_prob = c(14,31)

# simulations
N = 200
SATE = rep(NA, N)

for (sim in 1:N)
{
  Y0 = c(NA, 16, 20, 20 ,10, 14, NA)
  Y1 = c(14, NA, NA, NA, NA, NA, 31)
  
  for (i in 1:NROW(Y0))
    if (is.na(Y0[i]))
      Y0[i] = sample(Y0_prob, 1)
  
  for (i in 1:NROW(Y1))
    if (is.na(Y1[i]))
      Y1[i] = sample(Y1_prob, 1)
  
  SATE[sim] = mean(Y1 - Y0)
}

sd(SATE)  
