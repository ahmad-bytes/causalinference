rm(list = ls())
#set.seed(51)

#' #### Load packages
library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")

cows_data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/Cows.txt")
Y = cows_data$fat * cows_data$milk

M1 <- stan_glm(Y ~ level , data = cows_data, refresh = 0)
#' Print default summary of the fitted model
print(M1)

M2 <- stan_glm(Y ~ level + age + lactation + initial.weight , data = cows_data, refresh = 0)
#' Print default summary of the fitted model
print(M2)
