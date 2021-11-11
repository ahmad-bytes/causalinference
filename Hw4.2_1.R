rm(list = ls())

library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")

theme_set(bayesplot::theme_default(base_family = "sans"))

set.seed(51)

data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/Lalonde.txt")

Lalonde_df <- as.data.frame(data)

lm(re78 ~ treat, data =  Lalonde_df)

#x = Lalonde_df[Lalonde_df$treat == 0,]

lm(re78 ~ treat + age + black + hisp + married + educ  , data =  Lalonde_df)
lm(re74 ~ treat + age + black + hisp + married + educ  , data =  Lalonde_df)
lm(re75 ~ treat + age + black + hisp + married + educ  , data =  Lalonde_df)

hist(Lalonde_df[Lalonde_df$treat == 0,]$re74, xlim=c(0,80000), ylim=c(0,7000))

hist(Lalonde_df[Lalonde_df$treat == 1,]$re74, xlim=c(0,80000), ylim=c(0,7000))


