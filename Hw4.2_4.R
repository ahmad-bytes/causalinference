rm(list = ls())

library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")

theme_set(bayesplot::theme_default(base_family = "sans"))

data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/prop_data.txt")

prop_df <- as.data.frame(data)
prop_df

f = prop_df[1,]$e

e_X = invlogit(20 * prop_df$X1 - 5 * prop_df$X3 *  prop_df$X4)

fit_1 <- stan_glm(Z ~ X1 + X3:X4, family=binomial(link="logit"), data=prop_df, refresh=0)
summary(fit_1)
pscores_1 = invlogit(predict(fit_1))
round(sd(e_X - pscores_1) , 3)
hist(e_X - pscores_1)


fit_2 <- stan_glm(Z ~ X3:X4, family=binomial(link="logit"), data=prop_df, refresh=0)
summary(fit_2)
pscores_2 = invlogit(predict(fit_2))
round(sd(e_X - pscores_2) , 3)
hist(e_X - pscores_2)

fit_3 <- stan_glm(Z ~ X1 + X2 + X3 + X4 + X5 + X3:X1 + X3:X2 + X3:X4 + X3:X5, family=binomial(link="logit"), data=prop_df, refresh=0)
#fit_3 <- stan_glm(Z ~ X1 + X2 + X3 + X4 + X5 + X3:X1:X2:X4:X5, family=binomial(link="logit"), data=prop_df, refresh=0)
summary(fit_3)
pscores_3 = invlogit(predict(fit_3))
round(sd(e_X - pscores_3) , 3)
hist(e_X - pscores_3)
