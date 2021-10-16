rm(list = ls())

library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")

theme_set(bayesplot::theme_default(base_family = "sans"))

set.seed(51)

data <- read.csv("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/challenger.txt")
challenger_df <- as.data.frame(data)

fit_1 <- stan_glm(Fail ~ Temperature, family=binomial(link="logit"), data=challenger_df,
                  refresh=0)
print(fit_1)

intercept = coef(fit_1)[1]
beta = coef(fit_1)[2]

diff = (exp(intercept + beta * 69)/(1 + exp(intercept + beta * 69))) -   (exp(intercept + beta * 70)/(1 + exp(intercept + beta * 70)))
diff1 = invlogit(fit_1$coef[1] + fit_1$coef[2]*69) - invlogit(fit_1$coef[1] + fit_1$coef[2]*70)


plot(c(40, 90),c(-1,1), type="n", xlab="temperature", ylab="fail", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="temperature vs y", bty="l")
axis(1, seq(40,90,5), paste(seq(40,90,5),"",sep=""), mgp=c(2,.5,0))
axis(2, seq(-1,1,1), paste(seq(-1,1,1),"",sep=""), mgp=c(2,.5,0))
with(challenger_df, points(Temperature, Fail, pch=20))
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), add=TRUE)

sims <- as.matrix(fit_1)
a <- sims[,1]
b <- sims[,2]
print(median(b))
print(mad(b))


#' #### Predictions
new <- data.frame(Temperature=c(70,65))
#' Expected outcome with uncertainty
epred <- posterior_epred(fit_1, newdata=new)
head(epred)
print(c(mean(epred[2])-mean(epred[1])), c(sd(epred[2])-sd(epred[1]), digits=2)

