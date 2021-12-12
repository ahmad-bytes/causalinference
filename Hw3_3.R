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

C_temp = (data$Temperature - 32 )/2

data = cbind(data, C_temp = C_temp  )

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
postpred <- posterior_predict(fit_1, newdata=new)
head(postpred)
mean(postpred[,2] - postpred[,1])
sd(postpred[,2] - postpred[,1])


#' the posterior probability, according to the fitted model, that Bush
#' was more popular among people with income level 5 than among people
#' with income level 4
#mean(epred[,5] > epred[,4])

invlogit(-0.99)

invlogit(-0.99 + 0.5 * 6)

exp(-0.2)

exp(15.74 + -0.24 * 69) / (1 + exp(15.74 + -0.24 * 69)) - exp(15.74 + -0.24 * 70) / (1 + exp(15.74 + -0.24 * 70))


#fahrenheit calculation
fit_c <- stan_glm(Fail ~ challenger_df$C_temp, family=binomial(link="logit"), data=challenger_df,
                  refresh=0)
print(fit_c)
