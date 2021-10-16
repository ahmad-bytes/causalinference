rm(list = ls())

library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")

theme_set(bayesplot::theme_default(base_family = "sans"))

set.seed(51)

data <- read.csv("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/beauty.txt")
beauty_df <- as.data.frame(data)

#View(beauty_df)


#model <- lm(advertising$sales_millions ~ advertising$minutes)
#summary(model)

#' ## Linear regression
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
M1 <- stan_glm(eval ~ beauty + female + age + minority + nonenglish + lower, data = beauty_df, refresh = 0)
#' Print default summary of the fitted model
print(M1)
print(coef(M1))

M1.lm <- lm(eval ~ beauty + female + age + minority + nonenglish + lower, data = beauty_df)
summary(M1.lm)

M2 <- stan_glm(eval ~ beauty, data = beauty_df, refresh = 0)

M3 <- lm(eval ~ beauty + female + minority + nonenglish + lower, data = beauty_df)

visreg(M3, "beauty", gg = TRUE) 

# plot points and regression line
par(mar=c(3,3,2,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(c(-2, 2), c(2,6), type="n", xlab="beauty", ylab="eval", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="beauty vs evaluations", bty="l")
axis(1, seq(-2,2,1), paste(seq(-2,2,1),"",sep=""), mgp=c(2,.5,0))
axis(2, seq(2,6,1), paste(seq(2,6,1),"",sep=""), mgp=c(2,.5,0))
with(beauty_df, points(beauty, eval, pch=20))
abline(50, 0, lwd=.5, col="gray")
abline(coef(M2), col="gray15")
text(2.7, 53.5, paste("y =", fround(coef(M2)[1],1), "+", fround(coef(M2)[2],1), "x"), adj=0, col="gray15")

