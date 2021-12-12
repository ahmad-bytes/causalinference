rm(list = ls())

library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")

theme_set(bayesplot::theme_default(base_family = "sans"))

set.seed(51)

data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/advertising.txt")

advertising <- as.data.frame(data)

View(advertising)


#model <- lm(advertising$sales_millions ~ advertising$minutes)
#summary(model)

#' ## Linear regression
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
M1 <- stan_glm(sales_millions ~ minutes, data = advertising, refresh = 0)
#' Print default summary of the fitted model
print(M1)

#' Also include lm option for comparison
M1.lm <- lm(sales_millions ~ minutes, data = advertising)
summary(M1.lm)

prior_summary(M1)

# plot points and regression line
par(mar=c(3,3,2,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(c(0, 35), c(0,35), type="n", xlab="minutes", ylab="Sales in millions", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="Sales in Millions Vs Advertising minutes", bty="l")
axis(1, seq(0,35,5), paste(seq(0,35,5),"",sep=""), mgp=c(2,.5,0))
axis(2, seq(0,35,5), paste(seq(0,35,5),"",sep=""), mgp=c(2,.5,0))
with(advertising, points(minutes, sales_millions, pch=20))
abline(50, 0, lwd=.5, col="gray")
abline(coef(M1), col="gray15")
text(2.7, 53.5, paste("y =", fround(coef(M1)[1],1), "+", fround(coef(M1)[2],1), "x"), adj=0, col="gray15")


#' #### Extract the simulations
sims <- as.matrix(M1)
dim(sims)
head(sims)

a <- sims[,1]
b <- sims[,2]
sigma <- sims[,3]
n_sims <- nrow(sims)

#' #### Median and mean absolute deviation (MAD_SD)
Median <- apply(sims, 2, median)
MAD_SD <- apply(sims, 2, mad)
print(cbind(Median, MAD_SD))
print(M1)


#' #### Plot the Posterior Distributions to look at Uncertainty in Regression Coefficients
par(mfrow=c(1,2), mar=c(3,2,3,0), mgp=c(1.5,.5,0), tck=-.01)
hist(a, ylim=c(0,0.25*n_sims), xlab="a", ylab="", main="Posterior simulations of the intercept, a,\nand posterior median +/- 1 and 2 std err", cex.axis=.9, cex.lab=.9, yaxt="n", col="gray90")
abline(v=median(a), lwd=2)
arrows(median(a) - 1.483*median(abs(a - median(a))), 550, median(a) + 1.483*median(abs(a - median(a))), 550, length=.1, code=3, lwd=2)
arrows(median(a) - 2*1.483*median(abs(a - median(a))), 250, median(a) + 2*1.483*median(abs(a - median(a))), 250, length=.1, code=3, lwd=2)
hist(b, ylim=c(0,0.27*n_sims), xlab="b", ylab="", main="Posterior simulations of the slope, b,\nand posterior median +/- 1 and 2 std err", cex.axis=.9, cex.lab=.9, yaxt="n", col="gray90")
abline(v=median(b), lwd=2)
arrows(median(b) - 1.483*median(abs(b - median(b))), 550, median(b) + 1.483*median(abs(b - median(b))), 550, length=.1, code=3, lwd=2)
arrows(median(b) - 2*1.483*median(abs(b - median(b))), 250, median(b) + 2*1.483*median(abs(b - median(b))), 250, length=.1, code=3, lwd=2)

#' Look at the joint posterior of intercept,slope parameters to see
#' how they covary with one another
par(mfrow = c(1,1))
par(mar=c(3,3,2,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(a, b, xlab="a", ylab="b", main="Posterior draws of the regression coefficients a, b          ", bty="l", pch=20, cex=.2)

#' Plot a range of possible regression lines according to 
#' uncertainty in a, b
par(mar=c(3,3,2,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(c(0, 35), c(0,35), type="n", xlab="minutes", ylab="Sales in millions", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="Sales in Millions Vs Advertising minutes", bty="l")
axis(1, seq(0,35,5), paste(seq(0,35,5),"",sep=""), mgp=c(2,.5,0))
axis(2, seq(0,35,5), paste(seq(0,35,5),"",sep=""), mgp=c(2,.5,0))
for (i in 1:100){
  abline(a[i], b[i], lwd=.5)
}
abline(50, 0, lwd=.5, col="gray")
with(advertising, {
  points(minutes, sales_millions, pch=20, cex=1.7, col="white")
  points(minutes, sales_millions, pch=20)
})


#' #### Point prediction given 2% growth
new <- data.frame(minutes=20)
y_point_pred <- predict(M1, newdata=new)
y_point_pred

#' #### Uncertainty in prediction given 20 minutes of advertising
y_linpred <- posterior_linpred(M1, newdata=new)
length(y_linpred)
median(y_linpred)

#' #### Predictive uncertainty
y_pred <- posterior_predict(M1, newdata=new)

quantile(y_pred, c(.95, .98, 1))


#' #### Summarize predictions graphically
hist(sims[,2])

#length(which(sims[,2] >= 0))




