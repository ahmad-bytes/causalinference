rm(list = ls())
set.seed(51)

#' #### Load packages
library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' #### Load data
hibbs <- read.table("hibbs.dat", header=TRUE)

head(hibbs)

#' ## Graphing the bread and peace model
n <- nrow(hibbs)
par(mar=c(0,0,1.2,0))
left <- -.3
right <- -.28
center <- -.07
f <- .17
plot(c(left-.31,center+.23), c(-3.3,n+3), type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i")
mtext("Forecasting elections from the economy", 3, 0, cex=1.2)
with(hibbs, {
  for (i in 1:n){
    ii <- order(growth)[i]
    text(left-.3, i, paste (inc_party_candidate[ii], " vs. ", other_candidate[ii], " (", year[ii], ")", sep=""), adj=0, cex=.8)
    points(center+f*(vote[ii]-50)/10, i, pch=20)
    if (i>1){
      if (floor(growth[ii]) != floor(growth[order(growth)[i-1]])){
        lines(c(left-.3,center+.22), rep(i-.5,2), lwd=.5, col="darkgray")
      }
    }
  }
})
lines(center+f*c(-.65,1.3), rep(0,2), lwd=.5)
for (tick in seq(-.5,1,.5)){
  lines(center + f*rep(tick,2), c(0,-.2), lwd=.5)
  text(center + f*tick, -.5, paste(50+10*tick,"%",sep=""), cex=.8)
}
lines(rep(center,2), c(0,n+.5), lty=2, lwd=.5)
text(center+.05, n+1.5, "Incumbent party's share of the popular vote", cex=.8)
lines(c(center-.088,center+.19), rep(n+1,2), lwd=.5)
text(right, n+1.5, "Income growth", adj=.5, cex=.8)
lines(c(right-.05,right+.05), rep(n+1,2), lwd=.5)
text(right, 16.15, "more than 4%", cex=.8)
text(right, 14, "3% to 4%", cex=.8)
text(right, 10.5, "2% to 3%", cex=.8)
text(right, 7, "1% to 2%", cex=.8)
text(right, 3.5, "0% to 1%", cex=.8)
text(right, .85, "negative", cex=.8)
text(left-.3, -2.3, "Above matchups are all listed as incumbent party's candidate vs.\ other party's candidate.\nIncome growth is a weighted measure over the four years preceding the election.  Vote share excludes third parties.", adj=0, cex=.7)


par(mar=c(3,3,2,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(c(-.7, 4.5), c(43,63), type="n", xlab="Avg recent growth in personal income", ylab="Incumbent party's vote share", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="Forecasting the election from the economy      ", bty="l")
axis(1, 0:4, paste(0:4,"%",sep=""), mgp=c(2,.5,0))
axis(2, seq(45,60,5), paste(seq(45,60,5),"%",sep=""), mgp=c(2,.5,0))
with(hibbs, text(growth, vote, year, cex=.8))
abline(50, 0, lwd=.5, col="gray")

#' ## Linear regression
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
M1 <- stan_glm(vote ~ growth, data = hibbs, refresh = 0)
#' Print default summary of the fitted model
print(M1)

#' Also include lm option for comparison
M1.lm <- lm(vote~growth, data=hibbs)
summary(M1.lm)

#' Print summary of the priors used
prior_summary(M1)
#' Almost all models in Regression and Other Stories have very good
#' sampling behavior. `summary()` function can be used to obtain the
#' summary of the convergence diagnostics for MCMC sampling.
summary(M1)


#' #### Plot regression line
par(mar=c(3,3,2,.1), mgp=c(1.7,.5,0), tck=-.01)
plot(c(-.7, 4.5), c(43,63), type="n", xlab="Average recent growth in personal income", ylab="Incumbent party's vote share", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="Data and linear fit", bty="l")
axis(1, 0:4, paste(0:4,"%",sep=""), mgp=c(2,.5,0))
axis(2, seq(45,60,5), paste(seq(45,60,5),"%",sep=""), mgp=c(2,.5,0))
with(hibbs, points(growth, vote, pch=20))
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

#' #### Median and mean absolute deviation (MAD_SD) for a derived quantity a/b
a <- sims[,1]
b <- sims[,2]
z <- a/b
print(median(z))
print(mad(z))

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
plot(c(-.7, 4.5), c(43,63), type="n", xlab="Average recent growth in personal income", ylab="Incumbent party's vote share", xaxt="n", yaxt="n", mgp=c(2,.5,0), main="Data and 100 posterior draws of the line, y = a + bx           ", bty="l")
axis(1, 0:4, paste(0:4,"%",sep=""), mgp=c(2,.5,0))
axis(2, seq(45,60,5), paste(seq(45,60,5),"%",sep=""), mgp=c(2,.5,0))
for (i in 1:100){
  abline(a[i], b[i], lwd=.5)
}
abline(50, 0, lwd=.5, col="gray")
with(hibbs, {
  points(growth, vote, pch=20, cex=1.7, col="white")
  points(growth, vote, pch=20)
})



#' Prediction Uncertainty'

#' #### Point prediction given 2% growth
new <- data.frame(growth=2.0)
y_point_pred <- predict(M1, newdata=new)
y_point_pred

#' #### Alternative way to compute the point prediction
a_hat <- coef(M1)[1]
b_hat <- coef(M1)[2]
y_point_pred <- a_hat + b_hat*as.numeric(new)
y_point_pred


#' #### Uncertainty in prediction given 2% growth
y_linpred <- posterior_linpred(M1, newdata=new)
length(y_linpred)
median(y_linpred)

#' #### Do same computation "manually"
a <- sims[,1]
b <- sims[,2]
y_linpred <- a + b*as.numeric(new)
median(y_linpred)

#' #### Predictive uncertainty
y_pred <- posterior_predict(M1, newdata=new)
y_pred[1:10]

#' #### Predictive uncertainty manually
sigma <- sims[,3]
n_sims <- nrow(sims)
y_pred <- a + b*as.numeric(new) + rnorm(n_sims, 0, sigma)

#' #### Summarize predictions
Median <- median(y_pred)
MAD_SD <- mad(y_pred)
win_prob <- mean(y_pred > 50)
cat("Predicted Clinton percentage of 2-party vote: ", round(Median,1),
    ", with s.e. ", round(MAD_SD, 1), "\nPr (Clinton win) = ", round(win_prob, 2),
    sep="")

#' #### Summarize predictions graphically
hist(y_pred)


