rm(list = ls())

library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")

theme_set(bayesplot::theme_default(base_family = "sans"))

set.seed(51)

data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/Lalonde_5.txt")

Lalonde_df <- as.data.frame(data)

#lalondefit2 <- stan_glm(re78 ~ treat+age+re74+re75+educ+black+hisp+married+nodegree+educ_cat4, data=lalonde,refresh=0)

ps_fit_1 <- stan_glm(treat ~ age+re74+re75+educ+black+hisp+married+nodegree+educ_cat4, family=binomial(link='logit'), data=Lalonde_df, algorithm='optimizing', refresh=0)
summary(ps_fit_1)
pscores <- apply(posterior_epred(ps_fit_1), 2, mean)

matches <- matching(z=Lalonde_df$treat, score=pscores, replace=FALSE)
matched <- Lalonde_df[matches$match.ind,]

cov_names = c('X','ag','ed','bl','mar','nod','r74','r75','r78','his','sam','trt','cat4')
covs = c('X','age','educ','black','married','nodegree','re74','re75','re78','hisp','sample','treat','educ_cat4')
longcovnames  =c('X','age','educ','black','married','nodegree','re74','re75','re78','hisp','sample','treat','educ_cat4')

bal_nr <- balance(rawdata=Lalonde_df[,covs], treat=Lalonde_df$treat, matched=matches$cnts, estimand='ATE')

pts <- bal_nr$diff.means.raw[,4]
pts2 <- bal_nr$diff.means.matched[,4]

par(mfrow=c(1,2))
mar1 <- c(5, 4, 6, 2)
mar2 <- c(5, 3, 6, 4)
pts <- bal_nr$diff.means.raw[bal_nr$binary==TRUE,4]
pts2 <- bal_nr$diff.means.matched[bal_nr$binary==TRUE,4]
K <- length(pts)
idx <- 1:K
main <- 'Absolute Difference in Means'
longcovnames <- cov_names[bal_nr$binary==TRUE]

pts
pts2

mar <- mar1
par(mar=mar)
maxchar <- max(sapply(longcovnames, nchar))
min.mar <- par('mar')
mar[2] <- max(min.mar[2], trunc(mar[2] + maxchar/10)) + mar[2] + 0.5
par(mar=mar)

pts <- rev(pts)
pts2 <- rev(pts2)
longcovnames <- rev(longcovnames)

plot(c(pts,pts2), c(idx,idx),
     bty='n', xlab='', ylab='',
     xaxt='n', yaxt='n', type='n',
     main=main, cex.main=1.2,
     xlim=c(0,.55))
abline(v=0, lty=2)
points(pts, idx, cex=1)
points(pts2, idx, pch=19, cex=1)
axis(3, at=seq(0,.5,.1), xpd=TRUE)
axis(2, at=1:K, labels=longcovnames[1:K],
     las=2, hadj=1, lty=0, cex.axis=1)


pts <- bal_nr$diff.means.raw[bal_nr$binary==FALSE,4]
pts2 <- bal_nr$diff.means.matched[bal_nr$binary==FALSE,4]

pts
pts2
# AZC: hack to fix spacing of binary covariates against x axis
# the spacing of how spaced apart the ticks are changes as the number of covariates change. It's frustratingly hard, maybe impossible, to get the spacing to match between the continuous and binary plots with different number of covariates in each, so, I'll add fake data that won't show up
pts <- c(pts, rep(NA, 7))
pts2 <- c(pts2, rep(NA, 7))
K <- length(pts)
idx <- 1:K
main <- 'Absolute Standardized Difference in Means'
longcovnames <- cov_names[bal_nr$binary==FALSE]
# add extra names to match above
longcovnames <- c(longcovnames, rep('', 7))

mar <- mar2
par(mar=mar)
maxchar <- max(sapply(longcovnames, nchar))
min.mar <- par('mar')
mar[2] <- max(min.mar[2], trunc(mar[2] + maxchar/10)) + mar[2] + 0.5
par(mar=mar)

pts <- rev(pts)
pts2 <- rev(pts2)
longcovnames <- rev(longcovnames)

plot(c(pts,pts2), c(idx,idx),
     bty='n', xlab='', ylab='',
     xaxt='n', yaxt='n', type='n',
     main=main, cex.main=1.2)
segments(x0=0, y0=13, x1=0, y1=7.5, lty=2)
points(pts, idx, cex=1)
points(pts2, idx, pch=19, cex=1)
axis(3)
axis(2, at=8:12, labels=longcovnames[8:12],
     las=2, hadj=1, lty=0, cex.axis=1)

