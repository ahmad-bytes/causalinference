library("arm")
library("rstanarm")
library("foreign")

#' #### Load data
nes <- read.table("nes.txt", header=TRUE)
head(nes)

#' Use first only data from 1992 and remove missing data
ok <- nes$year==1992 & !is.na(nes$rvote) & !is.na(nes$dvote) & (nes$rvote==1 | nes$dvote==1)
nes92 <- nes[ok,]

fit_1 <- stan_glm(rvote ~ income, family=binomial(link="logit"), data=nes92,
                  refresh=0)
print(fit_1)



#' #### Plot jittered data and prediction from the logistic regression
n <- nrow(nes92)
income_jitt <- nes92$income + runif(n, -.2, .2)
vote_jitt <- nes92$rvote + ifelse(nes92$rvote==0, runif(n, .005, .05), runif(n, -.05, -.005))
par(mar=c(3,3,1,.1), tck=-.01, mgp=c(1.7, .3, 0))
ok <- nes92$presvote<3
vote <- nes92$presvote[ok] - 1
income <- nes92$income[ok]
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), 1, 5, ylim=c(0,1),
      xlim=c(-2,8), xaxt="n", xaxs="i", 
      ylab="Pr (Republican vote)", xlab="Income", lwd=4, yaxs="i")
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), -2, 8, lwd=.5, add=TRUE)
axis(1, 1:5)
mtext("(poor)", 1, 1.2, at=1, adj=.5)
mtext("(rich)", 1, 1.2, at=5, adj=.5)
points(income_jitt, vote_jitt, pch=20, cex=.1)

#' #### Plot jittered data and prediction with uncertainties
par(mar=c(3,3,1,.1), tck=-.01, mgp=c(1.7, .3, 0))
ok <- nes92$presvote<3
vote <- nes92$presvote[ok] - 1
income <- nes92$income[ok]
curve (invlogit(fit_1$coef[1] + fit_1$coef[2]*x), .5, 5.5, ylim=c(0,1),
       xlim=c(.5, 5.5), xaxt="n", xaxs="i", 
       ylab="Pr (Republican vote)", xlab="Income", yaxs="i")
axis(1, 1:5)
mtext("(poor)", 1, 1.2, at=1, adj=.5)
mtext("(rich)", 1, 1.2, at=5, adj=.5)
sims_1 <- as.matrix(fit_1)
n_sims <- nrow(sims_1)
for (j in sample(n_sims, 20)){
  curve(invlogit(sims_1[j,1] +sims_1[j,2]*x), .5, 5.5, lwd=.5, col="gray", add=TRUE)
}
curve(invlogit(fit_1$coef[1] + fit_1$coef[2]*x), .5, 5.5, add=TRUE)
points(income_jitt, vote_jitt, pch=20, cex=.1)


#' #### Predictions
new <- data.frame(income=5)
#' Predict vote preference point estimate
pred <- predict(fit_1, type="response", newdata=new)
print(pred, digits=2)

#' Could also do prediction on the linear predictor scale
pred_lp <- predict(fit_1, type="link", newdata=new)
print(pred_lp, digits=2)

print( invlogit(pred_lp), digits=2)


#' Linear predictor with uncertainty
linpred <- posterior_linpred(fit_1, newdata=new)
head(linpred)
length(linpred)
print(c(mean(linpred), sd(linpred)), digits=2)

#' Expected outcome with uncertainty
epred <- posterior_epred(fit_1, newdata=new)
head(epred)
print(c(mean(epred), sd(epred)), digits=2)

#' Mean of the predicted values is the same as what we got from predict()
print(pred, digits=2)

#' also the same as if we evaluate invlogit(predictions of linear predictor)
epred_2 = invlogit(linpred)
print(c(mean(epred_2), sd(epred_2)), digits=2)


#' Predictive distribution for a new observation
postpred <- posterior_predict(fit_1, newdata=new)
head(postpred)
print(c(mean(postpred), sd(postpred)), digits=2)

#' #### Prediction given a range of input values
new <- data.frame(income=1:5)
pred <- predict(fit_1, type="response", newdata=new)
linpred <- posterior_linpred(fit_1, newdata=new)
head(linpred)
epred <- posterior_epred(fit_1, newdata=new)
head(epred)
postpred <- posterior_predict(fit_1, newdata=new)
head(postpred)
#' the posterior probability, according to the fitted model, that Bush
#' was more popular among people with income level 5 than among people
#' with income level 4
mean(epred[,5] > epred[,4])
#' 95\% posterior distribution for the difference in support for Bush,
#' comparing people in the richest to the second-richest category
quantile(epred[,5] - epred[,4], c(0.025, 0.975))
