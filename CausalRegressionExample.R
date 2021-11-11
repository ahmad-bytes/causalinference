library("rstanarm")
invlogit <- plogis

#' #### Load data
electric_wide <- read.table("electric_wide.txt", header=TRUE)
head(electric_wide)

onlytext <- function (string){
  plot(0:1, 0:1, bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
  text(0.5, 0.5, string, cex=1.2, font=2)
}
nf<- layout(matrix(c(0,1:14), 5, 3, byrow=TRUE), c(5, 10, 10), c(1, 5, 5, 5, 5), TRUE)
par(mar=c(.2, .2, .2, .2))
onlytext('Test scores in control classes')
onlytext('Test scores in treated classes')
par(mar=c(1, 1, 1, 1), lwd=0.7)
attach(electric_wide)
for (j in 1:4){
  onlytext(paste('Grade', j))
  hist(control_posttest[grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text(2, 6.5, paste("mean =", round(mean(control_posttest[grade==j]))), adj=0)
  text(2, 5, paste("  sd =", round(sd(control_posttest[grade==j]))), adj=0)
  hist(treated_posttest[grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text(2, 6.5, paste("mean =", round(mean(treated_posttest[grade==j]))), adj=0)
  text(2, 5, paste("  sd =", round(sd(treated_posttest[grade==j]))), adj=0)
}


#' #### Plot the data the other way
onlytext<-function(string){
  plot(0:1, 0:1, bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
  text(0.5, 0.5, string, cex=1.2, font=2)
}
nf<-layout(matrix(c(0,1:14), 3, 5, byrow=FALSE), c(5, 10, 10, 10, 10), c(1, 5, 5), TRUE)
par(mar=c(.2, .2, .2, .2))
onlytext('Control\nclasses')
onlytext('Treated\nclasses')
par(mar=c(.2,.4,.2,.4), lwd=.5)
for (j in 1:4){
  onlytext(paste('Grade', j))
  hist(control_posttest[grade==j], breaks=seq(40,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,14))
  axis(side=1, seq(50,100,25), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0, lty="blank")
  lines(rep(mean(control_posttest[grade==j]),2), c(0,11), lwd=2)
  hist(treated_posttest[grade==j], breaks=seq(40,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,14))
  axis(side=1, seq(50,100,25), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0, lty="blank")
  lines(rep(mean(treated_posttest[grade==j]),2), c(0,11), lwd=2)
}

# Organize data
post_test <- c(treated_posttest, control_posttest)
pre_test <- c(treated_pretest, control_pretest)
grade <- rep(electric_wide$grade, 2)
treatment <- rep(c(1,0), rep(length(treated_posttest),2))
supp <- rep(NA, length(treatment))
n_pairs <- nrow(electric_wide)
pair_id <- rep(1:n_pairs, 2)
supp[treatment==1] <- ifelse(supplement=="Supplement", 1, 0)
n <- length(post_test)
electric <- data.frame(post_test, pre_test, grade, treatment, supp, pair_id)
head(electric)

#' Fit a linear regression model comparing the means outcomes in treated and control groups
fit_0 <- stan_glm(post_test~treatment, data=electric)
print(fit_0)

#' Since exploratory data suggests that things may be different across different
#' grades, fit the same analysis but separately for each grade
#' 
#' This is (nearly) equivalent to fitting a single regression with an interaction
#' between treatment and grade
est1 <- rep(NA,4)
se1 <- rep(NA,4)
for (k in 1:4) {
  fit_1 <- stan_glm(post_test ~ treatment, subset=(grade==k), data = electric,
                    refresh = 0, save_warmup = FALSE, 
                    open_progress = FALSE, cores = 1)
  est1[k] <- coef(fit_1)[2]
  se1[k] <- se(fit_1)[2]
}
round(est1,2)
round(se1,2)


#' Notice that pre-test score is highly correlated with post-test score
par(mfrow = c(1,1), mar=c(2, 2, 2, 2))
with(electric, plot(pre_test, post_test, col=grade, pch=16))
legend("bottomright", paste("Grade", 1:4), pch=16, col=1:4)

#' This motivates adjusting for pre-test score as a pre-treatment covariate
est2 <- rep(NA,4)
se2 <- rep(NA,4)
for (k in 1:4) {
  fit_2 <- stan_glm(post_test ~ treatment + pre_test, subset=(grade==k),
                    data = electric, refresh = 0, save_warmup = FALSE, 
                    open_progress = FALSE, cores = 1)
  est2[k] <- coef(fit_2)[2]
  se2[k] <- se(fit_2)[2]
}
round(est2,2)
round(se2,2)


regression.2tables <- function (name, est1, est2, se1, se2, label1, label2, file, bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est1+2*se1, est1-2*se1, est2+2*se2, est1-2*se2)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .6*J
  width <- 8*(name.range+1)
  gap <- .4
  
  if (!is.na(file)) postscript(file, horizontal=F, height=height, width=width)
  par (mar=c(0,0,0,0))
  plot (c(-name.range,2+gap), c(3,-J-2), bty="n", xlab="", ylab="",
        xaxt="n", yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Subpopulation", adj=0, cex=1)
  text (.5, 2, label1, adj=.5, cex=1)
  text (1+gap+.5, 2, label2, adj=.5, cex=1)
  lines (c(0,1), c(0,0))
  lines (1+gap+c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1), lty=2, lwd=.5)
  lines (1+gap+c(A,A), c(0,-J-1), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  segments (1+gap+A + B*ax, -.1, 1+gap+A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=1)
  text (1+gap+A + B*ax, .7, ax, cex=1)
  text (-name.range, -(1:J), name, adj=0, cex=1)
  points (A + B*est1, -(1:J), pch=20, cex=1)
  points (1+gap+A + B*est2, -(1:J), pch=20, cex=1)
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (1+gap+A + B*(est2-se2), -(1:J), 1+gap+A + B*(est2+se2), -(1:J), lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  segments (1+gap+A + B*(est2-2*se2), -(1:J), 1+gap+A + B*(est2+2*se2), -(1:J), lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1))
    lines (1+gap+c(0,1), c(-J-1,-J-1))
    segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    segments (1+gap+A + B*ax, -J-1-.1, 1+gap+A + B*ax, -J-1+.1, lwd=.5)
    text (A + B*ax, -J-1-.7, ax, cex=1)
    text (1+gap+A + B*ax, -J-1-.7, ax, cex=1)
  } 
  if (!is.na(file)) graphics.off()
}

par(mfrow = c(1,1))
regression.2tables(paste("Grade", 1:4), est1, est2, se1, se2, "Regression on treatment indicator", "Regression on treatment indicator,\ncontrolling for pre-test", NA)


#' #### Plot the regression (adjusted for pre-test) for each grade
par(mfrow=c(2,2), pty="s", mar = c(2,2,2,2))
x.range <- cbind(c(5,40,40,40), c(25,125,125,125))
for (j in 1:4){
  ok <- grade==j
  x <- c(treated_pretest[ok], control_pretest[ok])
  y <- c(treated_posttest[ok], control_posttest[ok])
  t <- rep(c(1,0), rep(sum(ok),2))
  plot(c(0,125), c(0,125), type="n", main=paste("Grade",j), xaxs="i", yaxs="i",
       xlab=expression(paste("pre-test, ",x[i])),
       ylab=expression(paste("post-test, ",y[i])),
       cex.axis=1.5, cex.lab=1.5, cex.main=1.8, mgp=c(2.5,.7,0))
  fit_1 <- stan_glm(y ~ x + t, data = electric_wide, refresh = 0, 
                    save_warmup = FALSE, open_progress = FALSE, cores = 1)
  abline(coef(fit_1)[1], coef(fit_1)[2], lwd=.5, lty=2)
  abline(coef(fit_1)[1]+coef(fit_1)[3], coef(fit_1)[2], lwd=.5)
  points(control_pretest[ok], control_posttest[ok], pch=20, cex=1.2)
  points(treated_pretest[ok], treated_posttest[ok], pch=21, cex=1.2)
}


#' Since pre-test score is clearly so important, it makes sense to 
#' see if there is heterogeneity of the treatment effect with respect
#' to pre-test score
#' 
#' That is, include an interaction between treatment and pre-test score

# let's look at just 4th grade first
fit_4 <- stan_glm(post_test ~ treatment + pre_test + treatment * pre_test,
                  subset = (grade==4), data=electric, refresh = 0)
print(fit_4)
sim_4 <- as.matrix(fit_4)
head(sim_4)

#' Now the treatment effect is actually a function of pre-test score
#' let's calculate the mean effect across the observed pre-test scores
#' in the 4th grade
#' #### Mean effect
n_sims <- 1000
effect <- array(NA, c(n_sims, sum(grade==4)))
for (i in 1:n_sims)
  effect[i,] <- sim_4[i,2] + sim_4[i,4]*pre_test[grade==4]

# This gives a 1000x42 matrix of 
# 1000 simulations of the effect for each of the 42 4th grade units
head(effect) 

# 1000 simulations of the mean effect across all of the 4 grade units
mean_effect <- rowMeans(effect) # average effect in the 4th grade
mean_effect[1:10] 

# print the posterior mean and SD of the average effect in 4th grade
print(paste("Mean =",round(mean(mean_effect), 2), "SD =", round(sd(mean_effect), 2)))


#' Now the treatment effect is actually a function of pre-test score
#' Should summarize with a plot
par(mfrow = c(1,1), mar = c(5,1,2,1))
plot(0, 0, xlim=range(pre_test[grade==4]), ylim=c(-5,10),
     xlab="pre-test", ylab="treatment effect", main="treatment effect in grade 4")
abline(0, 0, lwd=.5, lty=2)
curve(coef(fit_4)[2] + coef(fit_4)[4]*x, lwd=.5, add=TRUE)
# - And add some uncertainty to the plot with simulations from the posterior
for (i in 1:20)
  curve(sim_4[i,2] + sim_4[i,4]*x, lwd=.5, col="gray", add=TRUE)

#' #### Plot Resluts for Every Grade
par(mfrow=c(2,2), pty="s", mar = c(2,1,1,1))
for (j in 1:4){
  ok <- grade==j
  x <- c(treated_pretest[ok], control_pretest[ok])
  y <- c(treated_posttest[ok], control_posttest[ok])
  t <- rep(c(1,0), rep(sum(ok),2))
  plot(c(0,125),c(0,125), type="n", main=paste("Grade",j), xaxs="i", yaxs="i",
       xlab=expression(paste("pre-test, ",x[i])),
       ylab=expression(paste("post-test, ",y[i])),
       cex.axis=1.5, cex.lab=1.5, cex.main=1.8, mgp=c(2.5,.7,0))
  fit_1 <- stan_glm(y ~ x + t + x:t, data = electric_wide, refresh = 0, 
                    save_warmup = FALSE, open_progress = FALSE, cores = 1)
  abline(coef(fit_1)[1], coef(fit_1)[2], lwd=.5, lty=2)
  abline(coef(fit_1)[1]+coef(fit_1)[3], coef(fit_1)[2]+coef(fit_1)[4], lwd=.5)
  ## lm.1 <- lm(y ~ x + t + x*t)
  points(control_pretest[ok], control_posttest[ok], pch=20, cex=1.2)
  points(treated_pretest[ok], treated_posttest[ok], pch=21, cex=1.2)
}

