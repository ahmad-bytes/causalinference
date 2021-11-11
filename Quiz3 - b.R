rm(list = ls())
set.seed(51)

#' #### Load packages
library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")

cows_data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/Cows.txt")
Y = cows_data$fat * cows_data$milk
level_norm <- cows_data$level * 10
cows_data_new = cbind(cows_data, Y, level_norm)

#' M1 <- stan_glm(Y ~ level , data = cows_data_new, refresh = 0)
#' # Print default summary of the fitted model
#' print(M1)
#' se(M1)
#' #' 
#' M2 <- stan_glm(Y ~ level + age + lactation + initial.weight, data = cows_data_new, refresh = 0)
#' #Print default summary of the fitted model
#' print(M2)
#' se(M2)

n = 4

est1 <- rep(NA,n)
se1 <- rep(NA,n)
est2 <- rep(NA,n)
se2 <- rep(NA,n)

for (k in 1:n) {
  l = k-1
  filtered = with(cows_data_new, cows_data_new[ (level_norm == 0) | (level_norm == l), ])
  #filtered = data.frame(cows_data_new[cows_data_new$level_norm==0 | cows_data_new$level_norm ==l,])
  filtered$level_norm[filtered$level_norm == l] <- 1
  
  fit_1 <- stan_glm(Y ~ level_norm, data = filtered,
                    refresh = 0, save_warmup = FALSE,
                    open_progress = FALSE, cores = 1)
  est1[k] <- coef(fit_1)[2]
  se1[k] <- se(fit_1)[2]
  
  fit_2 <- stan_glm(Y ~ level_norm + age + lactation + initial.weight, data = filtered,
                    refresh = 0, save_warmup = FALSE,
                    open_progress = FALSE, cores = 1)
  est2[k] <- coef(fit_2)[2]
  se2[k] <- se(fit_2)[2]
}
round(est1,2)
round(se1,2)
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
  
  labels1 <- rep(NA,n)
  labels2 <- rep(NA,n)
  
  for (k in 1:n) {
    labels1[k] = paste('Mean:' ,round(est1[k],2), ' - 95% interval (:', round(est1[k],2) - 2 * round(se1[k],2) , ',', round(est1[k],2) + 2 * round(se1[k],2)   , ')' )
    labels2[k] = paste('Mean:' ,round(est2[k],2), ' - 95% interval (:', round(est2[k],2) - 2 * round(se2[k],2) , ',', round(est2[k],2) + 2 * round(se2[k],2)   , ')' )
  }
  text (A + B*est1, -(1:J) + 0.3, labels1 , cex=0.5)
  text (1+gap+A + B*est2, -(1:J) + 0.3, labels2 , cex=0.5)
  
  points (A + B*est1, -(1:J), pch=20, cex=1)
  points (1+gap+A + B*est2, -(1:J), pch=20, cex=1)
  
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (1+gap+A + B*(est2-se2), -(1:J), 1+gap+A + B*(est2+se2), -(1:J), lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  segments (1+gap+A + B*(est2-2*se2), -(1:J), 1+gap+A + B*(est2+2*se2), -(1:J), lwd=.5)
  if (bottom){
    #lines (c(0,1), c(-J-1,-J-1))
    #lines (1+gap+c(0,1), c(-J-1,-J-1))
    #segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    #segments (1+gap+A + B*ax, -J-1-.1, 1+gap+A + B*ax, -J-1+.1, lwd=.5)
    # text (A + B*ax, -J-1-.7, ax, cex=1)
    # text (1+gap+A + B*ax, -J-1-.7, ax, cex=1)
  }
  if (!is.na(file)) graphics.off()
}

par(mfrow = c(1,1))
regression.2tables(paste("Level", 1:n), est1, est2, se1, se2
                   , "Regression on treatment indicator",
                   "Regression on treatment indicator,\ncontrolling for pre-treatment", NA)


