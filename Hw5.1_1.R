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


par(mfrow=c(1,1))
#age
hist(Lalonde_df$age[Lalonde_df$treat==0],xlim=c(10,60),main="",border="darkgrey",breaks=c(10,20,30,40,50,60),mgp=c(2,.5,0),xlab="Age",freq=TRUE)
hist(Lalonde_df$age[Lalonde_df$treat==1],xlim=c(10,60),xlab="age",breaks=c(10,20,30,40,50,60),freq=TRUE,add=T)

#change variable to black, nodegree and married
hist(Lalonde_df$black[Lalonde_df$treat==0],xlim=c(-0.5, 1.5),main="",border="darkgrey",breaks=c(-0.5,0, 0.5, 1, 1.5),mgp=c(2,.5,0),xlab="Black",freq=TRUE)
hist(Lalonde_df$black[Lalonde_df$treat==1],xlim=c(-0.5, 1.5),xlab="Black",breaks=c(-0.5,0, 0.5, 1, 1.5),freq=TRUE,add=T)


#educ
hist(Lalonde_df$educ[Lalonde_df$treat==0],xlim=c(0,20),main="",border="darkgrey",breaks=c(0,4,8,12,16,20),mgp=c(2,.5,0),xlab="Educ",freq=TRUE)
hist(Lalonde_df$educ[Lalonde_df$treat==1],xlim=c(0,20),xlab="Educ",breaks=c(0,4,8,12,16,20),freq=TRUE,add=T)

#educ cat4
hist(Lalonde_df$educ_cat4[Lalonde_df$treat==0],xlim=c(0,5),main="",border="darkgrey",breaks=c(0,1,2,3,4,5),mgp=c(2,.5,0),xlab="Educ Cat4",freq=TRUE)
hist(Lalonde_df$educ_cat4[Lalonde_df$treat==1],xlim=c(0,5),xlab="Educ Cat4",breaks=c(0,1,2,3,4,5),freq=TRUE,add=T)

#re75
hist(Lalonde_df$re75[Lalonde_df$treat==0],xlim=c(0,160000),main="",border="darkgrey",breaks=seq(0,160000, 40000),mgp=c(2,.5,0),xlab="Re75",freq=TRUE)
hist(Lalonde_df$re75[Lalonde_df$treat==1],xlim=c(0,160000),xlab="Re75",breaks=seq(0,160000, 40000),freq=TRUE,add=T)

