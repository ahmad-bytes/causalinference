rm(list = ls())

balance_data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/balance_data.txt")
#View(balance_data)

smd1 = (mean(balance_data$Xcont.1[balance_data$Treat==1]) - mean(balance_data$Xcont.1[balance_data$Treat==0]))/ sd(balance_data$Xcont.1[balance_data$Treat==1])


smd2 = (mean(balance_data$Xcat.1[balance_data$Treat==1]) - mean(balance_data$Xcat.1[balance_data$Treat==0]))/ sd(balance_data$Xcat.1[balance_data$Treat==1])


smd3 = (mean(balance_data$Xcont.2[balance_data$Treat==1]) - mean(balance_data$Xcont.2[balance_data$Treat==0]))/ sd(balance_data$Xcont.2[balance_data$Treat==1])


smd4 = (mean(balance_data$Xcat.2[balance_data$Treat==1]) - mean(balance_data$Xcat.2[balance_data$Treat==0]))/ sd(balance_data$Xcat.2[balance_data$Treat==1])


matched_data <- read.delim("C:/Users/bilal/Dropbox/MS/Causal Inference/causalinference/matched_data.txt")


smd1 = (mean(matched_data$Xcont.1[matched_data$Treat==1]) - mean(matched_data$Xcont.1[matched_data$Treat==0]))/ sd(matched_data$Xcont.1[matched_data$Treat==1])


smd2 = (mean(matched_data$Xcat.1[matched_data$Treat==1]) - mean(matched_data$Xcat.1[matched_data$Treat==0]))/ sd(matched_data$Xcat.1[matched_data$Treat==1])


smd3 = (mean(matched_data$Xcont.2[matched_data$Treat==1]) - mean(matched_data$Xcont.2[matched_data$Treat==0]))/ sd(matched_data$Xcont.2[matched_data$Treat==1])


smd4 = (mean(matched_data$Xcat.2[matched_data$Treat==1]) - mean(matched_data$Xcat.2[matched_data$Treat==0]))/ sd(matched_data$Xcat.2[matched_data$Treat==1])


