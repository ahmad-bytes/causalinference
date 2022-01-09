rm(list = ls())

library("MatchIt")
library("rprojroot")
library("rstanarm")
library("arm")
library("ggplot2")
library("bayesplot")
library("visreg")

load('C:\\Users\\bilal\\Dropbox\\MS\\Causal Inference\\causalinference\\Scrubbers.RData') #loads in a data frame called 'dat'
head(dat)
dim(dat)

dat$Z <- with(dat, ifelse(ScrubbedFacility == FALSE, 0, 1))
fit_1 <- stan_glm(Z ~ TotPop+PctUrban+PctWhite+PctBlack+PctHisp+PctHighSchool+MedianHHInc+PctPoor+PctOccupied+meanSulfur+totOpTime+ totHeatInput+Phase2+Region, family=binomial(link="logit"), data=dat, refresh=0)

#summary(fit_1)
pscores_1 = predict(fit_1 , type = 'response')
mean(pscores_1)
sd(pscores_1)

pscores <- apply(posterior_epred(fit_1), 2, mean)
mean(pscores)
sd(pscores)

m.out1 <- matchit(Z ~ TotPop+PctUrban+PctWhite+PctBlack+PctHisp+PctHighSchool+MedianHHInc+PctPoor+PctOccupied+meanSulfur+totOpTime+ totHeatInput+Phase2
                  ,distance = dat$propscore, 
                  data = dat,
                  method = "nearest",
                  replace = F)

matched <- match.data(m.out1)
head(matched)
dim(matched)

ATTmat_TotPop = mean(matched$TotPop[matched$Z==1]) - mean(matched$TotPop[matched$Z==0])
sdATT_TotPop = sd(matched$TotPop[matched$Z==1])
smd_TotPop =  abs(ATTmat_TotPop)/sdATT_TotPop
smd_TotPop

ATTmat_PctUrban = mean(matched$PctUrban[matched$Z==1]) - mean(matched$PctUrban[matched$Z==0])
sdATT_PctUrban = sd(matched$PctUrban[matched$Z==1])
smd_PctUrban = ATTmat_PctUrban/sdATT_PctUrban
smd_PctUrban

ATTmat_PctWhite = mean(matched$PctWhite[matched$Z==1]) - mean(matched$PctWhite[matched$Z==0])
sdATT_PctWhite = sd(matched$PctWhite[matched$Z==1])
smd_PctWhite = ATTmat_PctWhite/sdATT_PctWhite
smd_PctWhite

ATTmat_PctBlack = mean(matched$PctBlack[matched$Z==1]) - mean(matched$PctBlack[matched$Z==0])
sdATT_PctBlack = sd(matched$PctBlack[matched$Z==1])
smd_PctBlack = ATTmat_PctBlack/sdATT_PctBlack
smd_PctBlack

ATTmat_PctHisp = mean(matched$PctHisp[matched$Z==1]) - mean(matched$PctHisp[matched$Z==0])
sdATT_PctHisp = sd(matched$PctHisp[matched$Z==1])
smd_PctHisp = ATTmat_PctHisp/sdATT_PctHisp
smd_PctHisp

ATTmat_PctHighSchool = mean(matched$PctHighSchool[matched$Z==1]) - mean(matched$PctHighSchool[matched$Z==0])
sdATT_PctHighSchool = sd(matched$PctHighSchool[matched$Z==1])
smd_PctHighSchool = ATTmat_PctHighSchool/sdATT_PctHighSchool
smd_PctHighSchool

ATTmat_MedianHHInc = mean(matched$MedianHHInc[matched$Z==1]) - mean(matched$MedianHHInc[matched$Z==0])
sdATT_MedianHHInc = sd(matched$MedianHHInc[matched$Z==1])
smd_MedianHHInc = ATTmat_MedianHHInc/sdATT_MedianHHInc
smd_MedianHHInc

ATTmat_PctPoor = mean(matched$PctPoor[matched$Z==1]) - mean(matched$PctPoor[matched$Z==0])
sdATT_PctPoor = sd(matched$PctPoor[matched$Z==1])
smd_PctPoor = ATTmat_PctPoor/sdATT_PctPoor
smd_PctPoor

ATTmat_PctOccupied = mean(matched$PctOccupied[matched$Z==1]) - mean(matched$PctOccupied[matched$Z==0])
sdATT_PctOccupied = sd(matched$PctOccupied[matched$Z==1])
smd_PctOccupied = ATTmat_PctOccupied/sdATT_PctOccupied
smd_PctOccupied


ATTmat_meanSulfur = mean(matched$meanSulfur[matched$Z==1]) - mean(matched$meanSulfur[matched$Z==0])
sdATT_meanSulfur = sd(matched$meanSulfur[matched$Z==1])
smd_meanSulfur = ATTmat_meanSulfur/sdATT_meanSulfur
smd_meanSulfur

ATTmat_totOpTime = mean(matched$totOpTime[matched$Z==1]) - mean(matched$totOpTime[matched$Z==0])
sdATT_totOpTime = sd(matched$totOpTime[matched$Z==1])
smd_totOpTime = ATTmat_totOpTime/sdATT_totOpTime
smd_totOpTime

ATTmat_totHeatInput = mean(matched$totHeatInput[matched$Z==1]) - mean(matched$totHeatInput[matched$Z==0])
sdATT_totHeatInput = sd(matched$totHeatInput[matched$Z==1])
smd_totHeatInput = ATTmat_totHeatInput/sdATT_totHeatInput
smd_totHeatInput

ATTmat_Phase2 = mean(matched$Phase2[matched$Z==1]) - mean(matched$Phase2[matched$Z==0])
sdATT_Phase2 = sd(matched$Phase2[matched$Z==1])
smd_Phase2 = ATTmat_Phase2/sdATT_Phase2
smd_Phase2

ATTmat_Region = mean(matched$Region[matched$Z==1]) - mean(matched$Region[matched$Z==0])
sdATT_Region = sd(matched$Region[matched$Z==1])
smd_Region = ATTmat_Region/sdATT_Region
smd_Region


fit_best <- stan_glm(PM ~ Z+TotPop+PctUrban+PctWhite+PctBlack+PctHisp+PctHighSchool+MedianHHInc+PctPoor+PctOccupied+meanSulfur+totOpTime+ totHeatInput+Phase2+Region, data=matched, refresh=0)
preddat = matched
ypred2 = posterior_predict(fit_best, newdata=preddat)
#ATTmat = round(matched$PM[matched$Z==1] - ypred2[,matched$Z==0] ,2)


mean(ypred2[,matched$Z==1]) - mean(ypred2[,matched$Z==0])

ATTmat = round(ypred2[,matched$Z==1] - ypred2[,matched$Z==0],2)
ATT_2 = mean(rowMeans(ATTmat))
sdATT_2 = sd(rowMeans(ATTmat))


