rm(list = ls())
set.seed(51)

## - Input the data from Figure 18.2 in Gelman, Hill Vehtari
name = c("Audrey", "Anna", "Bob", "Bill", "Caitlin", "Cara", "Dave", "Doug")
female = c(1,1,0,0,1,1,0,0)
age = c(40,40,50,50,60,60,70,70)
treatment = c(0,0,0,0,1,1,1,1)
y0 = c(140,140,150,150,160,160,170,170)
y1 = c(135,135,140,140,155,155,160,160)

## - Calculate the *true* causal effect for each person
ce_ind = y1 - y0
cbind(name, treatment, y0,y1,ce_ind)

## - Calculate the *true* causal effect on average
ce_avg = mean(ce_ind)
ce_avg     

## - Calculate the *true* causal effect for men and for women
ce_avg_men = mean(ce_ind[female==0])
ce_avg_men

ce_avg_women = mean(ce_ind[female==1])
ce_avg_women

# Note: The above ce_ind and ce_avg are not observable because they require knowledge of both
#       potential outcomes!

## - Now, mimic what would actually be observed by "uncovering" the which of the potential
##   outcomes is observed based on the treatment assignment
y = rep(NA, length(name))
y[treatment==0] = y0[treatment==0]
y[treatment==1] = y1[treatment==1]

## - Calculate the observed difference in those who took vs. did not take the supplement
diff_obs = mean(y[treatment==1]) - mean(y[treatment==0])
diff_obs 
summary(lm(y~treatment))

diff_obs_men = mean(y[treatment==1 & female==0]) - mean(y[treatment==0 & female==0])
diff_obs_men

diff_obs_women = mean(y[treatment==1 & female==1]) - mean(y[treatment==0 & female==1])
diff_obs_women

