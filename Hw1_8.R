rm(list = ls())
set.seed(51)

## - Input the data from Figure 18.2 in Gelman, Hill Vehtari
name = c("A", "B", "C", "D", "E", "F", "G", "H")
x = c(4,11,8,2,2,9,7,8)
treatment = c(0,1,0,0,1,1,0,1)
y0 = c(6,9,8,4,5,10,8,11)
y1 = c(7,15,9,3,2,6,15,13)

## - Calculate the *true* causal effect for each person
ce_ind = y1 - y0

df = cbind(data.frame(name, x, treatment, y0 , y1 , ce_ind, y1-x, y0-x))

## - Calculate the *true* causal effect on average
ce_avg = mean(ce_ind)
ce_avg     

#8.b

satt = mean(df$y1[treatment==1] - df$y0[treatment==1])

## - Calculate the *true* causal effect for men and for women
ce_avg_treated = mean(df$y1[treatment==1])
ce_avg_untreated = mean(df$y0[treatment==0])
answer_8_c = ce_avg_treated - ce_avg_untreated

y = rep(NA, length(name))
y[treatment==0] = y0[treatment==0]
y[treatment==1] = y1[treatment==1]

summary(lm(y ~ df$treatment + df$x))
