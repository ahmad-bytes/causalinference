rm(list = ls())
set.seed(51)

## - Input the data from Figure 18.2 in Gelman, Hill Vehtari
num_of_people = c(300,300,700,700,100,100,100,100)
x = c(0,1,0,1,0,1,0,1)
treatment = c(0,0,1,1,0,0,1,1)
y0 = c(5,5,4,4,10,10,10,10)
y1 = c(9,9,5,5,13,13,14,14)

y0_all = y0 * num_of_people
y1_all = y1 * num_of_people
  

## - Calculate the *true* causal effect for each person
ce_ind = (y1 - y0) * num_of_people
df = cbind(data.frame(num_of_people, x, treatment, y0_all,y1_all,ce_ind))

## - Calculate the *true* causal effect on average
ce_avg = sum(ce_ind) / sum(num_of_people)
ce_avg     

# Note: The above ce_ind and ce_avg are not observable because they require knowledge of both
#       potential outcomes!

## - Now, mimic what would actually be observed by "uncovering" the which of the potential
##   outcomes is observed based on the treatment assignment
y = rep(NA, length(name))
y[treatment==0] = y0[treatment==0]
y[treatment==1] = y1[treatment==1]


## - Calculate the observed difference in those who took vs. did not take the supplement
diff_obs = sum(df$y1_all[df$treatment==1])/sum(df$num_of_people[df$treatment==1]) - sum(df$y0_all[df$treatment==0])/sum(df$num_of_people[df$treatment==0])
diff_obs


df

sum(df$y1_all[df$treatment==1])
sum(df$num_of_people[df$treatment==1])
sum(df$y0_all[df$treatment==0])
sum(df$num_of_people[df$treatment==0])