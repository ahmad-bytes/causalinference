rm(list = ls())
set.seed(51)

# - Illustrate example of randomized block design
n= 120 # patients enrolled in a cancer surgery trial

# simulate covariate representing organ of the cancer
organ = sample(rep(c("liver", "pancreas", "stomach", "rectum"), 
                   times=1:4), n, 1)

# Simulate potential outcomes
y0 = round( rnorm(n, 10 + 2*(organ=="liver") + 3*(organ=="pancreas") + 4*(organ=="stomach"), .1),1)
y1 = round( y0 + 5 + 2*(organ=="liver") + 3*(organ=="pancreas") + 4*(organ=="stomach") + rnorm(n,0,.1),1)

dat = cbind(organ, y0, y1)
dat[order(organ), ]

tau_sate.true = mean(y1 - y0)
tau_sate.true


# ---- Completely Randomized Design
nt = round(n*.5) # number treated units, treat half of all units
# Repeat the completely randomized trial to get the randomization distribution
n_reps = 10000
Z.cr = matrix(NA, n_reps,n) # a matrix to store all of the different Z vectors
for (i in 1:n_reps)
  Z.cr[i,] = sample(c(rep(0,n-nt), rep(1,nt)), n)

table(Z.cr[11,], organ)


# Calculate the mean difference between treated/control for each randomization
tau_sate.cr = rep(NA, n_reps)
for (i in 1:n_reps)
  tau_sate.cr[i] = mean(y1[Z.cr[i,]==1]) - mean(y0[Z.cr[i,]==1])

m.cr = round(mean(tau_sate.cr), 2)
sd.cr = round(sd(tau_sate.cr), 2)
hist(tau_sate.cr, main = paste("CR: Mean = ", m.cr, " SD = ", sd.cr, sep=""))
             

# ---- Block Randomized Design
# treat organ as a blocking factor
block = ifelse(organ=="liver", 1, 
               ifelse(organ=="pancreas", 2, 
                      ifelse(organ=="stomach", 3
                             ,4)))
n.blocks = length(unique(block))
n.j = as.vector(table(block))

n.j
nt = round(n.j*.5) # number of treated per block, treat half per block

# Repeat the completely randomized trial to get the randomization distribution
n_reps = 10000
Z.br = matrix(NA, n_reps,n) # a matrix to store all of the different Z vectors
for (i in 1:n_reps)
  for (j in 1:n.blocks)
    Z.br[i,block==j] = sample(c(rep(0,n.j[j]-nt[j]), rep(1, nt[j])))

table(Z.br[11, ], block)

# Calculate the mean difference between treated/control for each randomization
tau_sate.br = rep(NA, n_reps)
taus.b = rep(NA, n.blocks)
for (i in 1:n_reps){
  for (j in 1:n.blocks)
    taus.b[j] = mean(y1[block==j & Z.br[i,]==1]) -  mean(y0[block==j & Z.br[i,]==0])
  
  tau_sate.br[i] = (n.j[1]/n)*taus.b[1] +  (n.j[2]/n)*taus.b[2] +  (n.j[3]/n)*taus.b[3] +  (n.j[4]/n)*taus.b[4] 
}  

m.br = round(mean(tau_sate.br), 2)
sd.br = round(sd(tau_sate.br), 2)
hist(tau_sate.br, main = paste("Blocked: Mean = ", m.br, " SD = ", sd.br, sep=""))


#--------------------------------------------
