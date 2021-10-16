rm(list = ls())

name = c(1,2,3,4,5,6,7,8,9,10)
treatment = c(NA, 1:10)
y0 = c(51, 35, 46, 78, 23, 45, 84, 52, 14, 21)
y1 = c(71, 45, 56, 78, 43, 55, 84, 72, 24, 31)

c_avg = mean(y1 - y0)


get_sate <- function(z){
  ds <- cbind(name, z, y0, y1)  
  mean(y1[z==1]) - mean(y0[z==0])
}

# z = c (1,0,1,1,0,0,1,1,0,0)
# a <- get_sate(z)
# 
# z = c (0,1,0,0,1,1,0,0,1,1)
# b <- get_sate(z)
# 
# z = c (1,1,1,1,0,1,1,1,0,0)
# c <- get_sate(z)
# 
# z = c (0,0,0,0,1,0,0,0,1,1)
# d <- get_sate(z)
# 
# 
# z = c (1,1,1,0,1,1,0,1,1,1)
# e <- get_sate(z)

z = c (1,0,1,1,0,1,1,1,0,0)
a <- get_sate(z)

z = c (1,1,0,0,1,1,0,1,1,1)
b <- get_sate(z)

z = c (1,1,1,1,0,1,1,1,0,0)
c <- get_sate(z)

z = c (0,0,0,1,1,1,1,1,1,1)
d <- get_sate(z)


z = c (1,1,0,0,1,1,0,1,1,1)
e <- get_sate(z)
