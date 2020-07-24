# This script generate the ratio of log(B2/B3) at equilibria for the extended model against combinations of feeding practices and bacteria supply from the environment. 

library(dplyr)

# create parameter space -----------------------------------------
k <- 1000
seq_short <- c(1,10,20,30,50)/k

f_z <- seq(1,501,25)/10^4
h <- seq(0,1600,80)
f2 <- seq_short
f3 <- seq_short

# make random combinations of the above parameters
pSpace <- expand.grid(f_z, h, f2, f3)
colnames(pSpace) <- c("f_z","h","f2","f3")

# initial conditions for c-section and vaginal birth 
CS <- c(1,1,50)/k
VB <- c(50,50,1)/k

# run model in parameter space ------------------------------------
source("extended model.R")

library(deSolve)

# make an empty dataframe 
output_pSpace <- data.frame()

# parameters for the function Z(t)
w <- 0.014

# growth rate
r <- 1

# effect of species interaction 
alpha <- 2

# effect of cross-feeding
alpha_c <- 1.7


# death rate 
mu_b <- 0.05*k
mu_c <- 0.2*k


# immune effect
gamma <- 0.0001


cnt <- 1
repeat {
  
  # assign values from pspace to the parameters below
  f_z <- pSpace[cnt,1]
  h <- pSpace[cnt,2]
  f_2 <- pSpace[cnt,3]
  f_3 <- pSpace[cnt,4]
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c,gamma=gamma,mu_b=mu_b,mu_c=mu_c) 
  
  
  # initial conditions 
   b1_0 <- CS[1]
   b2_0 <- CS[2]
   b3_0 <- CS[3]
  y0 <- c(B_1 = b1_0, B_2 = b2_0, B_3 = b3_0, M = 1/k) 
  
  # print pSpace row employed at each run
  print(pSpace[cnt,])
  print(y0) 

  # times
  times <- seq(0,3000,1)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  # add the last B2 and B3 values of from each run to output_pSpace
  output_pSpace <- rbind(output_pSpace, output[length(output$time),3:4])
  
  # repeat with all pSpace combinations 
  cnt <- cnt+1
  if(cnt == length(pSpace$f_z) + 1) {
    break
  }
}


# join pSpace with output_pSpace 
output_pSpace <- cbind(output_pSpace,pSpace)
# calculate log(B2/B3) for each run and add it as a new column 
output_pSpace$Ratio <- log10(output_pSpace$B_2/output_pSpace$B_3)

output_pSpace$f2 <- as.factor(output_pSpace$f2)
output_pSpace$f3 <- as.factor(output_pSpace$f3)

save(output_pSpace,file = "data_pspace_fz_h_c-section.rData")

