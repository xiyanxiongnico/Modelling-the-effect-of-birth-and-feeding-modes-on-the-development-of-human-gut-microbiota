# This script generate the ratio of log(B2/B3) at equilibria for the extended model
# where the immune response dM/dt = gamma*(B1+B2)-mu*M
# We explore the effect of varying gamma and mu on the equilibria

library(dplyr)

# make directory for saving data 
output_heatmap <- "data/pspace_onlyGammaMu_Mlinear.rData"

# create parameter space
k <- 1000
seq_short <- seq(0.2,5,0.4)/k
gamma <- seq_short
mu <- seq_short

# make random combinations of the above parameters
pSpace <- expand.grid(gamma, mu)
colnames(pSpace) <- c("gamma","mu")

# make random combinations of the above parameters
pSpace <- expand.grid( gamma, mu)
colnames(pSpace) <- c("gamma","mu")

# initial conditions for c-section and vaginal birth 
CS <- c(1,1,50)/k
VB <- c(50,50,1)/k

# run model 
source("../model/extended model_LinearCombinationM.R")

library(deSolve)

# make an empty dataframe 
output_pSpace <- data.frame()

# parameters for the function Z(t)
w <- 0.014; h <- 500

# growth rate
r <- 1

# effect of species interaction 
alpha <- 2

# effect of cross-feeding
alpha_c <- 1.7


# death rate 
mu_b <- 0.05*k
mu_c <- 0.2*k



# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k; f_z<- 10/k


cnt <- 1
repeat {
  
  # assign values from pspace to the parameters below
  gamma <- pSpace[cnt,1]
  mu <- pSpace[cnt,2]
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c, gamma=gamma, mu_b=mu_b, mu_c=mu_c, mu=mu) 
  
  
  # initial conditions for c-section
  b1_0 <- CS[1]
  b2_0 <- CS[2]
  b3_0 <- CS[3]
  y0 <- c(B_1 = b1_0, B_2 = b2_0, B_3 = b3_0, M = 1/k) 
  
  # print pSpace row employed at each run
  print(pSpace[cnt,])
  print(y0) 
  
  # times
  times <- seq(0,5000,1)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  # add the last B2 and B3 values of from each run to output_pSpace
  output_pSpace <- rbind(output_pSpace, output[length(output$time),3:4])
  
  # repeat with all pSpace combinations 
  cnt <- cnt+1
  if(cnt == length(pSpace$gamma) + 1) {
    break
  }
}


# join pSpace with output_pSpace 
output_pSpace <- cbind(output_pSpace,pSpace)

# calculate log(B2/B3) for each run and add it as a new column 
output_pSpace$Ratio <- log10(output_pSpace$B_2/output_pSpace$B_3)


# convert into factor 
output_pSpace$gamma <- as.factor(output_pSpace$gamma)
output_pSpace$mu <- as.factor(output_pSpace$mu)

# save the data 
save(output_pSpace,file = output_heatmap)
