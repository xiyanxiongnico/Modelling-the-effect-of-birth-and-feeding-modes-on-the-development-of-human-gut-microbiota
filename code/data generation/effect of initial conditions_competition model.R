# This script generates the equilibria of the competition model against a range of initial conditions. 

# call competition model
source("../model/competition model.R")

# set directory to store the generated data 
output_initial_conditions <- "data/initial conditions.rData"

# make an empty dataframe 
df_initial <- data.frame()

# parameters --------------------------------------------------------------
library(deSolve)
library(plyr)

# parameters for the function Z(t)
w <- 0.014; h <- 500

# growth rate
r <- 1

# competition
alpha <- 2
alpha_c <- 1.7

# carrying capacity
k <- 1000

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k; f_z<- 10/k


# get a sequence of initial conditions 
b1_init <- seq(0,50,0.5)/k
b3_init <- rep(50, times = 200)/k
b2_init <- seq(0,50,0.5)/k
cnt <- 1

repeat {
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c) 
  
  # times
  times <- seq(0,1000,1)
  
  # initial conditions 
  y0 <- c(B_1 = b1_init[cnt],B_2=b2_init[cnt],B_3=b3_init[cnt]) 
  
  # check sequence values
  print(cnt)
  print(y0)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  
  # add initial conditions to data frame 
  output$B_1_init <- y0[1]
  output$B_2_init <- y0[2]
  output$B_3_init <- y0[3]
  
  # add the last row of output to df 
  df_initial <- rbind(df_initial, output[length(output$time),2:7])
  
  
  # repeat to run the function 
  cnt <- cnt+1
  if(cnt == 102) {
    break
  }
}

save(df_initial,file = output_initial_conditions)






