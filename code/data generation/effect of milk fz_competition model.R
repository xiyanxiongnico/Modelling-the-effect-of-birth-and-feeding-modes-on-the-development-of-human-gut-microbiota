# This script generates the equilibria of the competition model against a range of various levels of breast milk.

# call competition model
source("../model/competition model.R")

# make directory for saving data 
output_fz_vaginal <- "data/f_z_vaginal_without_M.rData"
output_fz_csection <- "data/f_z_c-section_without_M.rData"

# make an empty dataframe 
df_fz_csection <- data.frame()
df_fz_vaginal <- data.frame()


# parameters --------------------------------------------------------------
library(deSolve)
library(plyr)

# parameters for the function Z(t)
w <- 0.014; h <- 500

# growth rate
r <- 1

# competition coefficient
alpha <- 2
alpha_c <- 1.7

# carrying capacity
k <- 1000

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k

# solve the function with different f_z values 
pow <- seq(-2,2,0.04)


cnt <- 1
repeat {
  
  f_z <- 10^pow[cnt]/k
  
  print(paste0("f_z = ", f_z))
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c) 
  
  
  # initial conditions for c-section
   y0_csection <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k) 
  
  # initial conditions for vaginal
   y0_vaginal <- c(B_1 = 50/k, B_2 = 50/k, B_3 = 1/k) 
  
  # times
  times <- seq(0,1000,1)
  
  # solve ode and save as data frame
  output_csection <- data.frame(ode(y = y0_csection, times, competition, p))
  output_vaginal <- data.frame(ode(y = y0_vaginal, times, competition, p))
  
  output_csection$f_z <- f_z
  output_vaginal$f_z <- f_z
  
  # add the last row of output to df 
  df_fz_csection <- rbind(df_fz_csection, output_csection[length(output_csection$time),2:5])
  df_fz_vaginal <- rbind(df_fz_vaginal, output_vaginal[length(output_vaginal$time),2:5])
  
  
  # repeat to run the function 
  cnt <- cnt+1
  if(cnt == 102) {
    break
  }
}

# save the data 
save(df_fz_vaginal,file = output_fz_vaginal)
save(df_fz_csection,file = output_fz_csection)


