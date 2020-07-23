
source("competition model.R")


# make an empty dataframe 
df_fz <- data.frame()

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
  # y0 <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k) 
  
  # initial conditions for vaginal
   y0 <- c(B_1 = 50/k, B_2 = 50/k, B_3 = 1/k) 
  
  # times
  times <- seq(0,1000,1)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  
  output$f_z <- f_z
  
  # add the last row of output to df 
  df_fz <- rbind(df_fz, output[length(output$time),2:5])
  
  
  # repeat to run the function 
  cnt <- cnt+1
  if(cnt == 102) {
    break
  }
}

 save(df_fz,file = "f_z_vaginal_without_M.rData")
# save(df_fz,file = "f_z_c-section_without_M.rData")


