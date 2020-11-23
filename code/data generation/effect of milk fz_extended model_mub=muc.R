# This script generates the equilibria of the extended model against various breast milk levels. 

# call extended model
source("../model/extended model.R")

# set directory to store data 
output_fz_c_section <- "data/f_z c-section_equalClearance.rData"
output_fz_vaginal <- "data/f_z vaginal_equalClearance.rData"


# c-section --------------------------------------------------------------

# make an empty dataframe 
df_fz <- data.frame()

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

# death rate 
mu_b <- 0.2*k
mu_c <- 0.2*k


# immune effect
gamma <- 0.0001

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k

# solve the function with different f_z values 
pow <- seq(-2,2,0.04)


cnt <- 1
repeat {
  
  f_z <- 10^pow[cnt]/k
  
  # print f_z values for each run
  print(paste0("f_z = ", f_z))
  
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, alpha_c=alpha_c,gamma=gamma,mu_b=mu_b,mu_c=mu_c) 
  
  
  # initial conditions for c-section
  y0 <- c(B_1 = 1/k, B_2 = 1/k, B_3 = 50/k, M = 1/k) 
  
  # times
  times <- seq(0,1000,1)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  
  # print b2/b3 at equilibira of each run 
  print(paste0("log10(B2/B3) = ", log10(output[length(output$time),3]/output[length(output$time),4])))
  
  output$f_z <- f_z
  
  # add the last row of output to df 
  df_fz <- rbind(df_fz, output[length(output$time),2:6])
  
  
  # repeat to run the function 
  cnt <- cnt+1
  if(cnt == 102) {
    break
  }
}

save(df_fz,file = output_fz_c_section)



# vaginal birth --------------------

# make an empty dataframe 
df_fz <- data.frame()


# parameters for the function Z(t)
w <- 0.014; h <- 500

# growth rate
r <- 1

# competition
alpha <- 2
alpha_c <- 1.7

# carrying capacity
k <- 1000

# death rate 
mu_b <- 0.2*k
mu_c <- 0.2*k

# immune effect
gamma <- 0.0001

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k

# solve the function with different f_z values 
pow <- seq(-2,2,0.04)


cnt <- 1
repeat {
  
  f_z <- 10^pow[cnt]/k
  
  # print f_z values for each run
  print(paste0("f_z = ", f_z))
  
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, alpha_c=alpha_c,gamma=gamma,mu_b=mu_b,mu_c=mu_c) 
  
  
  # initial conditions for vaginal
  y0 <- c(B_1 = 50/k, B_2 = 50/k, B_3 = 1/k, M = 1/k) 
  
  
  # times
  times <- seq(0,1000,1)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  
  # print b2/b3 at equilibira of each run 
  print(paste0("log10(B2/B3) = ", log10(output[length(output$time),3]/output[length(output$time),4])))
  
  output$f_z <- f_z
  
  # add the last row of output to df 
  df_fz <- rbind(df_fz, output[length(output$time),2:6])
  
  
  # repeat to run the function 
  cnt <- cnt+1
  if(cnt == 102) {
    break
  }
}

save(df_fz,file = output_fz_vaginal)





