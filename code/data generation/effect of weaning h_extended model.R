# This script generates the equilibria of the extended model against a range of weaning schedules. 

# call extended model
source("~/Documents/GitHub/Modelling-the-effect-of-birth-and-feeding-modes-on-the-development-of-human-gut-microbiota/code/model/extended model.R")

# set directory to store data 
output_h_c_section <- "data/h_c-section.rData"
output_h_vaginal <- "data/h_vaginal.rData"

# make an empty dataframe 
df_h <- data.frame()

# parameters --------------------------------------------------------------
library(deSolve)
library(plyr)

# parameters for the function Z(t)
w <- 0.014; h <- 0

# growth rate
r <- 1

# competition 
alpha <- 2
alpha_c <- 1.7

# carrying capacity
k <- 1000

# death rate 
mu_b <- 0.05*k
mu_c <- 0.2*k

# immune effect
gamma <- 0.0001

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k; f_z<- 10/k

cnt <- 0
repeat {
  print(paste0("h ", h))
  
  # parameters used in the function 
  p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z, 
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c,mu_b=mu_b, mu_c=mu_c, gamma=gamma) 
  
  
  # initial conditions for c-section
  # y0 <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k, M = 1/k) 
  
  # initial conditions for vaginal birth
   y0 <- c(B_1 = 50/k,B_2 = 50/k,B_3 = 1/k, M = 1/k) 
  
  # times
  times <- seq(0,4000,1)
  
  # solve ode and save as data frame
  output <- data.frame(ode(y = y0, times, competition, p))
  
  # print b2/b3 at equilibira of each run 
  print(paste0("log10(B2/B3) = ", log10(output[length(output$time),3]/output[length(output$time),4])))
  
  # output <- round(output,digits = 2)
  output$h <- h
  
  # add the last row of output to df 
  df_h <- rbind(df_h, output[length(output$time),2:6])
  
  h = h + 10
  
  # repeat to run the function 
  cnt <- cnt+1
  if(cnt == 151) {
    break
  }
}

 #save(df_h,file = output_h_c_section)
 save(df_h,file = output_h_vaginal)

