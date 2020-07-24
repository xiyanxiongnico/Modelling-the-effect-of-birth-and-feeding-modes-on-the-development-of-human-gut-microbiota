# This script generate the temporal dynamics of the competition model using baseline parameters. 

source("competition model.R")


library(deSolve)

# parameters for the function Z(t)
w <- 0.014; h <- 500

# growth rate
r <- 1

# effect of species interaction 
alpha <- 2
alpha_c <- 1.7

# carrying capacity
k <- 1000

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k; f_z<- 10/k


# parameters used in the function 
p <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
       r = r, alpha =alpha, k=k,
       alpha_c=alpha_c) 


# initial conditions for dynamical variables
# vaginal birth
# y0 <- c(B_1 = 50/k,B_2 = 50/k,B_3 = 1/k) 
 
# C-section
 y0 <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k) 
 

# times
times <- seq(0,1000,1)


# solve ode 
output <- ode(y = y0, times, competition, p)


# make data frame for ggplot --------------------------------------------------------------------

library(reshape2) 
library(plyr)
library(tidyverse)

# make a data frame for output 
bacteria <- data.frame(output)

names(bacteria)[2] <- "HMO-consuming Bifidobacteria B1"
names(bacteria)[3] <- "Fibre-consuming Bifidobacteria B2"
names(bacteria)[4] <- "Commensal competitor B3"

bacteria_melt <- melt(data = bacteria, id.vars = "time", 
                      measure.vars = c("HMO-consuming Bifidobacteria B1", "Fibre-consuming Bifidobacteria B2",
                                       "Commensal competitor B3"))


# save data frame 
# save(bacteria_melt,file = "dynamics_competition_vaginal.rData")
 save(bacteria_melt,file = "dynamics_competition_c-section.rData")

