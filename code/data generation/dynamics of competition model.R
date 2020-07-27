# This script generate the temporal dynamics of the competition model using baseline parameters. 

# call competition model
source("../model/competition model.R")

# set directory to store the generated data 
output_dynamics_csection <- "data/dynamics_competition_c-section.rData"
output_dynamics_vaginal <- "data/dynamics_competition_vaginal.rData"

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
 y0_vaginal <- c(B_1 = 50/k,B_2 = 50/k,B_3 = 1/k) 
 
# C-section
 y0_csection <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k) 
 

# times
times <- seq(0,1000,1)


# solve ode 
output_vaginal <- ode(y = y0_vaginal, times, competition, p)
output_csection <- ode(y = y0_csection, times, competition, p)



# make data frame for ggplot --------------------------------------------------------------------

library(reshape2) 
library(plyr)
library(tidyverse)

# make a data frame for output 
bacteria_vaginal <- data.frame(output_vaginal)
bacteria_csection <- data.frame(output_csection)

#add column names
names(bacteria_vaginal)[2] <- "HMO-consuming Bifidobacteria B1"
names(bacteria_vaginal)[3] <- "Fibre-consuming Bifidobacteria B2"
names(bacteria_vaginal)[4] <- "Commensal competitor B3"

names(bacteria_csection)[2] <- "HMO-consuming Bifidobacteria B1"
names(bacteria_csection)[3] <- "Fibre-consuming Bifidobacteria B2"
names(bacteria_csection)[4] <- "Commensal competitor B3"

# format data frame for plot
bacteria_melt_vaginal <- melt(data = bacteria_vaginal, id.vars = "time", 
                      measure.vars = c("HMO-consuming Bifidobacteria B1", "Fibre-consuming Bifidobacteria B2",
                                       "Commensal competitor B3"))

bacteria_melt_csection <- melt(data = bacteria_csection, id.vars = "time", 
                              measure.vars = c("HMO-consuming Bifidobacteria B1", "Fibre-consuming Bifidobacteria B2",
                                        "Commensal competitor B3"))
# save data frame 
save(bacteria_melt_vaginal,file = output_dynamics_vaginal)
save(bacteria_melt_csection,file = output_dynamics_csection)


