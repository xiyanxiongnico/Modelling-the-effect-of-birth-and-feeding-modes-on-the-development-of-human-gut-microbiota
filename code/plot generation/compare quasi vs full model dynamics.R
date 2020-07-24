# This script plot and compare the dynamics of the quasi-steady-state approximation and the full model.

# full momdel ----------------
full_model <- function(t,y,P){
  B_1 <- y[1] #bifidobacteria capable of HMO metabolism
  B_2 <- y[2] #bifidobacteria capable of plant polysaccharide metabolism 
  B_3 <- y[3] #another commensal group
  
  
  # proportion of milk in diet as function Z
  Z <- 1/(1 + exp(w*(t - h))) 
  
  with(as.list(p),{
    # the grow rate of B1 related to milk consumption
    dB_1dt <- r*B_1*(Z - (B_1 + B_2 + alpha*B_3)) + f_z*Z/k
    # the grow rate of B2 related to solid food consumption
    # competition between B2 & B2, B2 & B_3, B1 & B2 
    dB_2dt <- r*B_2*(1- (B_1 + B_2 + alpha*B_3 - alpha_c*B_1)) + f_2/k
    # competition between B_3 & B_3, B2 & B_3, B1 & B_3 
    # f3 is the input of B_3 from environment
    dB_3dt <- r*B_3*(1- (B_3 + alpha*(B_1 + B_2))) + f_3/k
    
    return(list(c(dB_1dt,dB_2dt,dB_3dt)))
  })}

# quasi steady state approximation ----------

quasi_model <- function(t,y,p){
  B_2 <- y[1] #bifidobacteria capable of plant polysaccharide metabolism 
  B_3 <- y[2] #another commensal group
  

  with(as.list(p),{
    # the grow rate of B2 related to solid food consumption
    # competition between B2 & B2, B2 & B_3 
    dB_2dt <- r*B_2*(1- B_2 - alpha*B_3) + f_2/k
    # competition between B_3 & B_3, B2 & B_3, B1 & B_3 
    # f3 is the input of B_3 from environment
    dB_3dt <- r*B_3*(1 - B_3 - alpha* B_2) + f_3/k
    
    return(list(c(dB_2dt,dB_3dt)))
  })}



# parameters ----------

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
f_2 <- 30; f_3 <- 50; f_z<- 10

# parameters used in the full model
P <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
       r = r, alpha =alpha, k=k,
       alpha_c=alpha_c) 

# parameters used in the approximate model
p <- c(f_2 = f_2, f_3 = f_3, r = r, alpha =alpha, k=k) 


# initial conditions -----------
# vaginal birth, full model
Y0_vaginal <- c(B_1 = 50/k,B_2 = 50/k,B_3 = 1/k) 

# C-section, full model 
Y0_csection <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k) 

# vaginal birth, quasi model
y0_vaginal <- c(B_2 = 50/k,B_3 = 1/k) 

# C-section, quasi model 
y0_csection <- c(B_2 = 1/k,B_3 = 50/k) 

# times
times <- seq(0,1000,1)

# Solve the models and store in a data frame--------

library(deSolve)
# solve full model with parameter set P
out_full_vaginal <- ode(y = Y0_vaginal, times, full_model, P)
out_full_csection <- ode(y = Y0_csection, times, full_model, P)

# solve quasi model with parameter set p
out_quasi_vaginal <- ode(y = y0_vaginal, times, quasi_model, p)
out_quasi_csection <- ode(y = y0_csection, times, quasi_model, p)


library(reshape2) 
library(plyr)

# make a data frame for results 
data_full_vaginal <- data.frame(out_full_vaginal)
names(data_full_vaginal) <- c('Time','B1_full_v','B2_full_v','B3_full_v')

data_full_csection <- data.frame(out_full_csection)
names(data_full_csection) <- c('Time','B1_full_c','B2_full_c','B3_full_c')

data_quasi_vaginal <- data.frame(out_quasi_vaginal)
names(data_quasi_vaginal) <- c('Time','B2_quasi_v','B3_quasi_v')

data_quasi_csection <- data.frame(out_quasi_csection)
names(data_quasi_csection) <- c('Time','B2_quasi_c','B3_quasi_c')

# merge data of full and quasi model, and drop the second coloumn of data_full 
data_v <- merge(data_full_vaginal[-2],data_quasi_vaginal)

data_c <- merge(data_full_csection[-2], data_quasi_csection)

data_melt_v <- melt(data = data_v, id.vars = "Time", measure.vars = c('B2_full_v','B3_full_v',
                                                                      'B2_quasi_v','B3_quasi_v'))

data_melt_c <- melt(data = data_c, id.vars = "Time", measure.vars = c('B2_full_c','B3_full_c',
                                                                   'B2_quasi_c','B3_quasi_c'))
  

# ggplot ------------------------------------------------------------------

library(ggplot2)
# put plots in the same page
library(ggpubr) 
# to access break formatting functions
library(scales) 

mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        legend.title = element_blank()) 



# plot dynamics of full model and quasi model under vaginal scheme 
plot_v <- ggplot(data = data_melt_v, aes(x = Time, y = value, color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("solid","twodash", "dotted","dotdash"),
                        labels = c('B2_full','B3_full','B2_quasi','B3_quasi')) +
  scale_color_manual(values = c("darkolivegreen2","darkolivegreen4",
                                "brown","brown1"),
                     labels= c('B2_full','B3_full','B2_quasi','B3_quasi')) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits = c(1e-3,1.1)) +
  mytheme +
  labs( x = "Time (days)", y = "Population density",title ="Vaginal")


# plot dynamics of full model and quasi model under c-section scheme 
plot_c <- ggplot(data = data_melt_c, aes(x = Time, y = value, color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("solid","twodash", "dotted","dotdash"),
                        labels = c('B2_full','B3_full','B2_quasi','B3_quasi')) +
  scale_color_manual(values = c("darkolivegreen2","darkolivegreen4",
                                "brown","brown1"),
                     labels= c('B2_full','B3_full','B2_quasi','B3_quasi')) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits = c(1e-3,1.1)) +
  mytheme +
  labs( x = "Time (days)", y = "Population density",title ="C-section")


# put two plot together 
quasi_full_compare <- ggarrange(plot_v, plot_c, 
                        labels = c("A", "B"), nrow = 1, ncol = 2,
                        common.legend = TRUE, legend = "bottom")

show(quasi_full_compare)

ggsave(file = "full_and_quasi_model_dynamics.pdf",width = 17, height = 8.5, units = "cm")

