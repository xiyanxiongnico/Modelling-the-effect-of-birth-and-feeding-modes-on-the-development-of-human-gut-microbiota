# This script plot and compare the equilibria of the quasi-steady-state approximation with that of the full model against various levels of competition and feeding practices.


# set directory for plots 
output_plot_h <- "plots/full_and_quasi_equilibira_against_h.pdf"
output_plot_alpha <- "plots/full_and_quasi_equilibira_gainst_alpha.pdf"


# full momdel and quasi steady state approximation ----------------
full_model <- function(t,y,P){
  B_1 <- y[1] #bifidobacteria capable of HMO metabolism
  B_2 <- y[2] #bifidobacteria capable of plant polysaccharide metabolism 
  B_3 <- y[3] #another commensal group
  
  
  # proportion of milk in diet as function Z
  Z <- 1/(1 + exp(w*(t - h))) 
  
  with(as.list(p),{
    # the grow rate of B1 related to milk consumption
    dB_1dt <- r*B_1*(Z - (B_1 + B_2 + alpha*B_3)) + f_z*Z
    # the grow rate of B2 related to solid food consumption
    # competition between B2 & B2, B2 & B_3, B1 & B2 
    dB_2dt <- r*B_2*(1- (B_1 + B_2 + alpha*B_3 - alpha_c*B_1)) + f_2
    # competition between B_3 & B_3, B2 & B_3, B1 & B_3 
    # f3 is the input of B_3 from environment
    dB_3dt <- r*B_3*(1- (B_3 + alpha*(B_1 + B_2))) + f_3
    
    return(list(c(dB_1dt,dB_2dt,dB_3dt)))
  })}


# quasi steady state approximation 
quasi_model <- function(t,y,p){
  B_2 <- y[1] #bifidobacteria capable of plant polysaccharide metabolism 
  B_3 <- y[2] #another commensal group
  
  
  with(as.list(p),{
    # the grow rate of B2 related to solid food consumption
    # competition between B2 & B2, B2 & B_3 
    dB_2dt <- r*B_2*(1- B_2 - alpha*B_3) + f_2
    # competition between B_3 & B_3, B2 & B_3, B1 & B_3 
    # f3 is the input of B_3 from environment
    dB_3dt <- r*B_3*(1 - B_3 - alpha* B_2) + f_3
    
    return(list(c(dB_2dt,dB_3dt)))
  })}



# effect of alpha  ----------------

# parameters for the function Z(t)
w <- 0.014; h <- 500


# growth rate
r <- 1

# effect of species interaction 
alpha_c <- 1.7

# carrying capacity
k <- 1000

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k; f_z<- 10/k

# varying alpha to check accuracy
seq_alpha <- seq(0,3,0.02)


# loop over a series of alpha values 

# make an empty dataframes 
df_alpha_v <- data.frame()
df_alpha_c <- data.frame()

# count iteration 
cnt <- 1
repeat {
  
  alpha <- seq_alpha[cnt]
  
  print(paste0("alpha = ", alpha))
  
  # parameters used in the full model
  P <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c) 
  
  # parameters used in the approximate model
  p <- c(f_2 = f_2, f_3 = f_3, r = r, alpha =alpha, k=k) 

  
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
  
  library(deSolve)
  # solve full model with parameter set P
  data_full_vaginal <- data.frame(ode(y = Y0_vaginal, times, full_model, P))
  # add coloumn names 
  names(data_full_vaginal) <- c('Time','B1_full_v','B2_full_v','B3_full_v')
  
  data_full_csection <- data.frame(ode(y = Y0_csection, times, full_model, P))
  names(data_full_csection) <- c('Time','B1_full_c','B2_full_c','B3_full_c')
  
  # solve quasi model with parameter set p
  data_quasi_vaginal <- data.frame(ode(y = y0_vaginal, times, quasi_model, p))
  names(data_quasi_vaginal) <- c('Time','B2_quasi_v','B3_quasi_v')
  
  data_quasi_csection <- data.frame(ode(y = y0_csection, times, quasi_model, p))
  names(data_quasi_csection) <- c('Time','B2_quasi_c','B3_quasi_c')
  
  # join the equilibria (last rows) of full and quasi model 
  data_v <- merge(data_full_vaginal,data_quasi_vaginal)
  data_c <- merge(data_full_csection,data_quasi_csection)
  
  # a new coloumn of alpha values 
  data_v$alpha <- alpha
  data_c$alpha <- alpha
  
  # add the last row of output to df 
  df_alpha_v <- rbind(df_alpha_v, data_v[length(data_v$Time),-2])
  df_alpha_c <- rbind(df_alpha_c, data_c[length(data_c$Time),-2])
  
  
  # repeat to run the function until alpha = 3
  cnt <- cnt+1
  if(cnt == 152) {
    break
  }
}



# plot full vs quasi against alpha ----------------------------------------
library(ggplot2)
# put plots in the same page
library(ggpubr) 
# to access break formatting functions
library(scales) 

library(tidyr)
# get data frame ready for ggplot 
data_long_v <- gather(df_alpha_v,variable,value,2:5,factor_key=TRUE)
data_long_c <- gather(df_alpha_c,variable,value,2:5,factor_key=TRUE)



mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        legend.title = element_blank()) 



# plot equilibira of full model and quasi model under vaginal scheme 
plot_v <- ggplot(data = data_long_v, aes(x = alpha, y = value, color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("solid","solid", "dotted","dotdash"),
                        labels = c('B2_full','B3_full','B2_quasi','B3_quasi')) +
  scale_color_manual(values = c("darkolivegreen2","darkolivegreen4",
                                "brown","brown1"),
                     labels= c('B2_full','B3_full','B2_quasi','B3_quasi')) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  mytheme +
  labs( x = "Alpha", y = "Population at equilibrium",title ="Vaginal")


# plot equilibria of full model and quasi model under c-section scheme 
plot_c <- ggplot(data = data_long_c, aes(x = alpha, y = value, color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("solid","solid", "dotted","dotdash"),
                        labels = c('B2_full','B3_full','B2_quasi','B3_quasi')) +
  scale_color_manual(values = c("darkolivegreen2","darkolivegreen4",
                                "brown","brown1"),
                     labels= c('B2_full','B3_full','B2_quasi','B3_quasi')) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  mytheme +
  labs( x = "Alpha", y = "Population at equilibrium",title ="Caesarean")

show(plot_c)

# put two plot together 
full_quasi_alpha <- ggarrange(plot_v, plot_c, 
                                labels = c("A", "B"), nrow = 1, ncol = 2,
                                common.legend = TRUE, legend = "bottom")

show(full_quasi_alpha)

ggsave(file = output_plot_alpha, width = 17, height = 8.5, units = "cm")



# effect of h -----------

# parameter for the function Z(t)
w <- 0.014


# growth rate
r <- 1

# effect of species interaction 
alpha <- 2
alpha_c <- 1.7

# carrying capacity
k <- 1000

# input of bacteria 
f_2 <- 30/k; f_3 <- 50/k; f_z<- 10/k


# initial value of h
h <- 0

# loop over a series of h values 
# make empty dataframes 
df_h_v <- data.frame()
df_h_c <- data.frame()


# count iteration 
cnt <- 0
repeat {
  

  print(paste0("h = ", h))
  
  # parameters used in the full model
  P <- c(f_2 = f_2, f_3 = f_3, f_z = f_z,
         r = r, alpha =alpha, k=k,
         alpha_c=alpha_c) 
  
  # parameters used in the approximate model
  p <- c(f_2 = f_2, f_3 = f_3, r = r, alpha =alpha, k=k) 
  
  
  # vaginal birth, full model
  Y0_vaginal <- c(B_1 = 50/k,B_2 = 50/k,B_3 = 1/k) 
  
  # C-section, full model 
  Y0_csection <- c(B_1 = 1/k,B_2 = 1/k,B_3 = 50/k) 
  
  # vaginal birth, quasi model
  y0_vaginal <- c(B_2 = 50/k,B_3 = 1/k) 
  
  # C-section, quasi model 
  y0_csection <- c(B_2 = 1/k,B_3 = 50/k) 
  
  # times
  times <- seq(0,3000,1)
  
  library(deSolve)
  # solve full model with parameter set P
  data_full_vaginal <- data.frame(ode(y = Y0_vaginal, times, full_model, P))
  # add coloumn names 
  names(data_full_vaginal) <- c('Time','B1_full_v','B2_full_v','B3_full_v')
  
  data_full_csection <- data.frame(ode(y = Y0_csection, times, full_model, P))
  names(data_full_csection) <- c('Time','B1_full_c','B2_full_c','B3_full_c')
  
  # solve quasi model with parameter set p
  data_quasi_vaginal <- data.frame(ode(y = y0_vaginal, times, quasi_model, p))
  names(data_quasi_vaginal) <- c('Time','B2_quasi_v','B3_quasi_v')
  
  data_quasi_csection <- data.frame(ode(y = y0_csection, times, quasi_model, p))
  names(data_quasi_csection) <- c('Time','B2_quasi_c','B3_quasi_c')
  
  # join the equilibria (last rows) of full and quasi model 
  data_v <- merge(data_full_vaginal,data_quasi_vaginal)
  data_c <- merge(data_full_csection,data_quasi_csection)
  
  # a new coloumn of h values 
  data_v$h <- h
  data_c$h <- h
  
  # add the last row of output to df 
  df_h_v <- rbind(df_h_v, data_v[length(data_v$Time),-2])
  df_h_c <- rbind(df_h_c, data_c[length(data_c$Time),-2])
  
  # make a new h for the next iteration
  h = h + 10
  
  # repeat to run the function until h=1500
  cnt <- cnt+1
  if(cnt == 151) {
    break
  }
}





# plot full vs quasi against h ---------------
library(ggplot2)
# put plots in the same page
library(ggpubr) 
# to access break formatting functions
library(scales) 

library(tidyr)

# load data 
# load("full_quasi_h_csection.rData")
# load("full_quasi_h_vaginal.rData")


# get data frame ready for ggplot 
data_long_v <- gather(df_h_v,variable,value,2:5,factor_key=TRUE)
data_long_c <- gather(df_h_c,variable,value,2:5,factor_key=TRUE)


mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        legend.title = element_blank()) 



# plot equilibira of full model and quasi model under vaginal scheme 
plot_v <- ggplot(data = data_long_v, aes(x = h, y = value, color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("solid","solid", "dotted","dotdash"),
                        labels = c('B2_full','B3_full','B2_quasi','B3_quasi')) +
  scale_color_manual(values = c("darkolivegreen2","darkolivegreen4",
                                "brown","brown1"),
                     labels= c('B2_full','B3_full','B2_quasi','B3_quasi')) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  mytheme +
  labs( x = "h (days)", y = "Population at equilibrium",title ="Vaginal")


# plot equilibria of full model and quasi model under c-section scheme 
plot_c <- ggplot(data = data_long_c, aes(x = h, y = value, color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("solid","solid", "dotted","dotdash"),
                        labels = c('B2_full','B3_full','B2_quasi','B3_quasi')) +
  scale_color_manual(values = c("darkolivegreen2","darkolivegreen4",
                                "brown","brown1"),
                     labels= c('B2_full','B3_full','B2_quasi','B3_quasi')) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  mytheme +
  labs( x = "h (days)", y = "Population at equilibrium",title ="Caesarean")

show(plot_c)

# put two plot together 
full_quasi_h <- ggarrange(plot_v, plot_c, 
                              labels = c("A", "B"), nrow = 1, ncol = 2,
                              common.legend = TRUE, legend = "bottom")

show(full_quasi_h)

ggsave(file = output_plot_h, width = 17, height = 8.5, units = "cm")

