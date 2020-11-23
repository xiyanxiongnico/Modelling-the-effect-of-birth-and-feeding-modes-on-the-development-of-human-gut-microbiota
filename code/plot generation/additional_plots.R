# This file contains scripts used to generate plots for the competition model and the extended model. 

# set directories for plots 
output_dynamics <- "additional_plots/dynamics_competition_alpha<1.pdf"
output_feeding_extendedModel_equalClearance <- "additional_plots/f_z_h_BirthMode_extendedModel_equalClearance.pdf"
output_alpha_c <- "additional_plots/alpha_c_CompetitionModel.pdf"
output_dynamics_Mlinear <- "additional_plots/dynamics_Mlinear.pdf"
output_feeding_Mlinear <- "additional_plots/f_z_h_BirthMode_Mlinear.pdf"
output_heatmap <- "additional_plots/pSpace_gamma_mu_Mlinear.pdf"


# library for plot
library(ggplot2)
# The effect of a range of initial conditions in the competition model 
library(tidyr)
# to access break formatting functions
library(scales) 
# put multiple plots in the same panel 
library(ggpubr)

# set my theme for plot 
mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") 


# dynamics of competition model with alpha=0.7  ---------

# load data
load("../data generation/additional_data/dynamics_competition_vaginal_alpha=0.7.rData")

# make plot 
dynamics_competition_vaginal <- ggplot(data = bacteria_melt_vaginal, aes(x = time, y = value,
                                                                         color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("twodash", "solid", "dashed"),
                        labels=c(expression(B[1]), expression(B[2]),expression(B[3]))) +
  scale_color_manual(values = c("skyblue2","seagreen3","violetred2"),
                     labels=c(expression(B[1]), expression(B[2]),expression(B[3]))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits = c(5e-4,1.5)) +
  mytheme +
  labs( x = "Time (days)", y = "Population density",
        title = "Vaginal") +
  annotate("text", x = 500, y = 0.005, label = "HMO-consuming Bifidobacteria B1",
           color = "skyblue2", size = 2.8) +
  annotate("text", x = 500, y = 0.4, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.9, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 


show(dynamics_competition_vaginal)

# load data 
load("../data generation/additional_data/dynamics_competition_c-section_alpha=0.7.rData")

# make plot 
dynamics_competition_c_section <- ggplot(data = bacteria_melt_csection, aes(x = time, y = value,
                                                                            color = variable)) + 
  geom_line(size = 0.8, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("twodash", "solid","dashed"),
                        labels=c(expression(B[1]), expression(B[2]),expression(B[3]))) +
  scale_color_manual(values = c("skyblue2","seagreen3","violetred2"),
                     labels=c(expression(B[1]), expression(B[2]),expression(B[3]))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits = c(5e-4,1.5)) +
  mytheme + 
  labs( x = "Time (days)", y = "Population density",
        title = "Caesarean") +
  annotate("text", x = 500, y = 0.005, label = "HMO-consuming Bifidobacteria B1",
           color = "skyblue2", size = 2.8) +
  annotate("text", x = 500, y = 0.4, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.9, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 


show(dynamics_competition_c_section)

# put plots in the same panel
dynamics_competition <- ggarrange(dynamics_competition_vaginal, dynamics_competition_c_section, 
                                  labels = c("A", "B"), nrow = 1, ncol = 2)
show(dynamics_competition)

# save the plot 
ggsave(file = output_dynamics, width = 17, height = 8.5, units = "cm")




# effect of milk in extended model when mu_b=mu_c (the same immune clearance for bifidobacteria and commensal)----


# load data 
load("../data generation/additional_data/f_z vaginal_equalClearance.rdata")


# plot f_z
df_long <- gather(df_fz,Variables,Values,B_2:M,factor_key=TRUE)
#source("myTheme.R")
f_z_vaginal <-  ggplot(data = df_long, aes(x = f_z, y = Values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Population at equilibrium",
        title ="Vaginal") + 
  annotate("text", x = 0.0003, y = 0.08, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.0002, y = 0.5, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 0.00005, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 


show(f_z_vaginal)

# load data 
load("../data generation/additional_data/f_z c-section_equalClearance.rdata")


df_long <- gather(df_fz,Variables,Values,B_2:M,factor_key=TRUE)

f_z_c_section <-  ggplot(data = df_long, aes(x = f_z, y = Values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Population at equilibrium",
        title ="Caesarean") + 
  annotate("text", x = 0.0003, y = 0.08, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.0002, y = 0.5, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 0.00005, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 

show(f_z_c_section)



# plot h

#load data 
load("../data generation/additional_data/h_vaginal_equalClearance.rdata")


df_long <- gather(df_h,Variables,Values,B_2:M,factor_key=TRUE)

h_vaginal <-  ggplot(data = df_long, aes(x = h, y = Values)) + 
  geom_line(size = 1,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid", "dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = "Half-life of milk, h (Days)", y = "Population at equilibrium") + 
  annotate("text", x = 500, y = 0.08, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.5, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 500, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 


show(h_vaginal)


# load data 
load("../data generation/additional_data/h_c-section_equalClearance.rdata")


df_long <- gather(df_h,Variables,Values,B_2:M,factor_key=TRUE)

h_c_section <-  ggplot(data = df_long, aes(x = h, y = Values)) + 
  geom_line(size = 1,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid", "dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = "Half-life of milk, h (Days)", y = "Population at equilibrium") + 
  annotate("text", x = 500, y = 0.08, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.5, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 500, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 

show(h_c_section)



f_z_h_compareBirthMode <- ggarrange(f_z_vaginal, f_z_c_section,h_vaginal, h_c_section, 
                                    labels = c("A", "B","C","D"), nrow = 2, ncol = 2)

show(f_z_h_compareBirthMode)

ggsave(file = output_feeding_extendedModel_equalClearance, width = 17, height = 17, units = "cm")


# dynamics of extended model with M linear combination   ---------

# load data
load("../data generation/additional_data/dynamics_LinearCombinationMmodel_c-section.rData")

# make plot
dynamics_Mlinear_csection <-  ggplot(data = bacteria_melt_csection, aes(x = time, y = value,color = variable)) + 
  geom_line(size = 1, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("twodash", "solid","dashed","dotted"),
                        labels=c(expression(B[1]), expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("skyblue2","seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[1]), expression(B[2]),expression(B[3]), "M")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits = c(5e-4,1.1)) +
  mytheme + 
  labs( x = "Time (days)", y = "Population density",
        title = "Cesarean") +
  annotate("text", x = 600, y = 0.005, label = "HMO-consuming Bifidobacteria B1",
           color = "skyblue2", size = 2.8) +
  annotate("text", x = 600, y = 0.2, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 600, y = 0.02, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 600, y = 0.1, label = "Immune factor M",color = "tomato4",
           size = 2.8) 



# load data
load("../data generation/additional_data/dynamics_LinearCombinationMmodel_vaginal.rData")

# make plot
dynamics_Mlinear_vaginal <-  ggplot(data = bacteria_melt_vaginal, aes(x = time, y = value,color = variable)) + 
  geom_line(size = 1, aes(linetype=variable)) + 
  scale_linetype_manual(values=c("twodash", "solid","dashed","dotted"),
                        labels=c(expression(B[1]), expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("skyblue2","seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[1]), expression(B[2]),expression(B[3]), "M")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits = c(5e-4,1.1)) +
  mytheme + 
  labs( x = "Time (days)", y = "Population density",
        title = "Vaginal") +
  annotate("text", x = 600, y = 0.005, label = "HMO-consuming Bifidobacteria B1",
           color = "skyblue2", size = 2.8) +
  annotate("text", x = 600, y = 0.2, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 600, y = 0.02, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 600, y = 0.1, label = "Immune factor M",color = "tomato4",
           size = 2.8) 


# put plots in the same panel
dynamics_Mlinear <- ggarrange(dynamics_Mlinear_vaginal, dynamics_Mlinear_csection, 
                                  labels = c("A", "B"), nrow = 1, ncol = 2)
show(dynamics_Mlinear)

# save the plot 
ggsave(file = output_dynamics_Mlinear, width = 17, height = 8.5, units = "cm")



# effect of cross-feeding alpha_c on equilibria of competition model-------


# load data
load("../data generation/additional_data/alpha_c_vaginal_competition.rData")

# make plot
df_long_vaginal <- gather(df_alpha_c_vaginal,Variables,values,ratio,factor_key=TRUE)

alpha_c_vaginal <-  ggplot(data = df_long_vaginal, aes(x = alpha_c, y = values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  # scale_linetype_manual(values=c("solid","dashed"),
  #                       labels=c(expression(B[2]),expression(B[3]))) +
  # scale_color_manual(values = c("seagreen3","violetred2"),
  #                    labels=c(expression(B[2]),expression(B[3]))) + 
  mytheme + 
  labs( x = bquote("Level of cross feeding, " ~ alpha[c]), y = "B2/B3 at equilibrium",
        title = "Vaginal") + 
  ylim(0, 20)

show(alpha_c_vaginal)


# load data
load("../data generation/additional_data/alpha_c_c-section_competition.rData")

# make plot
df_long_csection <- gather(df_alpha_c_csection,Variables,values,ratio,factor_key=TRUE)

alpha_c_csection <-  ggplot(data = df_long_csection, aes(x = alpha_c, y = values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  # scale_linetype_manual(values=c("solid","dashed"),
  #                       labels=c(expression(B[2]),expression(B[3]))) +
  # scale_color_manual(values = c("seagreen3","violetred2"),
  #                    labels=c(expression(B[2]),expression(B[3]))) + 
  mytheme + 
  labs( x = bquote("Level of cross feeding, " ~ alpha[c]), y = "B2/B3 at equilibrium",
        title = "Cesarean") + 
  ylim(0, 20)

show(alpha_c_csection)



# put plots in the same panel
alpha_c <- ggarrange(alpha_c_vaginal, alpha_c_csection, 
                                  labels = c("A", "B"), nrow = 1, ncol = 2)
show(alpha_c)

# save the plot 
ggsave(file = output_alpha_c, width = 17, height = 8.5, units = "cm")












# effect of feeding practices in M linear combination model--------------------------
# load data 
load("../data generation/additional_data/f_z_Mlinear_vaginal.rdata")


# plot f_z
df_long <- gather(df_fz_vaginal,Variables,Values,B_2:M,factor_key=TRUE)
#source("myTheme.R")
f_z_vaginal <-  ggplot(data = df_long, aes(x = f_z, y = Values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Population at equilibrium",
        title ="Vaginal") + 
  annotate("text", x = 0.0003, y = 0.6, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.0002, y = 0.08, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 0.00005, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 


show(f_z_vaginal)

# load data 
load("../data generation/additional_data/f_z_Mlinear_c-section.rdata")


df_long <- gather(df_fz_csection,Variables,Values,B_2:M,factor_key=TRUE)

f_z_c_section <-  ggplot(data = df_long, aes(x = f_z, y = Values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Population at equilibrium",
        title ="Caesarean") + 
  annotate("text", x = 0.0003, y = 0.08, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.0002, y = 0.5, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 0.00005, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 

show(f_z_c_section)



# plot h

#load data 
load("../data generation/additional_data/h_Mlinear_vaginal.rdata")


df_long <- gather(df_h_vaginal,Variables,Values,B_2:M,factor_key=TRUE)

h_vaginal <-  ggplot(data = df_long, aes(x = h, y = Values)) + 
  geom_line(size = 1,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid", "dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = "Half-life of milk, h (Days)", y = "Population at equilibrium") + 
  annotate("text", x = 600, y = 0.4, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.08, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 600, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 


show(h_vaginal)


# load data 
load("../data generation/additional_data/h_Mlinear_c-section.rdata")


df_long <- gather(df_h_csection,Variables,Values,B_2:M,factor_key=TRUE)

h_c_section <-  ggplot(data = df_long, aes(x = h, y = Values)) + 
  geom_line(size = 1,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid", "dashed","dotted"),
                        labels=c(expression(B[2]),expression(B[3]), "M")) +
  scale_color_manual(values = c("seagreen3","violetred2","tomato4"),
                     labels=c(expression(B[2]),expression(B[3]), "M")) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = "Half-life of milk, h (Days)", y = "Population at equilibrium") + 
  annotate("text", x = 800, y = 0.4, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 700, y = 0.07, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 600, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 

show(h_c_section)



f_z_h_compareBirthMode <- ggarrange(f_z_vaginal, f_z_c_section,h_vaginal, h_c_section, 
                                    labels = c("A", "B","C","D"), nrow = 2, ncol = 2)

show(f_z_h_compareBirthMode)
ggsave(file = output_feeding_Mlinear, width = 17, height = 17, units = "cm")












# effect of gamma and mu on M linear combination model --------
# heatmap

# plot a heatmap for csection birth 
# load data 
load("../data generation/additional_data/pspace_gamma_mu_Mlinear.rData")

output_pSpace$gamma <- factor(output_pSpace$gamma, labels=c("gamma: 0.001","gamma: 0.003",
                                                      "gamma: 0.005","gamma:000.7", "gamma: 0.01"))

output_pSpace$mu <- factor(output_pSpace$mu, labels=c("mu: 0.001","mu: 0.003",
                                                      "mu: 0.005","mu: 0.007","mu: 0.01"))


ggplot(output_pSpace, aes(f2, f3, fill= Ratio)) + geom_tile()+
  facet_grid(mu ~ gamma,labeller = label_parsed) + 
  scale_fill_gradient2(low = muted("blue"),mid = "white", high = muted("red"),midpoint = 0)+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 9),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        strip.text = element_text(
          size = 12, color = "black"),
        strip.background = element_rect(fill="white"))  + 
  labs( x = bquote("Bifidobacteria from environment, " ~ f[2]), y = ("Commensal, " ~ f[3]), fill = "Log(B2/B3)") 


ggsave(file = output_heatmap, height = 14,width = 17,units = "cm")


# plot a heatmap for vaginal birth 

# load data 
load("../data generation/additional_data/pspace_gamma_mu_Mlinear_vaginal.rData")


output_pSpace$gamma <- factor(output_pSpace$gamma, labels=c("gamma: 5e-05", "gamma: 1e-04", "gamma: 5e-04",
                                                            "gamma: 0.001","gamma: 0.005"))

output_pSpace$mu <- factor(output_pSpace$mu, labels=c("mu: 5e-05", "mu: 1e-04", "mu: 5e-04",
                                                      "mu: 0.001","mu: 0.005"))

ggplot(output_pSpace, aes(f2, f3, fill= Ratio)) + geom_tile()+
  facet_grid(mu ~ gamma,labeller = label_parsed) + 
  scale_fill_gradient2(low = muted("blue"),mid = "white", high = muted("red"),midpoint = 0)+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 9),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        strip.text = element_text(
          size = 12, color = "black"),
        strip.background = element_rect(fill="white"))  + 
  labs( x = bquote("Bifidobacteria from environment, " ~ f[2]), y = ("Commensal, " ~ f[3]), fill = "Log(B2/B3)") 



# plot a heatmap where only gamma and mu changes 

# set directory 
output_heatmap <- "additional_plots/pSpace_onlyGammaMu_Mlinear.pdf"


# load data 
load("../data generation/additional_data/pspace_onlyGammaMu_Mlinear.rData")


# output_pSpace$gamma <- factor(output_pSpace$gamma, labels=c("gamma: 5e-05", "gamma: 1e-04", "gamma: 5e-04",
#                                                             "gamma: 0.001","gamma: 0.005"))
# 
# output_pSpace$mu <- factor(output_pSpace$mu, labels=c("mu: 5e-05", "mu: 1e-04", "mu: 5e-04",
#                                                       "mu: 0.001","mu: 0.005"))

ggplot(output_pSpace, aes(mu, gamma, fill= Ratio)) + geom_tile() + 
  scale_fill_gradient2(low = muted("blue"),mid = "white", high = muted("red"),midpoint = 0)+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 9),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        strip.text = element_text(
          size = 12, color = "black"),
        strip.background = element_rect(fill="white"))  + 
  labs( x = bquote("Growth of M, " ~ gamma), y = ("Damping of M, " ~ mu), fill = "Log(B2/B3)") 

ggsave(file = output_heatmap, height = 14,width = 17,units = "cm")

