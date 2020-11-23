# This file contains scripts used to generate plots for the competition model and the extended model. 

# set directories for plots 

output_initial_conditions <- "plots/initial_conditions_competition_model.pdf"
output_feeding_extendedModel <- "plots/f_z_h_compareBirthMode.pdf"
output_nullclines <- "plots/nullclines.pdf"
output_dynamics <- "plots/dynamics_competition.pdf"
output_Z <- "plots/z.pdf"
output_fz_competition <- "plots/Bifidobacteria_input_competition_model.pdf"
output_heatmap <- "plots/pSpace_f_z_c-section.pdf"
output_dynamics_smallAlpha <- "plots/dynamics_competition_alpha<1.pdf"
output_feeding_extendedModel_equalClearance <- "plots/f_z_h_BirthMode_extendedModel_equalClearance.pdf"
output_dynamics_Mlinear <- "plots/dynamics_Mlinear.pdf"
output_gamma_mu_heatmap <- "plots/pSpace_onlyGammaMu_Mlinear.pdf"



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


# plot effect of initial condition for competition model  ----------------------------------------

# load data 

load("../data generation/data/initial conditions.rdata")

# format data frame 
df_initial$new <- (df_initial$B_2_init+df_initial$B_1_init)/df_initial$B_3_init
colnames(df_initial)[colnames(df_initial)=="new"] <- "ratio"
df_long <- gather(df_initial,Variables,values,B_2:B_3,factor_key=TRUE)

# make plot 
initialconditions_plot <-  ggplot(data = df_long, aes(x = ratio, y = values, color = Variables)) + 
  geom_line(size = 0.8,aes(linetype=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed"),
                        labels=c(expression(B[2]),expression(B[3]))) +
  scale_color_manual(values = c("seagreen3","violetred2"),
                     labels=c(expression(B[2]),expression(B[3]))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
 mytheme + 
  labs( x = expression(B[b][0]/B[c][0]), y = "Population at equilibrium") +
  annotate("text", x = 1.3, y = 0.7, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 1.3, y = 0.07, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 

show(initialconditions_plot)

# save plot 
ggsave(file= output_initial_conditions, width = 12, height = 8.5, units = "cm")




# Plot effect of feeding practices in the extended model -----------------------------------------------

# load data 
load("../data generation/data/f_z vaginal.rdata")


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
  annotate("text", x = 0.0003, y = 0.6, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.0002, y = 0.08, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 0.00005, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 
  
  
show(f_z_vaginal)

# load data 
load("../data generation/data/f_z c-section.rdata")


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
load("../data generation/data/h_vaginal.rdata")


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
  annotate("text", x = 600, y = 0.4, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.08, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) +
  annotate("text", x = 600, y = 0.002, label = "Immune factor M",color = "tomato4",
           size = 2.8) 


show(h_vaginal)


# load data 
load("../data generation/data/h_c-section.rdata")


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
ggsave(file = output_feeding_extendedModel, width = 17, height = 17, units = "cm")



# plot nullclines of B2 and B3 for competition model --------------------------------------------------------

library(png)
library(grid)
library(cowplot)

img1 <- readPNG("plots/nullcline_1.png")
img2 <- readPNG("plots/nullcline_2.png")
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)

df <- data.frame(x=1:10,y=1:10)

p1 <- ggplot(df) + annotation_custom(g1, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_minimal()
p2 <- ggplot(df) + annotation_custom(g2, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_minimal()

# the width and height of the graphics region in inches
pdf(output_nullclines, width = 6.7, height = 3.35)

plot_grid(p1,p2, labels = "AUTO",label_size = 24, scale = 0.9)
dev.off()



# plot longitudinal dynamics of competition model ----------------------------------------------

# load data
load("../data generation/data/dynamics_competition_vaginal.rData")

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
  annotate("text", x = 500, y = 0.5, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 700, y = 0.03, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 


show(dynamics_competition_vaginal)

# load data 
load("../data generation/data/dynamics_competition_c-section.rData")

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
  annotate("text", x = 500, y = 0.05, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 500, y = 0.5, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 
  

show(dynamics_competition_c_section)

# put plots in the same panel
dynamics_competition <- ggarrange(dynamics_competition_vaginal, dynamics_competition_c_section, 
                                    labels = c("A", "B"), nrow = 1, ncol = 2)
show(dynamics_competition)

# save the plot 
ggsave(file = output_dynamics, width = 17, height = 8.5, units = "cm")





# plot proportion of milk in infant diet Z(t) ------------------------------------------------------------------
# set my theme
mytheme_z <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        legend.title = element_blank())

# variable values for Z(t) function
w <- 0.014; h <- 500
t <-  seq(0,1000,1)
Z <- 1/(1 + exp(w*(t - h))) 

# make a data frame for plot 
z.data <- data.frame(Z,t)

# plot Z(t)
z.plot <- ggplot(data = z.data, aes(x = t, y = Z)) +
  geom_line(size = 1, color = "steelblue") +
  mytheme_z + 
  labs( x = "Time (days)", y = "Proportion of milk in diet (Z)")

show(z.plot)

# save the plot 
ggsave(file= output_Z, width = 8.5, height = 8.5, units = "cm")




# plot effect of breast milk f_z for the competition Model  --------------------------------

# load data 
load("../data generation/data/f_z_vaginal_without_M.rdata")

# make plot 
df_long <- gather(df_fz_vaginal,Variables,Values,B_2:B_3,factor_key=TRUE)

f_z_vaginal <-  ggplot(data = df_long, aes(x = f_z, y = Values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed"),
                        labels=c(expression(B[2]),expression(B[3]))) +
  scale_color_manual(values = c("seagreen3","violetred2"),
                     labels=c(expression(B[2]),expression(B[3]))) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme + 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Population at equilibrium",
        title = "Vaginal") +
  annotate("text", x = 0.001, y = 0.7, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.001, y = 0.08, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 

show(f_z_vaginal)


# load data 
load("../data generation/data/f_z_c-section_without_M.rdata")

# make plot 
df_long <- gather(df_fz_csection,Variables,Values,B_2:B_3,factor_key=TRUE)

f_z_c_section <-  ggplot(data = df_long, aes(x = f_z, y = Values)) + 
  geom_line(size = 0.8,aes(linetype=Variables,color=Variables)) + 
  scale_linetype_manual(values=c("solid","dashed"),
                        labels=c(expression(B[2]),expression(B[3]))) +
  scale_color_manual(values = c("seagreen3","violetred2"),
                     labels=c(expression(B[2]),expression(B[3]))) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  mytheme+ 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Population at equilibrium",
        title = "Caesarean") +
  annotate("text", x = 0.0006, y = 0.05, label = "Fibre-consuming Bifidobacteria B2",
           color = "seagreen3", size = 2.8) +
  annotate("text", x = 0.001, y = 0.7, label = "Commensal competitor B3",color = "violetred2",
           size = 2.8) 

show(f_z_c_section)


# put plots in the same panel

f_z <- ggarrange(f_z_vaginal, f_z_c_section, 
                  labels = c("A", "B"), nrow = 1, ncol = 2)
show(f_z)

ggsave(file = output_fz_competition, width = 17, height = 8.5, units = "cm")



# plot the effect of feeding practice and environmental supply for extended model -----------------------------------------------
# heatmap

# load data 
load("../data generation/data/data_pspace_fz_h.rData")

output_pSpace$f2 <- factor(output_pSpace$f2, labels=c("f[2]: 0.001", "f[2]: 0.01", "f[2]: 0.02",
                                                      "f[2]: 0.03","f[2]: 0.05"))

output_pSpace$f3 <- factor(output_pSpace$f3, labels=c("f[3]: 0.001", "f[3]: 0.01", "f[3]: 0.02",
                                                      "f[3]: 0.03","f[3]: 0.05"))

# make plot
ggplot(output_pSpace, aes(f_z, h, fill= Ratio)) + geom_tile()+
  facet_grid(f3 ~ f2,labeller = label_parsed) + 
  scale_fill_gradient(low="yellow", high="blue") +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 9),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        strip.text = element_text(
          size = 12, color = "black"),
        strip.background = element_rect(fill="white"))  + 
  labs( x = bquote("Bifidobacteria in milk, " ~ f[z]), y = "Half-life of milk, h (Days)") 


ggsave(file = output_heatmap, height = 14,width = 17,units = "cm")

# plot dynamics of competition model when alpha = 0.7 -----

# load data
load("../data generation/data/dynamics_competition_vaginal_alpha=0.7.rData")

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
load("../data generation/data/dynamics_competition_c-section_alpha=0.7.rData")

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
ggsave(file = output_dynamics_smallAlpha, width = 17, height = 8.5, units = "cm")

# plot the effect of milk in the extended model when mu_b = mu_c equal clearance -------


# load data 
load("../data generation/data/f_z vaginal_equalClearance.rdata")


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
load("../data generation/data/f_z c-section_equalClearance.rdata")


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
load("../data generation/data/h_vaginal_equalClearance.rdata")


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
load("../data generation/data/h_c-section_equalClearance.rdata")


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


# plot dynamics of the extended model where dM/dt = gamma*(B1+B2)-mu*M--------

# load data
load("../data generation/data/dynamics_LinearCombinationMmodel_c-section.rData")

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
load("../data generation/data/dynamics_LinearCombinationMmodel_vaginal.rData")

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


# plot the effect of gamma and mu on extended model where dM/dt=gamma*(B1+B2)-mu*M--------


# load data 
load("../data generation/data/pspace_onlyGammaMu_Mlinear.rData")


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

ggsave(file = output_gamma_mu_heatmap, height = 14,width = 17,units = "cm")



