
# plot effect of initial condition for competition model  ----------------------------------------

library(ggplot2)

# set my theme
mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") 


# The effect of a range of initial conditions in the competition model 
library(tidyr)
# to access break formatting functions
library(scales) 

load("initial conditions.rdata")

df_initial$new <- (df_initial$B_2_init+df_initial$B_1_init)/df_initial$B_3_init
colnames(df_initial)[colnames(df_initial)=="new"] <- "ratio"
df_long <- gather(df_initial,Variables,values,B_2:B_3,factor_key=TRUE)

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

ggsave(file="initial_conditions_competition_model.pdf",width = 12, height = 8.5, units = "cm")




# Plot effect of breast milk in the extended model -----------------------------------------------
library(tidyr)
# to access break formatting functions
library(scales) 
library(ggplot2)

# set my theme
mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") 

# plot f_z
load("f_z vaginal.rdata")
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


load("f_z c-section.rdata")
df_long <- gather(df_fz,Variables,Values,B_2:M,factor_key=TRUE)
#source("myTheme.R")
library(ggplot2)
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
load("h_vaginal.rData")
df_long <- gather(df_h,Variables,Values,B_2:M,factor_key=TRUE)
library(ggplot2)
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

load("h_c-section.rData")
df_long <- gather(df_h,Variables,Values,B_2:M,factor_key=TRUE)
library(ggplot2)
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

# put four plot together 
library(ggpubr)

f_z_h_compareBirthMode <- ggarrange(f_z_vaginal, f_z_c_section,h_vaginal, h_c_section, 
                                labels = c("A", "B","C","D"), nrow = 2, ncol = 2)

show(f_z_h_compareBirthMode)
ggsave(file="f_z_h_compareBirthMode.pdf",width = 17, height = 17, units = "cm")



# plot nullclines of B2 and B3 for competition model --------------------------------------------------------
library(ggplot2)
library(png)
library(grid)
library(cowplot)

img1 <- readPNG("nullcline_1.png")
img2 <- readPNG("nullcline_2.png")
g1 <- rasterGrob(img1, interpolate=TRUE)
g2 <- rasterGrob(img2, interpolate=TRUE)

df <- data.frame(x=1:10,y=1:10)

p1 <- ggplot(df) + annotation_custom(g1, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_minimal()
p2 <- ggplot(df) + annotation_custom(g2, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_minimal()

pdf("nullclines.pdf", width = 17, height = 8.5)

plot_grid(p1,p2, labels = "AUTO",label_size = 24, scale = 0.9)
dev.off()



# plot longitudinal dynamics of competition model ----------------------------------------------

# set my theme
mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") 

library(tidyr)
# to access break formatting functions
library(scales) 
library(ggplot2)

load("dynamics_competition_vaginal.rdata")

dynamics_competition_vaginal <- ggplot(data = bacteria_melt, aes(x = time, y = value,
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

load("dynamics_competition_c-section.rdata")

dynamics_competition_c_section <- ggplot(data = bacteria_melt, aes(x = time, y = value,
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

library(ggpubr)

dynamics_competition <- ggarrange(dynamics_competition_vaginal, dynamics_competition_c_section, 
                                    labels = c("A", "B"), nrow = 1, ncol = 2)
show(dynamics_competition)

ggsave("dynamics_competition.pdf",width = 17, height = 8.5, units = "cm")





# plot proportion of milk in infant diet Z(t) ------------------------------------------------------------------
# set my theme
library(ggplot2)
mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.text = element_text (size = 12),
        legend.title = element_blank())
# competition model 
library(tidyr)
# to access break formatting functions
library(scales) 

w <- 0.014; h <- 500
t <-  seq(0,1000,1)
Z <- 1/(1 + exp(w*(t - h))) 

z.data <- data.frame(Z,t)

library(ggplot2)
z.plot <- ggplot(data = z.data, aes(x = t, y = Z)) +
  geom_line(size = 1, color = "steelblue") +
  mytheme + 
  labs( x = "Time (days)", y = "Proportion of milk in diet (Z)")

show(z.plot)

ggsave(file="z.pdf",width = 8.5, height = 8.5, units = "cm")




# plot effect of breast milk f_z for the competition Model  --------------------------------

library(ggplot2)

# set my theme
mytheme <- theme_bw() + 
  theme(panel.grid.minor =  element_blank(),
        plot.title = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") 

# Plot  f_z 
library(tidyr)
# to access break formatting functions 
library(scales) 

load("f_z_vaginal_without_M.rdata")
df_long <- gather(df_fz,Variables,Values,B_2:B_3,factor_key=TRUE)
library(ggplot2)
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


load("f_z_c-section_without_M.rdata")
df_long <- gather(df_fz,Variables,Values,B_2:B_3,factor_key=TRUE)

library(ggplot2)
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

# plot f_z
# combine plots
library(ggpubr)
f_z <- ggarrange(f_z_vaginal, f_z_c_section, 
                  labels = c("A", "B"), nrow = 1, ncol = 2)
show(f_z)

ggsave("Bifidobacteria_input_competition_model.pdf",width = 17, height = 8.5, units = "cm")





# plot the effect of feeding practice and environmental supply for extended model -----------------------------------------------
# heatmap

library(ggplot2)


load("data_pspace_fz_h_c-section.rData")

output_pSpace$f2 <- factor(output_pSpace$f2, labels=c("f[2]: 0.001", "f[2]: 0.01", "f[2]: 0.02",
                                                      "f[2]: 0.03","f[2]: 0.05"))

output_pSpace$f3 <- factor(output_pSpace$f3, labels=c("f[3]: 0.001", "f[3]: 0.01", "f[3]: 0.02",
                                                      "f[3]: 0.03","f[3]: 0.05"))


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


ggsave(file="pSpace_f_z_c-section.pdf",height = 14,width = 17,units = "cm")



