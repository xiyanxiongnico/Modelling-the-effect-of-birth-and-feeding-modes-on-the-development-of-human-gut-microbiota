# This is a function for the competition model, which consists of three ordinary differential equations (one for each bacterial population) and a function representing the level of milk in diet Z(t). 

competition<-function(t,y,p){
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
