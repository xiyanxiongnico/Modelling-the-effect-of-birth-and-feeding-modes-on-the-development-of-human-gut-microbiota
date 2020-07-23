
competition<-function(t,y,p){
  B_1 <- y[1] #bifidobacteria capable of HMO metabolism
  B_2 <- y[2] #bifidobacteria capable of plant polysaccharide metabolism 
  B_3 <- y[3] #another commensal group
  M <- y[4] #immune effect 
  
  # proportion of milk in diet as function Z
  Z <- 1/(1 + exp(w*(t - h))) 
  
  with(as.list(p),{
    # the growth rate of B1 related to milk consumption  
    dB_1dt <- r*B_1*(Z - (B_1 + B_2) - alpha*B_3)  + f_z*Z - mu_b*B_1*M
    # competition 
    dB_2dt <- r*B_2*(1- (B_1 + B_2) - alpha*B_3 + alpha_c*B_1) + f_2 - mu_b*B_2*M
    # f3 is the input of B_3 from environment
    dB_3dt <- r*B_3*(1- B_3 - alpha*(B_1 + B_2)) + f_3 - mu_c*B_3*M
    # immune effect 
    dMdt <- gamma*B_1
    
    return(list(c(dB_1dt,dB_2dt,dB_3dt,dMdt)))
  })}
