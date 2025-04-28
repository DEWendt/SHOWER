##Input data:

# par is the string with parameter variables
# This function uses par[9] for fractured slow flow (sF1 in Wendt et al. 2025) 
# and par[10] for quicker fracture flow (sF2)
# the parallel buckets are treated equally in current model setup. 
# Hence recharge is divided equally between the two modules.

# soilmoisture is a dataframe from your soil moisture module with 
# c('D',"P","PET","ETa","SS","Qr","Rch") as existing columns

# D_GW is your groundwater demand, which can be static / dynamic or one number, as is here.

# GSini are your initial conditions
# run by GW_frac_Baseline with the spinup climate dataset

GW_frac_Baseline <- function(par, soilmoisture,D_GW,GS1_ini,GS2_ini){
  #create new dataframe o to store variables
  o = data.frame(matrix(nrow=length(soilmoisture$Rch), 
                        ncol=10, dimnames=list(NULL,
                      c("Rch","Qr","Qb","Qs","GS1","GS1imp","GS2","GS2imp","GS","GSimp"))))
  o$Rch <- soilmoisture$Rch           # import recharge from your soil moisture module
  o$Qr <- soilmoisture$Qr             # import runoff from your soilmoisture module
  
  #initial storage: 
  o$GSimp[1] <- 0
  o$GS1imp[1] <- 0
  o$GS2imp[1] <- 0
  o$GS1[1] <- GS1_ini                 # initial conditions Groundwater balance slow fracture flow
  o$GS2[1] <- GS2_ini                 # initial conditions Groundwater balance faster fracture flow
  o$GS[1] <- GS1_ini + GS2_ini
  
  #loop for time steps
  for(t in 1:(length(o$Rch)-1)){ 
    o$Qb[t] =par[9]*o$GS1[t]+par[10]*o$GS2[t]                         # Generating baseflow (Qb)
    o$Qs[t] = o$Qr[t]+o$Qb[t]                                         # Final term for discharge, sum of runoff (Qr) and baseflow (Qb)
    o$GS1[t+1] = o$GS1[t]+0.5*o$Rch[t]-par[9]*o$GS1[t]-0.5*D_GW       # Groundwater balance with outgoing baseflow and water demand, incoming recharge
    o$GS2[t+1] = o$GS2[t]+0.5*o$Rch[t]-par[10]*o$GS2[t]-0.5*D_GW
    
    o$GS1imp[t+1] = ifelse(o$GS1[t+1]>=0,0,-o$GS1[t+1])               # In case groundwater storage is below 0 and water is imported 
    o$GS1[t+1] = o$GS1[t+1] + o$GS1imp[t+1]                           
    o$GS2imp[t+1] = ifelse(o$GS2[t+1]>=0,0,-o$GS2[t+1])
    o$GS2[t+1] = o$GS2[t+1] + o$GS2imp[t+1]
    
    o$GS[t+1] = o$GS1[t+1]+o$GS2[t+1]                                 # New storage levels for next time step
    o$GSimp[t+1] = o$GS1imp[t+1]+o$GS2imp[t+1]
    
  }
  return( o)  #return the whole dataframe 

  #or change into a specific string using this alternative 
  # return(o$GS)     
}
