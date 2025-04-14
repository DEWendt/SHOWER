##Input data:
# par is the string with parameter variables
# This function uses par[9] for karstic non-linear flow (Karst in Wendt et al. 2025) 
 # and par[10] for the discharge-outflow parameter

# soilmoisture is a dataframe from your soil moisture module with 
# c('D',"P","PET","ETa","SS","Qr","Rch") as existing columns

# D_GW is your groundwater demand, which can be static / dynamic or one number, as is here.

# GSini are your initial conditions
# run by GW_karst_Baseline with the spinup climate dataset


GW_karst_Baseline<- function(par,soilmoisture, D_GW, GSini){
  #create new dataframe o to store variables
  o= data.frame(matrix(nrow=length(soilmoisture$Rch), ncol=9, 
                       dimnames=list(NULL, c('Rch','Qr','Qb','Qs','GSimp','GS','Res','Qimp','Qout'))))

  o$Rch <- soilmoisture$Rch           # import recharge from your soil moisture module
  o$Qr <- soilmoisture$Qr             # import runoff from your soilmoisture module
  
  #initial storage: 
  o$GSimp[1] <- 0
  o$GS[1] <- GSini
  
  #loop for time steps
  for(t in 1:(length(o$Rch)-1)){
    o$Qb[t] =par[9]*o$GS[t]^par[10]                   # Generating baseflow (Qb)
    o$Qs[t] = o$Qr[t]+o$Qb[t]                         # Final term for discharge, sum of runoff (Qr) and baseflow (Qb)
    o$GS[t+1] = o$GS[t]+o$Rch[t]-o$Qb[t]- D_GW        # Groundwater balance with outgoing baseflow and water demand, incoming recharge
    o$GSimp[t+1] = ifelse(o$GS[t+1]>=0,0,-o$GS[t+1])  # In case groundwater storage is below 0 and water is imported 
    o$GS[t+1] = o$GS[t+1] + o$GSimp[t+1]              # New storage levels for next time step
  }
  
  #return the whole dataframe 
  return(o)     

  #or change into a specific string using this alternative 
  # return(o$GS)     
}

