##Input data:
# Par string with parameter variables
# This function uses Par[9] for karstic non-linear flow (Karst in Wendt et al. 2025) and Par[10] for the discharge-outflow parameter

# soilmoisture is a dataframe from your soil moisture module

#D_GW is your groundwater demand, which can be static / dynamic or one number, as is here.

#GSini is your initial conditions
#run by GW_karst_Baseline with the spinup climate dataset


GW_karst_Baseline<- function(par,soilmoisture, D_GW, GSini){
  #create new dataframe o
  o= data.frame(matrix(nrow=length(soilmoisture$Rch), ncol=9, dimnames=list(NULL, c('Rch','Qb','Qt','Qs','GSimp','GS','Res','Qimp','Qout'))))
  #GW storage first to generate baseflow
  o$Rch <- soilmoisture$Rch
  o$Qb <- soilmoisture$Qb
  #initial storage: 
  o$GSimp[1] <- 0
  o$GS[1] <- GSini
  
#loop for time steps
    for(t in 1:(length(o$Rch)-1)){
    o$Qt[t] =par[9]*o$GS[t]^par[10] #Baseflow
    o$Qs[t] = o$Qb[t]+o$Qt[t] # final end term summing runoff and baseflow
    o$GS[t+1] = o$GS[t]+o$Rch[t]-o$Qt[t]- D_GW #groundwater storage
    o$GSimp[t+1] = ifelse(o$GS[t+1]>=0,0,-o$GS[t+1]) #any imported groundwater storage
    o$GS[t+1] = o$GS[t+1] + o$GSimp[t+1] # new storage levels for next time step
  }
  return(o)
}
