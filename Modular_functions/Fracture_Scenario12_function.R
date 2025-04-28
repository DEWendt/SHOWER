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

#This GW_frac function is for scenarios 1 and 2 that have trigger-level drought management practises (DMP)
#Water demand (D_GW) is adjusted depending on the trigger levels DMP1-3 that relate to either SPI or baseline GS
#Adjustments for water demand should be listed in D_S[1-3]. See also complete script for scenario 1 and 2

GW_frac_Scenario12<- function(par, soilmoisture,D_GW,D_S,GS1_ini,GS2_ini,SPI,GS_B){
  #create new dataframe o to store variables
  o = data.frame(matrix(nrow=length(soilmoisture$Rch), ncol=16, 
  dimnames=list(NULL,   c("Rch","Qr","Qb","Qs","GS1","GS1imp","GS2","GS2imp","GS","GSimp",'D_GW',"D_gwDMP",'Res','Qimp','Qrel','D_swDMP' ))))
 
  o$Rch <- soilmoisture$Rch         # import recharge from your soil moisture module
  o$Qr <- soilmoisture$Qr           # import runoff from your soilmoisture module
  
  #initial storage: 
  o$GSimp[1] <- 0                     # no groundwater import set
  o$GS1imp[1] <- 0
  o$GS2imp[1] <- 0
  o$GS1[1] <- GS1_ini                 # initial conditions Groundwater balance slow fracture flow
  o$GS2[1] <- GS2_ini                 # initial conditions Groundwater balance faster fracture flow
  o$GS[1] <- GS1_ini + GS2_ini
 
  o$D_gwDMP[1] <- D_GW                 # initial Adjusted GW demand is same as normal groundwater demand

  for(t in 1:(length(o$Rch)-1)){ 
    o$Qb[t] =par[9]*o$GS1[t]+par[10]*o$GS2[t]                             # Generating baseflow (Qb)
    o$Qs[t] = o$Qr[t]+o$Qb[t]                                             # Final term for discharge, sum of runoff (Qr) and baseflow (Qb)
    o$GS1[t+1] = o$GS1[t]+0.5*o$Rch[t]-par[9]*o$GS1[t]-0.5*o$D_gwDMP[t]   # Groundwater balance with outgoing baseflow and water demand, incoming recharge
    o$GS2[t+1] = o$GS2[t]+0.5*o$Rch[t]-par[10]*o$GS2[t]-0.5*o$D_gwDMP[t]
    
    o$GS1imp[t+1] = ifelse(o$GS1[t+1]>=0,0,-o$GS1[t+1])               # In case groundwater storage is below 0 and water is imported 
    o$GS1[t+1] = o$GS1[t+1] + o$GS1imp[t+1]                           
    o$GS2imp[t+1] = ifelse(o$GS2[t+1]>=0,0,-o$GS2[t+1])
    o$GS2[t+1] = o$GS2[t+1] + o$GS2imp[t+1]
    
    o$GS[t+1] = o$GS1[t+1]+o$GS2[t+1]                                 # New storage levels for next time step
    o$GSimp[t+1] = o$GS1imp[t+1]+o$GS2imp[t+1]
    
    #Drought management practices: 
    if(SPI$DMP3[t] == 1 | GS_B$GS_DMP3[t]==1 ){ # most severe reduction in demand / increases in D_GW according to DMP3
      o$D_gwDMP[t+1]= D_GW*(1+D_S[3])           # adjustments following D_S[3]
    }else{
      if(SPI$DMP2[t] == 1 |GS_B$GS_DMP2[t]==1){ # moderate reduction in demand / increases in D_GW according to DMP2
        o$D_gwDMP[t+1]= D_GW*(1+D_S[2])         # adjustments following D_S[3]
      } else{
        if(SPI$DMP1[t] == 1|GS_B$GS_DMP1[t]==1){# small reduction in demand / increase in D_GW according to DMP1
          o$D_gwDMP[t+1]= D_GW*(1+D_S[1])       # adjustments following D_S[3]
        }else{
          o$D_gwDMP[t+1]= D_GW }                # in case trigger levels are not reached
      }}
  }
  return(o)
}