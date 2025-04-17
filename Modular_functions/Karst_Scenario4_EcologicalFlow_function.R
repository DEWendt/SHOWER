##Input data:

# par is the string with parameter variables
# This function uses par[9] for karstic non-linear flow (Karst in Wendt et al. 2025) 
# and par[10] for the discharge-outflow parameter

# soilmoisture is a dataframe from your soil moisture module with 
# c('D',"P","PET","ETa","SS","Qr","Rch") as existing columns

# D_GW is your groundwater demand, which can be static / dynamic or one number, as is here.

# GSini are your initial conditions
# run by GW_karst_Baseline with the spinup climate dataset

#This GW_karst function is for the fourth (& last) scenario aiming to maintain the ecological minimum flow.

GW_karst_S4Eco<- function(par,soilmoisture, D_GW,D_SW, GSini,ResCAP,Qbeco){

  #create new dataframe o to store variables
  o= data.frame(matrix(nrow=length(soilmoisture$Rch), ncol=11, 
                       dimnames=list(NULL, c('Rch','Qr','Qb','QRes','Qs','GSimp','GS','Res','Qimp','Qrel','D_SWDMP'))))
  
  o$Rch <- soilmoisture$Rch       # import recharge from your soil moisture module
  o$Qr <- soilmoisture$Qr         # import runoff from your soil moisture module
  
  #initial storage: 
  o$GSimp[1] <- 0                     # no groundwater import set
  o$Qimp <- 0                         # no static / built-in surface water import
  o$D_SWDMP <- 0                      # initial adjusted SW demand is zero 
  o$GS[1] <- GSini                    # initial groundwater after spin up
  
  #Reservoir settings
  o$Res[1]=ResCAP                          # Reservoir is full at the start.
                                          # Note that ResCAP = Rcap parameter * mean Annual P
  ##adjustment in case the Reservoir capacity is set to zero (turning off the reservoir unit)
    if(ResCAP ==0){
    D_GW = D_GW +D_SW
    D_SW= 0
  }else{D_GW=D_GW}
  
  
  for(t in 1:(length(o$Rch)-1)){ 
    
    o$Qb[t] =par[9]*o$GS[t]^par[10]                       # Generating baseflow (Qb)
    o$Qs[t] = o$Qr[t]+o$Qb[t]                             # Final term for discharge, sum of runoff (Qr) and baseflow (Qb)
    o$QRes[t] <-ifelse(o$Qb[t]-Qbeco>0, o$Qb[t]-Qbeco, 0) # Remainder of baseflow (_Qbeco)to be routed to reservoir (QRes)
    
    #groundwater loop                                     # If Qb is larger than Qbeco; no interventions happen
    if(o$Qb[t] >= Qbeco){
      o$GS[t+1] = o$GS[t]+o$Rch[t]-o$Qb[t]- D_GW          # Groundwater balance with outgoing baseflow and water demand, incoming recharge
      o$GSimp[t+1] = ifelse(o$GS[t+1]>=0,0,-o$GS[t+1])    # In case groundwater storage is below 0 and water is imported 
      o$GS[t+1] = o$GS[t+1] + o$GSimp[t+1]                # New storage levels for next time step
      o$D_SWDMP[t]= D_SW                                  # D_SWDMP is set to only SW water demand
    
      }else{                                              # If Qb is smaller than Qbeco: hands off flow by no GW abstractions
      o$GS[t+1] = o$GS[t]+o$Rch[t]-o$Qb[t]                # new Groundwater balance without D_GW 
      o$GSimp[t+1] = ifelse(o$GS[t+1]>=0,0,-o$GS[t+1])    # In case groundwater storage is below 0 and water is imported 
      o$GS[t+1] = o$GS[t+1] + o$GSimp[t+1]                # New storage levels for next time step
      o$D_SWDMP[t]=D_SW + D_GW                            # Additional GW demand to D_SWDMP
                                                          # note that this column has been defined in the GS_S12 modules
      }
    
    #Determine reservoir storage with new water demand $D_SWDMP
    if(ResCAP == 0){                                               # If there is no reservoir set (ResCAP== 0), all surface water 
                                                                   # o$D_SWDMP[t] will be met by Qimp[t]
      o$Qimp[t]= o$D_SWDMP[t]
      o$Res = 0
      o$Qrel = 0
    }else{
      o$Res[t+1]=o$Res[t]-o$D_SWDMP[t]+o$Qr[t]+o$QRes[t]+o$Qimp[t] # Reservoir storage with incoming Qr, QRes, Imported SW and outgoing D_SWDMP
      if(o$Res[t+1] < ResCAP){                                     # If reservoir storage is less than capacity > no release flow (Qrel)
        o$Qrel[t] =0
        o$Res[t+1] = o$Res[t+1] 
      } else {                                                     # if reservoir capacity is exceeded > Qrel is defined
        o$Qrel[t]= o$Res[t+1] -ResCAP
        o$Res[t+1]=ResCAP
      }
      #re-fill reservoir  
      ## This is standard practise in the UK when reservoirs are reaching storage levels lower than 25% see Wendt et al. (2021) for details
      if(o$Res[t]<=0.25*ResCAP){                  
        o$Qimp[t+1] =  o$Qimp[t+1] + ifelse(o$Res[t]<=0.25*ResCAP,0.25*ResCAP - o$Res[t],0) 
                                                                  # imported water to meet the 25% threshold of reservoir capacity  
      }else{
        o$Qimp[t+1] =0
      }} 
  }
  return(o)
}

