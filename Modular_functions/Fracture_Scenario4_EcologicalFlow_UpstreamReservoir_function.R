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
#This GW_frac function is for the fourth (& last) scenario aiming to maintain the ecological minimum flow. 


GW_frac_S4Eco_UpstreamReservoir<- function(par,soilmoisture, D_GW,D_SW, GS1_ini,GS2_ini,ResCAP,Qbeco, SM_res){
  
  #create new dataframe o to store variables
  o= data.frame(matrix(nrow=length(soilmoisture$Rch), ncol=15, 
                       dimnames=list(NULL,c('Rch','Qr','Qb','QRes','Qs',
                                            "GS1","GS1imp","GS2","GS2imp","GS","GSimp",'Res'
                                            ,'Qimp','Qrel','D_SWDMP'))))
  
  o$Rch <- soilmoisture$Rch       # import recharge from your soil moisture module
  o$Qr <- soilmoisture$Qr         # import runoff from your soil moisture module
  
  #initial storage: 
  o$GSimp[1] <- 0                     # no groundwater import set
  o$GS1imp[1] <- 0
  o$GS2imp[1] <- 0
  o$Qimp <- 0                         # no static / built-in surface water import
  o$GS1[1] <- GS1_ini                 # initial conditions Groundwater balance slow fracture flow
  o$GS2[1] <- GS2_ini                 # initial conditions Groundwater balance faster fracture flow
  o$GS[1] <- GS1_ini + GS2_ini
  
  o$D_SWDMP <- 0                      # initial adjusted SW demand is zero 
  
  #Reservoir settings
  o$Res[1]=ResCAP                     # Reservoir is full at the start.
                                      # Note that ResCAP = Rcap parameter * mean Annual P
  
  ##adjustment in case the Reservoir capacity is set to zero (turning off the reservoir unit)
  if(ResCAP ==0){
    D_GW = D_GW +D_SW
    D_SW= 0
  }else{D_GW=D_GW}

  for(t in 1:(length(o$Rch)-1)){

    o$Qt[t] =par[9]*o$GS1[t]+par[10]*o$GS2[t]             # Generating baseflow (Qb)
    #o$QRes[t] <-ifelse(o$Qt[t]-Qeco>0, o$Qt[t]-Qeco, 0)
    #o$Qs[t] = o$Qb[t]+o$Qt[t] # final end term Q
   
    #groundwater loop                                     # If Qb is larger than Qbeco; no interventions happen
    if(o$Qb[t] >= Qbeco){
      o$GS1[t+1] = o$GS1[t]+0.5*o$Rch[t]-par[9]*o$GS1[t]-0.5*D_GW   # Groundwater balance with outgoing baseflow and water demand, incoming recharge
      o$GS2[t+1] = o$GS2[t]+0.5*o$Rch[t]-par[10]*o$GS2[t]-0.5*D_GW
      
      o$GS1imp[t+1] = ifelse(o$GS1[t+1]>=0,0,-o$GS1[t+1])           # In case groundwater storage is below 0 and water is imported 
      o$GS2imp[t+1] = ifelse(o$GS2[t+1]>=0,0,-o$GS2[t+1])
      
      o$GS1[t+1] = o$GS1[t+1] + o$GS1imp[t+1]                       # New storage levels for next time step
      o$GS2[t+1] = o$GS2[t+1] + o$GS2imp[t+1]
      o$GS[t+1] = o$GS1[t+1]+o$GS2[t+1] 
      
      o$D_SWDMP[t]= D_SW                                            # D_SWDMP is set to only SW water demand
      
      }else{                                                        # If Qb is smaller than Qbeco: hands off flow by no GW abstractions
      o$GS1[t+1] = o$GS1[t]+0.5*o$Rch[t]-par[9]*o$GS1[t]            # new Groundwater balance without D_GW 
      o$GS2[t+1] = o$GS2[t]+0.5*o$Rch[t]-par[10]*o$GS2[t] 
      
      o$GS1imp[t+1] = ifelse(o$GS1[t+1]>=0,0,-o$GS1[t+1])           # In case groundwater storage is below 0 and water is imported 
      o$GS2imp[t+1] = ifelse(o$GS2[t+1]>=0,0,-o$GS2[t+1])
       
      o$GS1[t+1] = o$GS1[t+1] + o$GS1imp[t+1]                       # New storage levels for next time step
      o$GS2[t+1] = o$GS2[t+1] + o$GS2imp[t+1]
      o$GS[t+1] = o$GS1[t+1]+o$GS2[t+1] 
      
      o$D_SWDMP[t]=D_SW + D_GW                                      # Additional GW demand to D_SWDMP
                                                                    # note that this column has been defined in the GS_S12 modules
      }
    #Determine reservoir storage with new water demand $D_SWDMP
    if(ResCAP == 0){                                               # If there is no reservoir set (ResCAP== 0), all surface water 
      # o$D_SWDMP[t] will be met by Qimp[t]
      o$Qimp[t]= o$D_SWDMP[t]
      o$Res = 0
      o$Qrel = 0
    }else{
      o$Res[t+1]=o$Res[t]-o$D_SWDMP[t]+SM_res$Qr[t]+SM_res$Rch[t]+o$Qimp[t] # Reservoir storage with incoming Qr, QRes, Imported SW and outgoing D_SWDMP
                                                                  #note the additional source (SM_res) for Qb and Rch. This is the upstream area of the reservoir!  
      if(o$Res[t+1] < ResCAP){                                     # If reservoir storage is less than capacity > release flow (Qrel) is Qbeco
        o$Qrel[t] =Qbeco
        o$Res[t+1] = o$Res[t+1] 
      } else {                                                     # if reservoir capacity is exceeded > Qrel is defined as excess + Qbeco
        o$Qrel[t]= o$Res[t+1] -ResCAP +Qbeco
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