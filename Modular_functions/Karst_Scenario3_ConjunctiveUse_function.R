##Input data:

# par is the string with parameter variables
# This function uses par[9] for karstic non-linear flow (Karst in Wendt et al. 2025) 
# and par[10] for the discharge-outflow parameter

# soilmoisture is a dataframe from your soil moisture module with 
# c('D',"P","PET","ETa","SS","Qr","Rch") as existing columns

# D_GW is your groundwater demand, which can be static / dynamic or one number, as is here.

# GSini are your initial conditions
# run by GW_karst_Baseline with the spinup climate dataset

#This GW_karst function is for the third scenario using conjunctive use. 

GW_karst_S3CU<- function(par,soilmoisture, D_GW,D_SW, GSini,GS_B,ResCAP,Qbeco){
  #create new dataframe o to store variables
  o= data.frame(matrix(nrow=length(soilmoisture$Rch), ncol=14, 
  dimnames=list(NULL, c('Rch','Qr','Qb','QRes','Qs','GSimp','GS','GS_Perc',"D_CUgw",'Res','Res_Perc','Qimp','Qrel','D_CUsw'))))
      #GW storage first to generate baseflow
  
  o$Rch <- soilmoisture$Rch       # import recharge from your soil moisture module
  o$Qr <- soilmoisture$Qr         # import runoff from your soil moisture module
  
  #initial storage: 
  o$GSimp[1] <- 0                     # no groundwater import set
  o$Qimp <- 0                         # no static / built-in surface water import
  o$D_CUgw <- 0                       # divert all demand to Groundwater
  o$D_CUsw <- 0                       # divert all demand to Surface water
  
  o$GS[1] <- GSini                    # initial groundwater after spin up
  o$D_gwDMP <- D_GW                   # initial adjusted GW demand is same as normal groundwater demand
  
  #Conjunctive use storage percentage
  o$GS_Perc[1] <- ifelse(max(GS_B$GS)> 0,o$GS[1]/max(GS_B$GS),0)  #Calculates the relative storage at a time compared to baseline
                                                                  ## adjusted to account for 0 groundwater storage
  o$Res[1]=ResCAP                                                 # Reservoir is full at the start.
                                                                  # Note that ResCAP = Rcap parameter * mean Annual P

  ##adjustment in case the Reservoir capacity is zero (turning off the reservoir unit)
  if(ResCAP==0){o$Res_Perc = 0
  }else{    o$Res_Perc[1] <- o$Res[1]/ResCAP}
  
  
  for(t in 1:(length(o$Rch)-1)){ 
    o$GS_Perc[t] <- ifelse(max(GS_B$GS)> 0,o$GS[1]/max(GS_B$GS),0)    # determine the relative storage levels for GW
    
    if(ResCAP==0){                                                    ##Accounting for no reservoir in o$Res 
      o$Res_Perc = 0
    }else{    o$Res_Perc[t] <- o$Res[t]/ResCAP}
    
    #conjunctive use dividing water demand 
    if(o$GS_Perc[t] >= o$Res_Perc[t]){
      o$D_CUgw[t] =D_GW + D_SW                                        #if GW storage > SW storage: all water demand met by GW
      
    }else{     
      o$D_CUsw[t] = D_GW + D_SW                                       #if SW storage > GW storage: all water demand met by SW
    }

    #Determine groundwater storage with new water demand $D_CUgw 
    o$Qb[t] =par[9]*o$GS[t]^par[10]                               # Generating baseflow (Qb)
    o$QRes[t] <-ifelse(o$Qb[t]-Qbeco>0, o$Qb[t]-Qbeco, 0)         # Remainder of baseflow (Qb - Qbeco)to be routed to reservoir as QRes
    o$Qs[t] = o$Qr[t]+o$Qb[t]                                     # Final term for discharge, sum of runoff (Qr) and baseflow (Qb)
    o$GS[t+1] = o$GS[t]+o$Rch[t]-o$Qb[t]- o$D_CUgw[t]             # Groundwater balance with outgoing baseflow and ADJUSTED water demand, incoming recharge
    o$GSimp[t+1] = ifelse(o$GS[t+1]>=0,0,-o$GS[t+1])              # In case groundwater storage is below 0 and water is imported 
    o$GS[t+1] = o$GS[t+1] + o$GSimp[t+1]                          # New storage levels for next time step
    
    #Determine reservoir storage with new water demand $D_CUsw
    o$Res[t+1]=o$Res[t]-o$D_CUsw[t]+o$Qr[t]+o$QRes[t]+o$Qimp[t]   # Reservoir storage with incoming Qr, QRes, Imported SW, and outgoing D_CUsw
      if(o$Res[t+1] < ResCAP){                                    # If reservoir storage is less than capacity > no release flow (Qrel)
      o$Qrel[t] =0
      o$Res[t+1] = o$Res[t+1] 
    } else {                                                      # if reservoir capacity is exceeded > Qrel is defined
      o$Qrel[t]= o$Res[t+1] -ResCAP
      o$Res[t+1]=ResCAP
    }
    #re-fill reservoir  
    ## This is standard practise in the UK when reservoirs are reaching storage levels lower than 25% see Wendt et al. (2021) for details
    if(o$Res[t]<=0.25*ResCAP){
      o$Qimp[t+1] =  o$Qimp[t+1] + ifelse(o$Res[t]<=0.25*ResCAP,0.25*ResCAP - o$Res[t],0) # imported water to meet the 25% threshold of reservoir capacity  
      }else{
      o$Qimp[t+1] =0
    } 
  }
  return(o)
}