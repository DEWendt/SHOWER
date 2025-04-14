##Input data:

#Precipitation data (P) and Potential Evaporation data are strings with driving P and PET in them.

# D is a as.Date sequence (can be from P or PET)

# par is the string with parameter variables
# This script uses par in string following Table 1 in manuscript:
    #par[1]: Wilting point ranges (WP) [mm]
    #par[2]: Critical moisture content (CR) [mm]
    #par[3]: Field capacity (FC) [mm]
    #par[4]: Unsaturated hydraulic conductivity (Kfc) [mm/d]
    #par[5]: shape parameter (b) [-]

##The HBV-like fixes are to avoid negative ETa and recharge due to strange parameter setup


SoilBalance_HBVAdjustment_CAMELS = function(P,PET,D,par) {

  #create dataframe to storage variables
  o_LT = data.frame(matrix(nrow=length(D), ncol=7, dimnames=list(NULL, c('D',"P","PET","ETa","SS","Qr","Rch"))))
  o_LT$D <- D
  o_LT$P= P 
  #  In case of using HADukP data, refer to P column
 #   o_LT$P= P$HADukP[P$D >= o_LT$D[1] & P$D <= o_LT$D[length(D)]]
  o_LT$PET = PET  
  
  ##spinup
  for(t in 1:(length(D)-1)){ # one less than last time step 
    o_LT$SS[1] = par[3] # initial condition set on Field Capacity
    
    #Following Equation S1-S4 in SI
    if(o_LT$SS[t] >= par[3]){               # if o$SS[t] >= field capacity: Soil becomes saturated
      o_LT$ETa[t] =  o_LT$ETp[t]            # Assumption that ETa is ETp when saturated 
      o_LT$Rch[t] <-  0                     # No infiltration: bucket fills till FC 
      o_LT$Qr[t]  <- o_LT$SS[t] - par[3]    # Generated runoff: water in excess in SS relative to FC 
    
      }else{
      if(o_LT$SS[t] <= par[1]) {            # if Soil is below Wilting Point, the soil is dried out 
        o_LT$ETa[t] =0                      # dry soil; no evaporation
        o_LT$Rch[t]=0                       # no recharge: soil wets up first to CR
        o_LT$Qr[t]=ifelse(o_LT$P[t]<= 2,0,par[6]*o_LT$P[t]) # only through flow
      
        } else{
        if(o_LT$SS[t] <=par[2]){            # if soil is between Wilting point and Critical moisture content 
          o_LT$ETa[t] = o_LT$ETp[t] * (o_LT$SS[t]-par[1])/(par[2]-par[1]) 
                                            #  ETa with correction factor for soil saturation (Equation S1)
          o_LT$ETa[t]<-  o_LT$ETa[t]+ max(o_LT$SS[t] - o_LT$ETa[t] - par[3], 0) + min(o_LT$SS[t] - o_LT$ETa[t], 0) 
                                            ## HBV-like fix to adjust ETa by an amount equal to the possible negative Soil moisture amount 
                                            # or to the possible Soil moisture amount above FC
          o_LT$Rch[t] = 0                   # no recharge
          o_LT$Qr[t] =ifelse(o_LT$P[t]<= 2,0,par[6]*o_LT$P[t]) # only through flow
        
          }else{                            # if o$SS[t] between Critical soil moisture and field capacity > dry and becomes wetter to FC  
          o_LT$ETa[t] = o_LT$ETp[t]         # optimal ETa
          o_LT$Rch[t] = par[4] * ((o_LT$SS[t]-par[2])/(par[3]-par[2]))^par[5] 
                                            # recharge with correction factor for soil saturation (Equation S3)
          o_LT$Rch[t]<- o_LT$Rch[t]+ max(o_LT$SS[t] +o_LT$Pra[t] - o_LT$Rch[t] - par[3], 0) + min(o_LT$SS[t] +o_LT$Pra[t] - o_LT$Rch[t], 0)
                                            ##HBV-like fix to adjust Recharge by an amount equal to the possible negative 
                                              #soil moisture amount or to the possible soil moisture amount above FC
          o_LT$Qr[t] <- 0                   # no through flow
        }
      }
    }
    o_LT$SS[t+1] = o_LT$SS[t] +o_LT$P[t] -o_LT$ETa[t]-o_LT$Rch[t] -o_LT$Qr[t]
  }
  return(o_LT) #return the whole dataset to use for further calculations
}
