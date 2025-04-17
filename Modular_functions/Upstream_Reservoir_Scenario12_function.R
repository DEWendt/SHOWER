##Input data:

# GS is groundwater calculated using the Baseline or Scenarios_12 
# This dataframe has columns [baseline] c('Rch','Qr','Qb','Qs','GSimp','GS','Res','Qimp','Qrel') or
#   [Scenarios12] c('Rch','Qr','Qb','Qs','GSimp','GS',"D_gwDMP",'Res','Qimp','Qrel','D_swDMP'))
# Note that you can use this for any of the groundwater modules!

# ResCAP is the converted reservoir capacity as ResCAP = Rcap parameter (par[7] is Rcap) * mean Annual P

# D_SW is your groundwater demand, which can be static / dynamic or one number, as is here.

#SPI and D_S are determining your scenario response. The trigger levels (3) respond to a SPI level, 
#  which is defined in your main function and imported here.
#D_S indicates what happens at the trigger levels, whether (S1) extra groundwater is used or (S2) water demand is reduced.

Res_S12_Release = function(ResCAP, GS, D_SW,SPI,D_S, Q95){
  if(ResCAP == 0){              # adjustment in case the Reservoir capacity is set to zero (turning off the reservoir unit)
    GS$Qimp <- D_SW
    GS$Qrel <- 0
    GS$Res <- 0
  }else{
    GS$Res[1] = ResCAP          # Reservoir is full at the start.
    GS$Qimp <- 0                # no static / built-in surface water import
    GS$Qrel <- as.numeric(Q95)
    GS$D_swDMP <- D_SW          # initial Adjusted SW demand is same as normal surface demand (assume no thresholds are reached)
    # note that this column has been defined in the GS_S12 modules
    
    #Determine reservoir storage with new water demand D_SWDMP
    for(t in 1:(length(GS$Rch)-1)){
     GS$Res[t+1]=GS$Res[t]-GS$D_swDMP[t] +
       GS$Qr[t]+GS$Qres[t]+GS$Qimp[t]                   # Reservoir storage with incoming Qr, QRes, Imported SW and outgoing D_swDMP
     if(GS$Res[t+1] < ResCAP){                          # If reservoir storage is less than capacity > only Q95 release flow (Qrel)
        GS$Qrel[t] =as.numeric(Q95)
        GS$Res[t+1] = GS$Res[t+1] + GS$Qimp[t+1]
          
      } else {                                          # if reservoir capacity is exceeded > Qrel is defined
        GS$Qrel[t]= GS$Res[t+1] -ResCAP + as.numeric(Q95)
        GS$Res[t+1]=ResCAP
    }
        
     #Drought management practices: 
     if(SPI$DMP3[t] == 1 | GS$Res[t]<=0.25*ResCAP){                # most severe reduction in demand / increases in D_SW according to DMP3
       GS$Qimp[t+1] =  GS$Qimp[t+1] + 
         ifelse(GS$Res[t]<=0.25*ResCAP,0.25*ResCAP - GS$Res[t],0)  # Surface water is imported water to 0.25*ResCAP
       GS$D_swDMP[t+1]= D_SW*(1+D_S[3])                            # adjustments following D_S[3]
     }else{
       if(SPI$DMP2[t] == 1 | GS$Res[t]<=0.5*ResCAP){               # moderate reduction in demand / increases in D_SW according to DMP2
         GS$D_swDMP[t+1]= D_SW*(1+D_S[2])}                         # adjustments following D_S[2]
       else{
         if(SPI$DMP1[t] == 1 | GS$Res[t]<=0.75*ResCAP){            # small reduction in demand / increase in D_SW according to DMP1
           GS$D_swDMP[t+1]= D_SW*(1+D_S[1])}                       # adjustments following D_S[1]
         else{
           GS$D_swDMP[t+1]= D_SW                                   # in case trigger levels are not reached
         }
       }
     }
    }
  }
  return(GS)
}
