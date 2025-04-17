##Input data:

# GS is groundwater calculated using the Baseline or Scenarios_12 
# This dataframe has columns [baseline] c('Rch','Qr','Qb','Qs','GSimp','GS','Res','Qimp','Qrel') or
#   [Scenarios12] c('Rch','Qr','Qb','Qs','GSimp','GS',"D_gwDMP",'Res','Qimp','Qrel','D_swDMP'))
# Note that you can use this for any of the groundwater modules!

# ResCAP is the converted reservoir capacity as ResCAP = Rcap parameter (par[7] is Rcap) * mean Annual P

# D_SW is your groundwater demand, which can be static / dynamic or one number, as is here.

Res_Baseline_Release =  function(ResCAP, GS, D_SW, Q95){
    if(ResCAP == 0){                        #adjustment in case the Reservoir capacity is set to zero (turning off the reservoir unit)
      GS$Qimp <- D_SW
      GS$Qrel <- 0
      GS$Res <- 0
    }else{
      GS$Res[1] = ResCAP                  # Reservoir is full at the start.
      GS$Qimp <- 0                        # no static / built-in surface water import
      GS$Qrel <- 0
      
  #Determine reservoir storage with new water demand D_SW
  for(t in 1:(length(GS$Rch)-1)){
      GS$Res[t+1]=GS$Res[t]-D_SW +GS$Qr[t]+
        GS$Rch[t]+GS$Qimp[t] - as.numeric(Q95)             # Reservoir storage with incoming Qr, QRes, Imported SW and outgoing D_SWDMP and Q95
      if(GS$Res[t+1] < ResCAP){                            # If reservoir storage is less than capacity > only Q95 as release flow (Qrel)
        GS$Qrel[t+1] =as.numeric(Q95)
        GS$Qimp[t+1]=ifelse(GS$Res[t+1]>=0,GS$Qimp[t+1],
                            GS$Qimp[t+1]-GS$Res[t+1])      # Qimp is only used when reservoir storage is below zero
        GS$Res[t+1] = GS$Res[t+1] + GS$Qimp[t+1]
      } else {                                             ## if reservoir capacity is exceeded > Qrel receives the surpluss + Q95
        GS$Qrel[t+1]= (GS$Res[t+1] -
                         as.numeric(K)) + as.numeric(Q95)
        GS$Res[t+1]=ResCAP
      }
    }
  }
  return(GS)
} 