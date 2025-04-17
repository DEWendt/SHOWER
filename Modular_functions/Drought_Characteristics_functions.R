##DROUGHTS
Dm_function = function(SHmodel_path, D, var){
  source(paste0(Localrepo,"AF_Drought_functions_DEW.r")) #Adapted Drought functions from Anne Van Loon.
                                                        ## See also Anne's Github for up to date Drought scripts
  source(paste0(Localrepo,"AF_Threshold_functions.r"))
  D_30y=D[1828:13880]                                    # adapted to capture the whole time series
  
  #thresholds
  Jdays=lubridate::yday(D_30y)
  thresVAR = as.data.frame(D_30y)
  Vthres_var=VarThres(var,0.8,D_30y,Jdays = Jdays)      #A variable 20th percentile threshold based on daily data
  
  for(i in 1:length(Jdays)){
    thresVAR$var[i] =Vthres_var[Jdays[i],1]
  }
  
  Mdrought=function(Q,thres,d){
    Q_D=Droughtchar(data=Q,threshold=thres,dates=d)
    Q_D[is.na(Q_D)] <- 0
    Q_Dm=if(sum(Q_D$dur)>0){Minor(file=Q_D,
                                  dur = Q_D$dur,
                                  MIN = 30)}else{Q_D} #30 days drought is minimum
                                                      ##This is the pooling of minor droughts (less than 30 days)
                                                      ##as described in Methods in the main manuscript
  }
  
  #Minor Droughts < 30 days 
  if(sum(var)==0){
    mean_dur=0
    mean_def=0
    freq=0
    max_dur=0
    max_def=0
    
  }else{Dm=Mdrought(Q=var, thres=thresVAR$var,d=D_30y)  # compiling of droughtcharacteristics
  mean_dur=mean(Dm$dur)                                 # mean duration
  max_dur=max(Dm$dur)                                   # maximum duration
  mean_def=mean(Dm$def)                                 # mean deficit 
  max_def=min(Dm$def)                                   # maximum deficit
  freq=ifelse(sum(Dm$dur)==0,0,length(Dm$dur))          # Frequency
  }
  #drought frequency
  return(c(mean_dur, mean_def,freq,max_dur, max_def))
  # return(Dm) # or return the whole dataframe
}
