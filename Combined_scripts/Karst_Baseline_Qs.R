##Combined function to compute the baseline for a karstic groundwater area ##

##Input data:
#Similar to the SoimMoisture_function: 
#Precipitation data (P) and Potential Evaporation data are strings with driving P and PET in them.
#D is a as.Date sequence (can be from P or PET)

#For the combined script it is crucial that your par string aligns with the script.
#I have ordered the variables the following (same as Table 1 in main manuscript).

#par[1]: Wilting point ranges (WP) [mm]
#par[2]: Critical moisture content (CR) [mm]
#par[3]: Field capacity (FC) [mm]
#par[4]: Unsaturated hydraulic conductivity (Kfc) [mm/d]
#par[5]: shape parameter (b) [-]
#par[6]: runoff factor (current value is 0), as in Wendt et al. (2021 NHESS) paper and Van Lanen et al. (2013 HESS)
##Note that this parameter is turned off in the current version, see S3 for details.
#par[7]: Reservoir capacity (Rcap) [%]
#par[8]: Proportional share of ecological flow (Qeco) [%]
#par[9]: Karstic storage-outflow parameter (sK1) [mm/d]
#par[10]: Non-linear (turbulence) flow component Karst [-]
#par[11]: Proportional share of surface water (Dsw) [%]
#par[12]: proportional share of groundwater (Dgw) [%]
#par[13]: scenario choice
#Note that this 13th parameter is only used in the combined script for all scenarios

##Additional notes:
##There are 'HBV-like fixes' are to avoid negative ETa, Rch, SS and Reservoir storage values that 
# might be calculated if the parameter setup is not optimal. All of these fixes are marked with a ## in the notes
#These were initiated for the GSA analysis where parameter selections of the 10K runs could go into physically impossible values.

##Save your functions in this location:
Localrepo = '~/GitHub/SHOWER/Modular_functions/'

# You can use this script as is, or comment the function (line 38) 
  # and return lines (79  and 80) to have a fully functional script.

Baseline_CAMELS_KarsticModule=function(P,PET,D,par){
    source(paste0(Localrepo,'SoilMoisture_function.r'))
  o_LT= SoilBalance_HBVAdjustment_CAMELS(P=P, PET=PET,D=D,par = par)

  #Calculate the mean annual runoff & recharge to define the amount of water use from Dsw and Dgw 
  SSlt_OUT <- o_LT %>% group_by(year(D)) %>% summarise(sumQr=sum(Qr,na.rm = T),sumRch=sum(Rch,na.rm = T))
  SSlt_OUT$Dsupply <- SSlt_OUT$sumRch+ SSlt_OUT$sumQr
  
  # Perc_Demand for SW and GW
  TD_SW=(quantile(SSlt_OUT$Dsupply,par[11],names = F,na.rm = T)/365) #Parameters 11 and 12 determine the daily mm taken each day
  TD_GW=(quantile(SSlt_OUT$Dsupply,par[12],names = F,na.rm = T)/365)
  
  # Initial conditions: Groundwater
  LT_Rch=mean(SSlt_OUT$sumRch)                            # Use long term recharge as input for the first time step
  LT_GSstorage=LT_Rch-(LT_Rch*(1-exp(-par[9]))^par[10])   # First value in mm 
  SS_spin=o_LT[1:spinup,]                                 # Subset the soil moisture dataframe for the initial spin up period

    source(paste0(Localrepo,'Karst_Baseline_function.r'))
  GS_spin=GW_karst_Baseline(par=par, soilmoisture = SS_spin,D_GW=TD_GW, GSini = LT_GSstorage)
  
  #Full groundwater run
  SS_run=o_LT[spinup:length(o_LT$D),]                     # Use the remainder of the calculated soil moisture dataframe
  GS_B =  GW_karst_Baseline(par=par, 
                     soilmoisture = SS_run,
                     D_GW=TD_GW, GSini = last(GS_spin$GS))# Groundwater run using initial conditions
  
  #reservoir storage run
  QEco_Ks=quantile(GS_B$Qb,par[8],names = F, na.rm=T)     # Determine the amount of Qbeco from Qeco and the calculated baseflow (Qb)
  GS_B$Qres = ifelse(GS_B$Qb-QEco_Ks>0, 
                     GS_B$Qb-QEco_Ks, 0)                  # in baseline, the Qres (remainder flow) is defined here as Qb-Qbeco
  
  #Define initial conditions for reservoir
  P_YM <-o_LT %>% group_by(month(D),year(D)) %>% 
    summarise(Pmon=sum(P,na.rm=T),.groups = 'drop')     # Use the mean annual precipitation of a site to determine
  ResCAP=par[7]*mean(P_YM$Pmon)                            # reservoir capacity with parameter 7 

    source(paste0(Localrepo,'Reservoir_Baseline_function.r'))
  WRR_L=Res_Baseline(ResCAP=ResCAP,GS=GS_B,D_SW = TD_SW)         #Calculate downstream reservoir storage 
  
  ##return full dataset
    # you can specify to only spit out groundwater / surface water / reservoir time series for further calculations
  return(WRR_L$Qs)
}