##Combined function to compute the baseline for a karstic groundwater area ##

##Input data (same as baseline script):
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
# and return lines (119 and 120) to have a fully functional script.

S2_CAMELS_KarsticModule=function(P, PET, D, par){
  
    source(paste0(Localrepo,'SoilMoisture_function.r'))
  o_LT= SoilBalance_HBVAdjustment_CAMELS(P=P, PET=PET,D=D,par = par)    #Soil moisture run of whole period
  
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
                            D_GW=TD_GW, 
                            GSini = last(GS_spin$GS))     # Groundwater run using initial conditions
  
  #Determine the trigger levels for groundwater
  ##Note that these years are a result of averaged trigger levels from drought plans published by UK drinking water companies
  ##See Wendt et al. (2021 NHESS) for details
  GS_B$GS_DMP1  <- ifelse(GS_B$GS<=quantile(GS_B$GS,(1/8.5)),1,0)   # Triggerlevels are set to 1 once in 8.5 years for the first DMP
  GS_B$GS_DMP2  <- ifelse(GS_B$GS<=quantile(GS_B$GS,(1/22.5)),1,0)  # Second trigger level is set to 1 in 22.5 years
  GS_B$GS_DMP3  <- ifelse(GS_B$GS<=quantile(GS_B$GS,(1/69)),1,0)    # Third trigger level is set to 1 in 69 years
  
  # SPI trigger levels 
  P_YM <- o_LT %>% group_by(month(D),year(D)) %>% 
    summarise(Pmon=sum(P,na.rm=T),.groups = 'drop')               #Use the mean monthly precipitation to calculate alternative threshold levels
  P_YM$D <- seq.Date(from=as.Date(D[1], format('%d-%m-%Y')),
                     to=as.Date(last(D), format('%d-%m-%Y')), 
                     by='month')
  dt1 <- data.table::data.table(id=P_YM$D,  val1=P_YM$Pmon, key="id")
  dt2 <- data.table::data.table(id=SS_run$D , val2=SS_run$Pra, key="id")
  rollSPI <- dt1[dt2,roll=TRUE]                                     #Slighlty clunky way of reframing the monthly precipitation.
  
  rollSPI$DMP1  <- ifelse(rollSPI$val1<= 
                            quantile(P_YM$Pmon, prob=(1/8.5)) ,1,0) # Not the most elegant way to get the thresholds but it works ;)
  rollSPI$DMP2  <- ifelse(rollSPI$val1<=
                            quantile(P_YM$Pmon, prob=(1/22.5)),1,0) # The quantiles function to mark with 1 and 0 when the thresholds are reached
  rollSPI$DMP3  <- ifelse(rollSPI$val1<=
                            quantile(P_YM$Pmon, prob=(1/69)),1,0)
  
  #Drought management practices 
  D_GW2 =c(-0.05, -0.12, -0.36) *0.5                              # Groundwater use is set to increase following these percentages
  ##Note that these percentages were a result of averaged drought measures, see Wendt et al. 2021 for details

  #Second groundwater run
    source(paste0(Localrepo,'Karst_Scenario12_function.r'))
  GS_S2=GW_karst_Scenario12(par=par, soilmoisture = SS_run,         # Groundwater run with scenario 2 implemented
                            D_GW=TD_GW,D_S = D_GW2, 
                            GSini = last(GS_spin$GS),
                            SPI = rollSPI, GS_B=GS_B) 
  
  #reservoir storage run
  QEco_Ks=quantile(GS_B$Qb,par[8],names = F, na.rm=T)     # Determine the amount of Qbeco from Qeco and the calculated baseflow (Qb)
  GS_S2$Qres = ifelse(GS_B$Qb-QEco_Ks>0, 
                     GS_B$Qb-QEco_Ks, 0)                  # in Scenario 2, the Qres (remainder flow) is defined here as Qb-Qbeco
  
  #Define initial conditions for reservoir
  ResCAP=par[7]*mean(P_YM$Pmon)                           # Converting the reservoir capacity (%) into  max amount

  #DMP measure *surface water_share
  D_SW2 <- c(-0.05, -0.12, -0.36) *.5                     # Surface water use is set to increase following these percentages
  ##Note that these percentages were a result of averaged drought measures, see Wendt et al. 2021 for details

    source(paste0(Localrepo,'Reservoir_Scenario12_function.r'))  
  WRR_L=Res_S12(ResCAP=ResCAP,GS=GS_S2,D_SW = TD_SW,
                SPI = rollSPI,D_S=D_SW2)                  #Calculate downstream reservoir storage 
  
  ##return full dataset
  # you can specify to only spit out groundwater / surface water / reservoir time series for further calculations
  return(WRR_L$Qs)
}