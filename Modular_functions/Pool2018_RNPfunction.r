RNP <- function(sim, obs){
  
  # Check if 'sim' and 'obs' are numeric
  if(!is.numeric(sim) || !is.numeric(obs)) stop("Invalid argument type: both 'sim' and 'obs' must be numeric!")
  
  # Check if 'sim' and 'obs' have the same length
  if(length(sim)!=length(obs)) stop("Invalid arguments: 'sim' and 'obs' must have the same length!")
  
  # Remove NaN values
  indices = which(!is.na(sim) & !is.na(obs))
  sim = sim[indices]
  obs = obs[indices]
  
  # Calculate mean sim and obs
  mean.sim = mean(sim, na.rm=TRUE)
  mean.obs = mean(obs, na.rm=TRUE)
  
  # Calculate normalized flow duration curves
  fdc.sim = sort(sim / (mean.sim * length(sim)))
  fdc.obs = sort(obs / (mean.obs * length(obs)))
  
  # Calculate alpha component
  RNP.alpha = 1 - 0.5 * sum(abs(fdc.sim - fdc.obs))
  
  # Calculate beta component
  RNP.beta = mean.sim / mean.obs
  
  # Calculate r component
  RNP.r = cor(sim, obs, method="spearman")
  
  # Return Non-Parametric Efficiency value
  return(1 - sqrt((RNP.alpha - 1)^2 + (RNP.beta - 1)^2 + (RNP.r - 1)^2))
}

##added to extract the values
#out.type = 'single' for one output, any other for 3 values

RNP_full <- function(sim, obs, out.type){
  
  # Check if 'sim' and 'obs' are numeric
  if(!is.numeric(sim) || !is.numeric(obs)) stop("Invalid argument type: both 'sim' and 'obs' must be numeric!")
  
  # Check if 'sim' and 'obs' have the same length
  if(length(sim)!=length(obs)) stop("Invalid arguments: 'sim' and 'obs' must have the same length!")
  
  # Remove NaN values
  indices = which(!is.na(sim) & !is.na(obs))
  sim = sim[indices]
  obs = obs[indices]
  
  # Calculate mean sim and obs
  mean.sim = mean(sim, na.rm=TRUE)
  mean.obs = mean(obs, na.rm=TRUE)
  
  # Calculate normalized flow duration curves
  fdc.sim = sort(sim / (mean.sim * length(sim)))
  fdc.obs = sort(obs / (mean.obs * length(obs)))
  
  # Calculate alpha component
  RNP.alpha = 1 - 0.5 * sum(abs(fdc.sim - fdc.obs))
  
  # Calculate beta component
  RNP.beta = mean.sim / mean.obs
  
  # Calculate r component
  RNP.r = cor(sim, obs, method="spearman")
  
  if(out.type == 'single'){
    # Return Non-Parametric Efficiency value
    return(1 - sqrt((RNP.alpha - 1)^2 + (RNP.beta - 1)^2 + (RNP.r - 1)^2))
  }else{
    RNP_values <- list(RNP.alpha,RNP.beta,RNP.r)
    return(RNP_values)
  }
}
