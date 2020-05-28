# APC model #
f_fitAPC <- function(df) {
  
  # Require the APC package #
  require(StMoMo)
  
  # Estimate the APC model #
  APC <- StMoMo::apc(link = "log")
  
  # Fit the model to data #
  APC_fit <- StMoMo::fit(APC, data = df)
  
  # Return the model #
  return(APC_fit)
  
}