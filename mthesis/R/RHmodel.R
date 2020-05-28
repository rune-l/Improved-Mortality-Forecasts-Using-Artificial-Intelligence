# Renshaw and Haberman (RH) model #
f_fitRH <- function(df) {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # Estimate RH model #
  RH <- StMoMo::rh(link = "log", cohortAgeFun = "1")
  
  # Fit the model to the data #
  RH_fit = StMoMo::fit(RH, data = df)
  
  # Return output #
  return(RH_fit)
  
}