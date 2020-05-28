# Estimate the CBD model #
f_fitCBD <- function(df) {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # Estimate the CBD model #
  CBD <- StMoMo::cbd(link = "log")
  
  # Fit the model to data #
  CBD_fit <- StMoMo::fit(CBD, data = df)
  
  # Return output #
  return(CBD_fit)
  
}


# M7 extension of the CBD model #
f_fitM7 <- function(df) {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # Estimate the M7 model #
  M7 <- StMoMo::m7(link = "log")
  
  # Fit the model to data #
  M7_fit <- StMoMo::fit(M7, data = df)
  
  # Return model #
  return(M7_fit)
  
  
}

# M6 Extension of the CBD model #
f_fitM6 <- function(df) {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # Estimate the M7 model #
  M6 <- StMoMo::m6(link = "log")
  
  # Fit the model to the data #
  M6_fit <- StMoMo::fit(M6, data = df)
  
  # Return model #
  return(M6_fit)
  
  
}

# M8 Extension of the CBD model #
f_fitM8 <- function(df) {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # Estimate the M7 model #
  M8 <- StMoMo::m8(link = "log", xc = 95)
  
  # Fit the model to the data #
  M8_fit <- StMoMo::fit(M8, data = df)
  
  # Return model #
  return(M8_fit)
  
}
