# Constraint function for Plat model #
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  
  # Calculate the number of years #
  iN_years <- dim(wxt)[2] # The number of columns #
  
  # Define the vector of ages #
  vX = ages
  
  # Define the years #
  vT = 1:iN_years
  
  # Define constraint #
  vC = (1 - tail(ages, 1)):(iN_years - ages[1])
  
  # Define the average age #
  dX_bar = mean(vX)
  
  # Phi regression #
  phiReg <- lm(gc ~ 1 + vC + I(vC^2), na.action = na.omit)
  
  # Extract phi coefficients #
  vPhi <- coef(phiReg)
  
  # Update gc #
  gc <- gc - vPhi[1] - vPhi[2] * vC - vPhi[3] * vC^2
  
  # Update kt #
  kt[2, ] = kt[2, ] + 2*vPhi[3] * vT
  kt[1, ] = kt[1, ] + vPhi[2] * vT + vPhi[3] * (vT^2 - 2 * dX_bar * vT)
  
  # Update ax #
  ax = ax + vPhi[1] - vPhi[2] * vX + vPhi[3] * vX^2
  
  # Calculate ci #
  ci = rowMeans(kt, na.rm = TRUE)
  
  # Update ax again #
  ax = ax + ci[1] + ci[2] * (dX_bar - vX)
  
  # Update kt again #
  kt[1, ] = kt[1, ] - ci[1]
  kt[2, ] = kt[2, ] - ci[2]
  
  # Define output list #
  lOut = list(ax = ax,
              bx = bx,
              kt = kt,
              b0x = b0x,
              gc = gc)
  
  # Return the output list #
  return(lOut)
  
}

# Define f2 function #
cf2 <-function(x, ages) {
  
  vOut = mean(ages) - x
  
  return(vOut)
  
}

# Plat model #
f_fitPlat <- function(df) {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # Define constraint #
  Plat <- StMoMo::StMoMo(link = "log", staticAgeFun = TRUE,
                         periodAgeFun = c("1", cf2),
                         cohortAgeFun = "1",
                         constFun = constPlat)
  
  # Fit the model to the data #
  Plat_fit = StMoMo::fit(Plat, data = df)
  
  # Return the plat model #
  return(Plat_fit)
  
}

