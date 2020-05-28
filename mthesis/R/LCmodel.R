
# Estimate LC model #
f_fitLC <- function(df) {
  
  # Require the necessary packages #
  require(StMoMo)
  
  # Define the model #
  LC <- StMoMo::lc(link = "log")
  
  # Fit to the data #
  LC_fit = StMoMo::fit(LC, data = df)
  
  # Return the output model #
  return(LC_fit)
  
}


# LC model with Singular Value Decomposition #
f_fitLC_SVD <- function(df) {
  
  # Require the necessary packages #
  require(demography)
  
  # Fit the LC model with demogdata object #
  lFit = demography::lca(df)
  
  # Return the model #
  return(lFit)
  
  
}

f_LC_new <- function(mx_list, vSub = c("Female", "Male", "Total")) {
  
  # Extract countries #
  vCountries = names(mx_list)
  
  # Create an output list #
  lOut = list()
  
  # Loop over lande #
  for (sC in vCountries) {
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Create the data #
      mx_df = f_DataTransform(mx_list[[sC]], sSubpop = sSub)
      
      # Create training split #
      lSplit = f_train_split(mx_df, iTest_start = 2000)
      
      # Extract training values #
      mTrain = lSplit$train
      
      # Find values that are equal to zero and set them just above zero #
      mTrain[mTrain == 0] = 1e-04
      
      # Extract test values #
      mTrue = lSplit$test
      
      # Calculate the log-rates #
      mTrain_log = log(mTrain)
      
      # Calculate intercept values #
      vAlpha_x = rowMeans(mTrain_log)
      
      # Calculate centered matrix #
      mTrain_log_cent = sweep(mTrain_log, 1, vAlpha_x, FUN="-")
      
      # Run SVD on log_rates #
      lSVD = svd(mTrain_log_cent)
      
      # Extract left singular vectors from SVD #
      mLeft = lSVD$u
      
      # Extract right singular vectors from SVD #
      mRight = lSVD$v
      
      # Extract matrix of singular values #
      mSingular = diag(lSVD$d)
      
      # Extract B(x) vector #
      vBx = mLeft[, 1] / sum(mLeft[, 1])
      
      # Extract K(t) vector #
      vKt = mSingular[1, 1] * mRight[, 1] * sum(mLeft[, 1])
      
      # Subtract mean so that vKt sums to zero #
      # vKt = vKt - mean(vKt)
      
      # Rescale K(t) to sum to zero #
      # vKt = rescale_zero(vKt)
      
      # Rescale B(x) to sum to one #
      # vBx = vBx / sum(vBx)
      
      # Calculate random walk with drift for K(t) #
      RW_Kt = arima(diff(vKt), order = c(0, 0, 0))
      
      # Random walk with drift intercept #
      dRW_int = RW_Kt$coef
      
      # First forecast value for K(t) #
      dK_t1 = dRW_int + tail(vKt, 1)
      
      # Create a matrix for predictions #
      mPred_log = matrix(data = NA, nrow = 46, ncol = 17,
                         dimnames = list(50:95, 2000:2016))
      
      # Calculate first 
      mPred_log[, paste0(2000)] = vAlpha_x + vBx * dK_t1
      
      # Forecast for remaining years #
      for (iYear in 2001:2016) {
        
        # Update K(t) value #
        dK_t1 = dRW_int + dK_t1
        
        # Calculate forecast #
        mPred_log[, paste0(iYear)] = vAlpha_x + vBx * dK_t1
        
      }
      
      # Calculate forecast in normal mortality rates #
      mPred = exp(mPred_log)
      
      # Calculate squared errors #
      mErr2 = (as.matrix(mTrue) - as.matrix(mPred))^2
      
      # Calculate MSE #
      dMSE = mean(mErr2)
      
      # Calculate RMSE #
      dRMSE = sqrt(dMSE)
      
      # Put results on output list #
      lOut[[sC]][[sSub]][["mTrue"]] = mTrue
      lOut[[sC]][[sSub]][["mPred"]] = mPred
      lOut[[sC]][[sSub]][["MSE"]] = dMSE
      lOut[[sC]][[sSub]][["RMSE"]] = dRMSE
      
    } # Close loop for subpopulations #
    
  } # Close loop for countries #
  
  # Return the output list #
  return(lOut)
  
}