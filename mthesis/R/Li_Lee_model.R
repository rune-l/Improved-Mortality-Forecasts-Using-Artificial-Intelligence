# Li-Lee model #
f_LiLee <- function(mx_list, ex_list, vSub, lRegion) {
  
  # lRegion is a list with the countries in the region in each element that is a region #
  
  # Make a list of output #
  lOut = list()
  
  # Get names of regions #
  vRegion_names = names(lRegion)
  
  vYears = 1950:2016
  vAges = 50:95
  
  vYears_train = 1950:1999
  vYears_test = 2000:2016
  
  nTrain = length(vYears_train)
  
  # Create array for centered mortality rates #
  aMx = array(data = NA, dim = c(46, nTrain, length(lRegion)),
              dimnames = list(NULL, NULL, vRegion_names))
  
  for (sRegion in vRegion_names) {
    
    # Loop over the subpopulations #
    for (sSub in vSub) {
    
      # Find countries in the region #
      vCountries = lRegion[[sRegion]]
    
      # Create an array for the given region #
      aRegion_mx = array(data = NA, dim = c(46, nTrain, length(vCountries)),
                         dimnames = list(NULL, NULL, vCountries))
    
      aRegion_ex = array(data = NA, dim = c(46, nTrain, length(vCountries)),
                         dimnames = list(NULL, NULL, vCountries))
    
      # Create array for rowmeans #
      aAlpha_mx = array(data = NA, dim = c(46, 1, length(vCountries)),
                        dimnames = list(NULL, NULL, vCountries))
    
      aAlpha_ex = array(data = NA, dim = c(46, 1, length(vCountries)),
                        dimnames = list(NULL, NULL, vCountries))
      
      # Create a matrix for centered log values #
      aLog_center_mx = array(data = NA, dim = c(46, nTrain, length(vCountries)),
                             dimnames = list(NULL, NULL, vCountries))
      
      aLog_center_ex = array(data = NA, dim = c(46, nTrain, length(vCountries)),
                             dimnames = list(NULL, NULL, vCountries))
      
    
      # Make a list for test data #
      lTest_mx = list()
      lTest_ex = list()
    
      # Loop over countries #
      for (sC in vCountries) {
      
        # Find the data #
        df_mx = f_DataTransform(mx_list[[sC]], sSubpop = sSub)
        df_ex = f_DataTransform(ex_list[[sC]], sSubpop = sSub)
      
        # Split the data into test and training data #
        lSplit_mx = f_train_split(df_mx, iTest_start = head(vYears_test, 1))
        lSplit_ex = f_train_split(df_ex, iTest_start = head(vYears_test, 1))
      
        # Save the test data #
        lTest_mx[[sC]] = lSplit_mx$test
        lTest_ex[[sC]] = lSplit_ex$test
      
        # Insert data for the given country #
        aRegion_mx[,, sC] = as.matrix(lSplit_mx$train)
        aRegion_ex[,, sC] = as.matrix(lSplit_ex$train)
      
        # Check for zero values #
        aRegion_mx[,,sC][aRegion_mx[,,sC] == 0] = 1e-04
        aRegion_ex[,,sC][aRegion_ex[,,sC] == 0] = 1e-04
      
        # Calculate rowmeans for each country #
        aAlpha_mx[,, sC] = rowMeans(log(aRegion_mx[,, sC]))
        aAlpha_ex[,, sC] = rowMeans(log(aRegion_ex[,, sC]))
        
        # Calculate log centered values #
        aLog_center_mx[,, sC] = sweep(log(aRegion_mx[,, sC]), 1, aAlpha_mx[,, sC], FUN = "-")
        aLog_center_ex[,, sC] = sweep(log(aRegion_ex[,, sC]), 1, aAlpha_ex[,, sC], FUN = "-")
        
      }
    
      # Calculate log versions #
      aRegion_mx_log = log(aRegion_mx)
      aRegion_ex_log = log(aRegion_ex)
    
      # Calculate age mean for each region #
      mMean_mx = apply(X = aRegion_mx, MARGIN = c(1,2), FUN = mean)
      mMean_ex = apply(X = aRegion_ex, MARGIN = c(1,2), FUN = mean)
    
      # Calculate log mean #
      mLogMean_mx = apply(X = aRegion_mx_log, MARGIN = c(1,2), FUN = mean)
      mLogMean_ex = apply(X = aRegion_ex_log, MARGIN = c(1,2), FUN = mean)
      
      # Calculate log centered #
      mLogMean_center_mx = apply(X = aLog_center_mx, MARGIN = c(1,2), FUN = mean)
      mLogMean_center_ex = apply(X = aLog_center_ex, MARGIN = c(1,2), FUN = mean)
    
      # Use SVD to find Global factor #
      lSVD_glob = svd(mLogMean_center_mx)
      
      # Extract left singular vectors for global factor #
      mLeft_glob = lSVD_glob$u
      
      # Extract right singular vectors for global factor #
      mRight_glob = lSVD_glob$v
      
      # Extract singular values for global factor #
      mSingular_glob = diag(lSVD_glob$d)
    
      # B(x) factor #
      vBx = mLeft_glob[, 1] / sum(mLeft_glob[, 1]) # Length (nAges = 46) #
    
      # K(t) factor #
      vKt = mSingular_glob[1, 1] * mRight_glob[, 1] * sum(mLeft_glob[, 1]) # Length (nYears = 50) #
      
      # Print statement #
      #cat("Sum of vKt (partly) =", round(sum(mSingular_glob[1, 1] * mRight_glob[, 1]), 0), "\n")
      
      # Rescale K(t) to zero #
      #vKt = rescale_zero(vKt)
      
      # Print statement #
      #cat("The sum of K(t) =", sum(vKt), "\n")
      
      # Rescale B(x) to one #
      #vBx = vBx / sum(vBx)
      
      # Calculate random walk for vKt term #
      RW_Kt = arima(diff(vKt), order = c(0, 0, 0))
    
      # Now we can find the population specific parameters #
      # Create matrices to store results on #
      mBx_i = matrix(data = NA, nrow = 46, ncol = length(vCountries))
      mKt_i = matrix(data = NA, nrow = nTrain, ncol = length(vCountries))
      
      # Name the columns after the countries #
      colnames(mBx_i) = vCountries
      colnames(mKt_i) = vCountries
    
      # Create a list for the random walks for each country #
      lRW_kt_i = list()
    
      # Create an empty forecast matrix #
      mForecast = matrix(data = NA, nrow = 46, ncol = 17,
                         dimnames = list(50:95, 2000:2016))
    
      for (sC in vCountries) {
        
        # Calculate centered mx for the population #
        mCenter_pop = sweep(aRegion_mx_log[,, sC], 1, aAlpha_mx[,, sC], FUN = "-")
        mCenter_pop = sweep(mCenter_pop, c(1, 2), vBx %*% t(vKt), FUN = "-")
        
        # mCenter_pop = aRegion_mx_log[,, sC] - aAlpha_mx[,, sC] - vBx %*% t(vKt)
        
        # Perform SVD on centered mx for the population #
        lSVD_pop = svd(mCenter_pop)
        
        # Extract left singular vectors for the population #
        mLeft_pop = lSVD_pop$u
        
        # Extract right singular vectors for the population #
        mRight_pop = lSVD_pop$v
        
        # Extract singular values for the population #
        mSingular_pop = diag(lSVD_pop$d)
      
        # Assign svd values #
        mBx_i[, sC] = mLeft_pop[, 1] / sum(mLeft_pop[, 1])
        mKt_i[, sC] = mSingular_pop[1, 1] * mRight_pop[, 1] * sum(mLeft_pop[, 1])
        
        # Rescale K(t, i) to 0 #
        #mKt_i[, sC] = rescale_zero(mKt_i[, sC])
        
        # Rescale B(x, i) to 1 #
        #mBx_i[, sC] = mBx_i[, sC]/sum(mBx_i[, sC])
        
        # Estimate RW for the country #
        lRW_kt_i[[sC]] = arima(diff(mKt_i[, sC]), order = c(0, 0, 0))
      
        # Calculate random walk with drift values #
        K_t1 = RW_Kt$coef + tail(vKt, 1)
        K_t1i = lRW_kt_i[[sC]]$coef + tail(mKt_i[, sC], 1)
      
        # Calculate the first forecast #
        mForecast[, paste0(2000)] = aAlpha_mx[,, sC] + (as.matrix(vBx) %*% K_t1) + 
          (as.matrix(mBx_i[, sC]) %*% K_t1i)
      
        # Forecast for remaining years #
        for (iYear in 2001:2016) {
        
          # Update random walk with drift values #
          K_t1 = RW_Kt$coef + K_t1
          K_t1i = lRW_kt_i[[sC]]$coef + K_t1i
        
          # Calculate forecast for the year #
          mForecast[, paste0(iYear)] = aAlpha_mx[,, sC] + (as.matrix(vBx) %*% K_t1) + 
            (as.matrix(mBx_i[, sC]) %*% K_t1i)
        
        }
      
        # Put forecast on output list #
        lOut[[sC]][[sSub]][["LogPred"]] = mForecast
        lOut[[sC]][[sSub]][["mPred"]] = exp(mForecast)
      
        # Put true values on output list #
        lOut[[sC]][[sSub]][["mTrue"]] = lTest_mx[[sC]]
      
        # Calculate squared errors #
        mErr2 = (as.matrix(exp(mForecast)) - as.matrix(lTest_mx[[sC]]))^2
      
        # Calculate MSE #
        lOut[[sC]][[sSub]][["MSE"]] = mean(mErr2)
      
        # Calculate RMSE #
        lOut[[sC]][[sSub]][["RMSE"]] = sqrt(lOut[[sC]][[sSub]][["MSE"]])
      
        } # Close the loop over countries #
    
      } # Close the loop over vSub #
    
    } # Close the loop over regions #
  
  # Return the model fits of the regions #
  return(lOut)
  
}

# Rescaling function to scale the sum to zero #
rescale_zero <- function(x) {
  x1 <- x[x>0]
  x2 <- x[x<0]
  d <- (sum(x1) + sum(x2)) / 2
  w1 <- (sum(x1) - d) / sum(x1)
  w2 <- (sum(x2) - d) / sum(x2)
  y <- x
  y[x>0] <- x1*w1
  y[x<0] <- x2*w2
  return(y)
  
}