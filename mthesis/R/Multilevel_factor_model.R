# Multilevel factor model #
f_Multilevel_FM <- function(mx_list,
                            vSub = c("Female", "Male", "Total")) {
  
  # Require the CCA package #
  require(CCA)
  require(candisc)
  
  # Define output list #
  lOut = list()
  
  # Define canonical correlation list #
  lCC <- list()
  lCC_max <- list()
  
  # Create list alpha (intercept) vectors #
  lAlpha = list()
  
  # Create log-centered for centered data matrices #
  lCenter = list()
  
  # lCountry factor #
  lPop_fact = list()
  
  # Create list for svd #
  lSVD = list()
  
  # Create list for RW with drift models #
  lRW = list()
  
  # Create list with models #
  lModel = list()
  
  # Define the countries #
  vCountries = names(mx_list)
  
  # Define for the maximum #
  dMax = -Inf
  sMax = NULL
  
  # Loop over the subpopulations #
  for (sSub in vSub) {
    
    # Create list element for the subpopulation #
    lCC_max[[sSub]] = list()
    
    # Loop over the country #
    for (sC in vCountries) {
      
      # Compute first df #
      df_1 = f_DataTransform(mx_list[[sC]], sSubpop = sSub)
      df_1[df_1 == 0] = 1e-04
      df_1 = log(df_1)
      
      # Demean with the age means #
      vMeanAge_df1 = rowMeans(df_1)
      
      mCenter_df1 = sweep(df_1, 1, vMeanAge_df1, FUN = "-")
      
      # Assign alpha values to list #
      lAlpha[[sC]][[sSub]] = vMeanAge_df1
      
      # Assign log-centered matrix to list #
      lCenter[[sC]][[sSub]] = mCenter_df1
      
      # Compute remaining countries #
      vRemain_Countries = vCountries[vCountries != sC]
      
      # Loop over the remaining countries #
      for (sC_2 in vRemain_Countries) {
        
        # Compute second df #
        df_2 = f_DataTransform(mx_list[[sC_2]], sSubpop = sSub)
        df_2[df_2 == 0] = 1e-04
        df_2 = log(df_2)
        
        # DeMean the df_2 #
        vMeanAge_df2 = rowMeans(df_2)
        
        mCenter_df2 = sweep(df_2, 1, vMeanAge_df2, FUN = "-")
        
        # Compute name #
        sName = paste0(sC, "_", sC_2, "_", sSub)
        
        # Print statement #
        # cat("Computing canonical correlation for", sC, "and", sC_2, "for subpop:", sSub, "\n")
        
        # Compute canonical correlation #
        lCor = CCA::cc(t(mCenter_df1), t(mCenter_df2))
        lCC[[sName]] = lCor
        
        # Assign if average canonical correlation is higher #
        if (mean(lCor$cor) > dMax) {
          
          cat("New max for:", sName, "\n")
          
          # Print statement #
          
          # Assign new maximum #
          lMax = lCor
          dMax = mean(lCor$cor)
          sMax = sName
          vMax = lCor$cor
          
          # Extract the global factor #
          vGt = lMax$scores$xscores[, 1]
          
          # Calculate the global factor loadings #
          vBx = solve(t(vGt) %*% vGt) %*% (t(vGt) %*% t(df_1))
          vBx = t(vBx)
          
        }
        
      } # Close remaining country loop #
      
    } # Close country loop #
    
    # Assign maximum to lCC_max list #
    lCC_max[[sSub]][["name"]] = sMax
    lCC_max[[sSub]][["avgmax"]] = dMax
    lCC_max[[sSub]][["cc"]] = vMax
    lCC_max[[sSub]][["Gt"]] = vGt
    lCC_max[[sSub]][["Bx"]] = vBx
    
    # Calculate global factor loadings for the subpopulation #
    lCC_max[[sSub]][["FL"]] = vBx %*% t(vGt)
    
    # Calculate random walk for global factor #
    lCC_max[[sSub]][["RW_Gt"]] = arima(diff(vGt), order = c(0, 0, 0))$coef
    
    # Next we need to estimate the country specific effects (iteration 0) #
    for (sC in vCountries) {
      
      # Calculate matrix to do svd on #
      mCenter_pop = sweep(x = lCenter[[sC]][[sSub]], MARGIN = c(1,2),
                          STATS =  lCC_max[[sSub]][["FL"]], FUN = "-")
      
      # Do svd on the centered matrix minus global factor #
      lSVD[[sC]][[sSub]] = svd(mCenter_pop)
      
      # Define the elements from the SVD #
      mLeft = lSVD[[sC]][[sSub]]$u
      mRight = lSVD[[sC]][[sSub]]$v
      mSingular = diag(lSVD[[sC]][[sSub]]$d)
      
      # Number of country factors #
      nFact = 3 # Calculate with Information criteria #
      
      # Make matrices ready #
      mFactor = matrix(NA, nrow = 67, ncol = nFact)
      mLoading = matrix(NA, nrow = 46, ncol = nFact)
      vRW_ft = numeric(0)
      
      # Calculate country factors #
      for (i in 1:nFact) {
        mLoading[, i] = mLeft[, i] / sum(mLeft[, i])
        mFactor[, i] = mSingular[i, i] * mRight[, i] * sum(mLeft[, i])
        
        # Estimate country random walk #
        vRW_ft[i] = arima(diff(mFactor[, i]), order = c(0, 0, 0))$coef
        
      }
      
      # Assign to list #
      lPop_fact[[sC]][[sSub]][["bx"]] = mLoading
      lPop_fact[[sC]][[sSub]][["ft"]] = mFactor
      
      # Assign the model #
      lModel[[sC]][[sSub]][["alpha"]] = lAlpha[[sC]][[sSub]]
      lModel[[sC]][[sSub]][["Bx"]] = vBx
      lModel[[sC]][[sSub]][["Gt"]] = vGt
      lModel[[sC]][[sSub]][["bx"]] = mLoading
      lModel[[sC]][[sSub]][["ft"]] = mFactor
      lModel[[sC]][[sSub]][["RW_ft"]] = vRW_ft
      lModel[[sC]][[sSub]][["RW_Gt"]] = lCC_max[[sSub]][["RW_Gt"]]
      
      
    } # Close country loop 
    
    
    
  } # Close subpopulation loop #
  
  # Return the output list #
  return(lCC_max)
  
}

# New model where the factors are already given #
f_Multilevel_FM_2 <- function(mx_list,
                              sMat_path = paste0("/Users/runelangergaard/OneDrive - Aarhus universitet",
                                                 "/Oecon/10. semester/Code/data/matlab data")) {
  
  # Create matlab split
  lSplit_matlab = f_Matlab_format(mx_list)
  
  # Load matlab result files #
  FemaleRes = readMat(paste0(sMat_path, "/Femalefactors.mat"))
  MaleRes = readMat(paste0(sMat_path, "/Malefactors.mat"))
  TotalRes = readMat(paste0(sMat_path, "/Totalfactors.mat"))
  
  # lSplit_matlab consists of log(mx) #
  # Each array has time on rows and ages on columns and countries on the third dimension #
  
  # Define countries #
  vCountries = dimnames(lSplit_matlab$Female$train)[[3]]
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Define the training years #
  vYears_train = 1950:1999
  
  # Define the testing years #
  vYears_test = 2000:2016
  
  # Define the ages #
  vAges = 50:95
  
  # Make a list for results #
  lRes = list()
  
  # Make a list for random walk (RW) with drift for global #
  lRW_glob = list()
  
  # Make a list for RW with drift for country #
  lRW_c = list()
  
  # Make a list over the model #
  lModel = list()
  
  # Loop over subpopulations #
  for (sSub in vSub) {
    
    # Get the results #
    ResList = get(paste0(sSub, "Res"))
    
    # Extract global factors for subpopulation #
    vFactor_g = ResList$Ghat2
    
    # Calculate RW with drift parameter for global factor #
    RW_g = arima(diff(vFactor_g), order = c(0, 0, 0))$coef
    
    # Assign to list #
    lRW_glob[[sSub]] = RW_g
    
    # Loop over countries #
    for (sC in vCountries) {
      
      # Extract the country number #
      iC = which(vCountries == sC)
      
      # Extract factors for country #
      mFactor_c = ResList$Fhat2[,, iC]
      
      # Extract the global loadings #
      mLoad_g = ResList$GAMhat2[,, iC]
      
      # Extract the country loadings #
      mLoad_c = ResList$LAMhat2[,, iC]
      
      # Calculate matrix of differences for country factors #
      mDiff_c = diff(mFactor_c)
      
      # Calculate RW for country #
      RW_c = apply(X = mDiff_c, MARGIN = 2,
                   FUN = function(vCol) {
                     ifelse(var(vCol) == 0, 0, arima(vCol, order = c(0, 0, 0))$coef)
                   })
      
      # Assign to model list #
      lModel[[sC]][[sSub]][["factor_g"]] = vFactor_g
      lModel[[sC]][[sSub]][["factor_c"]] = mFactor_c
      lModel[[sC]][[sSub]][["loading_g"]] = mLoad_g
      lModel[[sC]][[sSub]][["loading_c"]] = mLoad_c
      lModel[[sC]][[sSub]][["RW_g"]] = RW_g
      lModel[[sC]][[sSub]][["RW_c"]] = RW_c
      
      # Extract the data #
      mTrain = lSplit_matlab[[sSub]][["train"]][,, sC]
      
      # Get R^2 values from multiple regression (one for each age) #
      vR2_mult = apply(X = mTrain, MARGIN = 2,
                       FUN = function(vCol) {
                         lFit = lm(vCol ~ vFactor_g + mFactor_c)
                         return(summary(lFit)$r.squared)
                       })
      
      # Rename the entrances #
      names(vR2_mult) = vAges
      
      # Get R^2 values from simple reg on global factor #
      vR2_glob = apply(X = mTrain, MARGIN = 2,
                       FUN = function(vCol) {
                         lFit = lm(vCol ~ vFactor_g)
                         return(summary(lFit)$r.squared)
                       })
      
      # Rename the entrances #
      names(vR2_glob) = vAges
      
      # Get R^2 on first factor #
      if (var(mFactor_c[, 1]) == 0) {
        vR2_f1 = rep(0, length(vAges))
      } else {
        vR2_f1 = apply(X = mTrain, MARGIN = 2,
                       FUN = function(vCol) {
                         lFit = lm(vCol ~ mFactor_c[, 1])
                         return(summary(lFit)$r.squared)
                       })
      }
      
      # Get R^2 on second factor #
      if (var(mFactor_c[, 2]) == 0) {
        vR2_f2 = rep(0, length(vAges))
      } else {
        vR2_f2 = apply(X = mTrain, MARGIN = 2,
                       FUN = function(vCol) {
                         lFit = lm(vCol ~ mFactor_c[, 2])
                         return(summary(lFit)$r.squared)
                       })
      }
      
      # Get R^2 on third factor #
      if (var(mFactor_c[, 3]) == 0) {
        vR2_f3 = rep(0, length(vAges))
      } else {
        vR2_f3 = apply(X = mTrain, MARGIN = 2,
                       FUN = function(vCol) {
                         lFit = lm(vCol ~ mFactor_c[, 3])
                         return(summary(lFit)$r.squared)
                       })
      }
      
      
      # Make a matrix for country factors #
      mR2_c = cbind(vR2_f1, vR2_f2, vR2_f3)
      
      # Print dimensions #
      # cat("Length of factor1:", length(vR2_f1), "\n")
      # cat("Length of factor2:", length(vR2_f2), "\n")
      # cat("Length of factor2:", length(vR2_f3), "\n")
      # cat("Dim of mR2_c:", dim(mR2_c), "\n")
      
      # Change the names to the appropriate names #
      colnames(mR2_c) = c("Country factor 1", "Country factor 2", "Country factor 3")
      rownames(mR2_c) = vAges
      
      # Create named list #
      lR2 = list(all = vR2_mult,
                 global = vR2_glob,
                 country = mR2_c)
      
      
      # Make a matrix for predictions #
      mPred = matrix(data = NA,
                     nrow = 46, ncol = 17,
                     dimnames = list(50:95, paste0(2000:2016)))
      
      # Extract true values #
      mTrue = lSplit_matlab[[sSub]][["test"]][,, sC]
      mTrue = exp(t(mTrue))
      
      # Make first forecast #
      G_val = RW_g + tail(vFactor_g, 1)
      F_val = RW_c + tail(mFactor_c, 1)
      vPred = mLoad_g %*% G_val + mLoad_c %*% t(F_val)
      
      # Transfrom from log to level #
      vPred = exp(vPred)
      
      # Assign to prediction matrix column #
      mPred[, paste0(2000)] = vPred
      
      # Loop over remaining forecasting years #
      for (iYear in 2001:2016) {
        
        # Calculate global factor value #
        G_val = RW_g + G_val
        
        # Calculate country factor value #
        F_val = RW_c + F_val
        
        # Make predictions #
        vPred = mLoad_g %*% G_val + mLoad_c %*% t(F_val)
        
        # Transform from log to level #
        vPred = exp(vPred)
        
        # Assign to prediction matrix #
        mPred[, paste0(iYear)] = vPred
        
      } # End forecasting year loop #
      
      # Calculate squared errors for population #
      mErr2_pop = (as.matrix(mTrue) - as.matrix(mPred))^2
      
      # Calculate MSE for population #
      dMSE_pop = mean(mErr2_pop)
      
      # Calculate RMSE for population #
      dRMSE_pop = sqrt(dMSE_pop)
      
      # Assign results to result list #
      lRes[[sC]][[sSub]][["pred"]] = mPred
      lRes[[sC]][[sSub]][["true"]] = mTrue
      lRes[[sC]][[sSub]][["MSE"]] = dMSE_pop
      lRes[[sC]][[sSub]][["RMSE"]] = dRMSE_pop
      lRes[[sC]][[sSub]][["R2"]] = lR2
      
    } # End country loop #
    
  } # End subpopulation loop #
  
  # Extract MSE's * 10^4 as a vector #
  vMSE = as.vector(sapply(lRes, function(x) {
    sapply(x, function(y) {
      y$MSE * 10^4
    })
  }))
  
  # Calculate average MSE #
  dAvg_MSE = mean(vMSE)
  
  # Calculate median MSE #
  dMed_MSE = median(vMSE)
  
  # Create output list #
  lOut = list(results = lRes,
              Avg_MSE = dAvg_MSE,
              Med_MSE = dMed_MSE,
              models = lModel)
  
  # Return the output list #
  return(lOut)
  
}


