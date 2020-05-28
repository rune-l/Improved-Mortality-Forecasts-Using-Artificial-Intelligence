######################################################################
####################         New Function         ####################
######################################################################
# ML forecasting #
f_RF_recursive_one_sp <- function(lModel, mtest_obs, mTrue, bLog = FALSE, ...) {
  
  # Create new column for predictions in mTrue #
  mTrue[, "Pred"] = NA
  
  # Find the years we are forecasting for #
  vYears = sort(unique(mTrue$Year), decreasing = FALSE)
  
  # Create a vector for the ages #
  vAges = sort(unique(mTrue$Age), decreasing = FALSE)
  
  # Loop over years and ages #
  for (iYear in vYears) {
    
    # Change predict if we work with the xgb model #
    if (class(lModel)[1] == "xgb.Booster") {
      vPredict = predict(lModel, newdata = f_DataXGB(mtest_obs, NULL), ...)
    } else {
      vPredict = predict(lModel, newdata = mtest_obs, ...)
    }
    
    # Check if names are NULL #
    if (is.null(names(vPredict))) {
      names(vPredict) = vAges
    }
    
    for (iAge in vAges) {
      
      # Insert predicted values into mTrue matrix #
      mTrue[(mTrue$Year == iYear) & (mTrue$Age == iAge), "Pred"] = vPredict[as.numeric(names(vPredict)) == iAge]
      
      # Check if vY is log or not #
      if (bLog) {
        mTrue[(mTrue$Year == iYear) & (mTrue$Age == iAge), "Pred"] = 
          exp(vPredict[as.numeric(names(vPredict)) == iAge])
      }
      
      
      # Overwrite the mtest_obs values #
      mtest_obs[mtest_obs$Age == iAge, "value"] = vPredict[as.numeric(names(vPredict)) == iAge]
      
    }
  }
  
  # Calculate the MSE and RMSE #
  vErr2 = (mTrue[, "value"] - mTrue[, "Pred"])^2
  MSE = mean(vErr2)
  RMSE = sqrt(MSE)
  
  # Create output list #
  lOut = list(mTrue = mTrue, MSE = MSE, RMSE = RMSE)
  
  # Return output list #
  return(lOut)
  
}

######################################################################
####################         New Function         ####################
######################################################################
# Recursive forecasting for all population in single population models #
f_RF_recursive_all_sp <- function(mx_df, f_Fit, bLog = FALSE, ...) {
  
  # Transform into ML format #
  df_full = f_ML_transform(df_full = mx_df)
  
  # Make list to store outputs on #
  lOut = list()
  
  # Find all the countries #
  vCountries = unique(df_full$Country)
  
  # Find all the subpopulations #
  vSub = unique(df_full$variable)
  
  # Loop over all countries and subpopulations #
  for (sC in vCountries) {
    for (sSub in vSub) {
      
      # Print progress statement #
      cat("Now working on country:", sC, "and subpopulation:", sSub, "\n")
      
      # Extract dataframe for the population #
      df_pop = f_ML_single_pop(df_full, sC = sC, sSub = sSub)
      
      # Split the data into training and test data #
      lSplit = f_ML_split(df_pop, iTest_start = 2000, iLag = 1)
      
      # Extract training X and Y #
      mX_train = subset(lSplit$train, select = -c(vY))
      vY_train = lSplit$train[, "vY"]
      # If the Y-variable should be Log or not #
      if (bLog) {
        mX_train[, "value"][mX_train[, "value"] == 0] = 1e-04
        mX_train[, "value"] = log(mX_train[, "value"])
        
        vY_train[vY_train == 0] = 1e-04
        vY_train = log(vY_train)
        
      }
      
      # Remove constant columns from mX_train #
      mX_train_names = names(mX_train)
      vLen_check = apply(mX_train, 2, function(vCol) length(unique(vCol)))
      if(any(vLen_check == 1)) {
        cat("Removing column(s):", mX_train_names[which(vLen_check == 1)], "\n")
      }
      mX_train[,vLen_check != 1]
      
      # Define the test observations #
      mTest_obs = lSplit$test_obs
      
      # Check for log #
      if (bLog) {
        mTest_obs[, "value"][mTest_obs[, "value"] == 0] = 1e-04
        mTest_obs[, "value"] = log(mTest_obs[, "value"])
      }
      
      # Fit the model #
      lFit = f_Fit(mX_train, vY_train)
      
      # Predict for the given population #
      lPred = f_RF_recursive_one_sp(lModel = lFit, mtest_obs = mTest_obs, mTrue = lSplit$test,
                                    bLog = bLog, ...)
      
      # Put onto result list #
      lOut[[sC]][[sSub]] = lPred
      
    }
  }
  
  # Return the output list #
  return(lOut)
  
}

######################################################################
####################         New Function         ####################
######################################################################
# Multipopulation forecast #
f_Recursive_MP <- function(lSplit, f_Fit, bLog = FALSE, ...) {
  
  # Require the reshape package #
  require(reshape)
  
  # Make list to store outputs on #
  lOut = list()
  
  # Extract mX and vY #
  mX_train = subset(lSplit$train, select = -c(vY))
  vY_train = lSplit$train[, "vY"]
  
  # Check if Y variable should be Log #
  if (bLog) {
    mX_train[, "value"][mX_train[, "value"] == 0] = 1e-04
    mX_train[, "value"] = log(mX_train[, "value"])
    
    vY_train[vY_train == 0] = 1e-04
    vY_train = log(vY_train)
  }
  
  # Change country variable into a factor variable #
  mX_train[, "Country"] = as.factor(mX_train[, "Country"])
  
  # Remove constant columns #
  mX_train_names = names(mX_train)
  vLen_check = apply(mX_train, 2, function(vCol) length(unique(vCol)))
  if(any(vLen_check == 1)) {
    cat("Removing column(s):", mX_train_names[which(vLen_check == 1)], "\n")
  }
  mX_train[,vLen_check != 1]
  
  # Train the model #
  cat("Fitting the model....", "\n")
  lFit = f_Fit(mX_train, vY_train)
  
  # Find years to predict for #
  vYears_test = sort(unique(lSplit$test$Year), decreasing = FALSE)
  
  # Extract the test observations #
  mTest_obs = lSplit$test_obs
  if (bLog) {
    mTest_obs[, "value"][mTest_obs[, "value"] == 0] = 1e-04
    mTest_obs[, "value"] = log(mTest_obs[, "value"])
  }
  
  # Change country column to a factor variable #
  mTest_obs[, "Country"] = as.factor(mTest_obs[, "Country"])
  
  # Create a matrix for predictions #
  mPred = subset(lSplit$test_obs, select = -c(Year))
  
  for (iYear in vYears_test) {
    
    # Make predictions #
    cat("Making predictions for year:", iYear, "\n")
    
    # Change predict if we work with the xgb model #
    if (class(lFit)[1] == "xgb.Booster") {
      vPred = predict(lFit, newdata = f_DataXGB(mTest_obs, NULL),...)
    } else {
      vPred = predict(lFit, newdata = mTest_obs, ...)
    }
    
    # Assign predictions as the new test values #
    mTest_obs$value = vPred
    
    
    # Save the predictions for the year #
    # Again check for log #
    if (bLog) {
      mPred[, paste0(iYear)] = exp(vPred)
    } else {
      mPred[, paste0(iYear)] = vPred
    }
    
  }
  
  # Change name in mPred #
  vName_pred = colnames(mPred)
  iInd = which(vName_pred == "variable")
  vName_pred[iInd] = "Subpop"
  iInd_2 = which(vName_pred == "value")
  vName_pred[iInd_2] = "TestTrue"
  colnames(mPred) = vName_pred
  
  
  # Loop over countries and subpopulations to structure the output list #
  vCountries = unique(lSplit$train$Country)
  vSub = unique(lSplit$train$variable)
  
  for (sC in vCountries) {
    for (sSub in vSub) {
      
      # Print statement #
      cat("Splitting data for country:", sC, "and subpopulation:", sSub, "\n")
      
      # Subtract dataframe for this country and subpopulation #
      mTmp = mPred[(mPred$Country == sC) & (mPred$Subpop == sSub), ]
      
      # Melt the dataframe #
      mTmp = reshape::melt(mTmp, id.vars = c("Age", "Subpop", "Country", "TestTrue"),
                           measured.vars = paste0(vYears_test))
      
      # Extract the column-names #
      vTmp_name = colnames(mTmp)
      
      # Rename the variable column to Year #
      vTmp_ind_1 = which(vTmp_name == "variable")
      vTmp_name[vTmp_ind_1] = "Year"
      
      # Rename the value column to Pred #
      vTmp_ind_2 = which(vTmp_name == "value")
      vTmp_name[vTmp_ind_2] = "Pred"
      
      # Rename the Subpop column to variable #
      vTmp_ind_3 = which(vTmp_name == "Subpop")
      vTmp_name[vTmp_ind_3] = "variable"
      
      # Rename columns #
      colnames(mTmp) = vTmp_name
      
      # Change the Year column to an integer vector #
      # mTmp[, "Year"] = as.integer(mTmp[, "Year"])
      
      # Extract true values #
      mTrue = lSplit$test[(lSplit$test$Country == sC) & (lSplit$test$variable == sSub), ]
      
      # Create column for predictions #
      mTrue[, "Pred"] = NA
      
      vYears_true = sort(unique(mTrue[, "Year"]), decreasing = FALSE)
      vAges_true = sort(unique(mTrue[, "Age"]), decreasing = FALSE)
      
      # Loop over years and ages #
      for (iYear in vYears_true) {
        for (iAge in vAges_true) {
          mTrue[(mTrue$Year == iYear) & (mTrue$Age == iAge), "Pred"] =
            mTmp[(mTmp$Year == iYear) & (mTmp$Age == iAge), "Pred"]
        }
      }
      
      # Assign onto output list #
      lOut[[sC]][[sSub]][["mTrue"]] = mTrue
      
      # Calculate MSE #
      vErr2 = (mTrue$value - mTrue$Pred)^2
      dMSE = mean(vErr2)
      
      # Calculate RMSE #
      dRMSE = sqrt(dMSE)
      
      # Assign onto output list #
      lOut[[sC]][[sSub]][["MSE"]] = dMSE
      lOut[[sC]][[sSub]][["RMSE"]] = dRMSE
      
    }
  }
  
  # Return the output list #
  return(lOut)
  
  
}

######################################################################
####################         New Function         ####################
######################################################################
# Regional model fitting and forecasting (recursive forecast) #
f_Recursive_Region <- function(lSplit_region, f_Fit, bLog = FALSE, ...) {
  
  # Create empty output list to concatinate to #
  lOut = list()
  
  # Loop over the list elements in lSplit_region
  for (lRegion in lSplit_region) {
    
    # Fit and forecast #
    lOut_region = f_Recursive_MP(lSplit = lRegion, f_Fit = f_Fit, bLog = bLog, ...)
    
    # Concatenate to the output #
    lOut = c(lOut, lOut_region)
    
  }
  
  # Return the output list #
  return(lOut)
  
}

