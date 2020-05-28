# Function to transform the data #

# Input a data frame with data for the specific countries (can be found in the lists in the package)
# Columns in the dataframe is Year, Age, Male, Female, Total #
# Define the subpopulation you want #

# Output #
# Redefined data frame where the columns are the Years and rows are ages and numbers are the wanted subpopulation #

f_DataTransform <- function(df, sSubpop = "Total") {
  
  # Require needed packages #
  require(reshape)
  
  # Only work with relevant columns #
  df_out = cast(melt.data.frame(df, id.vars = c("Age", "Year"), measure.vars = sSubpop), Age ~ Year)
  row.names(df_out) = df_out$Age
  df_out = df_out[, -(names(df_out) %in% c("Age"))]
  
  # Return the output #
  return(df_out)
  
}

# Transform to demogdata #
f_demogdata <- function(df_mx, df_ex, sC, sSubpop = "Total") {
  
  # Require the demography package #
  require(demography)
  
  # Create demogdata #
  df_out = demography::demogdata(data = df_mx,
                                 pop = df_ex,
                                 ages = as.numeric(row.names(df_mx)),
                                 years = as.numeric(colnames(df_mx)),
                                 type = "mortality",
                                 label = sC,
                                 name = sSubpop,
                                 lambda = 0)
  
  # Output the demogdata #
  return(df_out)
  
  
}

# Transform to StMoMo data #
f_StMoMoData <- function(df_mx, df_ex, sC, sSubpop = "Total") {
  
  # Require the StMoMo package #
  require(StMoMo)
  
  # First input data into the demography package #
  df_out = f_demogdata(df_mx = df_mx, df_ex = df_ex, sC = sC, sSubpop = sSubpop)
  
  # Read into StMoMo data #
  df_out = StMoMo::StMoMoData(df_out, type = "central")
  
  return(df_out)
  
}

# Test-train split #
# Input dataframe including all data (rows are ages and columns are years) #
# iTest_start = The first year that is training data #
f_train_split <- function(df, iTest_start = 2000) {
  
  # Get a numeric of the columns #
  vYears = as.numeric(colnames(df))
  
  # Create train and test dataframes #
  df_train = df[, vYears < iTest_start]
  
  df_test = df[, vYears >= iTest_start]
  
  # Return a list with both #
  lOut = list("train" = df_train,
              "test" = df_test)
  
  return(lOut)
  
}


# Transform into the machine learning format #
# Input dataframe with columns "Year", "Age", "Female", "Male", "Total", "Country"
# Output dataframe has columns "Year", "Age", "Country", "variable", "value"
f_ML_transform <- function(df_full) {
  
  # Require the reshape package #
  require(reshape)
  
  # Molt the values together #
  df_melt = reshape::melt(df_full, id.vars = c("Year", "Age", "Country"),
                           measured.vars = c("Female", "Male", "Total"))
  
  return(df_melt)
  
}

f_ML_single_pop <- function(df_full, sC, sSub) {
  
  df_single = df_full[(df_full[, "Country"] == sC) & (df_full[, "variable"] == sSub), c("Year", "Age", "value")]
  
  return(df_single)
  
}


# xgboost transform #
f_DataXGB <- function(mX, vY) {
  
  # Require the xgboost package #
  require(xgboost)
  
  if ("Country" %in% colnames(mX)) {
    mX[, "Country"] = as.integer(as.factor(mX[, "Country"]))
  }
  if ("variable" %in% colnames(mX)) {
    mX[, "variable"] = as.integer(as.factor(mX[, "variable"])) 
  }
  
  mX = as.matrix(mX)
  
  # Create xgb data #
  if (is.null(vY)) {
    mOut = xgb.DMatrix(data = mX)
  } else {
    mOut = xgb.DMatrix(data = mX, label = vY)
  }
  
  
  # Return the xgb data #
  return(mOut)
  
  
}

# Function to make train-test split for ML data #
f_ML_split <- function(mDf, iTest_start = 2000, iLag = 1) {
  
  # Split based on the year variable #
  mTrain = subset(mDf, Year < iTest_start)
  mTest = subset(mDf, Year >= iTest_start)
  
  # Make the Y variable in the training data #
  lTrain = f_Create_Lag(mTrain, iLag = iLag)
  
  # Create output list #
  lOut = list(train = lTrain$train, test = mTest, test_obs = lTrain$test_obs)
  
  # Return output list #
  return(lOut)
  
}


f_Create_Lag <- function(mTrain, iLag = 1) {
  
  mTrain[, "vY"] = NA
  
  vYears = sort(unique(mTrain[, "Year"]), decreasing = FALSE)
  vAges = sort(unique(mTrain[, "Age"]), decreasing = FALSE)
  
  # Check if there are different subpopulations #
  vSubCheck = "variable" %in% colnames(mTrain)
  
  if (vSubCheck) {
    
    # The subpopulations #
    vSub = unique(mTrain[, "variable"])
    # The countries #
    vCountries = unique(mTrain[, "Country"])
    
    iC = 1
    iC_len = length(vCountries)
    
    for (sC in vCountries) {
      cat("Working on Country:", sC, 
          paste0("(", round(iC/iC_len, 2) * 100, "% done)"), "\n")
      for (sSub in vSub) {
        vYears = sort(unique(mTrain[(mTrain$Country == sC) & (mTrain$variable == sSub), "Year"]),
                      decreasing = FALSE)
        vAges = sort(unique(mTrain[(mTrain$Country == sC) & (mTrain$variable == sSub), "Age"]),
                     decreasing = FALSE)
        for (iYear in head(vYears, -iLag)) {
          for (iAge in vAges) {
            mTrain[(mTrain$Year == iYear) & (mTrain$Age == iAge) & (mTrain$variable == sSub) &
                   (mTrain$Country == sC), "vY"] =
              mTrain[(mTrain$Year == (iYear + 1)) & (mTrain$Age == iAge) & (mTrain$variable == sSub) &
                     (mTrain$Country == sC), "value"]
          }
        }
      }
      iC = iC + 1
    }
    
    
  } else {
    
    for (iYear in head(vYears, -iLag)) {
      for (iAge in vAges) {
       mTrain[(mTrain$Year == iYear) & (mTrain$Age == iAge), "vY"] = 
         mTrain[(mTrain$Year == (iYear + 1)) & (mTrain$Age == iAge), "value"]
      }
    }
    
    
  }
  
  # Create the first test observation values #
  mTest_obs = subset(mTrain[mTrain$Year == tail(vYears, 1), ], select = -c(vY))
  if (!vSubCheck) {
    row.names(mTest_obs) = mTest_obs$Age
  }
  
  
  # Create mTrain without the NA rows #
  mTrain = mTrain[complete.cases(mTrain), ]
  
  # Output list #
  lOut = list(train = mTrain, test_obs = mTest_obs)
  
  # Return output list #
  return(lOut)
  
}


# Function to recast the ML test data #
f_mTrue_cast <- function(mTest) {
  
  # Require the reshape package #
  require(reshape)
  
  # Reshape the dataframe #
  mCast_df = cast(data = mTest,
                  formula = Age + Country + variable ~ Year,
                  measure.vars = value)
  
  return(mCast_df)
  
}

# Min-max scaler #
f_MinMaxScaler <- function(vX_train, vX_test) {
  
  # Find minimum for training data #
  dMin = min(vX_train)
  
  # Find maximum for training data #
  dMax = max(vX_train)
  
  # Scale training data #
  vX_train_scaled = (vX_train - dMin) / (dMax - dMin)
  
  # Scale test data #
  vX_test_scaled = (vX_test - dMin) / (dMax - dMin)
  
  # Create output list #
  lOut = list(train = vX_train_scaled,
              test = vX_test_scaled,
              min = dMin,
              max = dMax)
  
  # Return output list #
  return(lOut)
  
}

# Z-score scaling #
f_ZscoreScaler <- function(vX_train, vX_test) {
  
  # Find mean of training data #
  dMean = mean(vX_train)
  
  # Find standard-deviation of training data #
  dSd = sd(vX_train)
  
  # Scale training data #
  vX_train_scaled = (vX_train - dMean) / dSd
  
  # Scale test data #
  vX_test_scaled = (vX_test - dMean) / dSd
  
  # Create output list #
  lOut = list(train = vX_train_scaled,
              test = vX_test_scaled,
              mean = dMean,
              sd = dSd)
  
  # Return output list #
  return(lOut)
  
}


# Inverse min-max scaling #
f_MinMaxScaler_inv <- function(vX_scaled, dMin, dMax) {
  
  # Calculate original variable #
  vX = vX_scaled * (dMax - dMin) + dMin
  
  # Return unscaled series #
  return(vX)
  
}

# Inverse z-score scaling #
f_ZscoreScaler_inv <- function(vX_scaled, dMean, dSd) {
  
  # Calculate unscaled variable #
  vX = vX_scaled * dSd + dMean
  
  # Return unscaled variable #
  return(vX)
  
}

# Turn data into a 3D array #
f_3D_data <- function(df_full, bNumeric = TRUE) {
  
  # Require the reshape package #
  require(reshape)
  
  # Extract all countries #
  vCountries = unique(df_full[, "Country"])
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Transform the dataframe to ML format #
  df_ML = f_ML_transform(df_full)
  
  # Check numeric boolean #
  if (bNumeric) {
    
    # Transform country column into a factor variable #
    df_ML[, "Country"] = as.factor(df_ML[, "Country"])
    
    # # Transform country column from factor to integer variable #
    # df_ML[, "Country"] = as.integer(df_ML[, "Country"])
    # 
    # # Transform variable into from factor to integer variable #
    # df_ML[, "variable"] = as.integer(df_ML[, "variable"])
    
  }
  
  # Extract column names #
  vColnames = colnames(df_ML)
  
  # Create a list to put results on #
  lData = list()
  
  # Loop over the column names #
  for (sCol in vColnames) {
    
    # Print statement #
    cat("Working on column:", sCol, "\n")
    cat("--------------------------------------------------", "\n")
    
    # Create an empty dataframe #
    lData[[sCol]] = setNames(data.frame(matrix(ncol = 67, nrow = 0)), paste0(unique(df_full[, "Year"])))
    
    # Loop over countries #
    for (sC in vCountries) {
      
      # Print statement #
      cat("Working on country:", sC, "\n")
      
      # Loop over subpopulations #
      for (sSub in vSub) {
        
        # Extract dataframe for population #
        df_pop = df_ML[(df_ML[, "Country"] == sC) &
                       (df_ML[, "variable"] == sSub), ]
        
        # Check if numeric #
        if (bNumeric) {
          
          # Extract country number #
          iCountry = which(vCountries == sC)
          
          # Extract variable number #
          iSub = which(vSub == sSub)
          
          # Assign number to country column #
          df_pop[, "Country"] = iCountry
          
          # Assign number to variable column #
          df_pop[, "variable"] = iSub
          
        }
        
        # Melt dataframe #
        mMelt = melt.data.frame(df_pop, id.vars = c("Age", "Year"), measure.vars = sCol)
        
        # Cast dataframe #
        mDf = cast(mMelt, Age ~ Year)
        
        # Rename the rows to the ages #
        # row.names(mDf) = mDf$Age
        
        # Remove the age column that is created #
        mDf = mDf[, -(names(mDf) %in% c("Age"))]
        
        # Assign the data to the lData list #
        lData[[sCol]] = rbind(lData[[sCol]], mDf)
        
      } # End subpopulation loop #
        
    } # End country loop #
    
    # Print line #
    cat("--------------------------------------------------", "\n")
    
  } # End colname loop #
  
  # Check if numeric #
  if (bNumeric) {
    
    # Print statement #
    cat("Converting data to array....", "\n")
    
    # Transform data from list to array #
    aData = array(as.numeric(unlist(lData)), dim = c(3174, 67, 5),
                  dimnames = list(NULL,
                                  paste0(unique(df_ML[, "Year"])),
                                  vColnames))
    
    # Return array #
    return(aData)
    
  } else {
    
    # Return data as a list #
    return(lData)
    
  }
  
}

# Function to split array into train and test #
f_3D_train_split <- function(aData, iTest_Year = 2000) {
  
  # Define the training years #
  vTrainYears = paste0(1950:(iTest_Year - 1))
  
  # Define the test observation year #
  vTest_obs_Years = tail(vTrainYears, 1)
  
  # Define the test years #
  vTest_Years = paste0(iTest_Year:2016)
  
  # Create the training array #
  aTrain = aData[, vTrainYears, ]
  
  # Create test_obs array #
  aTest_obs = aData[, vTest_obs_Years, ]
  
  # Create the test array #
  aTest = aData[, vTest_Years, ]
  
  # Create the output list #
  lSplit = list(train = aTrain,
                test_obs = aTest_obs,
                test = aTest)
  
  # Return the output list #
  return(lSplit)
  
}

# Function to transform into array format #
f_Matlab_format <- function(mx_list) {
  
  # Extract countries #
  vCountries = names(mx_list)
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Extract years #
  vYears = unique(mx_list[["AUS"]][, "Year"])
  
  # Extract ages #
  vAges = paste0(unique(mx_list[["AUS"]][, "Age"]))
  
  # Create list for output #
  lData = list()
  
  # Create list for array #
  lArray = list()
  
  # Create list for train/test split #
  lSplit = list()
  
  # Loop over subpopulations #
  for (sSub in vSub) {
    
    # Create list element for subpopulation #
    lData[[sSub]] = list()
    
    # Loop over countries #
    for (sC in vCountries) {
      
      # Extract data frame for country #
      mDf = f_DataTransform(mx_list[[sC]], sSubpop = sSub)
      
      # Take log-transformation #
      mDf = log(mDf)
      
      # Transpose data #
      t_mDf = t(mDf)
      
      # Append to list #
      lData[[sSub]][[sC]] = t_mDf
      
    } # End country loop #
    
    # Transform to array #
    aData = array(as.numeric(unlist(lData[[sSub]])), dim = c(67, 46, 23),
                  dimnames = list(vYears,
                                  vAges,
                                  vCountries))
    
    # Assign to list #
    lArray[[sSub]] = aData
    
    # Make training and test indices #
    vInd_train = (vYears < 2000)
    vInd_test = (vYears >= 2000)
    
    # Create split #
    lSplit[[sSub]][["train"]] = aData[vInd_train, , ]
    lSplit[[sSub]][["test"]] = aData[vInd_test, , ]
    
  } # End subpopulation loop #
  
  # Return lSplit list #
  return(lSplit)
  
}

# Function to add Global and country factors to array of results #
f_aData_add_factors <- function(aData, lML_FM, bLoadings = FALSE) {
  
  # Extract dimensions of aData #
  aDim = dim(aData)
  
  # Add to the third dimension #
  aDim[3] = aDim[3] + 4
  
  # Extract dimension names #
  lDimnames = dimnames(aData)
  
  # Add to the third dimension #
  lDimnames[[3]] = c(lDimnames[[3]], "factor_g", "factor_c1", "factor_c2", "factor_c3")
  
  # Create new array with the correct dimensions #
  aData_new = array(NA, dim = aDim,
                    dimnames = lDimnames)
  
  # Add the old data to this new array #
  aData_new[,, 1:5] = aData
  
  # Extract models from factor results #
  lModel = lML_FM$models
  
  # Extract countries #
  vCountries = names(lModel)
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Calculate time factors for the test years using a RW with drift #
  # Loop over countries #
  for (sC in vCountries) {
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Get tail values of global factor #
      dTail_g = tail(lModel[[sC]][[sSub]][["factor_g"]], 1)
      
      # Get tail values of country factors #
      vTail_c = tail(lModel[[sC]][[sSub]][["factor_c"]], 1)
      
      # Extract random walk parameter #
      dRW_g = lModel[[sC]][[sSub]][["RW_g"]]
      vRW_c = lModel[[sC]][[sSub]][["RW_c"]]
      
      # Loop over the test years #
      for (iYear in 2000:2016) {
        
        # Calculate global value #
        dG_val = dRW_g + dTail_g
        
        # Calculate country values #
        vF_val = vRW_c + vTail_c
        
        # Update tail values #
        dTail_g = dG_val
        vTail_c = vF_val
        
        # Add to global #
        lModel[[sC]][[sSub]][["factor_g"]] = rbind(lModel[[sC]][[sSub]][["factor_g"]], dG_val)
        
        # Add to country factors #
        lModel[[sC]][[sSub]][["factor_c"]] = rbind(lModel[[sC]][[sSub]][["factor_c"]], vF_val)
        
      } # End loop over years #
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Check if with or without loadings #
  if (bLoadings) {
    
    # Loop over the rows in the array #
    for (iRow in 1:aDim[1]) {
      
      # Extract country for the row #
      sCountry_row = aData_new[iRow, 1, "Country"]
      
      # Extract subpopulation for the row #
      sSub_row = aData_new[iRow, 1, "variable"]
      
      # Extract age for the row (minus 49 to start in 1) #
      dAge_row = aData_new[iRow, 1, "Age"] - 49
      
      # Calculate global factor loading matrix (dim: 46x67) #
      mFL_g = lModel[[sCountry_row]][[sSub_row]][["loading_g"]] %*%
        t(lModel[[sCountry_row]][[sSub_row]][["factor_g"]])
      
      # Calculate country factor 1 loading matrix (dim: 46x67) #
      mFL_c1 = lModel[[sCountry_row]][[sSub_row]][["loading_c"]][, 1] %*%
        t(lModel[[sCountry_row]][[sSub_row]][["factor_c"]][, 1])
      
      # Calculate country factor 2 loading matrix (dim: 46x67) #
      mFL_c2 = lModel[[sCountry_row]][[sSub_row]][["loading_c"]][, 2] %*%
        t(lModel[[sCountry_row]][[sSub_row]][["factor_c"]][, 2])
      
      # Calculate country factor 2 loading matrix (dim: 46x67) #
      mFL_c3 = lModel[[sCountry_row]][[sSub_row]][["loading_c"]][, 3] %*%
        t(lModel[[sCountry_row]][[sSub_row]][["factor_c"]][, 3])
      
      # Assign global factor #
      aData_new[iRow, ,"factor_g"] = mFL_g[dAge_row, ]
      
      # Assign country factor 1 #
      aData_new[iRow, ,"factor_c1"] = mFL_c1[dAge_row, ]
      
      # Assign country factor 2 #
      aData_new[iRow, ,"factor_c2"] = mFL_c2[dAge_row, ]
      
      # Assign country factor 3 #
      aData_new[iRow, ,"factor_c3"] = mFL_c3[dAge_row, ]
      
      # Calculate percentage done #
      dP_done = iRow/aDim[1] * 100
      
      # Print statement #
      cat(round(dP_done, 2), "% done in row loop", "\n")
      
    } # End row loop #
    
  } else { # If statement if yes to loadings #
    
    # Loop over the rows in the array #
    for (iRow in 1:aDim[1]) {
      
      # Extract Country for the row #
      sCountry_row = aData_new[iRow, 1, "Country"]
      
      # Extract subpopulation for the row #
      sSub_row = aData_new[iRow, 1, "variable"]
      
      # Assign to global factor #
      aData_new[iRow, ,"factor_g"] = lModel[[sCountry_row]][[sSub_row]][["factor_g"]]
      
      # Assign first country factor #
      aData_new[iRow, ,"factor_c1"] = lModel[[sCountry_row]][[sSub_row]][["factor_c"]][, 1]
      
      # Assign second country factor #
      aData_new[iRow, ,"factor_c2"] = lModel[[sCountry_row]][[sSub_row]][["factor_c"]][, 2]
      
      # Assign third country factor #
      aData_new[iRow, ,"factor_c3"] = lModel[[sCountry_row]][[sSub_row]][["factor_c"]][, 3]
      
      # Calculate percentage done #
      dP_done = iRow/aDim[1] * 100
      
      # Print statement #
      cat(round(dP_done, 2), "% done in row loop", "\n")
      
    } # End row loop #
    
    
  } # End if not loadings statement #
  
  # Return new data frame #
  return(aData_new)
  
}


# Function to transform aData #
f_aData_split <- function(aData,
                          iN_lags = 3) {
  
  # Create an output list for the results #
  lSplit = list(train = list(),
                test_obs = list(),
                test = list())
  
  # Find the starting year of training #
  iStart_train = 1950 + iN_lags - 1
  
  # Find ending year of training #
  iEnd_train = 1998
  
  # Find starting year of test_obs #
  iStart_testobs = 1998
  
  # Calculate the total number of training observations #
  iN_row = length(iStart_train:iEnd_train)
  iN_train = iN_row * 3174
  
  # Find the training array dimensions #
  vTrain_dim = dim(aData)
  vTrain_dim[1] = iN_train
  
  # Find dimension with values #
  iInd_val = which(dimnames(aData)[[3]] == "value")
  
  # Define aX #
  aX = aData[,, -iInd_val]
  
  # Define mY #
  mY = aData[,, iInd_val]
  
  # Create the training observations #
  lTrain = f_aData_train_fillC(aX = aX,
                               mY = mY,
                               iN_lags = iN_lags,
                               iN_row = iN_row)
  
  # Assign #
  lSplit[["train"]] = lTrain
  
  # Define test years #
  vTest_years = 2000:2016
  
  # Loop over testing years #
  for (iYear in vTest_years) {
    
    # Find appropriate aX values for the year #
    aX_testyear = aX[, paste0((iYear - iN_lags):(iYear - 1)), ]
    
    if (length(dim(aX_testyear)) == 2) {
      dim(aX_testyear) = c(dim(aX_testyear)[1], 1, dim(aX_testyear)[2])
    }
    
    mY_testyear = mY[, paste0(iYear)]
    
    # Assign values #
    lSplit[["test_obs"]][[paste0(iYear)]][["x"]] = aX_testyear
    lSplit[["test_obs"]][[paste0(iYear)]][["y"]] = mY_testyear
    
  }
  
  # Find test observations #
  lSplit[["test"]][["x"]] = aX[, paste0(vTest_years), ]
  lSplit[["test"]][["y"]] = mY[, paste0(vTest_years)]
  
  # Return output list #
  return(lSplit)
  
}

# Function for scaling #
f_aData_scale <- function(aData) {
  
  # Get the dimensions #
  vDim = dim(aData) 
  
  # Create vector to store mean results #
  vMean = numeric(vDim[3])
  
  # Create vector to store sd results #
  vSd = numeric(vDim[3])
  
  # Create scaled data #
  aData_scaled = aData
  
  # Loop over the third dimension #
  for (i in 1:vDim[3]) {
    
    # Calculate mean #
    vMean[i] = mean(aData[,, i])
    
    # Calculate sd #
    vSd[i] = sd(aData[,,i])
    
    # Scale data #
    aData_scaled[,, i] = (aData[,,i] - vMean[i])/vSd[i]
    
  }
  
  # Create output list #
  lOut = list(scaled = aData_scaled,
              mean = vMean,
              sd = vSd)
  
  # Return output list #
  return(lOut)
  
}

# Function to scale test obs #
f_testobs_scale <- function(aTest, vMean, vSd) {
  
  # Create scaled data array #
  aScaled = aTest 
  
  # Loop over third dimension #
  for (i in 1:dim(aTest)[3]) {
    
    # Scale feature i #
    aScaled[,,i] = (aTest[,,i] - vMean[i])/vSd[i]
    
    
  }
  
  # Return scaled data #
  return(aScaled)
  
  
}

# Add factors to lSplit #
f_lSplit_factors <- function(lSplit, lML_FM, bLoadings) {
  
  # Create new lSplit that is manipulated #
  lSplit_out = lSplit
  
  # Create new columns for training data filled with NA #
  lSplit_out$train[, "factor_g"] = NA
  lSplit_out$train[, "factor_c1"] = NA
  lSplit_out$train[, "factor_c2"] = NA
  lSplit_out$train[, "factor_c3"] = NA
  
  # Create new columns for test_obs data filled with NA #
  lSplit_out$test_obs[, "factor_g"] = NA
  lSplit_out$test_obs[, "factor_c1"] = NA
  lSplit_out$test_obs[, "factor_c2"] = NA
  lSplit_out$test_obs[, "factor_c3"] = NA
  
  # Create new columns for test data filled with NA #
  lSplit_out$test[, "factor_g"] = NA
  lSplit_out$test[, "factor_c1"] = NA
  lSplit_out$test[, "factor_c2"] = NA
  lSplit_out$test[, "factor_c3"] = NA
  
  # Extract models from factor results #
  lModel = lML_FM$models
  
  # Extract countries #
  vCountries = names(lModel)
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Define ages #
  vAges = 50:95
  
  # Define years #
  vYears = 1950:2016
  
  # Define matrix dimension names (Age x Time) #
  lDimname = list(vAges, paste0(vYears))
  
  # Create a counter for the loop #
  iCounter = 1
  
  # Define total counter #
  iTot_count = length(vCountries) * length(vSub)
  
  # Calculate time factors for the test years using a RW with drift #
  # Loop over countries #
  for (sC in vCountries) {
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Print statement #
      cat("Now working on country:", sC,
          "and subpopulation:", sSub, "\n")
      cat(paste0(round(iCounter/iTot_count,  2) * 100, "% done"), "....", "\n")
      
      # Update counter #
      iCounter = iCounter + 1
      
      # Get tail values of global factor #
      dTail_g = tail(lModel[[sC]][[sSub]][["factor_g"]], 1)
      
      # Get tail values of country factors #
      vTail_c = tail(lModel[[sC]][[sSub]][["factor_c"]], 1)
      
      # Extract random walk parameter #
      dRW_g = lModel[[sC]][[sSub]][["RW_g"]]
      vRW_c = lModel[[sC]][[sSub]][["RW_c"]]
      
      # Loop over the test years #
      for (iYear in 2000:2016) {
        
        # Calculate global value #
        dG_val = dRW_g + dTail_g
        
        # Calculate country values #
        vF_val = vRW_c + vTail_c
        
        # Update tail values #
        dTail_g = dG_val
        vTail_c = vF_val
        
        # Add to global #
        lModel[[sC]][[sSub]][["factor_g"]] = rbind(lModel[[sC]][[sSub]][["factor_g"]], dG_val)
        
        # Add to country factors #
        lModel[[sC]][[sSub]][["factor_c"]] = rbind(lModel[[sC]][[sSub]][["factor_c"]], vF_val)
        
      } # End loop over years #
      
      # Calculate matrix to add #
      if (bLoadings) {
        
        # Global factor #
        mG = lModel[[sC]][[sSub]][["loading_g"]] %*% t(lModel[[sC]][[sSub]][["factor_g"]]) 
        
        # Country factor 1 #
        mC_1 = lModel[[sC]][[sSub]][["loading_c"]][, 1] %*% t(lModel[[sC]][[sSub]][["factor_c"]][, 1])
        
        # Country factor 2 #
        mC_2 = lModel[[sC]][[sSub]][["loading_c"]][, 2] %*% t(lModel[[sC]][[sSub]][["factor_c"]][, 2])
        
        # Country factor 3 #
        mC_3 = lModel[[sC]][[sSub]][["loading_c"]][, 3] %*% t(lModel[[sC]][[sSub]][["factor_c"]][, 3])
        
      } else {
        
        # Global factor #
        mG = t(replicate(46, lModel[[sC]][[sSub]][["factor_g"]], simplify = "matrix"))
        
        # Country factor 1 #
        mC_1 = t(replicate(46, lModel[[sC]][[sSub]][["factor_c"]][, 1], simplify = "matrix"))
        
        # Country factor 2 #
        mC_2 = t(replicate(46, lModel[[sC]][[sSub]][["factor_c"]][, 2], simplify = "matrix"))
        
        # Country factor 3 #
        mC_3 = t(replicate(46, lModel[[sC]][[sSub]][["factor_c"]][, 3], simplify = "matrix"))
        
      }
      
      # Change dimnames #
      dimnames(mG) = lDimname
      dimnames(mC_1) = lDimname
      dimnames(mC_2) = lDimname
      dimnames(mC_3) = lDimname
      
      # Loop over training years #
      for (iYear in 1950:1998) {
        
        # Loop over ages #
        for (iAge in vAges) {
          
          # Create condition #
          vCond = (lSplit_out[["train"]][, "Year"] == iYear) &
                  (lSplit_out[["train"]][, "Age"] == iAge) &
                  (lSplit_out[["train"]][, "Country"] == sC) &
                  (lSplit_out[["train"]][, "variable"] == sSub)
            
          
          # Assign #
          lSplit_out[["train"]][vCond, "factor_g"] = mG[paste0(iAge), paste0(iYear)]
          lSplit_out[["train"]][vCond, "factor_c1"] = mC_1[paste0(iAge), paste0(iYear)]
          lSplit_out[["train"]][vCond, "factor_c2"] = mC_2[paste0(iAge), paste0(iYear)]
          lSplit_out[["train"]][vCond, "factor_c3"] = mC_3[paste0(iAge), paste0(iYear)]
          
        } # End loop over ages #
        
      } # End loop over years #
      
      # Define test_obs year #
      iYear = 1999
      
      # Loop over ages #
      for (iAge in vAges) {
        
        # Create condition #
        vCond = (lSplit_out[["test_obs"]][, "Year"] == iYear) &
          (lSplit_out[["test_obs"]][, "Age"] == iAge) &
          (lSplit_out[["test_obs"]][, "Country"] == sC) &
          (lSplit_out[["test_obs"]][, "variable"] == sSub)
        
        
        # Assign #
        lSplit_out[["test_obs"]][vCond, "factor_g"] = mG[paste0(iAge), paste0(iYear)]
        lSplit_out[["test_obs"]][vCond, "factor_c1"] = mC_1[paste0(iAge), paste0(iYear)]
        lSplit_out[["test_obs"]][vCond, "factor_c2"] = mC_2[paste0(iAge), paste0(iYear)]
        lSplit_out[["test_obs"]][vCond, "factor_c3"] = mC_3[paste0(iAge), paste0(iYear)]
        
        
      } # End age loop #
      
      # Loop over test years #
      for (iYear in 2000:2016) {
        
        # Loop over ages #
        for (iAge in vAges) {
          
          # Create condition #
          vCond = (lSplit_out[["test"]][, "Year"] == iYear) &
            (lSplit_out[["test"]][, "Age"] == iAge) &
            (lSplit_out[["test"]][, "Country"] == sC) &
            (lSplit_out[["test"]][, "variable"] == sSub)
          
          # Assign #
          lSplit_out[["test"]][vCond, "factor_g"] = mG[paste0(iAge), paste0(iYear)]
          lSplit_out[["test"]][vCond, "factor_c1"] = mC_1[paste0(iAge), paste0(iYear)]
          lSplit_out[["test"]][vCond, "factor_c2"] = mC_2[paste0(iAge), paste0(iYear)]
          lSplit_out[["test"]][vCond, "factor_c3"] = mC_3[paste0(iAge), paste0(iYear)]
          
        }
        
      } # End year loop #
      
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Return the new lSplit #
  return(lSplit_out)
  
  
}

