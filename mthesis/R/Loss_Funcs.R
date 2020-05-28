# Loss functions #
# Input a matrix with ages in rows and years in the columns #
f_MatMSE <- function(mTrue, mPred, bRoot = FALSE) {
  
  # Calculate matrix of squared errors #
  mErr2 = (as.matrix(mTrue) - as.matrix(mPred))^2
  
  # Calculate the mean squared erorr #
  dOut = mean(mErr2)
  
  # Check if RMSE is wanted #
  if (bRoot) {
    dOut = sqrt(dOut)
  }
  
  return(dOut)
  
}

# Calculate MSE, RMSE for all populations, calculate average and median MSE and RMSE for all #
f_ResAllPop_sp <- function(lRes, bSVD = FALSE) {
  
  # Create matrix to store the results #
  mRes = matrix(nrow = 0, ncol = 4)
  
  # Find names of countries #
  vCountries = names(lRes)
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Find subpopulations #
    vSubpop = names(lRes[[sC]])
    
    for (sSub in vSubpop) {
      
      mTrue = lRes[[sC]][[sSub]][["test"]]
      mPred = lRes[[sC]][[sSub]][["forecast"]][["rates"]]
      
      # Check if SVD results, because they are different #
      if (bSVD) {
        mPred = lRes[[sC]][[sSub]][["forecast"]][["rate"]][[sSub]]
      }
      
      MSE = f_MatMSE(mTrue = mTrue, mPred = mPred)
      
      RMSE = f_MatMSE(mTrue = mTrue, mPred = mPred,
                      bRoot = TRUE)
      
      
      # Append results #
      mRes = rbind(mRes, c(sC, sSub, MSE, RMSE))
      
    }
    
    
  }
  
  # Write the columnnames #
  colnames(mRes) = c("Country", "Subpop", "MSE", "RMSE")
  
  # Return the result matrix #
  return(mRes)
  
}


# Calculate for all files #
# Input a list of files #
# Output list of result matrices #
f_ResAll_sp = function(lResFiles) {
  
  # Create output list #
  lRes_out = list()
  
  # Loop over all the files #
  for (sFile in lResFiles) {
    
    # Load the file and save the name #
    sName = load(sFile)
    
    # Get the list #
    lRes = get(sName)
    
    # Remove the loaded list #
    rm(list = c(sName))
    
    # Compute results #
    bSVD = ifelse(sName == "LC_SVD", TRUE, FALSE)
    mRes = f_ResAllPop_sp(lRes = lRes, bSVD = bSVD)
    
    # Put results on list #
    lRes_out[[sName]] = mRes
    
  }
  
  return(lRes_out)
  
}


# Count how many time each model is the best performing model #
# Input a list where each list element is the given model and inside each list element is a matrix #
# Matrix has the columns Country, Subpop, MSE, RMSE #

sapply_find_func <- function(mX, sC, sSub) {
  
  as.numeric(mX[(mX[, "Country"] == sC) & (mX[, "Subpop"] == sSub), "MSE"]) * 10000
  
}

f_Count_sp <- function(lRes) {
  
  # Number of models #
  iN_mod = length(lRes)
  
  # Create a counter matrix #
  mCount = data.frame(matrix(nrow = iN_mod, ncol = 4))
  
  # Populate the first column with the names of the models #
  mCount[, 1] = names(lRes)
  
  # Set the counter to zero for the second column #
  mCount[, 2] = 0
  
  mCount[, 2] = as.numeric(mCount[, 2])
  
  # Define the countries to search over #
  vCountries = unique(lRes[[1]][, "Country"])
  
  # Define subpopulations to search over #
  vSub = unique(lRes[[1]][, "Subpop"])
  
  # Matrix for the MSE's #
  mMSE = matrix(nrow = 0, ncol = length(lRes))
  
  
  # Loop over countries and subpopulations #
  for (sC in vCountries) {
    for (sSub in vSub) {
      
      # Find MSE for each model for the given country and subpopulation #
      vMSE = sapply(lRes, sapply_find_func, sC = sC, sSub = sSub)
      
      # Add MSE to the MSE matrix #
      mMSE = rbind(mMSE, vMSE)
      
      vIdxMin = which(vMSE == min(vMSE))
      
      mCount[vIdxMin, 2] = as.numeric(mCount[vIdxMin, 2]) + 1
      
    }
  }
  
  # Calculate average and meadian mse #
  mCount[, 3] = apply(mMSE, MARGIN = 2, FUN = mean)
  mCount[, 4] = apply(mMSE, MARGIN = 2, FUN = median)
  
  # Define column names #
  colnames(mCount) = c("Model", "nBest", "Avg. MSE x10000", "Median MSE x10000")
  
  # Return the output matrix #
  return(mCount)
  
}

# Function that can extract average and median MSE #
f_MSE_extract <- function(lRes) {
  
  vMSE = as.vector(sapply(lRes, function(x) {
    sapply(x, function(y) {
      y$MSE * 10^4
    })
  }))
  
  # Create output list #
  lOut = list(mean = mean(vMSE),
              median = median(vMSE))
  
  # Return output list #
  return(lOut)
  
}

# MSE counter #
f_MSE_count <- function(sModel_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/Results") {
  
  # Get the files for the model results #
  vModels = list.files(path = sModel_dir, recursive = TRUE)
  
  # Create a list to store the results of each model #
  lRes = list()
  
  # Create a matrix for average and median MSE #
  mMSE = matrix(data = NA, nrow = 2, ncol = length(vModels),
                dimnames = list(c("Avg_MSE", "Med_MSE"), vModels))
  
  # Create a matrix for counting #
  mCount = matrix(data = 0, nrow = 1, ncol = length(vModels),
                  dimnames = list(NULL, vModels))
  
  # Create a temporary matrix for the subpopulation MSE #
  mTemp_MSE = matrix(data = 0, nrow = 1, ncol = length(vModels),
                     dimnames = list(NULL, vModels))
  
  # Loop over files to get the results #
  for (sModel in vModels) {
    
    # Define the file name #
    model_file = paste0(sModel_dir, "/", sModel)
    
    # Load the results #
    sLoad = load(model_file)
    
    # define model results #
    lMod_res = get(sLoad)
    
    # Store on result list #
    lRes[[sModel]] = lMod_res
    
    # Put avg and med mse on mse matrix #
    mMSE["Avg_MSE", sModel] = lMod_res[["Avg_MSE"]]
    mMSE["Med_MSE", sModel] = lMod_res[["Med_MSE"]]
    
  } # End model loop #
  
  # Define the vector of countries #
  vCountries = sort(unique(mthesis::df_mx_all[, "Country"]))
  
  # Define the subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Loop over the models #
      for (sModel in vModels) {
        
        # Put value on temp MSE matrix #
        mTemp_MSE[1, sModel] = lRes[[sModel]][["Results"]][[sC]][[sSub]][["MSE"]]
        
      } # End model loop #
      
      # Find which model has the lowest MSE #
      ind_min = which.min(mTemp_MSE[1, ])
      
      # Name of the best model #
      sBest_mod = colnames(mTemp_MSE)[ind_min]
      
      # Add to model count #
      mCount[1, sBest_mod] = mCount[1, sBest_mod] + 1
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Create output list #
  lOut = list(mCount = mCount,
              mMSE = mMSE)
  
  # Return the output list #
  return(lOut)
  
  
}