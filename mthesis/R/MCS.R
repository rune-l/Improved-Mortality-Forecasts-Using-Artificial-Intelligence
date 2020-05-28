# Model confidence set one age #
f_MCS_age <- function(sDir, dAlpha, loss = "SE",
                      save_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/MCS_results",
                      iStart_age = 50) {
  
  # Require the MCS package #
  require(MCS)
  require(parallel)
  
  # Stop if loss is not either "SE" or "AE" #
  if (!(loss %in% c("SE", "AE"))) {
    stop("Loss has to be SE or AE")
  }
  
  # Detect number of cores #
  n_cores = detectCores()
  
  # Make cluster #
  cl = makeCluster(n_cores - 1)
  
  # Define the number of ages #
  vAges = 50:95
  
  # Create output list #
  lOut = list()
  
  # Create a list for model results #
  lRes = list()
  
  # Get all the files from the top directory #
  vFiles = list.files(sDir, recursive = TRUE)
  
  # Define countries #
  vCountries = sort(unique(mthesis::df_mx_all[, "Country"]))
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Create a list for the result of each age #
  lMCS_age = list()
  
  # Create a list to store loss matrixes in results in #
  lLoss <- list()
  
  # Loop over ages, countries, and subpopulations #
  for (iAge in vAges) {
    lLoss[[paste0(iAge)]] = list()
    lMCS_age[[paste0(iAge)]] = list()
    
    for (sC in vCountries) {
      
      lLoss[[paste0(iAge)]][[sC]] = list()
      lMCS_age[[paste0(iAge)]][[sC]] = list()
      
      for (sSub in vSub) {
        
        # Create matrix for losses #
        lLoss[[paste0(iAge)]][[sC]][[sSub]] = matrix(data = NA, nrow = 17, ncol = length(vFiles),
                                                     dimnames = list(NULL, vFiles))
        # Get empty list for MCS results #
        lMCS_age[[paste0(iAge)]][[sC]][[sSub]] = list()
        
      }
      
    }
    
  }
  
  # Counter for which file is worked on #
  iFile_count = 1
  
  # Loop over the files #
  for (sFile in vFiles) {
    
    # Working on file print statement #
    p_done = 100*round(iFile_count/length(vFiles), 4)
    cat("Working on file:", sFile, paste0("(", p_done, "% done)"), "\n")
    
    # Update file counter #
    iFile_count = iFile_count + 1
    
    # Get the full file directory #
    sDir_file = paste0(sDir, "/", sFile)
    
    # Load the file #
    sLoad = load(sDir_file)
    
    # Get the object inside the file #
    lRes[[sFile]] = get(sLoad)
    
    # Loop over the ages #
    for (ind_age in 1:length(vAges)) {
      
      # Loop over countries #
      for (sC in vCountries) {
        
        # Loop over subpopulations #
        for (sSub in vSub) {
          
          # Get true values #
          vTrue = lRes[[sFile]][["Results"]][[sC]][[sSub]][["mTrue"]][ind_age, paste0(2000:2016)]
          vTrue = as.numeric(vTrue)
          
          # Get predicted values #
          vPred = lRes[[sFile]][["Results"]][[sC]][[sSub]][["mPred"]][ind_age, paste0(2000:2016)]
          vPred = as.numeric(vPred)
          
          # Calculate vector of loss #
          if (loss == "SE") {
            vLoss = (vTrue - vPred)^2
          } else {
            vLoss = abs(vTrue - vPred)
          }
          vLoss = as.numeric(vLoss)
          
          # Append loss to loss list #
          lLoss[[paste0(vAges[ind_age])]][[sC]][[sSub]][, sFile] = as.numeric(vLoss)
          
        }
        
      } # End country loop #
      
    } # End age loop #
    
  } # End file loop #
  
  # Print statement start on MCS #
  cat("------------------------------", "\n")
  cat("Now starting on MCS results", "\n")
  
  # Calculate starting positions for MCS results #
  iStart = which(vAges == iStart_age)
  
  # Loop over the ages again #
  for (ind_age in iStart:length(vAges)) {
    
    # Print statement for age #
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("Now working on age", vAges[ind_age], "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    
    # Define folder for age #
    age_folder = paste0(save_dir, "/", vAges[ind_age])
    
    # Check if folder exist else create it #
    if (!(dir.exists(age_folder))) {
      dir.create(age_folder)
    }
    
    # Loop over countries #
    for (sC in vCountries) {
      
      # Define folder for country #
      country_folder = paste0(age_folder, "/", sC)
      
      # Check if folder exist else create it #
      if (!(dir.exists(country_folder))) {
        dir.create(country_folder)
      }
      
      # Loop over subpopulations #
      for (sSub in vSub) {
        
        # Define subpopulation folder #
        sub_folder = paste0(country_folder, "/", sSub)
        
        # Check if folder exists else create it #
        if (!(dir.exists(sub_folder))) {
          dir.create(sub_folder)
        }
        
        # Get matrix of losses #
        mLoss = lLoss[[paste0(vAges[ind_age])]][[sC]][[sSub]]
        
        # Get MCS results #
        SSM = MCS::MCSprocedure(Loss = mLoss, alpha = dAlpha,
                                B = 5000, statistic = "Tmax",
                                cl = cl)
        
        # Define output name #
        sOutput = paste0("SSM", "_", vAges[ind_age], "_",
                         sC, "_", sSub)
        
        # Assign to the new name #
        assign(sOutput, SSM)
        
        # Define save file #
        sSave_file = paste0(sub_folder, "/", "MCS_res_",
                            vAges[ind_age], "_", sC, "_", sSub,
                            ".RData")
        
        # Save the MCS results #
        save(list = sOutput, file = sSave_file)
        
        # Put result on list #
        #lMCS_age[[paste0(iAge)]][[sC]][[sSub]] = SSM
        
        
        
      } # End subpopulation loop #
      
    } # End country loop #
    
  } # End age loop #
  
  # Stop the cluster #
  stopCluster(cl)
  
  # Return list with MCS results #
  #return(lMCS_age)
  
}


# Model confidence set all ages #
f_MCS_all_age <- function(sDir, dAlpha, loss = "MSE",
                          save_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/MCS_results_all_age"
                          ) {
  
  # Require the MCS package #
  require(MCS)
  require(parallel)
  
  # Stop if loss is not either "SE" or "AE" #
  if (!(loss %in% c("MSE", "MAE"))) {
    stop("Loss has to be MSE or MAE")
  }
  
  # Detect number of cores #
  n_cores = detectCores()
  
  # Make cluster #
  cl = makeCluster(n_cores - 1)
  
  # Define the number of ages #
  vAges = 50:95
  
  # Create output list #
  lOut = list()
  
  # Create a list for model results #
  lRes = list()
  
  # Get all the files from the top directory #
  vFiles = list.files(sDir, recursive = TRUE)
  
  # Define countries #
  vCountries = sort(unique(mthesis::df_mx_all[, "Country"]))
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Create a list for the result of each age #
  lMCS_age = list()
  
  # Create a list to store loss matrixes in results in #
  lLoss <- list()
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Create list element for country #
    lLoss[[sC]] = list()
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Create matrix for subpopulation #
      lLoss[[sC]][[sSub]] = matrix(data = NA, nrow = 17, ncol = length(vFiles),
                                   dimnames = list(NULL, vFiles))
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Counter for which file is worked on #
  iFile_count = 1
  
  # Loop over the files #
  for (sFile in vFiles) {
    
    # Working on file print statement #
    p_done = 100*round(iFile_count/length(vFiles), 4)
    cat("Working on file:", sFile, paste0("(", p_done, "% done)"), "\n")
    
    # Update file counter #
    iFile_count = iFile_count + 1
    
    # Get the full file directory #
    sDir_file = paste0(sDir, "/", sFile)
    
    # Load the file #
    sLoad = load(sDir_file)
    
    # Get the object inside the file #
    lRes[[sFile]] = get(sLoad)
      
    # Loop over countries #
    for (sC in vCountries) {
        
      # Loop over subpopulations #
      for (sSub in vSub) {
          
        # Get true values #
        mTrue = lRes[[sFile]][["Results"]][[sC]][[sSub]][["mTrue"]][, paste0(2000:2016)]
        mTrue = as.matrix(mTrue)
          
        # Get predicted values #
        mPred = lRes[[sFile]][["Results"]][[sC]][[sSub]][["mPred"]][, paste0(2000:2016)]
        mPred = as.matrix(mPred)
          
        # Calculate vector of loss #
        if (loss == "MSE") {
          vLoss = colMeans((mTrue - mPred)^2)
        } else {
          vLoss = colMeans(abs(mTrue - mPred))
        }
        vLoss = as.numeric(vLoss)
          
        # Append loss to loss list #
        lLoss[[sC]][[sSub]][, sFile] = as.numeric(vLoss)
        
          
      } # End sub loop #
        
    } # End country loop #
    
  } # End file loop #
  
  # Print statement start on MCS #
  cat("------------------------------", "\n")
  cat("Now starting on MCS results", "\n")
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Print statement #
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("Now working on country:", sC, "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    cat("------------------------------------------------------------------------------------------", "\n")
    
    # Define folder directory #
    country_folder = paste0(save_dir, "/", sC)
    
    # Check if folder exist else create it #
    if (!(dir.exists(country_folder))) {
      dir.create(country_folder)
    }
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Define subpopulation folder #
      sub_folder = paste0(country_folder, "/", sSub)
      
      # Check if folder exists else create it #
      if (!(dir.exists(sub_folder))) {
        dir.create(sub_folder)
      }
      
      # Get matrix of losses #
      mLoss = lLoss[[sC]][[sSub]]
      
      # Get MCS results #
      SSM = MCS::MCSprocedure(Loss = mLoss, alpha = dAlpha,
                              B = 5000, statistic = "Tmax",
                              cl = cl)
      
      # Define output name #
      sOutput = paste0("SSM", "_",
                       sC, "_", sSub)
      
      # Assign to the new name #
      assign(sOutput, SSM)
      
      # Define save file #
      sSave_file = paste0(sub_folder, "/", "MCS_res_",
                          "all_age", "_", sC, "_", sSub,
                          ".RData")
      
      # Save the MCS results #
      save(list = sOutput, file = sSave_file)
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Stop the cluster #
  stopCluster(cl)
  
}


# Count results individual ages and total #
f_MCS_count_age <- function(sRes_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/MCS_results",
                            sModel_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/Results") {
  
  # Require the MCS package #
  require(MCS)
  
  # List models #
  vModels = list.files(sModel_dir, recursive = TRUE)
  
  # Change . with _ to fit MCS result names #
  vModels = gsub(".RData", "_RData", vModels)
  
  # List the ages #
  vAges = list.dirs(sRes_dir, full.names = FALSE, recursive = FALSE)
  
  # Create a matrix for the results #
  mRes = matrix(data = 0, nrow = length(vAges), ncol = length(vModels),
                dimnames = list(vAges, vModels))
  
  # Loop over the ages #
  for (iAge in vAges) {
    
    # Make age progress print statement #
    cat("Now working on age:", iAge, "\n")
    
    # Define the age folder #
    age_folder = paste0(sRes_dir, "/", iAge)
    
    # Define the countries #
    vCountries = list.dirs(age_folder, full.names = FALSE, recursive = FALSE)
    
    # Loop over the countries #
    for (sC in vCountries) {
      
      # Define country folder #
      country_folder = paste0(age_folder, "/", sC)
      
      # List subpopulations #
      vSub = list.dirs(country_folder, full.names = FALSE, recursive = FALSE)
      
      # Loop over subpopulations #
      for (sSub in vSub) {
        
        # Define subpopulation folder #
        sub_folder = paste0(country_folder, "/", sSub)
        
        # Define directory for result file #
        sDir_file = list.files(sub_folder, full.names = TRUE)
        
        # Load results #
        sLoad = load(sDir_file)
        
        # Get the MCS result object inside the file #
        SSM_res = get(sLoad)
        
        # Extract the models from the superior set #
        vMCS_models = SSM_res@Info$model.names
        
        # Add a count to these models #
        mRes[paste0(iAge), vMCS_models] = mRes[paste0(iAge), vMCS_models] + 1
        
        
      } # End subpop loop #
      
    } # End country loop #
    
  } # End age loop #
  
  # Grab colnames #
  col_names = colnames(mRes)
  
  # Create new column names #
  vColnames = f_MCS_name(col_names)
  
  # Overwrite colnames of mRes #
  colnames(mRes) = vColnames
  
  # Return the result matrix #
  return(mRes)
  
}

# Count results for all ages #
f_MCS_count_all_age <- function(sRes_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/MCS_results_all_age",
                                sModel_dir = "/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/Results") {
  
  # Require the MCS package #
  require(MCS)
  
  # List models #
  vModels = list.files(sModel_dir, recursive = TRUE)
  
  # Change . with _ to fit MCS result names #
  vModels = gsub(".RData", "_RData", vModels)
  
  # List the countries #
  vCountries = list.dirs(sRes_dir, full.names = FALSE, recursive = FALSE)
  
  # Create a matrix of the models for counting #
  mRes = matrix(data = 0, nrow = 1, ncol = length(vModels),
                dimnames = list(NULL, vModels))
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Print statement #
    cat("Now extracting results for country:", sC, "\n")
    
    # Define the country directory #
    country_folder = paste0(sRes_dir, "/", sC)
    
    # Find subpopulations #
    vSub = list.dirs(country_folder, full.names = FALSE, recursive = FALSE)
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Define subpopulation directory #
      sub_folder = paste0(country_folder, "/", sSub)
      
      # Define directory for result file #
      sDir_file = list.files(sub_folder, full.names = TRUE)
      
      # Load results #
      sLoad = load(sDir_file)
      
      # Get the MCS result object inside the file #
      SSM_res = get(sLoad)
      
      # Extract the models from the superior set #
      vMCS_models = SSM_res@Info$model.names
      
      # Add a count to these models #
      mRes[, vMCS_models] = mRes[, vMCS_models] + 1
      
      
    }
    
  }
  
  # Grab colnames #
  col_names = colnames(mRes)
  
  # Create new column names #
  vColnames = f_MCS_name(col_names)
  
  # Overwrite colnames of mRes #
  colnames(mRes) = vColnames
  
  # Return the matrix with results #
  return(mRes)
  
  
  
}

f_MCS_name = function(vCol_names) {
  
  # Define wanted names #
  vNames = c("Deep Learning/res_FFNN_1_2000epochs_RData" = "FFNN (2000 epochs)",
            "Deep Learning/res_FFNN_1_500epochs_RData" = "FFNN (500 epochs)",
            "Deep Learning/res_FFNN_1_cohort_RData" = "FFNN (with cohort)",
            "Deep Learning/res_FFNN_1_factor_RData" = "FFNN (with factors)",
            "Deep Learning/res_FFNN_1_factorload_RData" = "FFNN (with factorloadings)",
            "Deep Learning/res_FFNN_1_RData" = "FFNN (200 epochs)",
            "Deep Learning/res_GRU_1_cohort_nodropout_RData" = "GRU (with cohort, no dropout)",
            "Deep Learning/res_GRU_1_cohort_RData" = "GRU (with cohort)",
            "Deep Learning/res_LSTM_1_cohort_nodropout_RData" = "LSTM (with cohort, no dropout)",
            "Deep Learning/res_LSTM_1_cohort_RData" = "LSTM (with cohort)",
            "Deep Learning/res_RNN_1_cohort_nodropout_RData" = "RNN (with cohort, no dropout)",
            "Deep Learning/res_RNN_1_cohort_RData" = "RNN (with cohort)",
            "Deep Learning/res_TCN_1_RData" = "TCN",
            "MSE/MSE_boost_sp_RData" = "Boosting (sp)",
            "MSE/MSE_RF_sp_RData" = "RF (sp)",
            "MSE/MSE_svm_sp_RData" = "SVM (sp)",
            "MSE/MSE_xgb_sp_RData" = "XGB (sp)",
            "Multi population/MultilevelFactorResults_RData" = "Multi level factor",
            "Multi population/res_mp_boost_RData" = "Boosting (mp)",
            "Multi population/res_mp_rf_RData" = "RF (mp)",
            "Multi population/res_mp_svm_RData" = "SVM (mp)",
            "Multi population/res_mp_xgb_RData" = "XGB (mp)",
            "Single population/res_sp_APC_GNM_RData" = "APC",
            "Single population/res_sp_CBD_GNM_RData" = "CBD",
            "Single population/res_sp_LC_GNM_RData" = "LC (GNM)",
            "Single population/res_sp_LC_SVD_RData" = "LC (SVD)",
            "Single population/res_sp_M6_GNM_RData" = "M6",
            "Single population/res_sp_M7_GNM_RData" = "M7",
            "Single population/res_sp_M8_GNM_RData" = "M8",
            "Single population/res_sp_Plat_GNM_RData" = "Plat",
            "Single population/res_sp_RH_GNM_RData" = "RH")
  
  # Find the entrances #
  vOut = vNames[vCol_names]
  
  # Return output vector #
  return(vOut)
  
}




