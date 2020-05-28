# Graphs #

##############################
#---- New graph function ----#
##############################
f_graph_mortality_trend <- function(vAges, sRes_file, sCountry, vSub,
                                    sRes_file_2 = NULL,
                                    bCutOff = TRUE) {
  
  # Require the ggplot2 and reshape package #
  require(ggplot2)
  require(reshape)
  
  # Define all the data #
  df_all = mthesis::df_mx_all
  
  # Load model results #
  sLoad = load(sRes_file)
  
  # Get the results #
  lRes = get(sLoad)
  
  # Check if there is a second model #
  if (!(is.null(sRes_file_2))) {
    
    # Load second model #
    sLoad_2 = load(sRes_file_2)
    
    # Get the results of secondary model #
    lRes_2 = get(sLoad_2)
    
  }
  
  # Create a list for data #
  lData = replicate(length(vAges), list())
  
  # Define age vector #
  vAll_age = 50:95
  
  # Get the results for each age #
  for (iAge in vAges) {
    
    # Extract data #
    mData = df_all[(df_all[, "Age"] == iAge) &
                     (df_all[, "Country"] == sCountry), c("Year", "Age", "Country", vSub)]
    
    # Get the index of the age #
    ind_age = which(vAll_age == iAge)
    
    # Create prediction matrix #
    mPred = matrix(data = NA, nrow = 17, ncol = (3 + length(vSub)),
                   dimnames = list(NULL, c("Year", "Age", "Country", vSub)))
    
    # Convert predictions to data frame #
    mPred = as.data.frame(mPred)
    
    # Add data to prediction matrix #
    mPred[, "Age"] = iAge
    mPred[, "Year"] = 2000:2016
    mPred[, "Country"] = sCountry
    
    # Check if second model is not NULL #
    if (!(is.null(sRes_file_2))) {
      
      # Create prediction matrix #
      mPred_2 = matrix(data = NA, nrow = 17, ncol = (3 + length(vSub)),
                       dimnames = list(NULL, c("Year", "Age", "Country", vSub)))
      
      # Convert predictions to data frame #
      mPred_2 = as.data.frame(mPred_2)
      
      # Add data to prediction matrix #
      mPred_2[, "Age"] = iAge
      mPred_2[, "Year"] = 2000:2016
      mPred_2[, "Country"] = sCountry
      
    }
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Extract predictions for subpopulation #
      vPred = lRes[["Results"]][[sCountry]][[sSub]][["mPred"]][ind_age, paste0(2000:2016)]
      
      # Convert vPred to numeric #
      vPred = as.numeric(vPred)
      
      # Append to prediction matrix
      mPred[, sSub] = vPred
      
      # Check if secondary model is not NULL #
      if (!(is.null(sRes_file_2))) {
        
        # Extract predictions for subpopulation #
        vPred_2 = lRes_2[["Results"]][[sCountry]][[sSub]][["mPred"]][ind_age, paste0(2000:2016)]
        
        # Convert vPred to numeric #
        vPred_2 = as.numeric(vPred_2)
        
        # Append to prediction matrix
        mPred_2[, sSub] = vPred_2
        
      }
      
    }
    
    # Cutoff bias for model 1 #
    if (bCutOff) {
      
      # Calculate cut-off biases #
      bias_mod1_female = mPred[mPred[, "Year"] == 2000, "Female"] - mData[mData[, "Year"] == 2000, "Female"]
      bias_mod1_male = mPred[mPred[, "Year"] == 2000, "Male"] - mData[mData[, "Year"] == 2000, "Male"]
      
      # Correct for the bias #
      mPred[, "Female"] = mPred[, "Female"] - bias_mod1_female
      mPred[, "Male"] = mPred[, "Male"] - bias_mod1_male
      
    }
    
    # Sort the data frames #
    mData = mData[with(mData, order(Year)), ]
    mPred = mPred[with(mPred, order(Year)), ]
    
    # Calculate the CI #
    vCI_mod1_female = f_Confidence_band(vTrue = as.numeric(mData[(mData[, "Year"] %in% 2000:2016), "Female"]),
                                        vPred = as.numeric(mPred[, "Female"]))
    vCI_mod1_female = as.numeric(vCI_mod1_female)
    
                                                           
    vCI_mod1_male = f_Confidence_band(vTrue = as.numeric(mData[(mData[, "Year"] %in% 2000:2016), "Male"]),
                                        vPred = as.numeric(mPred[, "Male"]))
    vCI_mod1_male = as.numeric(vCI_mod1_male)
    
    # Convert to dataframes #
    mCI_female = data.frame("Year" = 2000:2016,
                            "Lower" = mPred[, "Female"] - 1.96*vCI_mod1_female,
                            "Upper" = mPred[, "Female"] + 1.96*vCI_mod1_female,
                            "variable" = "Female")
    mCI_male = data.frame("Year" = 2000:2016,
                          "Lower" = mPred[, "Male"] - 1.96*vCI_mod1_male,
                          "Upper" = mPred[, "Male"] + 1.96*vCI_mod1_male,
                          "variable" = "Male")
    
    # Concatenate the two data frames together #
    mCI = rbind(mCI_female, mCI_male)
    
    
    # Melt dataframes #
    mData_melt = reshape::melt(mData, id.vars = c("Year", "Age", "Country"),
                      measure.vars = vSub)
    mPred_melt = reshape::melt(mPred, id.vars = c("Year", "Age", "Country"),
                      measure.vars = vSub)
    
    mPred_melt = cbind(mPred_melt, mCI[, c("Lower", "Upper")])
    
    # Check if secondary model is present #
    # Check if secondary model is not NULL #
    if (!(is.null(sRes_file_2))) {
      
      # Cut-off-bias correct for model 2#
      if (bCutOff) {
        
        # Calculate cut-off biases #
        bias_mod2_female = mPred_2[mPred_2[, "Year"] == 2000, "Female"] - mData[mData[, "Year"] == 2000, "Female"]
        bias_mod2_male = mPred_2[mPred_2[, "Year"] == 2000, "Male"] - mData[mData[, "Year"] == 2000, "Male"]
        
        # Correct for the bias #
        mPred_2[, "Female"] = mPred_2[, "Female"] - bias_mod2_female
        mPred_2[, "Male"] = mPred_2[, "Male"] - bias_mod2_male
        
      }
      
      # Sort the data frames #
      mPred_2 = mPred_2[with(mPred_2, order(Year)), ]
      
      # Calculate the CI #
      vCI_mod2_female = f_Confidence_band(vTrue = as.numeric(mData[(mData[, "Year"] %in% 2000:2016), "Female"]),
                                          vPred = as.numeric(mPred_2[, "Female"]))
      vCI_mod2_female = as.numeric(vCI_mod2_female)
      
      
      vCI_mod2_male = f_Confidence_band(vTrue = as.numeric(mData[(mData[, "Year"] %in% 2000:2016), "Male"]),
                                        vPred = as.numeric(mPred_2[, "Male"]))
      vCI_mod2_male = as.numeric(vCI_mod2_male)
      
      # Convert to dataframes #
      mCI_2_female = data.frame("Year" = 2000:2016,
                              "Lower" = mPred_2[, "Female"] - 1.96*vCI_mod2_female,
                              "Upper" = mPred_2[, "Female"] + 1.96*vCI_mod2_female,
                              "variable" = "Female")
      mCI_2_male = data.frame("Year" = 2000:2016,
                            "Lower" = mPred_2[, "Male"] - 1.96*vCI_mod2_male,
                            "Upper" = mPred_2[, "Male"] + 1.96*vCI_mod2_male,
                            "variable" = "Male")
      
      # Concatenate the two data frames together #
      mCI_2 = rbind(mCI_2_female, mCI_2_male)
      
      
      # Melt secondary dataframe #
      mPred_melt_2 = reshape::melt(mPred_2, id.vars = c("Year", "Age", "Country"),
                          measure.vars = vSub)
      
      mPred_melt_2 = cbind(mPred_melt_2, mCI_2[, c("Lower", "Upper")])
      
    }
    
    # Append data to data list #
    lData[[paste0(iAge)]][["mData"]] = mData_melt
    lData[[paste0(iAge)]][["mPred"]] = mPred_melt
    
    # Calculate y_placement for text #
    y_place = max(mData_melt[, "value"])
    
    # Extract model name #
    sName = f_graph_name(sLoad)
    
    # Create graph for each age #
    graph <- ggplot(mData_melt, aes(x=Year, y=value, group=variable, linetype=variable)) +
      geom_line(show.legend = TRUE, aes(colour = "series")) +
      scale_linetype_discrete(name = "Groups")
    
    graph = graph + 
      geom_line(data = mPred_melt, aes(colour = "mod1"),
                show.legend = TRUE) +
      geom_ribbon(data = mPred_melt, aes(ymin = Lower, ymax = Upper, fill = "mod1"),
                  alpha = 0.25, show.legend = FALSE) + 
      geom_vline(xintercept=2000, colour="grey") +
      geom_text(aes(x=1993, y=y_place + 0.001, label="Estimation region"),
                colour="grey40", angle=0, size = 3,
                check_overlap = TRUE) +
      geom_text(aes(x=2008, y=y_place + 0.001, label="Forecasting region"),
                colour="grey40", angle=0, size = 3,
                check_overlap = TRUE) +
      ggtitle(paste0("Mortality rates for age ", iAge, " and country ", f_sC_name(sCountry)))
    
    # Define caption #
    sCaption = paste0("Forecast models - ", "Red: ", sName)
    
    # Check if secondary model is present #
    if (!(is.null(sRes_file_2))) {
      
      # Extract name #
      sName_2 = f_graph_name(sLoad_2)
      
      # Define caption #
      sCaption = paste0("Forecast models - ", "Red: ", sName, " Blue: ", sName_2)
      
      # Add to graph #
      graph = graph + 
        geom_line(data = mPred_melt_2, aes(colour = "mod2"),
                  show.legend = TRUE) +
        geom_ribbon(data = mPred_melt_2, aes(ymin = Lower, ymax = Upper, fill = "mod2"),
                    alpha = 0.2, show.legend = FALSE)
      
    }
    
    # Add caption #
    graph = graph + labs(x = "Year", y = "Mortality rate",
                         caption = sCaption) +
      scale_colour_manual(name = "Series", values = c("series" = "black", "mod1" = "red", "mod2" = "blue"),
                          labels = c("series" = "Time series", "mod1" = paste0(sName), "mod2" = paste0(sName_2)))
    
    # Change how long between ticks on x-axis #
    graph <- graph + scale_x_continuous(breaks = round(seq(1950, 2016, by = 10),1)) +
      guides(fill=FALSE)
    
    # Show the graph #
    print(graph)
    
  }
  
}

##############################
#---- New graph function ----#
##############################
f_graph_name <- function(sLoad) {
  
  # Define the vector of names #
  vName = c("lFFNN" = "FFNN",
            "lFFNN_cohort" = "FFNN (with cohort)",
            "lFFNN_factor" = "FFNN (with factor)",
            "lFFNN_factorload" = "FFNN (with factorloadings)",
            "lGRU_cohort_nodrop" = "GRU (with cohort)",
            "lGRU_cohort" = "GRU (with cohort)",
            "lLSTM_cohort_nodrop" = "LSTM (with cohort)",
            "lLSTM_cohort" = "LSTM (with cohort)",
            "lRNN_cohort_nodrop" = "RNN (with cohort)",
            "lRNN_cohort" = "RNN (with cohort)",
            "lTCN" = "TCN",
            "lPred_Boost_all" = "Boosting (single-pop)",
            "lPred_RF_all" = "RF (single-pop)",
            "lPred_svm_all" = "SVM (single-pop)",
            "lPred_xgb_all" = "XGB (single-pop)",
            "lML_FM" = "Multi-level Factor Model",
            "lBoost" = "Boosting (multi-pop)",
            "lRF" = "RF (multi-pop)",
            "lSVM" = "SVM (multi-pop)",
            "lXgb" = "XGB (multi-pop)",
            "APC_GNM" = "APC",
            "CBD_GNM" = "CBD",
            "LC_GNM" = "LC",
            "LC_SVD" = "LC",
            "M6_GNM" = "M6",
            "M7_GNM" = "M7",
            "M8_GNM" = "M8",
            "Plat_GNM" = "Plat",
            "RH_GNM" = "RH")
  
  # Find model name #
  mod_name = vName[sLoad]
  
  # Return model name #
  return(mod_name)
  
  
  
}

# Find country name #
f_sC_name = function(sC) {
  
  # Define the names #
  vNames = c("AUS" = "Australia",
             "AUT" = "Austria",
             "BEL" = "Belgium",
             "BGR" = "Bulgaria",
             "CAN" = "Canada",
             "CHE" = "Switzerland",
             "CZE" = "Czechia",
             "DNK" = "Denmark",
             "ESP" = "Spain",
             "FIN" = "Finland",
             "FRATNP" = "France",
             "GBR_NIR" = "Northern Ireland",
             "GBR_SCO" = "Scotland",
             "GBRENW" = "England & Wales",
             "HUN" = "Hungary",
             "IRL" = "Ireland",
             "ISL" = "Iceland",
             "JPN" = "Japan",
             "NLD" = "Netherlands",
             "NOR" = "Norway",
             "SVK" = "Slovakia",
             "SWE" = "Sweden",
             "USA" = "USA")
  
  # Find given country full name #
  if (sC %in% names(vNames)) {
    sOut = vNames[sC]
    return(sOut)
  } else {
    return(sC)
  }
  
}

# Projections graph #
f_graph_projection <- function(sCountry, iAge) {
  
  # Require packages #
  require(ggplot2)
  require(keras)
  require(reshape)
  
  # Define all the data #
  df_all = mthesis::df_mx_all
  
  # Define the country as sC #
  sC = sCountry
  
  # Get the series of true values #
  mTrue = df_all[(df_all[, "Country"] == sC) &
                 (df_all[, "Age"] == iAge), c("Year", "Age", "Country", "Female", "Male")]
  
  # Find indices of correct years (1950:2000) #
  # vInd_years = which(mTrue[, "Year"] %in% 1950:2016)
  # 
  # # Subset mTrue to the correct years #
  # mTrue = mTrue[vInd_years, ]
  
  # Create list for predictions #
  lPred = list()
  
  # Load FFNN model results #
  sLoad_FFNN = load("/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/Projections/FFNN_500_projections.RData")
  
  # Get the results onto list #
  lPred[["FFNN"]] = get(sLoad_FFNN)
  
  # Load LSTM model results #
  # sLoad_LSTM = load("/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/Projections/LSTM_500_projections.RData")
  
  # Get the results onto list #
  # lPred[["LSTM"]] = get(sLoad_LSTM)
  
  # Load LC model results #
  sLoad_LC = load("/Users/runelangergaard/OneDrive - Aarhus universitet/Oecon/10. semester/Code/data/Projections/LC_projections.RData")
  
  # Get the results onto the list #
  lPred[["LC"]] = get(sLoad_LC)
  
  # Get the predicted series for LC #
  vLC_pred_female = lPred[["LC"]][[sC]][["Female"]][["mPred"]][paste0(iAge), paste0(2017:2060)]
  vLC_pred_male = lPred[["LC"]][[sC]][["Male"]][["mPred"]][paste0(iAge), paste0(2017:2060)]
  #vLC_pred_total = lPred[["LC"]][[sC]][["Total"]][["mPred"]][paste0(iAge), paste0(2017:2040)]
  
  # Combine #
  mLC = cbind("Female" = vLC_pred_female, "Male" = vLC_pred_male)
  
  # Add 2016 data #
  mLC = rbind("2016" = c(lPred[["LC"]][[sC]][["Female"]][["forecast"]][["fitted"]][paste0(iAge), paste0(2016)],
                         lPred[["LC"]][[sC]][["Male"]][["forecast"]][["fitted"]][paste0(iAge), paste0(2016)]),
              mLC)
  
  # Get the FFNN predicted series #
  mPred_FFNN_female = lPred[["FFNN"]][["Results"]][[sC]][["Female"]][["mPred"]]
  mPred_FFNN_male = lPred[["FFNN"]][["Results"]][[sC]][["Male"]][["mPred"]]
  vFFNN_pred_female = mPred_FFNN_female[mPred_FFNN_female[, "Age"] == iAge, paste0(2017:2060)]
  vFFNN_pred_male = mPred_FFNN_male[mPred_FFNN_male[, "Age"] == iAge, paste0(2017:2060)]
  
  # Combine #
  mFFNN = cbind("Female" = t(vFFNN_pred_female), "Male" = t(vFFNN_pred_male))
  colnames(mFFNN) = c("Female", "Male")
  
  # Get 2016 data #
  mTrain_female = lPred[["FFNN"]][["Results"]][[sC]][["Female"]][["mPred_train"]]
  mTrain_male = lPred[["FFNN"]][["Results"]][[sC]][["Male"]][["mPred_train"]]
  mFFNN = rbind("2016" = c(mTrain_female[(mTrain_female[, "Age"] == iAge), paste0(2016)],
                           mTrain_male[(mTrain_male[, "Age"] == iAge), paste0(2016)]),
                mFFNN)
  
  # Get the LSTM predicted series #
  # mPred_LSTM_female = lPred[["LSTM"]][["Results"]][[sC]][["Female"]][["mPred"]]
  # mPred_LSTM_male = lPred[["LSTM"]][["Results"]][[sC]][["Male"]][["mPred"]]
  # vLSTM_pred_female = mPred_LSTM_female[mPred_LSTM_female[, "Age"] == iAge, paste0(2017:2060)]
  # vLSTM_pred_male = mPred_LSTM_male[mPred_LSTM_male[, "Age"] == iAge, paste0(2017:2060)]
  # vLSTM_pred_total = lPred[["LSTM"]][["Results"]][[sC]][["Total"]][["mPred"]][paste0(iAge), paste0(2017:2040)]
  
  # Combine #
  # mLSTM = cbind("Female" = t(vLSTM_pred_female), "Male" = t(vLSTM_pred_male))
  # colnames(mLSTM) = c("Female", "Male")
  
  # Add year column #
  mLC = cbind(mLC, "Year" = as.numeric(rownames(mLC)))
  mFFNN = cbind(mFFNN, "Year" = as.numeric(rownames(mFFNN)))
  # mLSTM = cbind(mLSTM, "Year" = as.numeric(rownames(mLSTM)))
  
  # Calculate cut-off bias #
  bias_LC_female = mLC[paste0(2016), "Female"] - mTrue[mTrue[, "Year"] == 2016, "Female"]
  bias_LC_male = mLC[paste0(2016), "Male"] - mTrue[mTrue[, "Year"] == 2016, "Male"]
  bias_FFNN_female = mFFNN[paste0(2016), "Female"] - mTrue[mTrue[, "Year"] == 2016, "Female"]
  bias_FFNN_male = mFFNN[paste0(2016), "Male"] - mTrue[mTrue[, "Year"] == 2016, "Male"]
  # bias_LSTM_female = mLSTM[paste0(2016), "Female"] - mTrue[mTrue[, "Year"] == 2016, "Female"]
  # bias_LSTM_male = mLSTM[paste0(2016), "Male"] - mTrue[mTrue[, "Year"] == 2016, "Male"]
  
  # Correct for cut-off bias #
  mLC[, "Female"] = mLC[, "Female"] - bias_LC_female
  mLC[, "Male"] = mLC[, "Male"] - bias_LC_male
  mFFNN[, "Female"] = mFFNN[, "Female"] - bias_FFNN_female
  mFFNN[, "Male"] = mFFNN[, "Male"] - bias_FFNN_male
  # mLSTM[, "Female"] = mLSTM[, "Female"] - bias_LSTM_female
  # mLSTM[, "Male"] = mLSTM[, "Male"] - bias_LSTM_male
  
  # Melt the data frames #
  mTrue_melt = reshape::melt(mTrue, id.vars = c("Year", "Age", "Country"),
                             measure.vars = c("Female", "Male"))
  mLC_melt = reshape::melt(as.data.frame(mLC), id.vars = "Year",
                           measure.vars = c("Female", "Male"))
  mFFNN_melt = reshape::melt(as.data.frame(mFFNN), id.vars = "Year",
                             measure.vars = c("Female", "Male"))
  # mLSTM_melt = reshape::melt(as.data.frame(mLSTM), id.vars = "Year",
  #                            measure.vars = c("Female", "Male"))
  
  # Create graph #
  graph <- ggplot(mTrue_melt, aes(x=Year, y=value, group=variable, linetype=variable)) +
    geom_line(show.legend = TRUE) +
    scale_linetype_discrete(name = "Subpopulation")
  
  # Add model projections #
  graph <- graph +
    geom_line(data = mLC_melt, aes(colour = "LC")) +
    geom_line(data = mFFNN_melt, aes(colour = "FFNN")) + 
    #geom_line(data = mLSTM_melt, aes(colour = "LSTM")) + 
    labs(colour = "Model")
    
  # Add vertical line with text #
  y_place = max(mTrue_melt[, "value"])
  graph <- graph + 
    geom_vline(xintercept=2016, colour="grey") +
    geom_text(aes(x=2005, y=y_place, label="Estimation region"),
              colour="grey40", angle=0, size = 3,
              check_overlap = TRUE) +
    geom_text(aes(x=2027, y=y_place, label="Projection region"),
              colour="grey40", angle=0, size = 3,
              check_overlap = TRUE)
  
  # Change length between ticks on x-axis #
  graph <- graph + 
    scale_x_continuous(breaks = round(seq(1950, 2060, by = 10),1))
  
  # Get the full country name from the country code#
  sC_name = f_sC_name(sC)
  
  # Add plot title and axis titles #
  graph <- graph +
    ggtitle(paste0("Mortality projections for age ", iAge, " and country ", sC_name)) +
    labs(x = "Year", y = "Mortality rate")
  
  # Print graph #
  print(graph)
  
} 