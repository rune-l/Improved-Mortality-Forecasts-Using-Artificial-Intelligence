##############################
#########  New Model #########
##############################
# LSTM neural network #
f_LSTM_recursive_1 <- function(lSplit,
                               sPython_path = "/Users/runelangergaard/opt/anaconda3/bin/python3",
                               iBatch_size = 1024,
                               sSave_path = paste0("/Users/runelangergaard/OneDrive - Aarhus universitet",
                                                   "/Oecon/10. semester/Code/data/DL_models"),
                               iN_epochs = 200) {
  
  # Require the Deep Learning packages #
  require(keras)
  require(tensorflow)
  require(CatEncoders)
  
  # Set python paths #
  Sys.setenv(TENSORFLOW_PYTHON = sPython_path)
  use_python(sPython_path)
  
  # Remove total from data #
  #lSplit$train = lSplit$train[lSplit$train$variable != "Total", ]
  #lSplit$test = lSplit$test[lSplit$test$variable != "Total", ]
  #lSplit$test_obs = lSplit$test_obs[lSplit$test_obs$variable != "Total", ]
  
  # Grab the training data #
  mTrain = lSplit$train
  
  # Extract training data X #
  mX_train = subset(mTrain, select = -c(vY))
  
  # Transform Country variable to factor variable #
  mX_train[, "Country"] = as.factor(mX_train[, "Country"])
  
  # Transform Country variable to integer variable #
  mX_train[, "Country"] = as.integer(mX_train[, "Country"])
  
  # Create country encoder #
  # Country_encoder = CatEncoders::OneHotEncoder.fit(as.matrix(mX_train[, "Country"]))
  
  # Create country matrix #
  # mCountry = CatEncoders::transform(Country_encoder, as.matrix(mX_train[, "Country"]),
  #                                   sparse = FALSE)
  
  # Transform gender/variable to factor #
  mX_train[, "variable"] = as.factor(mX_train[, "variable"])
  
  # Create gender encoder #
  # Gender_encoder = CatEncoders::OneHotEncoder.fit(as.matrix(mX_train[, "variable"]))
  
  # Transform to gender to one-hot encoding #
  # mGender = CatEncoders::transform(Gender_encoder, as.matrix(mX_train[, "variable"]),
  #                                  sparse = FALSE)
  
  # Transform gender/variable to integer #
  mX_train[, "variable"] = as.integer(mX_train[, "variable"])
  
  # Transform training dataframe to a matrix #
  mX_train = as.matrix(mX_train)
  
  # Make age go from 0 to 45 #
  mX_train[, "Age"] = mX_train[, "Age"] - 50
  
  # Make Country go from 0 to 22 #
  mX_train[, "Country"] = mX_train[, "Country"] - 1
  
  # Make gender/variable go from 0 to 2 #
  mX_train[, "variable"] = mX_train[, "variable"] - 1
  
  # Grab Y values #
  vY_train = subset(mTrain, select = c(vY))
  
  # Transform vY to matrix #
  vY_train = as.matrix(vY_train)
  
  # Log transformation of value in mX_train #
  mX_train[, "value"] = log(mX_train[, "value"])
  
  # Log transformation of Y variable #
  vY_train = log(vY_train)
  
  # Grab the test observations #
  mX_test = lSplit$test_obs
  
  # Transform Country variable to factor variable #
  mX_test[, "Country"] = as.factor(mX_test[, "Country"])
  
  # Transform Country variable to integer variable #
  mX_test[, "Country"] = as.integer(mX_test[, "Country"])
  
  # Transform Country to one-hot-encoding #
  # mCountry_test = CatEncoders::transform(Country_encoder, as.matrix(mX_test[, "Country"]),
  #                                        sparse = FALSE)
  
  # Transform gender/variable to factor #
  mX_test[, "variable"] = as.factor(mX_test[, "variable"])
  
  # Transform gender/variable to integer #
  mX_test[, "variable"] = as.integer(mX_test[, "variable"])
  
  # One-hot encode gender #
  # mGender_test = CatEncoders::transform(Gender_encoder, as.matrix(mX_test[, "variable"]),
  #                                       sparse = FALSE)
  
  # Transform training dataframe to a matrix #
  mX_test = as.matrix(mX_test)
  
  # Make age go from 0 to 45 #
  mX_test[, "Age"] = mX_test[, "Age"] - 50
  
  # Make Country go from 0 to 22 #
  mX_test[, "Country"] = mX_test[, "Country"] - 1
  
  # Make gender/variable go from 0 to 2 #
  mX_test[, "variable"] = mX_test[, "variable"] - 1
  
  # Make log-transformation of value #
  mX_test[, "value"] = log(mX_test[, "value"])
  
  # Scale Year #
  # lScale_year = f_MinMaxScaler(mX_train[, "Year"],
  #                              mX_test[, "Year"])
  # 
  # # Assign scaled vaulues #
  # mX_train[, "Year"] = lScale_year$train
  # mX_test[, "Year"] = lScale_year$test
  
  # Scale log death rates #
  # lScale_logmx = f_MinMaxScaler(mX_train[, "value"],
  #                               mX_test[, "value"])
  # lScale_vY = f_MinMaxScaler(vY_train,
  #                            vY_train)
  # 
  # 
  # # Assign scaled values #
  # mX_train[, "value"] = lScale_logmx$train
  # vY_train = lScale_vY$train
  # mX_test[, "value"] = lScale_logmx$test
  
  # Define the training inputs #
  vYear = mX_train[, "Year"]
  vAge = mX_train[, "Age"]
  mCountry = mX_train[, "Country"]
  mGender = mX_train[, "variable"]
  vValue = mX_train[, "value"]
  
  # Define test inputs #
  vYear_test = mX_test[, "Year"]
  vAge_test = mX_test[, "Age"]
  mCountry_test = mX_test[, "Country"]
  mGender_test = mX_test[, "variable"]
  vValue_test = mX_test[, "value"]
  
  # Create the inout for the model #
  Year = keras::layer_input(shape = c(1), dtype = "float32", name = "Year")
  Age = keras::layer_input(shape = c(1), dtype = "int32", name = "Age")
  Country = keras::layer_input(shape = c(1), dtype = "int32", name = "Country")
  Gender = keras::layer_input(shape = c(1), dtype = "int32", name = "Gender")
  value = keras::layer_input(shape = c(1), dtype = "float32", name = "value")
  
  # Embed the Age input variable #
  Age_embed = Age %>% keras::layer_embedding(input_dim = 46, output_dim = 46/2,
                                             input_length = 1, name = 'Age_embed') %>%
    keras::layer_flatten()
  
  # Embed the Country input variable #
  Country_embed = Country %>% keras::layer_embedding(input_dim = 23, output_dim = 11,
                                                     input_length = 1, name = "Country_embed") %>%
    keras::layer_flatten()
  
  # Embed the Gender input variable #
  Gender_embed = Gender %>% keras::layer_embedding(input_dim = 3, output_dim = 1,
                                                   input_length = 1, name = "Gender_embed") %>%
    keras::layer_flatten()
  
  # Define the features #
  vFeatures = keras::layer_concatenate(list(Year, Age_embed, Country_embed, Gender_embed, value))
  
  # Create Add 1st dense layer to model #
  Model_middle = vFeatures %>%
    keras::layer_lstm(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    
    # Add 2nd dense layer #
    keras::layer_lstm(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    
    # Add 3rd dense layer #
    keras::layer_lstm(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    
    # Add 4th dense layer #
    keras::layer_lstm(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05)
  
  # Create model output #
  Main_output = keras::layer_concatenate(list(vFeatures, Model_middle)) %>% 
    keras::layer_lstm(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    keras::layer_dense(units = 1, activation = NULL, name = "Main_output")
  
  # Create model output #
  Model = keras::keras_model(inputs = c(Year, Age, Country, Gender, value),
                             outputs = c(Main_output))
  
  # Define optimizer #
  #lAdam = keras::optimizer_adam(lr = 0.001)
  lAdam = keras::optimizer_adam(lr = 0.1)
  
  # Compile the model #
  Model %>% keras::compile(loss = 'mean_squared_error',
                           optimizer = lAdam
  )
  
  # Define callback to reduce learning rate on plateau #
  lr_callback = callback_reduce_lr_on_plateau(factor = 0.80,
                                              patience = 10,
                                              verbose = 1,
                                              cooldown = 5,
                                              min_lr = 0.0005)
  
  # Define saving path #
  sSave_file = paste0(sSave_path, "/",
                      "LSTM_simple_2_all_pop.mod")
  
  # Define model saving callback #
  model_callback = callback_model_checkpoint(filepath = sSave_file,
                                             verbose = 1,
                                             save_best_only = TRUE)
  
  # Fit the model #
  Model %>% fit(x = list(vYear, vAge,
                         mCountry, mGender,
                         vValue),
                y = vY_train, batch_size = iBatch_size,
                epochs = iN_epochs, verbose = 1,
                validation_split = 0.1,
                callbacks = list(lr_callback, model_callback),
                view_metrics = TRUE)
  
  # Load the best model #
  Model = load_model_hdf5(sSave_file)
  
  # Make predictions matrix #
  mPred = subset(lSplit$test_obs, select = -c(Year))
  
  # Grab true values #
  mTrue = lSplit$test
  
  # Loop over the prediction years #
  for (iYear in 2000:2016) {
    
    # Make predictions #
    vLogPredict = Model %>% predict(list(vYear_test, vAge_test,
                                         mCountry_test, mGender_test,
                                         vValue_test))
    
    # Update year in test values #
    vYear_test = rep(iYear, length(vYear_test))
    
    # Put new predictions onto test values #
    vValue_test = vLogPredict
    
    #cat("Head of predictions", head(vLogPredict), "\n")
    
    # Rescale predictions to original log-levels #
    # vLogPredict = f_MinMaxScaler_inv(vX_scaled = vLogPredict,
    #                                  dMin = lScale_vY$min,
    #                                  dMax = lScale_vY$max)
    
    # Attach predictions to mPred #
    mPred[, paste0(iYear)] = exp(vLogPredict)
    #mPred[, paste0(iYear)] = vLogPredict
    
  }
  
  # Reshape the mTrue #
  mTrue = f_mTrue_cast(mTest = mTrue)
  
  # Sort the mTrue dataframe #
  mTrue = mTrue[with(mTrue, order(Country, variable, Age)), ]
  
  # Sort the mPred dataframe #
  mPred = mPred[with(mPred, order(Country, variable, Age)), ]
  
  # Create list for population results #
  lRes = list()
  
  # Extract countries #
  vCountries = unique(mTrue[, "Country"])
  
  # Extract subpopulations #
  vSub = unique(mTrue[, "variable"])
  
  # Loop over countries and subpopulations #
  for (sC in vCountries) {
    
    # Create list element for country #
    lRes[[sC]] = list()
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Extract mTrue for the population #
      mTrue_pop = mTrue[(mTrue$Country == sC) &
                          (mTrue$variable == sSub), ]
      
      # Extract mPred for the population #
      mPred_pop = mPred[(mPred$Country == sC) &
                          (mPred$variable == sSub), ]
      
      # Calculate Squared errors for the population #
      mErr2_pop = (as.matrix(mTrue_pop[, paste0(2000:2016)]) - as.matrix(mPred_pop[, paste0(2000:2016)]))^2
      
      # Calculate MSE for the population #
      dMSE_pop = mean(mErr2_pop)
      
      # Calculate RMSE for the population #
      dRMSE_pop = sqrt(dMSE_pop)
      
      # Append results to output #
      lRes[[sC]][[sSub]][["mTrue"]] = mTrue_pop
      lRes[[sC]][[sSub]][["mPred"]] = mPred_pop
      lRes[[sC]][[sSub]][["MSE"]] = dMSE_pop
      lRes[[sC]][[sSub]][["RMSE"]] = dRMSE_pop
      
    } # End subpopulation loop #
  } # End country loop #
  
  # Calculate Average and Median MSE #
  lMSE = f_MSE_extract(lRes = lRes)
  
  # Define output list #
  lOut = list(Model = Model,
              Results = lRes,
              Avg_MSE = lMSE$mean,
              Med_MSE = lMSE$median)
  
  # Return output list #
  return(lOut)
  
}

##############################
#########  New Model #########
##############################
# Generator function #
f_generator <- function(aData, iStart_year = 1951, iEnd_year = 1998) {
  
  iRow <- 1
  
  iYear <- iStart_year
  
  # Start generator function #
  function() {
    
    # Define the years #
    vYears_input <- paste0(c(iYear - 1, iYear))
    vYears_target <- paste0(iYear+1)
    
    # Grab the input data #
    aSample <- aData[iRow:(iRow+45), vYears_input, ]
    
    # Grab the target values #
    mTarget <- aData[iRow:(iRow+45), vYears_target, "value"]
    
    iRow <<- iRow + 46
    
    # Check if need to move one column #
    if (iRow >= 3174) {
      iRow <<- 1
      iYear <<- iYear + 1
    }
    
    # Restart variables if we are at the limit #
    if (iYear > iEnd_year) {
      iYear <<- iStart_year
    }
    
    # Define output list #
    lOut <- list(samples = aSample,
                 targets = mTarget)
    
    # Return the output list #
    return(lOut)
    
  }
  
}

# New generator #
f_generator_2 <- function(aData, iStart_year, iEnd_year, bShuffle = FALSE,
                          iBatch_size = 138) {
  
  iYear <- iStart_year
  iRow <- 1
  
  # Define the generator function #
  function() {
    
    # Check if NOT shuffle #
    if (!bShuffle) {
      
      # Get the sample values #
      aSamples = aData[iRow:(iRow + iBatch_size - 1), paste0(c(iYear - 1, iYear)), ]
      
      # Get the target values #
      # Targets #
      mTargets = aData[iRow:(iRow + iBatch_size - 1), paste0(iYear + 1), "value"]
      
    } else {
      
      # Select rows randomly #
      vRows = sample(1:dim(aData)[1], iBatch_size)
      
      # Get the sample values #
      aSamples = aData[vRows, paste0(c(iYear - 1, iYear)), ]
      
      # Get the target values #
      mTargets = aData[vRows, paste0(iYear + 1), "value"]
      
    }
    
    
    # Remove values from samples #
    iVal_ind = which(dimnames(aSamples)[[3]] == "value")
    aSamples = aSamples[,, -iVal_ind]
    #iYear_ind = which(dimnames(aSamples)[[3]] == "Year")
    #aSamples = aSamples[,, -iYear_ind]
    
    # Update values #
    iRow <<- iRow + iBatch_size
    
    if (iRow >= 3174) {
      
      # Go to next year #
      iYear <<- iYear + 1
      
      # Reset to first row #
      iRow <<- 1
      
    }
    
    # Check if at bound #
    if (iYear > iEnd_year) {
      iYear <<- iStart_year
    }
    
    # Print statements #
    # cat("Row val:", iRow, "\n")
    # cat("Year val:", iYear, "\n")
    
    # Create output list #
    lOut = list(samples = aSamples,
                targets = mTargets)
    
    # Return output list #
    return(lOut)
    
  }
  
}


# LSTM neural network #
f_LSTM_recursive_2 <- function(aData,
                             sPython_path = "/Users/runelangergaard/opt/anaconda3/bin/python3",
                             iBatch_size = 138, # Needs to be divisible with 3174 #
                             sSave_path = paste0("/Users/runelangergaard/OneDrive - Aarhus universitet",
                                                 "/Oecon/10. semester/Code/data/DL_models"),
                             iN_epochs = 200) {
  
  # Load the required packages #
  require(keras)
  require(tensorflow)
  
  # Set the python paths #
  Sys.setenv(TENSORFLOW_PYTHON = sPython_path)
  use_python(sPython_path)
  
  # Get the vector of countries #
  vCountries = names(mthesis::mx_list)
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Create list for subpopulation indices #
  lSub_ind = list()
  
  # Find subpopulation indices #
  for (iSub in 1:length(vSub)) {
    lSub_ind[[vSub[iSub]]] = which(aData[, "1950", "variable"] == iSub)
  }
  
  # Create list for country indicies #
  lCountry_ind = list()
  
  # Find country indices #
  for (iC in 1:length(vCountries)) {
    lCountry_ind[[vCountries[iC]]] = which(aData[, "1950", "Country"] == iC)
  }
  
  # Log transform the values #
  aData_log = aData
  aData_log[,,"value"] = log(aData_log[,,"value"])
  
  # Define the training generator #
  train_gen <- f_generator_2(aData = aData_log, iStart_year = 1951, iEnd_year = 1994,
                             bShuffle = TRUE, iBatch_size = iBatch_size)
  
  # Define the validation generator #
  val_gen <- f_generator_2(aData = aData_log, iStart_year = 1995, iEnd_year = 1998,
                           bShuffle = FALSE, iBatch_size = iBatch_size)
  
  # Define the test generator #
  test_gen <- f_generator_2(aData = aData_log, iStart_year = 1999, iEnd_year = 2015,
                            bShuffle = FALSE, iBatch_size = iBatch_size)
  
  # How many steps to draw from the train_gen to see entire training data #
  # 44 columns of training data with 69 subpopulations in each = 44 * 69 = 3036 steps per epoch #
  # iTrain_steps = (1994-1951+1)*69
  iTrain_steps = (1994-1951 + 1)*(3174/iBatch_size)
  
  # How many steps to draw from the val_gen in order to see entire validation set #
  # iVal_steps = (1998-1995+1)*69
  iVal_steps = (1998-1995+1)*(3174/iBatch_size)

  # How many steps to draw from the test_gen in order to see entire test set #
  # iTest_steps = (2015-1999+1)*69
  iTest_steps = (2015-1999+1)*(3174/iBatch_size)
  
  # Define the model #
  model <- keras_model_sequential() %>%
    
  # Add LSTM layer #
    layer_lstm(units = 128,
               dropout = 0.1,
               recurrent_dropout = 0.1,
               return_sequences = TRUE,
               input_shape = c(2, dim(aData_log)[3] - 1),
               activation = "relu") %>%
    #layer_batch_normalization() %>%
    
  # Add another LSTM layer #
    layer_lstm(units = 128,
               dropout = 0.1,
               recurrent_dropout = 0.1,
               activation = "relu",
               return_sequences = FALSE) %>%
    #layer_batch_normalization() %>%
  
  # Add another LSTM layer #
    # layer_lstm(units = 64,
    #            dropout = 0.1,
    #            activation = "relu") %>%
    # layer_batch_normalization() %>%
    
  # Add normal dense layer #
    layer_dense(units = 64,
                activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(0.05) %>%

  # Add another normal dense layer #
    layer_dense(units = 64,
                activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(0.05) %>%
    
  # Add output layer #
    layer_dense(units = 1, activation = NULL)
  
  # Compile the model #
  model %>% compile(optimizer = optimizer_adam(lr = 0.1),
                    loss = "mse")
  
  # Define callback to reduce learning rate on plateau #
  lr_callback = callback_reduce_lr_on_plateau(factor = 0.80,
                                              patience = 10,
                                              verbose = 1,
                                              cooldown = 5,
                                              min_lr = 0.0005)
  
  # Define saving path #
  sSave_file = paste0(sSave_path, "/",
                      "LSTM_1_all_pop.mod")
  
  # Define model saving callback #
  model_callback = callback_model_checkpoint(filepath = sSave_file,
                                             verbose = 1,
                                             save_best_only = TRUE)
  
  # Fit the model #
  history <- model %>% fit_generator(train_gen,
                                     steps_per_epoch = iTrain_steps,
                                     epochs = iN_epochs,
                                     validation_data = val_gen,
                                     validation_steps = iVal_steps,
                                     callbacks = list(lr_callback, model_callback),
                                     view_metrics = TRUE)
  
  # Load the best saved model #
  cat("\n", "Loading best model...........", "\n")
  
  model = load_model_hdf5(sSave_file)
  
  # Define test years #
  vTest_years <- 2000:2016
  
  # Define true values #
  mTrue = aData[,paste0(vTest_years), "value"]
  
  # Create empty matrix for predictions #
  mPred = matrix(data = NA, nrow = 3174, ncol = length(vTest_years),
                 dimnames = list(NULL, paste0(vTest_years)))
  
  # Make predictions #
  for (iYear in vTest_years) {
    
    # Define the test data for the given year #
    mTest = aData_log[, paste0(c(iYear-2, iYear-1)), ]
    
    # Remove value and year dimensions #
    iVal_ind_test = which(dimnames(mTest)[[3]] == "value")
    mTest = mTest[,, -iVal_ind_test]
    #iYear_ind_test = which(dimnames(mTest)[[3]] == "Year")
    #mTest = mTest[,, -iYear_ind_test]
    
    # Make the predictions #
    vLogPred = model %>% predict(mTest)
    
    # Assign to prediction matrix #
    mPred[, paste0(iYear)] = exp(vLogPred)
    
    # Update aData #
    aData_log[,paste0(iYear), "value"] = vLogPred
    
    
  }
  
  # Create a list for results #
  lRes = list()
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Find indicies #
      vInd_pop = intersect(lCountry_ind[[sC]], lSub_ind[[sSub]])
      
      # Define true values for population #
      mTrue_pop = mTrue[vInd_pop, ]
      
      # Define predictions for population #
      mPred_pop = mPred[vInd_pop, ]
      
      # Calculate squared errors for population #
      mErr2_pop = (as.matrix(mTrue_pop) - as.matrix(mPred_pop))^2
      
      # Calculate mse for population #
      dMSE_pop = mean(mErr2_pop)
      
      # Calculate RMSE for population #
      dRMSE_pop = sqrt(dMSE_pop)
      
      # Assign values #
      lRes[[sC]][[sSub]][["true"]] = mTrue_pop
      lRes[[sC]][[sSub]][["pred"]] = mPred_pop
      lRes[[sC]][[sSub]][["MSE"]] = dMSE_pop
      lRes[[sC]][[sSub]][["RMSE"]] = dRMSE_pop
      
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Calculate Average and Median MSE #
  lMSE = f_MSE_extract(lRes = lRes)
  
  # Define output list #
  lOut = list(Model = model,
              Results = lRes,
              Avg_MSE = lMSE$mean,
              Med_MSE = lMSE$median)
  
  # Return output list #
  return(lOut)
  
}

# Helper function to add recurrent LSTM layer #
f_add_LSTM_layer <- function(layer_in, bRet_seq, iLSTM_nodes,
                             input_shape = NULL,
                             l1_term, l2_term,
                             dAlpha,
                             dLSTM_dropout) {
  
  # Define the recurrent layer #
  layer_out = layer_in %>% layer_lstm(units = iLSTM_nodes,
                                      input_shape = input_shape,
                                      return_sequences = bRet_seq,
                                      kernel_regularizer = regularizer_l1_l2(l1 = l1_term,
                                                                             l2 = l2_term)) %>%
    layer_batch_normalization() %>%
    layer_activation_leaky_relu(alpha = dAlpha) %>%
    layer_dropout(rate = dLSTM_dropout)
  
  # Return the output layer #
  return(layer_out)
  
}

##############################
#########  New Model #########
##############################
# LSTM neural network #
f_LSTM_recursive_3 <- function(aData,
                               sPython_path = "/Users/runelangergaard/opt/anaconda3/bin/python3",
                               iBatch_size = 138, 
                               sSave_path = paste0("/Users/runelangergaard/OneDrive - Aarhus universitet",
                                                   "/Oecon/10. semester/Code/data/DL_models"),
                               iN_epochs = 200,
                               iN_lags = 3,
                               iN_rnn_layers = 3,
                               iN_dense_layers = 3,
                               l1_term = 0.0001,
                               l2_term = 0.0001,
                               dLSTM_dropout = 0.2,
                               dAlpha = 0.5,
                               bScale = TRUE) {
  
  # Load the required packages #
  require(keras)
  require(tensorflow)
  
  # Set the python paths #
  Sys.setenv(TENSORFLOW_PYTHON = sPython_path)
  use_python(sPython_path)
  
  # Get the vector of countries #
  vCountries = names(mthesis::mx_list)
  
  # Define subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Create list for subpopulation indices #
  lSub_ind = list()
  
  # Find subpopulation indices #
  for (iSub in 1:length(vSub)) {
    lSub_ind[[vSub[iSub]]] = which(aData[, "1950", "variable"] == iSub)
  }
  
  # Create list for country indicies #
  lCountry_ind = list()
  
  # Find country indices #
  for (iC in 1:length(vCountries)) {
    lCountry_ind[[vCountries[iC]]] = which(aData[, "1950", "Country"] == iC)
  }
  
  # Log transform the values #
  aData_log = aData
  aData_log[,,"value"] = log(aData_log[,,"value"])
  
  # Create the data split #
  lSplit = f_aData_split(aData = aData_log, iN_lags = iN_lags)
  
  # Get the training dimensions #
  vDim_train = dim(lSplit$train$x)
  
  # Extract inputs #
  aX_train = lSplit$train$x
  vY_train = lSplit$train$y
  
  # Check if scaling of inputs #
  if (bScale) {
    
    # Extract list of scaling #
    lScale = f_aData_scale(aX_train)
    
    # Return scaled data #
    aX_train = lScale$scaled
    
  }
  
  
  # Create the input layer #
  input = layer_input(shape = c(vDim_train[2], vDim_train[3]),
                      dtype = "float32",
                      name = "main_input")
  
  # Add recurrent layers #
  if (iN_rnn_layers == 1) {
    
    # Create output #
    output = input %>% f_add_LSTM_layer(bRet_seq = FALSE, iLSTM_nodes = 128,
                                        input_shape = c(vDim_train[2], vDim_train[3]),
                                        l1_term = l1_term, l2_term = l2_term,
                                        dAlpha = dAlpha,
                                        dLSTM_dropout = dLSTM_dropout)
    
  } else {
    
    # Create output #
    output = input %>% f_add_LSTM_layer(bRet_seq = TRUE, iLSTM_nodes = 128,
                                        input_shape = c(vDim_train[2], vDim_train[3]),
                                        l1_term = l1_term, l2_term = l2_term,
                                        dAlpha = dAlpha,
                                        dLSTM_dropout = dLSTM_dropout)
    
    # Add LSTM layers #
    for (k in 2:iN_rnn_layers) {
      
      # Make number of nodes decreasing #
      iN_nodes = floor(128 / 2^(k-1))
      
      # Ifelse statement to control bRet_seq #
      bRet_seq = ifelse(k == iN_rnn_layers, FALSE, TRUE)
      
      # Create output #
      output = output %>% f_add_LSTM_layer(bRet_seq = bRet_seq, iLSTM_nodes = iN_nodes,
                                           input_shape = c(vDim_train[2], vDim_train[3]),
                                           l1_term = l1_term, l2_term = l2_term,
                                           dAlpha = dAlpha,
                                           dLSTM_dropout = dLSTM_dropout)
      
      
      
    }
    
  }
    
  # Add dense layers #
  for (k in 1:iN_dense_layers) {
      
    # Make number of nodes decreasing #
    iN_nodes = floor(128 / 2^(k-1))
      
    # Add dense layer #
    output = output %>% layer_dense(units = iN_nodes) %>% 
      layer_batch_normalization() %>%
      layer_activation_leaky_relu(alpha = dAlpha) %>%
      layer_dropout(rate = dLSTM_dropout)
    
  }
    
  # Add output layer #
  output = output %>% layer_dense(units = 1,
                                  activation = NULL,
                                  name = "main_output")
    
  # Create the model #
  Model <- keras_model(inputs = input,
                       outputs = output)
    
  # Define optimizer #
  #lAdam = keras::optimizer_adam(lr = 0.001)
  lAdam = keras::optimizer_adam(lr = 0.1)
    
  # Compile the model #
  Model %>% keras::compile(loss = 'mean_squared_error',
                           optimizer = lAdam
  )
    
  # Define callback to reduce learning rate on plateau #
  lr_callback = callback_reduce_lr_on_plateau(factor = 0.80,
                                              patience = 10,
                                              verbose = 1,
                                              cooldown = 5,
                                              min_lr = 0.0005)
    
  # Define saving path #
  sSave_file = paste0(sSave_path, "/",
                      "LSTM_3_all_pop.mod")
    
  # Define model saving callback #
  model_callback = callback_model_checkpoint(filepath = sSave_file,
                                             verbose = 1,
                                             save_best_only = TRUE)
    
  # Fit the model #
  Model %>% fit(x = aX_train,
                y = vY_train,
                epochs = iN_epochs, verbose = 1,
                validation_split = 0.1,
                callbacks = list(lr_callback, model_callback),
                view_metrics = TRUE)
    
  # Load the best model #
  cat("Loading the best model......", "\n")
  Model = load_model_hdf5(sSave_file)
    
  # Define test years #
  vTest_years <- 2000:2016
  
  # Define true values #
  mTrue = aData[,paste0(vTest_years), "value"]
  
  # Create empty matrix for predictions #
  mPred = matrix(data = NA, nrow = 3174, ncol = length(vTest_years),
                 dimnames = list(NULL, paste0(vTest_years)))
  
  # Make predictions #
  for (iYear in vTest_years) {
    
    # Define the test data for the given year #
    mTest = lSplit$test_obs[[paste0(iYear)]][["x"]]
    
    # Check if scaling #
    if (bScale) {
      
      mTest = f_testobs_scale(aTest = mTest, vMean = lScale$mean, vSd = lScale$sd)
      
    }
    
    # Make the predictions #
    vLogPred = Model %>% predict(mTest)
    
    # Assign to prediction matrix #
    mPred[, paste0(iYear)] = exp(vLogPred)
    
  }
  
  # Create a list for results #
  lRes = list()
  
  # Loop over countries #
  for (sC in vCountries) {
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Find indicies #
      vInd_pop = intersect(lCountry_ind[[sC]], lSub_ind[[sSub]])
      
      # Define true values for population #
      mTrue_pop = mTrue[vInd_pop, ]
      
      # Define predictions for population #
      mPred_pop = mPred[vInd_pop, ]
      
      # Calculate squared errors for population #
      mErr2_pop = (as.matrix(mTrue_pop) - as.matrix(mPred_pop))^2
      
      # Calculate mse for population #
      dMSE_pop = mean(mErr2_pop)
      
      # Calculate RMSE for population #
      dRMSE_pop = sqrt(dMSE_pop)
      
      # Assign values #
      lRes[[sC]][[sSub]][["true"]] = mTrue_pop
      lRes[[sC]][[sSub]][["pred"]] = mPred_pop
      lRes[[sC]][[sSub]][["MSE"]] = dMSE_pop
      lRes[[sC]][[sSub]][["RMSE"]] = dRMSE_pop
      
    } # End subpopulation loop #
    
  } # End country loop #
  
  # Calculate Average and Median MSE #
  lMSE = f_MSE_extract(lRes = lRes)
  
  # Define output list #
  lOut = list(Model = Model,
              Results = lRes,
              Avg_MSE = lMSE$mean,
              Med_MSE = lMSE$median)
  
  # Return output list #
  return(lOut)
  
}

##############################
#########  New Model #########
##############################
# LSTM Neural Network #
# With the added cohort effect #
f_LSTM_recursive_cohort <- function(lSplit,
                                    sPython_path = "/Users/runelangergaard/opt/anaconda3/bin/python3",
                                    iBatch_size = 1024,
                                    sSave_path = paste0("/Users/runelangergaard/OneDrive - Aarhus universitet",
                                                        "/Oecon/10. semester/Code/data/DL_models"),
                                    iN_epochs = 200,
                                    iDrop_rate = 0.1) {
  
  # Require the Deep Learning packages #
  require(keras)
  require(tensorflow)
  require(CatEncoders)
  
  # Set python paths #
  Sys.setenv(TENSORFLOW_PYTHON = sPython_path)
  use_python(sPython_path)
  
  # Remove total from data #
  #lSplit$train = lSplit$train[lSplit$train$variable != "Total", ]
  #lSplit$test = lSplit$test[lSplit$test$variable != "Total", ]
  #lSplit$test_obs = lSplit$test_obs[lSplit$test_obs$variable != "Total", ]
  
  # Adding the cohort effect #
  lSplit[["train"]][, "Cohort"] = lSplit[["train"]][, "Year"] - lSplit[["train"]][, "Age"]
  lSplit[["test_obs"]][, "Cohort"] = lSplit[["test_obs"]][, "Year"] - lSplit[["test_obs"]][, "Age"]
  
  # Grab the training data #
  mTrain = lSplit$train
  
  # Extract training data X #
  mX_train = subset(mTrain, select = -c(vY))
  
  # Transform Country variable to factor variable #
  mX_train[, "Country"] = as.factor(mX_train[, "Country"])
  
  # Transform Country variable to integer variable #
  mX_train[, "Country"] = as.integer(mX_train[, "Country"])
  
  # Create country encoder #
  # Country_encoder = CatEncoders::OneHotEncoder.fit(as.matrix(mX_train[, "Country"]))
  
  # Create country matrix #
  # mCountry = CatEncoders::transform(Country_encoder, as.matrix(mX_train[, "Country"]),
  #                                   sparse = FALSE)
  
  # Transform gender/variable to factor #
  mX_train[, "variable"] = as.factor(mX_train[, "variable"])
  
  # Create gender encoder #
  # Gender_encoder = CatEncoders::OneHotEncoder.fit(as.matrix(mX_train[, "variable"]))
  
  # Transform to gender to one-hot encoding #
  # mGender = CatEncoders::transform(Gender_encoder, as.matrix(mX_train[, "variable"]),
  #                                  sparse = FALSE)
  
  # Transform gender/variable to integer #
  mX_train[, "variable"] = as.integer(mX_train[, "variable"])
  
  # Transform training dataframe to a matrix #
  mX_train = as.matrix(mX_train)
  
  # Make age go from 0 to 45 #
  mX_train[, "Age"] = mX_train[, "Age"] - 50
  
  # Make Country go from 0 to 22 #
  mX_train[, "Country"] = mX_train[, "Country"] - 1
  
  # Make gender/variable go from 0 to 2 #
  mX_train[, "variable"] = mX_train[, "variable"] - 1
  
  # Make cohort start in 0 #
  min_cohort_train = min(mX_train[, "Cohort"])
  mX_train[, "Cohort"] = mX_train[, "Cohort"] - min_cohort_train
  
  #cat("Min cohort train:", min_cohort_train, "\n")
  
  # Grab Y values #
  vY_train = subset(mTrain, select = c(vY))
  
  # Transform vY to matrix #
  vY_train = as.matrix(vY_train)
  
  # Log transformation of value in mX_train #
  mX_train[, "value"] = log(mX_train[, "value"])
  
  # Log transformation of Y variable #
  vY_train = log(vY_train)
  
  # Grab the test observations #
  mX_test = lSplit$test_obs
  
  # Transform Country variable to factor variable #
  mX_test[, "Country"] = as.factor(mX_test[, "Country"])
  
  # Transform Country variable to integer variable #
  mX_test[, "Country"] = as.integer(mX_test[, "Country"])
  
  # Transform Country to one-hot-encoding #
  # mCountry_test = CatEncoders::transform(Country_encoder, as.matrix(mX_test[, "Country"]),
  #                                        sparse = FALSE)
  
  # Transform gender/variable to factor #
  mX_test[, "variable"] = as.factor(mX_test[, "variable"])
  
  # Transform gender/variable to integer #
  mX_test[, "variable"] = as.integer(mX_test[, "variable"])
  
  # One-hot encode gender #
  # mGender_test = CatEncoders::transform(Gender_encoder, as.matrix(mX_test[, "variable"]),
  #                                       sparse = FALSE)
  
  # Transform training dataframe to a matrix #
  mX_test = as.matrix(mX_test)
  
  # Make age go from 0 to 45 #
  mX_test[, "Age"] = mX_test[, "Age"] - 50
  
  # Make Country go from 0 to 22 #
  mX_test[, "Country"] = mX_test[, "Country"] - 1
  
  # Make gender/variable go from 0 to 2 #
  mX_test[, "variable"] = mX_test[, "variable"] - 1
  
  # Normalize cohort to training values (that start in 0) #
  mX_test[, "Cohort"] = mX_test[, "Cohort"] - min_cohort_train
  
  # Make log-transformation of value #
  mX_test[, "value"] = log(mX_test[, "value"])
  
  # Scale Year #
  # lScale_year = f_MinMaxScaler(mX_train[, "Year"],
  #                              mX_test[, "Year"])
  # 
  # # Assign scaled vaulues #
  # mX_train[, "Year"] = lScale_year$train
  # mX_test[, "Year"] = lScale_year$test
  
  # Scale log death rates #
  # lScale_logmx = f_MinMaxScaler(mX_train[, "value"],
  #                               mX_test[, "value"])
  # lScale_vY = f_MinMaxScaler(vY_train,
  #                            vY_train)
  # 
  # 
  # # Assign scaled values #
  # mX_train[, "value"] = lScale_logmx$train
  # vY_train = lScale_vY$train
  # mX_test[, "value"] = lScale_logmx$test
  
  # Define the training inputs #
  vYear = mX_train[, "Year"]
  vAge = mX_train[, "Age"]
  mCountry = mX_train[, "Country"]
  mGender = mX_train[, "variable"]
  vValue = mX_train[, "value"]
  vCohort = mX_train[, "Cohort"]
  
  # Define test inputs #
  vYear_test = mX_test[, "Year"]
  vAge_test = mX_test[, "Age"]
  mCountry_test = mX_test[, "Country"]
  mGender_test = mX_test[, "variable"]
  vValue_test = mX_test[, "value"]
  vCohort_test = mX_test[, "Cohort"]
  
  # Create the inout for the model #
  Year = keras::layer_input(shape = c(1), dtype = "float32", name = "Year")
  Age = keras::layer_input(shape = c(1), dtype = "int32", name = "Age")
  Country = keras::layer_input(shape = c(1), dtype = "int32", name = "Country")
  Gender = keras::layer_input(shape = c(1), dtype = "int32", name = "Gender")
  value = keras::layer_input(shape = c(1), dtype = "float32", name = "value")
  Cohort = keras::layer_input(shape = c(1), dtype = "int32", name = "Cohort")
  
  # Embed the Age input variable #
  Age_embed = Age %>% keras::layer_embedding(input_dim = 46, output_dim = 46/2,
                                             input_length = 1, name = 'Age_embed')
  
  # Embed the Country input variable #
  Country_embed = Country %>% keras::layer_embedding(input_dim = 23, output_dim = 11,
                                                     input_length = 1, name = "Country_embed")
  
  # Embed the Gender input variable #
  Gender_embed = Gender %>% keras::layer_embedding(input_dim = 3, output_dim = 1,
                                                   input_length = 1, name = "Gender_embed")
  
  # Embed the Cohort input variable #
  Cohort_embed = Cohort %>% keras::layer_embedding(input_dim = 112, output_dim = floor(112/2),
                                                   input_length = 1, name = "Cohort_embed")
  
  # LSTM for Age #
  LSTM_age = Age_embed %>% keras::layer_lstm(128, activation = "relu",
                                             dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # LSTM for Country #
  LSTM_country = Country_embed %>% keras::layer_lstm(128, activation = "relu",
                                                     dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # LSTM for gender #
  LSTM_gender = Gender_embed %>% keras::layer_lstm(128, activation = "relu",
                                                   dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # LSTM for cohort #
  LSTM_cohort = Cohort_embed %>% keras::layer_lstm(128, activation = "relu",
                                                   dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # Define the features #
  vFeatures = keras::layer_concatenate(list(Year, LSTM_age, LSTM_country, LSTM_gender, value, LSTM_cohort))
  
  # Create Add 1st dense layer to model #
  Model_middle = vFeatures %>%
    keras::layer_dense(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    
    # Add 2nd dense layer #
    keras::layer_dense(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    
    # Add 3rd dense layer #
    keras::layer_dense(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    
    # Add 4th dense layer #
    keras::layer_dense(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05)
  
  # Create model output #
  Main_output = keras::layer_concatenate(list(vFeatures, Model_middle)) %>% 
    keras::layer_dense(units = 128, activation = "relu") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_dropout(0.05) %>%
    keras::layer_dense(units = 1, activation = NULL, name = "Main_output")
  
  # Create model output #
  Model = keras::keras_model(inputs = c(Year, Age, Country, Gender, value, Cohort),
                             outputs = c(Main_output))
  
  # Define optimizer #
  #lAdam = keras::optimizer_adam(lr = 0.001)
  lAdam = keras::optimizer_adam(lr = 0.1)
  
  # Compile the model #
  Model %>% keras::compile(loss = 'mean_squared_error',
                           optimizer = lAdam
  )
  
  # Define callback to reduce learning rate on plateau #
  lr_callback = callback_reduce_lr_on_plateau(factor = 0.80,
                                              patience = 10,
                                              verbose = 1,
                                              cooldown = 5,
                                              min_lr = 0.0005)
  
  # Define saving path #
  sSave_file = paste0(sSave_path, "/",
                      "LSTM_1_all_pop_cohort.mod")
  
  # Define model saving callback #
  model_callback = callback_model_checkpoint(filepath = sSave_file,
                                             verbose = 1,
                                             save_best_only = TRUE)
  
  # Fit the model #
  Model %>% fit(x = list(vYear, vAge,
                         mCountry, mGender,
                         vValue, vCohort),
                y = vY_train, batch_size = iBatch_size,
                epochs = iN_epochs, verbose = 1,
                validation_split = 0.1,
                callbacks = list(lr_callback, model_callback),
                view_metrics = TRUE)
  
  # Load the best model #
  Model = load_model_hdf5(sSave_file)
  
  # Make predictions matrix #
  mPred = subset(lSplit$test_obs, select = -c(Year))
  
  # Grab true values #
  mTrue = lSplit$test
  
  # Loop over the prediction years #
  for (iYear in 2000:2016) {
    
    # Make predictions #
    vLogPredict = Model %>% predict(list(vYear_test, vAge_test,
                                         mCountry_test, mGender_test,
                                         vValue_test, vCohort_test))
    
    # Replace NaN values with -Inf which means exp(-Inf) = 0 #
    vLogPredict[is.na(vLogPredict)] = -Inf
    
    # Update year in test values #
    vYear_test = rep(iYear, length(vYear_test))
    
    # Update cohort #
    vCohort_test = vCohort_test + 1
    
    # Put new predictions onto test values #
    vValue_test = vLogPredict
    
    #cat("Head of predictions", head(vLogPredict), "\n")
    
    # Rescale predictions to original log-levels #
    # vLogPredict = f_MinMaxScaler_inv(vX_scaled = vLogPredict,
    #                                  dMin = lScale_vY$min,
    #                                  dMax = lScale_vY$max)
    
    # Attach predictions to mPred #
    mPred[, paste0(iYear)] = exp(vLogPredict)
    #mPred[, paste0(iYear)] = vLogPredict
    
  }
  
  # Reshape the mTrue #
  mTrue = f_mTrue_cast(mTest = mTrue)
  
  # Sort the mTrue dataframe #
  mTrue = mTrue[with(mTrue, order(Country, variable, Age)), ]
  
  # Sort the mPred dataframe #
  mPred = mPred[with(mPred, order(Country, variable, Age)), ]
  
  # Create list for population results #
  lRes = list()
  
  # Extract countries #
  vCountries = unique(mTrue[, "Country"])
  
  # Extract subpopulations #
  vSub = unique(mTrue[, "variable"])
  
  # Loop over countries and subpopulations #
  for (sC in vCountries) {
    
    # Create list element for country #
    lRes[[sC]] = list()
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Extract mTrue for the population #
      mTrue_pop = mTrue[(mTrue$Country == sC) &
                          (mTrue$variable == sSub), ]
      
      # Extract mPred for the population #
      mPred_pop = mPred[(mPred$Country == sC) &
                          (mPred$variable == sSub), ]
      
      # Calculate Squared errors for the population #
      mErr2_pop = (as.matrix(mTrue_pop[, paste0(2000:2016)]) - as.matrix(mPred_pop[, paste0(2000:2016)]))^2
      
      # Calculate MSE for the population #
      dMSE_pop = mean(mErr2_pop)
      
      # Calculate RMSE for the population #
      dRMSE_pop = sqrt(dMSE_pop)
      
      # Append results to output #
      lRes[[sC]][[sSub]][["mTrue"]] = mTrue_pop
      lRes[[sC]][[sSub]][["mPred"]] = mPred_pop
      lRes[[sC]][[sSub]][["MSE"]] = dMSE_pop
      lRes[[sC]][[sSub]][["RMSE"]] = dRMSE_pop
      
    } # End subpopulation loop #
  } # End country loop #
  
  # Calculate Average and Median MSE #
  lMSE = f_MSE_extract(lRes = lRes)
  
  # Define output list #
  lOut = list(Model = Model,
              Results = lRes,
              Avg_MSE = lMSE$mean,
              Med_MSE = lMSE$median)
  
  # Return output list #
  return(lOut)
  
}
