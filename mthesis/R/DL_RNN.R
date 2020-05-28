##############################
#########  New Model #########
##############################
# Normal Recurrent Neural Network #
# With the added cohort effect #
f_RNN_recursive_cohort <- function(lSplit,
                                   sPython_path,
                                   iBatch_size = 1024,
                                   sSave_path,
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
  
  # RNN for Age #
  RNN_age = Age_embed %>% keras::layer_simple_rnn(128, activation = "relu",
                                                  dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # RNN for Country #
  RNN_country = Country_embed %>% keras::layer_simple_rnn(128, activation = "relu",
                                                          dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # RNN for gender #
  RNN_gender = Gender_embed %>% keras::layer_simple_rnn(128, activation = "relu",
                                                        dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # RNN for cohort #
  RNN_cohort = Cohort_embed %>% keras::layer_simple_rnn(128, activation = "relu",
                                                        dropout = iDrop_rate, recurrent_dropout = iDrop_rate)
  
  # Define the features #
  vFeatures = keras::layer_concatenate(list(Year, RNN_age, RNN_country, RNN_gender, value, RNN_cohort))
  
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
                      "RNN_1_all_pop_cohort.mod")
  
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
