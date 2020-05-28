# Mortality projections far into the future #

# For the LC model #
# All population forecasting using single population models #
f_LC_projection <- function(mx_list, ex_list) {
  
  # Extract all the countries #
  vCountries = names(mx_list)
  
  # Define the subpopulations #
  vSubpop = c("Female", "Male", "Total")
  
  # Create list to store results on #
  lRes = list()
  
  # Loop over each country and subpopulation #
  for (sCountry in vCountries) {
    
    for (sSubpop in vSubpop) {
      
      # Prepare the data #
      tmp_mx_df = f_DataTransform(df = mx_list[[sCountry]],
                                  sSubpop = sSubpop)
      tmp_ex_df = f_DataTransform(df = ex_list[[sCountry]],
                                  sSubpop = sSubpop)
      
      # Split the data #
      tmp_mx_split = f_train_split(tmp_mx_df, iTest_start = 2017)
      tmp_ex_split = f_train_split(tmp_ex_df, iTest_start = 2017)
      
      # Transform to StMoMoData or (in one case demogdata) #
      tmp_mx_train = tmp_mx_split$train
      tmp_ex_train = tmp_ex_split$train
      
      df_train = f_StMoMoData(df_mx = tmp_mx_train, df_ex = tmp_ex_train,
                              sC = sCountry, sSubpop = sSubpop)
      
      # Fit the model #
      cat("Country = ", sCountry," and subpopulation = ", sSubpop, "\n")
      lFit = f_fitLC(df_train)
      
      # Forecast #
      gc.order = c(1,1,0)
      kt.method = "mrwd"
      
      lForecast = forecast(lFit, h = length(2017:2060), gc.order = gc.order, kt.method = kt.method)
      
      # Save forecast results #
      lRes[[sCountry]][[sSubpop]][["forecast"]] = lForecast
      lRes[[sCountry]][[sSubpop]][["mPred"]] = lForecast[["rates"]]
      
    }
    
    
  }
  
  # Output the result list #
  return(lRes)
  
}


# For LSTM #
f_LSTM_recursive_cohort_projections <- function(lSplit,
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
  mX_train = base::subset(mTrain, select = -c(vY))
  
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
  vY_train = base::subset(mTrain, select = c(vY))
  
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
  
  #cat("Min cohort train:", min_cohort_train)
  
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
  Cohort_embed = Cohort %>% keras::layer_embedding(input_dim = 156, output_dim = floor(156/2),
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
                      "LSTM_1_all_pop_cohort_projections.mod")
  
  # Define model saving callback #
  model_callback = callback_model_checkpoint(filepath = sSave_file,
                                             verbose = 1,
                                             save_best_only = TRUE)
  
  # Fit the model #
  Model %>% keras::fit(x = list(vYear, vAge,
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
  mPred = base::subset(lSplit$test_obs, select = -c(Year))
  
  # Loop over the prediction years #
  for (iYear in 2017:2060) {
    
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
  
  # Sort the mPred dataframe #
  mPred = mPred[with(mPred, order(Country, variable, Age)), ]
  
  # Create list for population results #
  lRes = list()
  
  # Extract countries #
  vCountries = sort(unique(mthesis::df_mx_all[, "Country"]))
  
  # Extract subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Loop over countries and subpopulations #
  for (sC in vCountries) {
    
    # Create list element for country #
    lRes[[sC]] = list()
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Extract mPred for the population #
      mPred_pop = mPred[(mPred$Country == sC) &
                          (mPred$variable == sSub), ]
      
      # Append results to output #
      lRes[[sC]][[sSub]][["mPred"]] = mPred_pop
      
    } # End subpopulation loop #
  } # End country loop #
  
  # Define output list #
  lOut = list(Model = Model,
              Results = lRes)
  
  # Return output list #
  return(lOut)
  
}



# For FFNN #
f_FFNN_recursive_projections <- function(lSplit,
                               sPython_path = "/Users/runelangergaard/opt/anaconda3/bin/python3",
                               iBatch_size = 1024,
                               sSave_path = paste0("/Users/runelangergaard/OneDrive - Aarhus universitet",
                                                   "/Oecon/10. semester/Code/data/DL_models"),
                               iN_epochs = 500) {
  
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
  mX_train = base::subset(mTrain, select = -c(vY))
  
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
  vY_train = base::subset(mTrain, select = c(vY))
  
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
                      "FFNN_projections.mod")
  
  # Define model saving callback #
  model_callback = callback_model_checkpoint(filepath = sSave_file,
                                             verbose = 1,
                                             save_best_only = TRUE)
  
  # Fit the model #
  Model %>% keras::fit(x = list(vYear, vAge,
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
  mPred = base::subset(lSplit$test_obs, select = -c(Year))
  
  # Loop over the prediction years #
  for (iYear in 2017:2060) {
    
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
  
  # Sort the mPred dataframe #
  mPred = mPred[with(mPred, order(Country, variable, Age)), ]
  
  # Create list for population results #
  lRes = list()
  
  # Extract countries #
  vCountries = sort(unique(mthesis::df_mx_all[, "Country"]))
  
  # Extract subpopulations #
  vSub = c("Female", "Male", "Total")
  
  # Make predictions for training data #
  vLogPredict_train = Model %>% predict(list(vYear, vAge,
                                             mCountry, mGender,
                                             vValue))
  vPredict_train = exp(vLogPredict_train)
  
  # Convert data frame #
  mTrain_pred = base::subset(mTrain, select = c("Age", "Country", "variable", "Year", "value"))
  mTrain_pred[, "value"] = vPredict_train
  mTrain_pred[, "Year"] = mTrain_pred[, "Year"] + 1
  
  # Cast the data frame #
  mPred_train = reshape::cast(data = mTrain_pred, value = "value",
                      formula = Age + Country + variable ~ Year,
                      measure.vars = value)
  
  # Sort the mPred_train dataframe #
  mPred_train = mPred_train[with(mPred_train, order(Country, variable, Age)), ]
  
  # Loop over countries and subpopulations #
  for (sC in vCountries) {
    
    # Create list element for country #
    lRes[[sC]] = list()
    
    # Loop over subpopulations #
    for (sSub in vSub) {
      
      # Extract mPred for the population #
      mPred_pop = mPred[(mPred$Country == sC) &
                          (mPred$variable == sSub), ]
      
      mPred_pop_train = mPred_train[(mPred_train$Country == sC) &
                                (mPred_train$variable == sSub), ]
      
      lRes[[sC]][[sSub]][["mPred"]] = mPred_pop
      lRes[[sC]][[sSub]][["mPred_train"]] = mPred_pop_train
      
    } # End subpopulation loop #
  } # End country loop #
  
  # Define output list #
  lOut = list(Model = Model,
              Results = lRes)
  
  # Return output list #
  return(lOut)
  
}

