# Forecasting #
# Input transformed to have rows being ages and columns being years #
f_OneStepForecast <- function(df_mx_full, df_ex_full, sModel = "LC", iForecast_start = 2000, sC, sSubpop) {
  
  # Require the forecast package #
  require(forecast)
  
  # Grab the years #
  iYears = as.numeric(colnames(df_mx_full))
  
  # Function name #
  sFunc = paste0("f_fit", sModel)
  
  # Find the function #
  f_fitFunc = match.fun(sFunc)
  
  # Perform forecast for the first period #
  lmx_split = f_train_split(df_mx_full, iForecast_start)
  lex_split = f_train_split(df_ex_full, iForecast_start)
  df_for_model = f_StMoMoData(lmx_split$train, lex_split$train, sC = sC, sSubpop = sSubpop)
  lFit = f_fitFunc(df_for_model)
  lForecast = forecast(lFit, h = 1, gc.order = c(1,1,0), kt.method = "iarima")
  
  # Forecast for the remaining periods and append #
  for (iYear in (iForecast_start+1):iYears[length(iYears)]) {
    
    lmx_split = f_train_split(df_mx_full, iYear)
    lex_split = f_train_split(df_ex_full, iYear)
    df_for_model = f_StMoMoData(lmx_split$train, lex_split$train, sC = sC, sSubpop = sSubpop)
    lFit = f_fitFunc(df_for_model)
    
    lTmp_forecast = forecast(lFit, h = 1, gc.order = c(1,1,0), kt.method = "iarima")
    
    lForecast$rates = cbind(lForecast$rates, lTmp_forecast$rates)
    
  }
  
  return(lForecast)
  
}

# All population forecasting using single population models #
f_all_forecast_sp <- function(mx_list, ex_list, f_fitFunc, gc.order, kt.method, bSVD = FALSE) {
  
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
      tmp_mx_split = f_train_split(tmp_mx_df, iTest_start = 2000)
      tmp_ex_split = f_train_split(tmp_ex_df, iTest_start = 2000)
      
      # Save the test data #
      lRes[[sCountry]][[sSubpop]][["test"]] = tmp_mx_split$test
      
      # Transform to StMoMoData or (in one case demogdata) #
      tmp_mx_train = tmp_mx_split$train
      tmp_ex_train = tmp_ex_split$train
      
      df_train = f_StMoMoData(df_mx = tmp_mx_train, df_ex = tmp_ex_train,
                              sC = sCountry, sSubpop = sSubpop)
      
      if (bSVD) {
        
        # Check for zeros (log to 0 gives problems) #
        mZero_mx = tmp_mx_train==0
        mZero_ex = tmp_ex_train==0
        
        # Correct zeros to slightly above zero #
        tmp_mx_train[mZero_mx] = tmp_mx_train[mZero_mx]+1e-04
        tmp_mx_train[mZero_ex] = tmp_ex_train[mZero_ex]+1e-04
        
        # Make the demogdata #
        df_train = f_demogdata(df_mx = tmp_mx_train, df_ex = tmp_ex_train,
                               sC = sCountry, sSubpop = sSubpop)
      }
      
      # Fit the model #
      cat("Country = ", sCountry," and subpopulation = ", sSubpop, "\n")
      lFit = f_fitFunc(df_train)
      
      # Forecast #
      lForecast = forecast(lFit, h = 17, gc.order = gc.order, kt.method = kt.method)
      
      # Save forecast results #
      lRes[[sCountry]][[sSubpop]][["forecast"]] = lForecast
      
    }
    
    
  }
  
  # Output the result list #
  return(lRes)
  
}

