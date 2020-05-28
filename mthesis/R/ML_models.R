# Random forest model #
f_RF = function(mX, vY) {
  
  # Require the randomForest package #
  require(randomForest)
  
  # Fit a random forest model #
  lFit = randomForest::randomForest(x = mX, y = vY, do.trace = TRUE)
  
  # Return the fitted random forest model #
  return(lFit)
  
}

# Boosting model #
f_Boost <- function(mX, vY) {
  
  # Define data frame #
  mDf = cbind(mX, vY = vY)
  
  # Require the gbm package #
  require(gbm)
  
  # Train boosting model #
  lGBM = gbm(vY ~ ., data = mDf, n.trees = 10000, interaction.depth = 2, shrinkage = 0.01,
             verbose = TRUE)
  
  # Return the model
  return(lGBM)
  
}

# Extreme gradient boosting #
f_XGB <- function(mX, vY) {
  
  # Require the xgboost package #
  require(xgboost)
  
  # Put data into the correct format #
  mInp = f_DataXGB(mX, vY)
  
  # Train the boosting model #
  lXGB = xgboost(data = mInp, nrounds = 10000, print_every_n = 100, early_stopping_rounds = 250)
  
  # Output the boosting model #
  return(lXGB)
  
}

# SVM #
f_SVM <- function(mX, vY) {
  
  # Define mDf #
  mDf = cbind(mX, vY = vY)
  
  # Require the e1071 package #
  require(e1071)
  
  # Fit the SVM model #
  lSVM = e1071::svm(vY ~ ., mDf)
  
  # Return the model #
  return(lSVM)
  
}

# Deep forest #
f_DeepForest <- function(mX, vY) {
  
  # Require the deep forest package #
  require(gcForest)
  
  # Calculate number of input variables #
  iN_var = dim(mX)[2]
  
  # Reshape the data to be a matrix #
  mX[, "Country"] = as.integer(as.factor(mX[, "Country"]))
  mX[, "variable"] = as.integer(as.factor(mX[, "variable"]))
  
  mX = as.matrix(mX)
  
  # Rename the columns to integers #
  colnames(mX) <- NULL
  
  # Define the model #
  lMod = gcforest(shape_1X = iN_var)
  #window = max(floor(iN_var/2), 1)
  
  # Train the model #
  lMod$fit(mX, vY)
  
  # Output the model #
  return(lMod)
  
}
