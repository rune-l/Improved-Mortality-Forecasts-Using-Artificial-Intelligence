#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::vec f_Confidence_band(arma::vec vTrue, arma::vec vPred) {
  
  // Calculate size //
  int iT = vTrue.size();
  
  // Calculate output vector //
  arma::vec vOut(iT);
  
  // Loop over values //
  for (int t = 0; t < iT; t++) {
    
    // Print statement //
    // Rprintf("t = %i", t);
    
    // Calculate MSE for observations up until time t #
    double dMSE = mean(pow((vTrue.head(t+1) - vPred.head(t+1)), 2));
    
    // Append RMSE //
    vOut(t) = pow(dMSE, 0.5);
    
  }
  
  // Return output //
  return(vOut);
  
}