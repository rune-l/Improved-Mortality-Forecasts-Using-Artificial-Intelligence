#include <RcppArmadillo.h>


using namespace arma;

// [[Rcpp::export]]
Rcpp::List f_aData_train_fillC(arma::cube aX,
                               arma::mat mY,
                               int iN_lags,
                               int iN_row) {
  
  // Extract the third dimension of aData //
  int iSlices = aX.n_slices;
  
  // Calculate iN_train //
  int iN_train = iN_row * 3174;
  
  // Define starting column //
  int iStart = (iN_lags - 1);
  
  // Define the stopping column //
  int iStop = 49; // 1998 is column 49 //
  
  // Create output array for x values //
  arma::cube aOut(iN_train, iN_lags, iSlices);
  
  // Create vector for y values //
  arma::vec vY(iN_train);
  
  // Create counter //
  int iRow = 0;
  
  // Loop over the rows //
  for (int i = 0; i < 3174; i++) {
    
    // Loop inside each row //
    for (int k = iStart; k < iStop; k++) {
      
      // Find X values //
      aOut(span(iRow), span::all, span::all) = 
        aX(span(i), span(k - iStart, k), span::all);
      
      // Find Y value //
      vY(iRow) = mY(i, k + 1);
      
      // Update counter with 1 //
      iRow += 1;
      
    }
    
    
  }
  
  // Printe statement of iRow //
  // cout << "iRow is at " << iRow << " at the end" << endl;
  
  // Create output list //
  Rcpp::List lOut =  Rcpp::List::create(Rcpp::Named("x") = aOut,
                                        Rcpp::Named("y") = vY);
  
  // Return output list //
  return(lOut);
  
}

// [[Rcpp::export]]
Rcpp::List f_aData_scale_C(arma::cube aX) {
  
  // Calculate number of slices //
  int iN_slices = aX.n_slices;
  
  // Create empty vector for storing means //
  arma::vec vMean(iN_slices);
  
  // Create empty vector for storing standard deviations //
  arma::vec vSd(iN_slices);
  
  // Create cube for scaled data //
  arma::cube aX_scaled = aX;
  
  // Loop over the slices //
  for (int i = 0; i < iN_slices; i++) {
    
    // Calculate mean for slice and assign //
    double dMean = mean(mean(aX.slice(i)));
    vMean(i) = dMean;
    
    // Calculate sd for slice and assign //
    arma::vec vSlice = vectorise(aX.slice(i));
    double dSd = stddev(vSlice);
    vSd(i) = dSd;
    
    // Scale the slice //
    aX_scaled.slice(i) = (aX_scaled.slice(i) - dMean)/dSd;
    
  }
  
  
  // Create output list //
  Rcpp::List lOut =  Rcpp::List::create(Rcpp::Named("scaled") = aX_scaled,
                                        Rcpp::Named("mean") = vMean,
                                        Rcpp::Named("sd") = vSd);
  
  // Return vector of means //
  return(lOut);
  
}