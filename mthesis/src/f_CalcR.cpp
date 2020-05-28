#include <RcppArmadillo.h>


using namespace arma;

// [[Rcpp::export]]
arma::mat f_CalcR(arma::mat Rxx, arma::mat Rxy, arma::mat Ryy, arma::mat Ryx) {
  
  // Calculate R
  arma::mat mOut = Rxx.i() * Rxy * Ryy.i() * Ryx;
  
  // Return output matrix //
  return(mOut);
  
}

// [[Rcpp::export]]
arma::cube f_hmm(arma::cube aData, int iN_lags) {
  
  // Find rows //
  int iRow = aData.n_rows;
  int iCol = aData.n_cols;
  int iSlice = aData.n_slices;
  
  // Define empty array //
  arma::cube aOut(iRow, iCol, iSlice);
  
  // Return empty array //
  return(aOut);
  
  // the result is that it just returns an array of the same dim as aData filled with zeros //
  
}