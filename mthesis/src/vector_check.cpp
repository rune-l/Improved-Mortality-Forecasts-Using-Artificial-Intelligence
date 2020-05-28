#include <Rcpp.h>


using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::LogicalVector contains(Rcpp::StringVector vMCS_models, Rcpp::StringVector vModels) {
  
  // Find size of MCS model vector //
  int iMCS = vMCS_models.size();
  
  // Create logical vector with iMCS size //
  Rcpp::LogicalVector vBool(iMCS);
  
  // Loop over the MCS models //
  for (int i = 0; i < iMCS; i++) {
    
    // Check if given model is in the models vector //
    bool bCheck = std::find(vModels.begin(), vModels.end(), vMCS_models(i))!=vModels.end();
    
    // Append result //
    vBool(i) = bCheck;
    
  }
  
  // Return the checking vector //
  return vBool;
  
}