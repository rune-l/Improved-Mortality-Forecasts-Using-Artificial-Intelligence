#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
Rcpp::List f_KalmanFilterC(arma::mat mY, arma::cube aZ,
                           arma::cube aH, arma::cube aT,
                           arma::cube aR, arma::cube aQ,
                           arma::vec vA1, arma::mat mP1,
                           int iJ) {
  
  // Dimensions
  int iN = mY.n_rows;
  int iP = mY.n_cols;
  int iM = vA1.size();
  
  // Create empty matrices and arrays to store results
  arma::mat mV_Filt(iN, iP); // Prediction error.
  arma::mat mF_Filt(iN, iP); // Prediction error variance.
  arma::cube aK_Filt(iM, iP, iN); // Kalman gain.
  arma::cube aA_Filt(iM, iP + 1, iN); // Filtered state mean.
  arma::cube aA_Pred(iM, 1, iN); // Predicted state mean.
  arma::cube aP_Filt(iM, iM, iN); // Filtered state variance.
  arma::cube aP_Pred(iM, iM, iN); // Predicted state variance.
  
  arma::vec vLL(iN); // Likelihood.
  
  // Create empty matrices and arrays for forecasting results 
  arma::mat mA_For(iM, iJ); // State forecast.
  arma::mat mY_For(iP, iJ); // Condtional mean forecast.
  arma::cube aP_For(iM, iM, iJ); // State variance forecast.
  arma::cube aF_For(iP, iP, iJ); // Condtional mean variance forecast.
  
  // Create arrays for storing smoothing results //
  arma::cube aL_Smoo(iM, iM, iN); 
  arma::cube aR_Smoo(iM, iP+1, iN+1); // Smoothed cummulant.
  arma::cube aN_Smoo(iM, iM, iN+1); // Smoothed cummulant variance.
  arma::cube aA_Smoo(iM, 1, iN); // Smoothed state mean.
  arma::cube aV_Smoo(iM, iM, iN); // Smoothed state variance.
  
  // Initialize values for filtering 
  aA_Pred.subcube(arma::span::all, arma::span(0), arma::span(0)) = vA1;
  aP_Pred.slice(0) = mP1;
  
  
  
  double dC = -0.5 * (iN * iP * 1.0) * log(M_PI * 2.0);
  
  // Initialization for smoothing:
  aR_Smoo.subcube(arma::span::all, arma::span(iP), arma::span(iN)).fill(0);
  aN_Smoo.slice(iN).fill(0);
  
  // Filtering:
  for (int t = 0; t < iN; t++) {
    
    // Check aH //
    arma::mat aH_Check = aH.slice(t);
    aH_Check.diag().fill(0);
    bool baH_Check = arma::all(vectorise(aH_Check));
  
    arma::mat mL(iP, iP);
    
    // Check aH //
    if (baH_Check == true) {
      
      // Cholensky decomposition //
      arma::mat aH_chol = arma::chol(aH.slice(t));
      arma::mat aH_chol_diag = arma::diagmat(aH_chol);
      
      // LDL decomposition from cholensky //
      arma::mat mD = arma::powmat(aH_chol_diag, 2);
      mL = aH_chol.t() * aH_chol_diag.i();
      
      // Transform observations //
      arma::mat mLInv1 = mL.i();
      arma::mat mLInv2 = mL.t().i();
      
      aH.slice(t) = mLInv1 * aH.slice(t) * mLInv2;
      aZ.slice(t) = mLInv1 * aZ.slice(t);
      mY.row(t) = mLInv1 * mY.row(t);
      
    } else {
      
      mL.fill(arma::fill::eye);
      
    }
    
    // Initialization for filtering:
    aA_Filt.subcube(arma::span::all, arma::span(0), arma::span(t)) = aA_Pred.subcube(arma::span::all, arma::span(0), arma::span(t));
    aP_Filt.slice(t) = aP_Pred.slice(t);
    
    // Sub-loop //
    for (int i = 0; i < iP; i++) {
      
      if (mY(arma::span(t), arma::span(i)).has_nan()) {
        
        
        mY(arma::span(t), arma::span(i)).fill(0);
        
        // Prediction error 
        mV_Filt(arma::span(t), arma::span(i)) = mY(arma::span(t), arma::span(i));
        
        // Prediction error variance:
        mF_Filt(arma::span(t), arma::span(i)) = arma::as_scalar(aH(arma::span(i), arma::span(i), arma::span(t)));

        // Kalman gain:
        aK_Filt(arma::span::all, arma::span(i), arma::span(t)).fill(0);
        
        // Filtered state mean:
        aA_Filt(arma::span::all, arma::span(i + 1), arma::span(t)) = aA_Filt(arma::span::all, arma::span(i), arma::span(t));
        
        // Filtered state variance:
        aP_Filt.slice(t) = aP_Filt.slice(t);
        
      }
      
      // Prediction error:
      mV_Filt(arma::span(t), arma::span(i)) = mY(arma::span(t), arma::span(i)) - aZ.slice(t).row(i) * aA_Filt.slice(t).col(i);
      
      // Prediction error variance:
      mF_Filt(arma::span(t), arma::span(i)) = aZ.slice(t).row(i) * aP_Filt.slice(t) * aZ.slice(t).row(i) +
        aH.slice(t).row(i).col(i);
      
      // Kalman gain:
      aK_Filt.slice(t).col(i) = aP_Filt.slice(t) * aZ.slice(t).row(i) * mF_Filt.col(i).row(t).i();
      
      
      if (sum(mF_Filt.col(i).row(t)) <= 0) {
        
        aA_Filt.slice(t).col(i+1) = aA_Filt.slice(t).col(i);
        
      }
      
      aA_Filt.slice(t).col(i+1) = aA_Filt.slice(t).col(i) + aK_Filt.slice(t).col(i) * mV_Filt.col(i).row(t);
      
      // Filtered state variance:
      
      if (sum(mF_Filt.col(i).row(t)) <= 0) {
        
        aP_Filt.slice(t) = aP_Filt.slice(t);
        
      }
      
      aP_Filt.slice(t) = aP_Filt.slice(t) - aK_Filt.slice(t).col(i) * mF_Filt.col(i).row(t) *
        aK_Filt.slice(t).col(i);
      
    } // End of sub-loop //
    
      
    // Likelihood contribution:
    arma::vec mF_Filt_row = mF_Filt.row(t);
    arma::vec mV_Filt_row = mV_Filt.row(t);
    
    vLL(t) = 0;
    
    if (find(mF_Filt_row > 0).is_empty() == false) {
      vLL(t) = vLL(t) + sum(log(mF_Filt_row.elem(find(mF_Filt_row > 0))) +
        pow(mV_Filt_row.elem(find(mF_Filt_row > 0)), 2) *
          pow(mF_Filt_row.elem(find(mF_Filt_row > 0)), -1));
    }
    
    
    if (t < (iN-1)) {
        
      // Predicted state mean:
      aA_Pred.slice(t+1).col(0) = aT.slice(t) * aA_Filt.slice(t).col(iP);
        
      // Predicted state variance:
      aP_Pred.slice(t+1) = aT.slice(t) * aP_Filt.slice(t) * aT.slice(t).t() + 
        aR.slice(t) * aQ.slice(t) * aR.slice(t).t();
            
    }
      
    // Forecasting:
    
    if (t == (iN-1) & iJ != 0) {
        
      // Forecasted state mean (iJ = 1):
      mA_For.col(0) = aT.slice(t) * aA_Filt.slice(t).col(iP);
        
      // Forecasted state variance (iJ = 1):
      aP_For.slice(0) = aT.slice(t) * aP_Filt.slice(t) * aT.slice(t).t() +
        aR.slice(t) * aQ.slice(t) * aR.slice(t).t();
        
      // Conditional mean forecast (iJ = 1):
      mY_For.col(0) = aZ.slice(t) * mA_For.col(0);
        
      mY_For.col(0) = mL * mY_For.col(0);
        
      // Conditional mean forecast square error (iJ = 1):
      aF_For.slice(0) = aZ.slice(t) * aP_For.slice(0) * aZ.slice(t).t() + aH.slice(t);
        
      aF_For.slice(0) = mL * aF_For.slice(0) * mL.t();
        
      if (iJ > 1) {
          
        for (int j = 1; j < iJ; j++) {
            
          // Forecasted state mean:
          mA_For.col(j) = aT.slice(t) * mA_For.col(j-1);
            
          // Forecasted state variance:
          aP_For.slice(j) = aT.slice(t) * aP_For.slice(j-1) * aT.slice(t).t() +
            aR.slice(t) * aQ.slice(t) * aR.slice(t).t();
            
          // Conditional mean forecast:
          mY_For.col(j) = aZ.slice(t) * mA_For.col(j);
            
          mY_For.col(j) = mL * mY_For.col(j);
            
          // Conditional mean forecast square error:
          aF_For.slice(j) = aZ.slice(t) * aP_For.slice(j) * aZ.slice(t).t() + aH.slice(t);
            
          aF_For.slice(j) = mL * aF_For.slice(j) * mL.t();
            
            
        }
          
      }
        
    }
    
  } // End filtering loop 
  
  
  // Smoothing:
  for (int t = (iN - 1); t >= 0; t--) {
    
    for (int i = (iP - 1); i >= 0; i--) {
      
      arma::mat iM_diag(iM, iM, arma::fill::eye);
      
      aL_Smoo.slice(t) = iM_diag - aK_Filt.slice(t).col(i) * aZ.slice(t).row(i);
      
      // Smmothed cumulant:
      aR_Smoo.slice(t+1).col(i) = aZ.slice(t).row(i) * pow(mF_Filt.col(i).row(t), -1) *
        mV_Filt.col(i).row(t) + aL_Smoo.slice(t).t() * aR_Smoo.slice(t+1).col(i+1);
      
      // Smmothed cumulant variance:
      aN_Smoo.slice(t+1) = aZ.slice(t).row(i) * pow(mF_Filt.col(i).row(t), -1) * aZ.slice(t).row(i) +
        aL_Smoo.slice(t) * aN_Smoo.slice(t+1) * aL_Smoo.slice(t);
      
    }
    
    if (t > 0) {
      
      // Smmothed cumulant:
      aR_Smoo.slice(t).col(iP) = aT.slice(t-1) * aR_Smoo.slice(t+1).col(0);
      
      // Smoothed cumulant variance:
      aN_Smoo.slice(t) = aT.slice(t-1) * aN_Smoo.slice(t+1) * aT.slice(t-1);
      
      // Smoothed state mean:
      aA_Smoo.slice(t).col(0) = aA_Pred.slice(t).col(0) + aP_Pred.slice(t) * aR_Smoo.slice(t+1).col(0);
        
      // Smoothed state variance:
      aV_Smoo.slice(t) = aP_Pred.slice(t) - aP_Pred.slice(t) * aN_Smoo.slice(t+1) * aP_Pred.slice(t);
      
      
    }
    
    if (t == 0) {
      
      // Smmothed cumulant:
      aR_Smoo.slice(t).col(iP) = aT.slice(t) * aR_Smoo.slice(t+1).col(0);
      
      // Smmothed cumulant variance:
      aN_Smoo.slice(t) = aT.slice(t).t() * aN_Smoo.slice(t+1) * aT.slice(t);
      
      // Smoothed state mean:
      aA_Smoo.slice(t).col(0) = aA_Pred.slice(t).col(0) + aP_Pred.slice(t) * aR_Smoo.slice(t+1).col(0);
        
      // Smoothed state variance:
      aV_Smoo.slice(t) = aP_Pred.slice(t) - aP_Pred.slice(t) * aN_Smoo.slice(t+1) * aP_Pred.slice(t);
          
    }
    
  }
  
  // Output:
  Rcpp::List lKFS =  Rcpp::List::create(Rcpp::Named("mV_Filt") = mV_Filt,
                                        Rcpp::Named("mF_Filt") = mF_Filt,
                                        Rcpp::Named("aK_Filt") = aK_Filt,
                                        Rcpp::Named("aA_Filt") = aA_Filt,
                                        Rcpp::Named("aA_Pred") = aA_Pred,
                                        Rcpp::Named("aP_Filt") = aP_Filt,
                                        Rcpp::Named("aP_Pred") = aP_Pred,
                                        Rcpp::Named("mA_For") = mA_For,
                                        Rcpp::Named("aP_For") = aP_For,
                                        Rcpp::Named("mY_For") = mY_For,
                                        Rcpp::Named("aF_For") = aF_For,
                                        Rcpp::Named("aL_Smoo") = aL_Smoo,
                                        Rcpp::Named("aR_Smoo") = aR_Smoo,
                                        Rcpp::Named("aN_Smoo") = aN_Smoo,
                                        Rcpp::Named("aA_Smoo") = aA_Smoo,
                                        Rcpp::Named("aV_Smoo") = aV_Smoo,
                                        Rcpp::Named("vLL") = vLL,
                                        Rcpp::Named("dLL") = dC - 0.5 * sum(vLL));
  
  return(lKFS);
  
}

// [[Rcpp::export]]
Rcpp::List chol_comp(arma::mat mX) {
  
  arma::mat mX_chol = chol(mX);
  arma::mat mX_chol_diag = arma::diagmat(mX_chol);
  
  Rcpp::List lOut =  Rcpp::List::create(Rcpp::Named("mX_chol") = mX_chol,
                                        Rcpp::Named("mX_chol_diag") = mX_chol_diag);
  
  return(lOut);
  
  
}
