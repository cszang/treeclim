#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
SEXP pcor(SEXP x1, SEXP x2, SEXP y, SEXP y0) {
  // x1 are matrices with with primary and secondary climate variables
  // in seasons, y is a matrix with simulated tree-ring data; y0 is
  // the original tree-ring data

  // make arma::mat out of both matrix SEXP
  arma::mat ymat = Rcpp::as<arma::mat>(y);
  arma::mat xmat1 = Rcpp::as<arma::mat>(x1);
  arma::mat xmat2 = Rcpp::as<arma::mat>(x2);
  
  // make arma::vec out of SEXP y0, which is the original tree-ring
  // data
  arma::vec y0vec = Rcpp::as<arma::vec>(y0);

  // get some dimension information
  int nrow = xmat1.n_rows, ncol = xmat1.n_cols;
  
  // combine y and y0 to one matrix
  arma::mat y0mat(nrow, 1001);
  y0mat.col(0) = y0vec;
  y0mat.cols(1, 1000) = ymat;

  // initialize output matrices for primary and secondary variables
  mat cor_coef1(ncol, 1001);
  mat cor_coef2(ncol, 1001);
  mat cor_coef2e(ncol, 1001);

  // there are 1000 bootstrap samples + 1 set of coefs for real data
  for(int q = 0; q < 1001; ++q) {

    // select subview (only for y)
    vec thisy = y0mat.col(q);

    // loop through seaons
    for(int s = 0; s < ncol; ++s) {

      // select x1 and x2
      vec thisx1 = xmat1.col(s);
      vec thisx2 = xmat2.col(s);

      // compute correlation between tree-ring data and primary
      // variable and store in results
      arma::Mat<double> cor_c1 = cor(thisy, thisx1);

      // compute intermediates
      arma::Mat<double> cor_c2 = cor(thisy, thisx2);
      arma::Mat<double> cor_12 = cor(thisx1, thisx2);

      // compute partial correlation for x2
      arma::Mat<double> cor_c2_1 = (cor_c2 - cor_12 * cor_c1)/
        (sqrt(1 - cor_12 * cor_12) * sqrt(1 - cor_c1 * cor_c1));

      // simulated partial correlations for x2 are computed
      // differently in original seascorr:
      // we want an intercept!
      arma::Mat<double> X1 = ones(nrow, 2);
      for (int w = 0; w < nrow; w++) {
        X1(w, 1) = thisx1(w);
      }
      arma::Mat<double> coef1 = arma::solve(X1, thisy);
      arma::Mat<double> resid1 = thisy - X1 * coef1;
      arma::Mat<double> coef2 = arma::solve(X1, thisx2);
      arma::Mat<double> resid2 = thisx2 - X1 * coef2;
      arma::Mat<double> cor_e2 = cor(resid1, resid2);
          
      // write in output matrices
      cor_coef1(s, q) = cor_c1(0, 0);
      cor_coef2(s, q) = cor_c2_1(0, 0);
      cor_coef2e(s, q) = cor_e2(0, 0);
    }
  }
  List res;
  res["primary"] = cor_coef1;
  res["secondary"] = cor_coef2;
  res["exact"] = cor_coef2e;
  return res;
}
