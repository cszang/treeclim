#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
SEXP corfunnoboot(SEXP x, SEXP y) {
  
  // make arma::mat out of both SEXP x
  arma::mat xmat = Rcpp::as<arma::mat>(x);
  
  // make arma::vec out of SEXP y
  arma::vec yvec = Rcpp::as<arma::vec>(y);

  // get some dimension information
  int nrow = xmat.n_rows, ncol = xmat.n_cols;

  // initialize output vector
  vec cor_coef(ncol);
  
  // scale variable: xmat
  mat xscale(nrow, ncol);
  rowvec xmean = mean(xmat, 0);
  rowvec xstd = stddev(xmat, 0, 0);
  for(int i = 0; i < ncol; ++i) {
    for (int j = 0; j < nrow; ++j) {
      xscale(j, i) = (xmat(j, i) - xmean[i]) / xstd[i];
    }
  }
  
  // scale variable: y
  int o = nrow;
  double ymean = mean(yvec);
  double ystd = stddev(yvec);
  colvec yscale(o);
  for(int i = 0; i < o; ++i) {
    yscale[i] = (yvec[i] - ymean) / ystd;
  }

  // calculate solution for linear system for each parameter
  // separately
  for(int pt = 0; pt < ncol; ++pt) {
    mat solu = solve(xscale.col(pt), yscale);
    cor_coef[pt] = solu(0, 0);
  }

  List res;
  res["coef"] = cor_coef;
  return res;
}
