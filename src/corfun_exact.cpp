#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
SEXP corfunexact(SEXP x, SEXP y, SEXP y0) {

  // make arma::mat out of both SEXP
  arma::mat ymat = Rcpp::as<arma::mat>(y);
  arma::mat xmat = Rcpp::as<arma::mat>(x);
  
  // make arma::vec out of SEXP y0, which is the original tree-ring data
  arma::vec y0vec = Rcpp::as<arma::vec>(y0);

  // get some dimension information
  int nrow = xmat.n_rows, ncol = xmat.n_cols;
  
  // combine y and y0 to one matrix
  arma::mat y0mat(nrow, 1001);
  y0mat.col(0) = y0vec;
  y0mat.cols(1, 1000) = ymat;

  // initialize output matrix
  mat cor_coef(ncol, 1001);
  
  // scale variable: xmat
  mat xscale(nrow, ncol);
  rowvec xmean = mean(xmat, 0);
  rowvec xstd = stddev(xmat, 0, 0);
  for(int i = 0; i < ncol; ++i) {
    for (int j = 0; j < nrow; ++j) {
      xscale(j, i) = (xmat(j, i) - xmean[i]) / xstd[i];
    }
  }

  // there are 1000 bootstrap samples + 1 set of coefs for real data
  for(int q = 0; q < 1001; ++q) {
    
    // select subview (only for y)
    colvec thisy = y0mat.col(q);

    // scale thisy
    int o = nrow;
    double ymean = mean(thisy);
    double ystd = stddev(thisy);
    colvec yscale(o);
    for(int i = 0; i < o; ++i) {
      yscale[i] = (thisy[i] - ymean) / ystd;
    }

    // calculate solution for linear system for each parameter
    // separately
    for(int pt = 0; pt < ncol; ++pt) {
      mat solu = solve(xscale.col(pt), yscale);
      cor_coef(pt, q) = solu(0, 0);
    }
  }
  List res;
  res["coef"] = cor_coef;
  return res;
}
