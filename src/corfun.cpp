#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
SEXP corfun(SEXP x, SEXP y) {

  // reformat SEXP x into arma:cube
  NumericVector vecArray(x);
  IntegerVector arrayDims = vecArray.attr("dim");
  arma::cube xcube(vecArray.begin(), arrayDims[0],
                   arrayDims[1], arrayDims[2], false);
                   
  // make arma::mat out of SEXP y
  arma::mat ymat = Rcpp::as<arma::mat>(y);

  // get some dimension information; ncol = number of parameters
  int nrow = arrayDims[0], ncol = arrayDims[1];

  // initialize output matrix
  mat cor_coef(ncol, 1000);

  // there are 1000 bootstrap samples
  for(int q = 0; q < 1000; ++q) {
    
    // select subviews
    mat thisx = xcube.slice(q);
    colvec thisy = ymat.col(q);

    // scale variables: thisx
    mat xscale(nrow, ncol);
    rowvec xmean = mean(thisx, 0);
    rowvec xstd = stddev(thisx, 0, 0);
    for(int i = 0; i < ncol; ++i) {
      for (int j = 0; j < nrow; ++j) {
        xscale(j, i) = (thisx(j, i) - xmean[i]) / xstd[i];
      }
    }
    // scale thisy
    int o = nrow;
    double ymean = mean(thisy);
    double ystd = stddev(thisy);
    colvec yscale(o);
    for(int i = 0; i < o; ++i) {
      yscale[i] = (thisy[i] - ymean) / ystd;
    }

//     calculate solution for linear system for each parameter
//     separately
    for(int pt = 0; pt < ncol; ++pt) {
      mat solu = solve(xscale.col(pt), yscale);
      cor_coef(pt, q) = solu(0, 0);
    }
  }
  List res;
  res["coef"] = cor_coef;
  return res;
}
