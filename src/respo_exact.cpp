#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
SEXP respoexact(SEXP x, SEXP y, SEXP y0) {

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
  mat resp_coef(ncol, 1001);

  // scale variable: xmat
  mat xscale(nrow, ncol);
  rowvec xmean = mean(xmat, 0);
  rowvec xstd = stddev(xmat, 0, 0);
  for(int i = 0; i < ncol; ++i) {
    for (int j = 0; j < nrow; ++j) {
      xscale(j, i) = (xmat(j, i) - xmean[i]) / xstd[i];
    }
  }
  
  // there are 1000 bootstrap samples + 1 original sample
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
    
    // correlation matrix
    mat cormat = cor(xscale);
    
    // eigenvector decomp
    vec eigval;
    mat eigvec;
    eig_sym(eigval, eigvec, cormat);
    
    // reverse order of eigenvalues and eigenvectors to be compliant
    // with R code
    vec eigvalr = flipud(eigval);
    mat eigvecr = fliplr(eigvec);
    
    // calculate cumulative product of eigenvalues
    vec cumprod(ncol);
    cumprod[0] = eigvalr[0];
    for(int i = 1; i < ncol; ++i) {
      cumprod[i] = cumprod[i - 1] * eigvalr[i];
    }
    
    // select only eigenvectors adhering to PVP criterion
    mat eigvecpvp = eigvecr.cols(find(cumprod > 1));
    
    // calculate pc scores for climate data
    mat pcscores = xscale * eigvecpvp;
    
    // calculate solution for linear system
    vec beta = solve(pcscores, yscale);
    
    // pad coefficients with 0 to obtain original dimensionality
    vec betapad = zeros<vec>(ncol);
    betapad.rows(0, beta.n_rows - 1) = beta;
    
    // calculate response coefficients
    vec this_co = eigvecr * betapad;

    // write into output matrix
    resp_coef.col(q) = this_co;
  }
  
  List res;
  res["coef"] = resp_coef;
  return res;
}
