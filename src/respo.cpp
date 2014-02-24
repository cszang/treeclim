#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
SEXP respo(SEXP x, SEXP y) {

  // reformat SEXP x into arma:cube
  NumericVector vecArray(x);
  IntegerVector arrayDims = vecArray.attr("dim");
  arma::cube xcube(vecArray.begin(), arrayDims[0],
                   arrayDims[1], arrayDims[2], false);
                   
  // make arma::mat out of SEXP y
  arma::mat ymat = Rcpp::as<arma::mat>(y);

  // get some dimension information
  int nrow = arrayDims[0], ncol = arrayDims[1];

  // initialize output matrix
  mat resp_coef(ncol, 1000);

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
    
    // correlation matrix
    mat cormat = cor(thisx);
    
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
