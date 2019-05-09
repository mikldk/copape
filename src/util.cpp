// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <cmath>

#include "util.h"

//' Random index between 0 and n-1 (both included)
//' 
//' @param n upper limit (not included)
// [[Rcpp::export]]
int random_index(int n) {
  if (n < 0) {
    Rcpp::stop("n must be >= 0");  
  }
  
  if (n == 0) {
    return 0;
  }
  
  double u = R::runif(0, 1);
  double n_dbl = static_cast<double>(n);
  double nu = std::floor(n_dbl * u);
  int idx = (int)nu;
  
  return idx;
}

