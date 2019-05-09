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


// Validate input
void validate_merge_input(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids) {

  int n = pids.length();

  // Validate input
  if (pids_dad.length() != n || 
      birthyears.length() != n || 
      paternalped_ids.length() != n) {
    
    Rcpp::stop("Need same lengths of vectors");
  }
  
  for (int pid : pids) {
    if (Rcpp::IntegerVector::is_na(pid)) {
      Rcpp::Rcout << "Found individual with pid = " << pid << std::endl;
      Rcpp::stop("pids must cannot be NA");
    } 
    
    if (pid <= 0) {
      Rcpp::Rcout << "Found individual with pid = " << pid << std::endl;
      Rcpp::stop("pids must be >= 1");
    }
  }
  
  for (int pid_dad : pids_dad) {
    if (Rcpp::IntegerVector::is_na(pid_dad)) {
      continue;
    }
    
    if (pid_dad <= 0) {
      Rcpp::Rcout << "Found individual with pid_dad = " << pid_dad << std::endl;
      Rcpp::stop("pid_dad must be NA or >= 1");
    }
  }
}

