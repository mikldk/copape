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


int required_surrogate_fathers(const std::vector<int>& paternalped_ids) {
  std::unordered_map<int, int> pedids_counts;
  std::unordered_map<int, int>::const_iterator got;
  
  for (int pedid : paternalped_ids) {
    got = pedids_counts.find(pedid);
    
    if (got == pedids_counts.end()) {
      pedids_counts[pedid] = 1;
    }
  }
  int M = pedids_counts.size();

  /* TODO: Check up:
   * When there are 2 unique pedigrees, we must introduce 1 new surrogate father.
   * When there are 3 unique pedigrees, we must introduce 2 new surrogate fathers.
   * When there are 4 unique pedigrees, we must introduce 1+1+1=3 new surrogate fathers.
   * When there are M unique pedigrees, we must introduce M-1 new surrogate fathers.
   * Proof by induction:
   * Assume that there are M-1 unique pedigrees and by induction that 
   * it requires M-2 new surrogate fathers to get to having one common founder.
   * Now add a new pedigree. To get a new common founder, add a new father,
   * thus obtaining a total of M-1 introduced surrogate fathers.
   */
  return M-1;
}


