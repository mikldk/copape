// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <vector>
#include <unordered_map>

#include "util.h"

//' Merge single random
//' 
// [[Rcpp::export]]
Rcpp::DataFrame merge_single_random(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids,
    const int pid_to_merge, 
    const int max_meioses,
    int surr_pid_start = 50000000) {
  
  int n = pids.length();
  
  ////////////////////////////////////////////////////////////////
  // Validate input
  validate_merge_input(pids, pids_dad, birthyears, paternalped_ids);
  
  for (int pid : pids) {
    if (pid >= surr_pid_start) {
      Rcpp::Rcout << "Found individual with pid = " << pid << std::endl;
      Rcpp::stop("surr_pid_start too small");
    }
  }
  ////////////////////////////////////////////////////////////////
  
  // max meioses: continue until done...
  // if less than distance to founder, then don't do anything
  
  Rcpp::DataFrame res;
  
  return res;
}
