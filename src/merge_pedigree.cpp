// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <cmath>
#include <vector>
#include <unordered_map>
#include <queue>

#include "util.h"
#include "util-validate.h"

//' Merge a pedigree randomly
//' 
//' Note that children will be added according to `sons_configs`.
//' 
//' @inheritParams validate_merge_input
//' @inheritParams validate_surr_pid_start
//' @inheritParams validate_sons_configs
//' @param pedid_to_merge id of pedigree that will be merged with other pedigrees
//' @param no_surrogate_ancestors number of surrogate ancestors to add
//' @param verbose verbose output
//' 
//' @return Only individuals in new pedigree.
//' 
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame merge_pedigree(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids,
    const int pedid_to_merge,
    const Rcpp::ListOf<Rcpp::IntegerVector>& sons_configs,
    const int no_surrogate_ancestors = 1,
    const int surr_pid_start = 50000000,
    const bool verbose = false) {
  
  
  
  ////////////////////////////////////////////////////////////////
  // Validate input
  //--------------------------------------------------------------
  validate_merge_input(pids, pids_dad, birthyears, paternalped_ids);
  validate_surr_pid_start(pids, surr_pid_start);
  
  if (pedid_to_merge <= 0) {
    Rcpp::stop("pedid_to_merge must be >= 1");
  }
  
  if (no_surrogate_ancestors <= 0) {
    Rcpp::stop("no_surrogate_ancestors must be >= 1");
  }
  
  validate_sons_configs(sons_configs);
  ////////////////////////////////////////////////////////////////
  
  
  
  
  ////////////////////////////////////////////////////////////////
  // Make map from ped to indices for faster look-up  
  //--------------------------------------------------------------
  std::unordered_map<int, std::vector<int>> pedid_to_indices = 
    vector_to_hash(paternalped_ids);
  //print_map_int_vecint(pedid_to_indices);
  
  // Check that pedid_to_merge exists
  validate_key_exists(pedid_to_indices, pedid_to_merge);
  ////////////////////////////////////////////////////////////////
  
  

  ////////////////////////////////////////////////////////////////
  // Make map from ped to indices for faster look-up  
  //--------------------------------------------------------------
  std::unordered_map<int, int> pedid_founder_indices = find_founder_indices(
    pedid_to_indices, pids_dad);
  Rcpp::print(Rcpp::wrap(pedid_founder_indices));
  Rcpp::stop("to here...");
  ////////////////////////////////////////////////////////////////
  
  
  ////////////////////////////////////////////////////////////////
  // Book-keeping of an index' children (not pid, but index)
  // Used for surrogate fathers.
  // i.e. 
  Rcpp:.stop("Class for surrogate founders? And 'children' - real individuals?");
  //--------------------------------------------------------------
  std::unordered_map<int, std::vector<int>> pedid_founder_indices = 
    find_founder_indices(pedid_to_indices, pids_dad);
  ////////////////////////////////////////////////////////////////
    
    
    
  
  ////////////////////////////////////////////////////////////////
  
  Rcpp::DataFrame res;
  
  return res;
}
