// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <cmath>
#include <vector>
#include <unordered_map>
#include <queue>

#include "util.h"
#include "util-validate.h"


// Assumes that input has been validated
Rcpp::DataFrame build_merge_result(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids,
    const std::unordered_map<int, std::vector<int>>& pedid_to_indices,
    const std::vector<int>& selected_pedids) {

  ////////////////////////////////////////////////////////////////
  // Find founder index in each pedigree
  std::unordered_map<int, int> pedid_founder_indices;
  for (auto v : pedid_to_indices) {
    for (auto i : v.second) {
      bool found = false;
      
      if (Rcpp::IntegerVector::is_na(pids_dad[i])) {
        if (found) {
          Rcpp::stop("Pedigree have more than one founder. Unexpected.");
        }
        
        pedid_founder_indices[v.first] = i;
        found = true;
        // TODO: break instead?
      }
    }
  }
  Rcpp::print(Rcpp::wrap(pedid_founder_indices));
  Rcpp::stop("to here...");
  
  ////////////////////////////////////////////////////////////////
  // Perform merge!
  ////////////////////////////////////////////////////////////////
  // Allocate result
  std::vector<int> res_pids = Rcpp::as< std::vector<int> >(pids);
  std::vector<int> res_pids_dads = Rcpp::as< std::vector<int> >(pids_dad);
  std::vector<int> res_birthyears = Rcpp::as< std::vector<int> >(birthyears);
  std::vector<int> res_paternalped_ids = Rcpp::as< std::vector<int> >(paternalped_ids);
  
  // Reserve memory for added founders
  // We need as many surrogate fathers as the 
  // number of pedigrees_needed:
  // TODO: Check:
  int n = pids.size();
  int pedigrees_needed = selected_pedids.size();
  int total_size = n + pedigrees_needed;
  
  res_pids.reserve(total_size);
  res_pids_dads.reserve(total_size);
  res_birthyears.reserve(total_size);
  res_paternalped_ids.reserve(total_size);
  

  
  ////////////////////////////////////////////////////////////////
  // Structure: FIFO queue, when size = 1, done:
  std::queue<int> pedids;
  
  
  /*
  for (int pedid : selected_pedids) {
    pedids.push(pedid);
  }
  
  std::qu vector<int> pedids_left(selected_pedids);
  
  while (pedids_left.s)
  */
  
  Rcpp::warning("MERGE!");
  
  ////////////////////////////////////////////////////////////////
  // Return
  
  Rcpp::IntegerVector out_pids = Rcpp::wrap(res_pids);
  Rcpp::IntegerVector out_pids_dads = Rcpp::wrap(res_pids_dads);
  Rcpp::IntegerVector out_birthyears = Rcpp::wrap(res_birthyears);
  Rcpp::IntegerVector out_paternalped_ids = Rcpp::wrap(res_paternalped_ids);
  
  int n_out = out_pids.size();
  
  if (out_pids_dads.size() != n_out || 
      out_birthyears.size() != n_out || 
      out_paternalped_ids.size() != n_out) {
    
    Rcpp::stop("Need same lengths of output vectors");
  }  
  
  // Convert 0 to NA in:
  // * res_pids_dads
  // * res_birthyears
  
  for (int i = 0; i < n_out; ++i) {
    if (out_pids_dads(i) == 0) {
      out_pids_dads(i) = Rcpp::IntegerVector::get_na();
    }
    
    if (out_birthyears(i) == 0) {
      out_birthyears(i) = Rcpp::IntegerVector::get_na();
    }
  }
  
  // Surrogate has pid >= surr_pid_start  
  Rcpp::DataFrame res = Rcpp::DataFrame::create(
    Rcpp::Named("pid") = out_pids,
    Rcpp::Named("pid_dad") = out_pids_dads,
    Rcpp::Named("birthyears") = out_birthyears,
    Rcpp::Named("paternalped_id") = out_paternalped_ids
  );
  
  return res;
}



/*
  x: surrogate individuals
  o: existing founders
  P: ped with PoI
  
  ------------------------------------------------------------
  no_surrogate_ancestors = 1: 
  2^1 - 1 = 2 - 1 = 1 other ped (number of "o"'s)
    x
   / \
  P   o
  randomly pick 1 other ped, discard the rest?
  ------------------------------------------------------------
  
  
  ------------------------------------------------------------
  no_surrogate_ancestors = 2: 3 others
  2^2 - 1 = 4 - 1 = 3 other peds (number of "o"'s)
       x
      / \
    x     x
   / \   / \
  P   o o   o
  randomly pick 3 other peds, discard the rest?
  ------------------------------------------------------------
  
  
  ------------------------------------------------------------
  no_surrogate_ancestors = 3: 7 others
  2^3 - 1 = 8 - 1 = 7 other peds (number of "o"'s)
             x
        ____/ \____
       x           x
      / \         / \
    x     x     x     x
   / \   / \   / \   / \
  P   o o   o o   o o   o
  randomly pick 7 other peds, discard the rest?
  ------------------------------------------------------------
  
  ------------------------------------------------------------
  no_surrogate_ancestors = 4: 15 others
  2^4 - 1 = 16 - 1 = 15 other peds (number of "o"'s)
                         x
              __________/ \__________
             x                       x
        ____/ \____             ____/ \____
       x           x           x           x
      / \         / \         / \         / \
    x     x     x     x     x     x     x     x
   / \   / \   / \   / \   / \   / \   / \   / \
  P   o o   o o   o o   o o   o o   o o   o o   o
  randomly pick 15 other peds, discard the rest?
  ------------------------------------------------------------
  
  In general for no_surrogate_ancestors for a certain pid:
  pick 2^no_surrogate_ancestors - 1 peds, and use those.
*/

//' Merge certain pedigree randomly
//' 
//' 
//' 
//' @inheritParams validate_merge_input
//' @inheritParams validate_surr_pid_start
//' @param pedid_to_merge id of pedigree that will be merged with other pedigrees
//' @param no_surrogate_ancestors number of surrogate ancestors to add
//' @param verbose verbose output
//' 
//' @return Only individuals in new pedigree.
//' 
// [[Rcpp::export]]
Rcpp::DataFrame merge_certain_pedigree_randomly(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids,
    const int pedid_to_merge,
    int no_surrogate_ancestors = 1,
    int surr_pid_start = 50000000,
    bool verbose = false) {
  
  ////////////////////////////////////////////////////////////////
  // Validate input
  validate_merge_input(pids, pids_dad, birthyears, paternalped_ids);
  validate_surr_pid_start(pids, surr_pid_start);
  
  if (pedid_to_merge <= 0) {
    Rcpp::stop("pedid_to_merge must be >= 1");
  }
  
  if (no_surrogate_ancestors <= 0) {
    Rcpp::stop("no_surrogate_ancestors must be >= 1");
  }
  
  ////////////////////////////////////////////////////////////////
  // Make map from ped to indices for faster look-up  
  std::unordered_map<int, std::vector<int>> pedid_to_indices = 
    vector_to_hash(paternalped_ids);
  //print_map_int_vecint(pedid_to_indices);
  
  // Check that pedid_to_merge exists
  validate_key_exists(pedid_to_indices, pedid_to_merge);

  ////////////////////////////////////////////////////////////////
  // Select pedigrees to merge with:
  
  // Randomly select
  // 2^no_surrogate_ancestors - 1 peds
  // 2^x is 
  int pedigrees_needed = (1 << no_surrogate_ancestors) - 1;
  //Rcpp::Rcout << "2^no_surrogate_ancestors - 1 = " << 
  //"2^" << no_surrogate_ancestors << " - 1 = " << pedigrees_needed << std::endl;
  
  // This also checks that there are a sufficient number of pedigrees available
  std::vector<int> selected_pedids = 
    sample_pedids_to_merge(pedid_to_indices, pedigrees_needed, pedid_to_merge);
  //Rcpp::print(Rcpp::wrap(selected_pedids));
  // selected_pedids already shuffled/random, no need to do it again
  
  ////////////////////////////////////////////////////////////////
  Rcpp::DataFrame res = build_merge_result(
    pids,
    pids_dad, 
    birthyears, 
    paternalped_ids,
    pedid_to_indices,
    selected_pedids
  );
  
  return res;
}
