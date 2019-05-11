// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <vector>
#include <unordered_map>

#include "util.h"


// TODO: 
// stop when a certain pid is merged to min_meioses:
// when there are min_meioses between pid and the pedigree's founder, the remaining
// merges will only result in a meiotic distance > min_meioses, i.e. maybe of no interest:

// Anything to speed-up:
// Always merge pid's pedigree, ped:
// First time: merge ped to another one, and take two others and merge. Now these two are merged.
// Second time: take two pedigrees and merge, and two more and merge. And merge these. Now that is merged to the other with ped.
/*
merge pid extra_meioses times (e.g. extra_meioses = min_meioses - current_ped_depth)?

x: surrogate individuals
o: existing founders
P: ped with PoI

------------------------------------------------------------
min_meioses = 1: 
2^1 - 1 = 2 - 1 = 1 other ped (number of "o"'s)
  x
 / \
P   o
randomly pick 1 other ped, discard the rest?
------------------------------------------------------------


------------------------------------------------------------
min_meioses = 2: 3 others
2^2 - 1 = 4 - 1 = 3 other peds (number of "o"'s)
     x
    / \
  x     x
 / \   / \
P   o o   o
randomly pick 3 other peds, discard the rest?
------------------------------------------------------------


------------------------------------------------------------
min_meioses = 3: 7 others
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
min_meioses = 4: 15 others
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

In general for extra_meioses for a certain pid:
pick 2^extra_meioses - 1 peds, and use those.
*/

//' Merge all random
//' 
//' @inheritParams validate_merge_input
//' @param max_it maximum number of iterations (merge events), `-1` for disable
//' @param surr_pid_start start person id for new surrogate individuals
//' @param verbose verbose output
//' 
// [[Rcpp::export]]
Rcpp::DataFrame merge_all_random(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids,
    int max_it = -1,
    int surr_pid_start = 50000000,
    bool verbose = false) {
  
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
  
  ////////////////////////////////////////////////////////////////
  // Allocate result
  std::vector<int> res_pids = Rcpp::as< std::vector<int> >(pids);
  std::vector<int> res_pids_dads = Rcpp::as< std::vector<int> >(pids_dad);
  std::vector<int> res_birthyears = Rcpp::as< std::vector<int> >(birthyears);
  std::vector<int> res_paternalped_ids = Rcpp::as< std::vector<int> >(paternalped_ids);
  
  // Reserve memory for added founders
  int no_new_surr_fathers = required_surrogate_fathers(res_paternalped_ids);
  
  if (verbose) {
    Rcpp::Rcout << "Will introduce " << no_new_surr_fathers << " new surrogate fathers (but only if max_it allows)" << std::endl;
  }
  
  int total_size = n + no_new_surr_fathers;
  
  res_pids.reserve(total_size);
  res_pids_dads.reserve(total_size);
  res_birthyears.reserve(total_size);
  res_paternalped_ids.reserve(total_size);
    
  ////////////////////////////////////////////////////////////////
  // Find candidates, i.e. those currently without founder
  std::vector<int> indices_candidates;
  
  for (int i = 0; i < n; ++i) {
    if (Rcpp::IntegerVector::is_na(pids_dad(i))) {
      res_pids_dads[i] = 0; // explicitely convert to 0 for later use
      indices_candidates.push_back(i);
    } else if (pids_dad(i) <= 0) {
      Rcpp::Rcout << "Found individual with pid_dad = " << pids_dad(i) << std::endl;
      Rcpp::stop("pid_dad must all be strictly positive");
    }
  }
  
  //Rcpp::print(Rcpp::wrap(indices_candidates));

  ////////////////////////////////////////////////////////////////
  // Maps for look-up
  // TODO
  //std::unordered_map<int, int> pedid_to_index;
  
  ////////////////////////////////////////////////////////////////
  // Main loop
  
  int i = 0;
  do {
    int n_candidates = indices_candidates.size();
    if (n_candidates <= 1) {
      break;
    }
    
    if (i % 1000 == 999) { // instead of 0 to avoid status for small problems
      if (verbose) {
        Rcpp::Rcout << "Candidates left: " << n_candidates << std::endl;
      }
      Rcpp::checkUserInterrupt();
    }
    
    int pos1 = random_index(n_candidates);
    int pos2 = random_index(n_candidates);
    while (pos2 == pos1) {
      pos2 = random_index(n_candidates);
    }
    //Rcpp::Rcout << "pos1 = " << pos1 << ", pos2 = " << pos2 << std::endl;
    
    int idx1 = indices_candidates[pos1];
    int idx2 = indices_candidates[pos2];
    
    /*
    if (verbose) {
      Rcpp::Rcout << "row1 = " << (idx1+1) << ", row2 = " << (idx2+1) << std::endl;
    }
    */
    //Rcpp::Rcout << "idx1 = " << idx1 << ", idx2 = " << idx2 << std::endl;
    
    
    /////////////////////////////////////////////////////////////
    
    int surr_founder_pid = surr_pid_start;
    res_pids.push_back(surr_founder_pid);
    
    if (res_pids_dads[idx1] != 0 || res_pids_dads[idx2] != 0) {
      Rcpp::stop("Candidates were not candidates anyway!");
    }
    res_pids_dads[idx1] = surr_founder_pid;
    res_pids_dads[idx2] = surr_founder_pid;
    res_pids_dads.push_back(0); // for the new surrogate founder
    
    res_birthyears.push_back(0);
    
    // Use i1's pedid:
    int pedid = res_paternalped_ids[idx1];
    // Update all with that pedigree id, one of which is when i = idx2, such that
    // res_paternalped_ids[idx2] = pedid;
    int pedid_idx2 = res_paternalped_ids[idx2];
    // TODO: Use pedid_to_index hash
    for (int j = 0; j < res_paternalped_ids.size(); ++j) {
      if (res_paternalped_ids[j] == pedid_idx2) {
        res_paternalped_ids[j] = pedid;
      }
    }
    res_paternalped_ids.push_back(pedid);  // for the new surrogate founder
    
    
    // include new surrogate as candidate next:
    indices_candidates.push_back(res_pids.size() - 1);
    
    // Be sure to delete highest index first to keep ordering!
    if (pos2 > pos1) {
      indices_candidates.erase(indices_candidates.begin() + pos2);
      indices_candidates.erase(indices_candidates.begin() + pos1);
    } else {
      indices_candidates.erase(indices_candidates.begin() + pos1);
      indices_candidates.erase(indices_candidates.begin() + pos2);
    }
    
    
    ////////////////////////////////////////////////////////////////
    /*
    Rcpp::Rcout << ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" << std::endl;
    
    Rcpp::Rcout << "res_pids:" << std::endl;
    Rcpp::print(Rcpp::wrap(res_pids));
    
    Rcpp::Rcout << "res_pids_dads:" << std::endl;
    Rcpp::print(Rcpp::wrap(res_pids_dads));
    
    Rcpp::Rcout << "res_birthyears:" << std::endl;
    Rcpp::print(Rcpp::wrap(res_birthyears));
    
    Rcpp::Rcout << "res_paternalped_ids:" << std::endl;
    Rcpp::print(Rcpp::wrap(res_paternalped_ids));
    
    Rcpp::Rcout << "indices_candidates:" << std::endl;
    Rcpp::print(Rcpp::wrap(indices_candidates));
    Rcpp::Rcout << "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<" << std::endl;
    */
    ////////////////////////////////////////////////////////////////
    
    // Get ready for next  
    surr_pid_start++;
    if (surr_pid_start < 0) {
      Rcpp::stop("Overflow in surr_pid_start");
    }
    
    ++i;
  //} while (indices_candidates.size() >= 2 && (max_it == -1 || (max_it > 0 && i < max_it)));
  } while (max_it == -1 || (max_it > 0 && i < max_it));
  
  if (verbose) {
    Rcpp::Rcout << "Candidates left: " << indices_candidates.size() << std::endl;
  }
    
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
  
  Rcpp::DataFrame res = Rcpp::DataFrame::create(
    Rcpp::Named("pid") = out_pids,
    Rcpp::Named("pid_dad") = out_pids_dads,
    Rcpp::Named("birthyears") = out_birthyears,
    Rcpp::Named("paternalped_id") = out_paternalped_ids
  );
  
  return res;
}
