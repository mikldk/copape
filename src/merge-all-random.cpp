// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <vector>
#include <unordered_map>

#include "util.h"

// [[Rcpp::export]]
Rcpp::DataFrame merge_all_random(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids,
    int surr_pid_start = 50000000, 
    int max_it = -1) {
  
  int n = pids.length();
  
  ////////////////////////////////////////////////////////////////
  // Validate input
  if (pids_dad.length() != n || 
      birthyears.length() != n || 
      paternalped_ids.length() != n) {
    
    Rcpp::stop("Need same lengths of vectors");
  }
  
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
  //std::unordered_map<int, int> pid_to_index;
  
  ////////////////////////////////////////////////////////////////
  // Main loop
  
  int i = 0;
  do {
    int n_candidates = indices_candidates.size();
    if (n_candidates <= 1) {
      break;
    }
    
    if (i % 1000 == 0) {
      Rcpp::Rcout << "Candidates left: " << n_candidates << std::endl;
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
    
    //Rcpp::Rcout << "idx1 = " << idx1 << ", idx2 = " << idx2 << std::endl;
    
    /////////////////////////////////////////////////////////////
    
    int surr_founder_pid = surr_pid_start;
    res_pids.push_back(surr_founder_pid);
    
    res_pids_dads[idx1] = surr_founder_pid;
    res_pids_dads[idx2] = surr_founder_pid;
    res_pids_dads.push_back(0); // for the new surrogate founder
    
    res_birthyears.push_back(0);
    
    // Use i1's pedid:
    int pedid = res_paternalped_ids[idx1];
    res_paternalped_ids[idx2] = pedid;
    res_paternalped_ids.push_back(pedid);  // for the new surrogate founder
    
    
    // include new surrogate as candidate next:
    indices_candidates.push_back(res_pids.size() - 1);
    
    indices_candidates.erase(indices_candidates.begin() + pos1);
    indices_candidates.erase(indices_candidates.begin() + pos2);
    
      
    // Get ready for next  
    surr_pid_start++;
    
    ++i;
  } while (indices_candidates.size() >= 2 && (max_it == -1 || (max_it > 0 && i < max_it)));
  
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
