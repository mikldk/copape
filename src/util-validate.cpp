// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <cmath>

#include "util-validate.h"

//' Validate merge input
//' 
//' @param pids vector of person ids: >= 1 (`NA` not allowed)
//' @param pids_dad vector of person ids for dad: >= 1 (`NA` for founders)
//' @param birthyears vector of birth years: free
//' @param paternalped_ids vector of pedigree ids: >= 1 (`NA` not allowed)
//' 
// [[Rcpp::export]]
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
      Rcpp::stop("pid cannot be NA");
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
  
  for (int ped_id : paternalped_ids) {
    if (Rcpp::IntegerVector::is_na(ped_id)) {
      Rcpp::stop("ped_id cannot be NA");
    }

    if (ped_id <= 0) {
      Rcpp::Rcout << "Found individual with ped_id = " << ped_id << std::endl;
      Rcpp::stop("ped_id must be >= 1");
    }
  }
}


//' Validate surrogate father id input
//' 
//' @param pids person id: >= 1 (`NA` not allowed)
//' @param surr_pid_start start person id for new surrogate individuals
//' 
// [[Rcpp::export]]
void validate_surr_pid_start(
    const Rcpp::IntegerVector& pids,
    const int surr_pid_start) {
  
  for (int pid : pids) {
    if (pid >= surr_pid_start) {
      Rcpp::Rcout << "Found individual with pid = " << pid << std::endl;
      Rcpp::stop("surr_pid_start too small");
    }
  }
}


void validate_key_exists(
    const std::unordered_map<int, std::vector<int>>& map,
    const int key) {
  
  std::unordered_map<int, std::vector<int>>::const_iterator find;
  
  find = map.find(key);
  
  if (find == map.end()) {
    Rcpp::stop("key not found");
  }
}
