/**
 util-validate.h
 Purpose: Header for C++ helper functions
 Details: C++ header.
  
 @author Mikkel Meyer Andersen
 */
 
#ifndef UTIL_VALIDATE_H
#define UTIL_VALIDATE_H

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

void validate_merge_input(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids);

void validate_surr_pid_start(
    const Rcpp::IntegerVector& pids,
    const int surr_pid_start);


void validate_key_exists(
    const std::unordered_map<int, std::vector<int>>& map,
    const int key);

#endif
