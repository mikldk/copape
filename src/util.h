/**
 util.h
 Purpose: Header for C++ helper functions for Individual class.
 Details: C++ header.
  
 @author Mikkel Meyer Andersen
 */
 
#ifndef UTIL_H
#define UTIL_H

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

int random_index(int n);

void validate_merge_input(
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& pids_dad, 
    const Rcpp::IntegerVector& birthyears, 
    const Rcpp::IntegerVector& paternalped_ids);

#endif
