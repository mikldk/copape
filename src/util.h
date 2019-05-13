/**
 util.h
 Purpose: Header for C++ helper functions
 Details: C++ header.
  
 @author Mikkel Meyer Andersen
 */
 
#ifndef UTIL_H
#define UTIL_H

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

int random_index(int n);

int required_surrogate_fathers(const std::vector<int>& paternalped_ids);

void print_map_int_vecint(const std::unordered_map<int, std::vector<int>>& x);

std::unordered_map<int, std::vector<int>> vector_to_hash(const Rcpp::IntegerVector& x);

std::vector<int> sample_pedids_to_merge(
    const std::unordered_map<int, std::vector<int>>& map,
    const int size,
    const int pedid_skip);

#endif
