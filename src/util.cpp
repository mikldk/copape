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


// ??????
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


void print_map_int_vecint(const std::unordered_map<int, std::vector<int>>& x) {
  for (auto v : x) {
    Rcpp::Rcout << v.first << ":\n" << std::endl;
    
    for (auto y : v.second) {
      Rcpp::Rcout << " " << y;
    }
    
    Rcpp::Rcout << std::endl;
  }
}


std::unordered_map<int, std::vector<int>> vector_to_hash(const Rcpp::IntegerVector& x) {
  std::unordered_map<int, std::vector<int>> map;
  int n = x.length();
  for (int i = 0; i < n; ++i) {
    map[ x[i] ].push_back(i);
  }
  
  return map;
}

std::unordered_map<int, int> find_founder_indices(
    const std::unordered_map<int, std::vector<int>>& pedid_to_indices,
    const Rcpp::IntegerVector& pids_dad) {
  
  std::unordered_map<int, int> pedid_founder_indices;
  
  for (auto v : pedid_to_indices) {
    bool found = false;
    
    for (auto i : v.second) {
      if (Rcpp::IntegerVector::is_na(pids_dad[i])) {
        if (found) {
          Rcpp::stop("Pedigree have more than one founder. Unexpected.");
        }
        
        pedid_founder_indices[v.first] = i;
        found = true;
        // TODO: break instead?
      }
    }
    
    if (!found) {
      Rcpp::stop("Pedigree did not have a founder. Unexpected.");
    }
  }
  
  return pedid_founder_indices;
}


/*
// Returns a shuffled vector
std::vector<int> sample_pedids_to_merge(const std::unordered_map<int, std::vector<int>>& map,
                                        const int size,
                                        const int pedid_skip) {
  
  // Find existing pedids:
  std::vector<int> pedids_candidates;
  pedids_candidates.reserve(map.size() - 1);
  for(auto x : map) {
    if (x.first == pedid_skip) {
      continue;
    }
    
    pedids_candidates.push_back(x.first);
  } 
  
  
  if (pedids_candidates.size() < size) {
    Rcpp::stop("Not enough pedigrees to perform that number of ancestors");
  }
  
  int n_cand = pedids_candidates.size();
  Rcpp::IntegerVector selected_pedids_indices = Rcpp::sample(
    n_cand, // n
    size, // size
    false, // replace
    R_NilValue, // probs
    false); // one-based
  
  std::vector<int> selected_pedids(size);
  for (int i = 0; i < size; ++i) {
    selected_pedids[i] = pedids_candidates[ selected_pedids_indices[i] ];
  }
  
  return selected_pedids;
}
 */
