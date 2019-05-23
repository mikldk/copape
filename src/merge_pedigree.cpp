// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>

#include <cmath>
#include <vector>
#include <unordered_map>
#include <queue>
#include <memory>

#include "copape_types.h"
#include "util.h"
#include "util-validate.h"

/*
 * P: ped with PoI
 * x: surrogate ancestor
 * o: existing founders
 * 
 * ---------------------------------------------------------------------
 * METHODS
 * ---------------------------------------------------------------------
 * METHOD 1 (rejection):
 *   Parameters: `max_rejections`
 *   
 * METHOD 2 (relaxation)
 *   Parameters: `max_years_diff`
 *   
 * ---------------------------------------------------------------------
 * Initial step: We have only P.
 * ---------------------------------------------------------------------
 * 
 * `sample_ancestor_with_children(P)`:
 *   a) Sample a sons configuration (number of sons and ages).
 *   b) Randomly pick which of the sons P is. 
 *      Now birthyear of new surrogate ancestor and the the other sons can be calculated.
 *   c) Sample among the other existing pedigree founders some with those birthyears.
 *        + If none exists:
 *            METHOD 1 (rejection):
 *              Go to a) 
 *              [but only `max_rejections` number of times]
 *            
 *            METHOD 2 (relaxation):
 *              Try birthyear at distance one, distance two, ... 
 *              [but only up to `max_years_diff`]
 *          
 *   Now we have (e.g. for three sons):
 *
 *        x
 *      / | \
 *     P  o  o
 *  
 * ---------------------------------------------------------------------
 * Induction step: Attach new surrogate ancestor to surrogate ancestor.
 * ---------------------------------------------------------------------
 * 
 * Same as above. But step c):
 * If birthyear(y) < min(observed_birthyears): create surrogate individual, 
 * and continue sampling sons and picking until required birthyear is within range
 * and then sample according to METHOD 1 (rejection) or METHOD 2 (relaxation).
 * 
 * Thus, only need 1 `sample_ancestor_with_children(x)`, where `x` initially is PoI.
 */

void sample_ancestor_with_children() {
  
}


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
  // Make map from birthyear to founder indices for faster look-up  
  //--------------------------------------------------------------
  std::unordered_map<int, std::vector<int>> map_birthyear_founder_index;
  int n = birthyears.length();
  for (int i = 0; i < n; ++i) {
    if (Rcpp::IntegerVector::is_na(pids_dad[i])) {
      map_birthyear_founder_index[ birthyears[i] ].push_back(i);
    }
  }
  ////////////////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////////////////
  // Make map from ped to indices for faster look-up  
  //--------------------------------------------------------------
  std::unordered_map< int, std::vector<int> > pedid_to_indices = 
    vector_to_hash(paternalped_ids);
  //print_map_int_vecint(pedid_to_indices);
  
  // Check that pedid_to_merge exists
  validate_key_exists(pedid_to_indices, pedid_to_merge);
  
  // Map from pedid to index
  std::unordered_map<int, int> map_pedid_founder_indices = 
    find_founder_indices(pedid_to_indices, pids_dad);
  
  int idx_PoI_founder = map_pedid_founder_indices[pedid_to_merge];
  
  if (verbose) {
    Rcpp::Rcout << "pedid_to_merge = " << 
      pedid_to_merge << " has founder at row " << 
        (idx_PoI_founder + 1) << std::endl;
  }
  
  ////////////////////////////////////////////////////////////////
  // Construct all individuals in PoI pedigree
  //--------------------------------------------------------------
  std::vector<int> pedigree_PoI = pedid_to_indices.find(pedid_to_merge)->second;
  std::shared_ptr<Individual> PoI_founder = nullptr;
  std::vector< std::shared_ptr<Individual> > indvs_new;
  int pid_PoI_founder = pids[idx_PoI_founder];
  
  // !! These do not need all the connections established (sons/father)
  //    as they will never be used !!
  for (int idx : pedigree_PoI) {
    Individual indv(pids[idx], birthyears[idx], false);
    std::shared_ptr<Individual> indv_ptr = std::make_shared<Individual>(indv);
      
    if (indv.get_pid() == pid_PoI_founder) {
      if (PoI_founder != nullptr) {
        Rcpp::stop("Expected only one founder");
      }
      
      PoI_founder = indv_ptr;
    }
    
    indvs_new.push_back(indv_ptr);
  }
  
  if (PoI_founder == nullptr) {
    Rcpp::stop("Expected one founder");
  }
  
  if (verbose) {
    Rcpp::Rcout << "pedid_to_merge = " << 
      pedid_to_merge << " has founder with pid = " << 
        PoI_founder->get_pid() << std::endl;
  }
  
  ////////////////////////////////////////////////////////////////
  
 
  
  ////////////////////////////////////////////////////////////////
  
  Rcpp::DataFrame res;
  
  return res;
}
