// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

#include <cmath>
#include <vector>
#include <unordered_map>
#include <queue>

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
 *      Now birthyear of new surrogate ancestor can be calculated.
 *      And birthyear of the other sons than be can be calculated.
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
 * Let Q be the founder.
 * 
 *        Q
 *      / | \
 *     P  o  o
 * 
 *  Same as base step above for Q = P:
 *  Except that some new surrogate children must be added.
 *  
 * `sample_ancestor_with_children(Q)`:
 *   a) Sample a sons configuration (number of sons and ages).
 *   b) Randomly pick which of the sons Q is. 
 *      Now birthyear of new surrogate ancestor can be calculated.
 *      And birthyear of the other sons than be can be calculated.
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
 *            x _____
 *          /  \      \
 *        Q      x     x
 *      / | \
 *     P  o  o
 *     
 *  Let y != Q be children to the new founder (except Q):
 *
 *
 * Generation
 * 
 * 2         x  _____
 *          /  \      \
 * 1       Q      y     y
 *       / | \
 * 0    P  o  o
 *  
 *  
 *  For each y:
 *  `sample_descendents(y)`:   
 *   a) If `generation(y) > generation(P) + 1`
 *        Sample a sons configuration (number of sons and ages).
 *        Create surrogate individuals, z, as children to y.
 *        Call `sample_descendents(z)`
 *   b) If `generation(y) = generation(P) + 1`
 *        Step c) from `sample_ancestor_with_children`:
 *        I.e. attach to existing pedigrees.
 *        
 *   TODO:
 *   HOW DOES DISCRETE GENERATIONS WORK WITH THIS?
 *   DO WE NEED GENERATION AND BIRTHYEAR? OR JUST BIRTHYEAR?
 *   FIXME: JUST BIRTHYEAR!
 *          TRY TO SAMPLE INDIVIDUALS WITH NEEDED BIRTHYEARS. 
 *          IF NONE EXISTS, INTRODUCE SURROGATE.
 *          WITH SOME SURROGATE ANCESTORS, WE KNOW THAT SURROGATE MUST BE INTRODUCED.
 *          BUT THERE CAN BE BIAS?!
 *          PARAMETER CONTROLLING THIS?
 *     
 *     
 *  Always two sons:
 * 
 *             x
 *        ____/ \____
 *       x           x
 *      / \         / \
 *    x     x     x     x
 *   / \   / \   / \   / \
 *  P   o o   o o   o o   o
 */


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
