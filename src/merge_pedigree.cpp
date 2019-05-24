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

std::vector<int> sample_sons_ages(const std::vector< std::vector<int> >& sons_configs_vec) {
  int i = random_index(sons_configs_vec.size());
  return sons_configs_vec[i];
}


std::shared_ptr<Individual> get_or_create_indv(
    const int wanted_birthyear,
    int* next_pid,
    const std::unordered_map<int, std::vector<int>>& map_birthyear_founder_index,
    const std::unordered_map<int, int> map_pedid_founder_indices,
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& paternalped_ids,
    std::vector<int>& pedids_new
) {
  // FIXME: Implement method 1/2, for now just for easier implementation:
  
  const std::unordered_map<int, std::vector<int>>::const_iterator find_birthyear =
    map_birthyear_founder_index.find(wanted_birthyear);
  
  // Surrogate
  if (find_birthyear == map_birthyear_founder_index.end()) {
    int pid = *next_pid;
    *next_pid = *next_pid + 1;
    
    Individual surrogate_indv(pid, wanted_birthyear, true);
    std::shared_ptr<Individual> surrogate_indv_ptr = 
      std::make_shared<Individual>(surrogate_indv);
    
    return surrogate_indv_ptr;
  }
  
  // Else: use a random founder:
  
  // NOTE: ONLY FOUNDERS! HENCE ASSUME ALL SONS ARE KNOWN AND CORRECT, 
  //       ONLY ANCESTORS ARE NOT REGISTERED CORRECT.
  //       THIS IS NOT TRUE.
  //       BUT THIS APPROXIMATION MAY BE BETTER THAN INCLUDING NON-FOUNDERS.
  
  std::vector<int> individuals_with_birthyear = find_birthyear->second;
  
  int rnd_idx = random_index(individuals_with_birthyear.size());
  int individual_idx = individuals_with_birthyear[rnd_idx];
  int pedid = paternalped_ids[individual_idx];
  
  const std::unordered_map<int, int>::const_iterator find_founder_idx = 
    map_pedid_founder_indices.find(pedid);
  
  if (find_founder_idx == map_pedid_founder_indices.end()) {
    Rcpp::stop("UNEXPECTED: Could not find index for founder");
  }
  
  int founder_index = find_founder_idx->second;
  
  pedids_new.push_back(pedid);
  
  int founder_pid = pids[founder_index];
  
  
  Individual existing_indv(founder_pid, wanted_birthyear, false);
  std::shared_ptr<Individual> existing_indv_ptr = 
    std::make_shared<Individual>(existing_indv);
  
  return existing_indv_ptr;
}

void add_children_until_existing_individual(
    std::shared_ptr<Individual> father_ptr, 
    int* next_pid,
    std::vector< std::shared_ptr<Individual> >& indvs_new,
    const std::unordered_map<int, std::vector<int>>& map_birthyear_founder_index,
    const std::unordered_map<int, int> map_pedid_founder_indices,
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& paternalped_ids,
    std::vector<int>& pedids_new,
    const std::vector< std::vector<int> >& sons_configs,
    const int stop_birthyear) {
  
  if (father_ptr->is_surrogate() == false) {
    // stop recursing
    return;
  }
  
  std::vector<int> sons_ages = sample_sons_ages(sons_configs);
  
  for (int age : sons_ages) {
    int son_birthyear = father_ptr->get_birthyear() + age;
    
    Rcpp::Rcout << "son_birthyear = " << son_birthyear << "; father_birthyear = " << father_ptr->get_birthyear() << std::endl;

    std::shared_ptr<Individual> son_ptr = 
      get_or_create_indv(
        son_birthyear,
        next_pid,
        map_birthyear_founder_index, map_pedid_founder_indices,
        pids, paternalped_ids, pedids_new);

    indvs_new.push_back(son_ptr);
    father_ptr->add_child(son_ptr);
    son_ptr->set_father(father_ptr);
    
    if (son_birthyear > stop_birthyear) {
      continue;
    }
    
    if (son_ptr->is_surrogate()) {
      add_children_until_existing_individual(
        son_ptr,
        next_pid,
        indvs_new,
        map_birthyear_founder_index, map_pedid_founder_indices,
        pids, paternalped_ids, pedids_new,
        sons_configs,
        stop_birthyear);
    }
  }
}



void add_ancestor_with_children(
    std::shared_ptr<Individual> individual, 
    int* next_pid,
    std::vector< std::shared_ptr<Individual> >& indvs_new,
    const std::unordered_map<int, std::vector<int>>& map_birthyear_founder_index,
    const std::unordered_map<int, int> map_pedid_founder_indices,
    const Rcpp::IntegerVector& pids,
    const Rcpp::IntegerVector& paternalped_ids,
    std::vector<int>& pedids_new,
    const std::vector< std::vector<int> >& sons_configs,
    const int stop_birthyear) {
  
  // 
  
  std::vector<int> sons_ages = sample_sons_ages(sons_configs);
  
  int rnd_idx = random_index(sons_ages.size());

  int father_pid = *next_pid;
  *next_pid = *next_pid + 1;
  
  int father_birthyear = individual->get_birthyear() - sons_ages[rnd_idx];
  
  
  std::shared_ptr<Individual> father_ptr = 
    get_or_create_indv(
      father_birthyear,
      next_pid,
      map_birthyear_founder_index, map_pedid_founder_indices,
      pids, paternalped_ids, pedids_new);
  
  //Rcpp::print(Rcpp::wrap(sons_birthyears));
  //Rcpp::print(Rcpp::wrap(sons_birthyears[individual_idx]));
  //Rcpp::print(Rcpp::wrap(father_birthyear));
  //Rcpp::Rcout << std::endl;
  
  indvs_new.push_back(father_ptr);
  father_ptr->add_child(individual);
  individual->set_father(father_ptr);
  
  for (int i_sons; i_sons < sons_ages.size(); ++i_sons) {
    if (i_sons == rnd_idx) {
      continue;
    }
    
    int son_birthyear = father_birthyear + sons_ages[i_sons];
    //Rcpp::Rcout << "son_birthyear = " << son_birthyear << "; father_birthyear = " << father_birthyear << std::endl;
    
    std::shared_ptr<Individual> son_ptr = 
      get_or_create_indv(
        son_birthyear,
        next_pid,
        map_birthyear_founder_index, map_pedid_founder_indices,
        pids, paternalped_ids, pedids_new);
    
    
    indvs_new.push_back(son_ptr);
    father_ptr->add_child(son_ptr);
    son_ptr->set_father(father_ptr);
    
    if (son_birthyear >= stop_birthyear) {
      continue;
    }
    
    if (son_ptr->is_surrogate()) {
      add_children_until_existing_individual(
        son_ptr,
        next_pid,
        indvs_new,
        map_birthyear_founder_index, map_pedid_founder_indices,
        pids, paternalped_ids, pedids_new,
        sons_configs,
        stop_birthyear);
    }
  }
  
  
  
  
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
    const int stop_birthyear = 1970,
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
  // Use STL structure instead
  //--------------------------------------------------------------
  int n_sons_configs = sons_configs.size();
  std::vector< std::vector<int> > sons_configs_vec;
  sons_configs_vec.reserve(sons_configs.size());
  
  for (Rcpp::IntegerVector sons_config : sons_configs) {
    std::vector<int> c = Rcpp::as< std::vector<int> >(sons_config);
    sons_configs_vec.push_back(c);
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
  std::vector<int> pedids_new;
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
  
  // Add ancestors.
  // This also ensures children
  std::shared_ptr<Individual> MRCA = PoI_founder;
  pedids_new.push_back(pedid_to_merge);
  
  int next_pid = surr_pid_start;
  
  for (int k = 1; k <= no_surrogate_ancestors; ++k) {
    add_ancestor_with_children(
      MRCA, 
      &next_pid,
      indvs_new,
      map_birthyear_founder_index,
      map_pedid_founder_indices,
      pids,
      paternalped_ids,
      pedids_new,
      sons_configs_vec,
      stop_birthyear);
    
    MRCA = MRCA->get_father();
  }
  
  ////////////////////////////////////////////////////////////////
  
  if (verbose) {
    Rcpp::Rcout << "New individiduals:" << std::endl;
    
    for (auto indv_ptr : indvs_new) {
      Individual indv = *indv_ptr;
      Rcpp::Rcout << indv << std::endl;
    }
  }

  ////////////////////////////////////////////////////////////////
  // Construct result
  //--------------------------------------------------------------
  
  // Construct resulting 1 pedigree:
  //  + pid
  //  + pid_dad
  //  + birthyear
  //  + surrogate
  // 
  // Done by elements in
  //  + indvs_new
  //  + non-founders in pedids_new (founders are in indvs_new)
  
  std::vector<int> res_vec_pids;
  std::vector<int> res_vec_pids_dad;
  std::vector<int> res_vec_birthyears;
  std::vector<bool> res_vec_surrogate;
  
  // pedids_new
  for (int ped_id : pedids_new) {
    const std::unordered_map< int, std::vector<int> >::const_iterator find =
      pedid_to_indices.find(ped_id);
    
    if (find == pedid_to_indices.end()) {
      Rcpp::stop("UNEXPECTED: Pedigree not found");
    }
    
    std::vector<int> indices = find->second;
    
    for (int i : indices) {
      // Founder, so is in indvs_new
      if (Rcpp::IntegerVector::is_na(pids_dad[i])) {
        continue;
      }
      
      res_vec_pids.push_back(pids[i]);
      res_vec_pids_dad.push_back(pids_dad[i]);
      res_vec_birthyears.push_back(birthyears[i]);
      res_vec_surrogate.push_back(false);
    }
  }
  
  // indvs_new
  for (auto indv_ptr : indvs_new) {
    Individual indv = *indv_ptr;

    res_vec_pids.push_back(indv.get_pid());
    res_vec_pids_dad.push_back(indv.get_father_pid_safe());
    res_vec_birthyears.push_back(indv.get_birthyear());
    res_vec_surrogate.push_back(indv.is_surrogate());
  }
  
  // 
  
  //--------------------------------------------------------------
  Rcpp::IntegerVector res_pids = Rcpp::wrap(res_vec_pids);
  Rcpp::IntegerVector res_pids_dad = Rcpp::wrap(res_vec_pids_dad);
  Rcpp::IntegerVector res_birthyears = Rcpp::wrap(res_vec_birthyears);
  Rcpp::LogicalVector res_surrogate = Rcpp::wrap(res_vec_surrogate);
  
  // Convert -1 to NA
  int n_res = res_pids_dad.size();
  
  for (int i = 0; i < n_res; ++i) {
    if (res_pids_dad[i] == -1) {
      res_pids_dad[i] = Rcpp::IntegerVector::get_na();
    }
  }
  
  
  Rcpp::DataFrame res = Rcpp::DataFrame::create(
    Rcpp::Named("pid") = res_pids,
    Rcpp::Named("pid_dad") = res_pids_dad,
    Rcpp::Named("birthyears") = res_birthyears,
    Rcpp::Named("is_surrogate") = res_surrogate
  );
  
  return res;
}
