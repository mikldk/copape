visualise_merges <- function(
  pids,
  pids_dad, 
  birthyears, 
  paternalped_ids,
  pedid_to_merge,
  sons_configs,
  seed = 1,
  no_surrogate_ancestors = 1,
  stop_birthyear = 1970,
  surr_pid_start = 50000000,
  verbose = FALSE) {
  
  # NOTE: Make better?
  
  res_lst <- lapply(c(0L, seq_len(no_surrogate_ancestors)), function(i) {
    # i <- 
    set.seed(seed)
    
    res_i <- merge_pedigree(
      pids = dplyr::pull(test_males, pid),
      pids_dad = dplyr::pull(test_males, pid_dad),  
      birthyears = dplyr::pull(test_males, birthyear), 
      paternalped_ids = dplyr::pull(test_males, paternalped_id),
      pedid_to_merge = pedid_to_merge,
      sons_configs = test_sons_configs,
      no_surrogate_ancestors = i,
      stop_birthyear = stop_birthyear,
      surr_pid_start = surr_pid_start,
      verbose = verbose)
    
  })

}