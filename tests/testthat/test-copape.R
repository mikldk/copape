context("Test")

if (FALSE) {
  dplyr::count(test_males, paternalped_id)
}


test_that("validate_merge_input()", {
  expect_error(
    validate_merge_input(pids = 1, 
                         pids_dad = NA, 
                         birthyears = 0, 
                         paternalped_ids = 1),
    NA) # No error
  
  # pid cannot be NA, and must be >= 1
  expect_error(
    validate_merge_input(
      pids = c(NA, 2L),
      pids_dad = c(NA, NA),
      birthyears = c(NA, NA),
      paternalped_ids = c(1L, 2L)),
    regexp = "^pid cannot be NA$"
  )
  
  # TODO: Check pids unique!
  
  # pid_dad either NA or >= 1
  expect_error(
    validate_merge_input(
      pids = c(1L, 2L),
      pids_dad = c(-1L, NA),
      birthyears = c(NA, NA),
      paternalped_ids = c(1L, 2L)),
    regexp = "^pid_dad must be NA or >= 1$"
  )
  
  # birthyears free
  
  # paternalped_ids cannot be NA, and must be >= 1 
  expect_error(
    validate_merge_input(
      pids = c(1L, 2L),
      pids_dad = c(1L, NA),
      birthyears = c(NA, NA),
      paternalped_ids = c(NA, 2L)),
    regexp = "^ped_id cannot be NA$"
  )
})



test_that("validate_sons_configs()", {
  expect_error(
    validate_sons_configs(test_sons_configs),
    NA) # No error
  
  expect_error(validate_sons_configs(list(c(28, 30), c(24))), 
               regexp = NA) # No error
  
  expect_error(validate_sons_configs(list()), 
               regexp = "^sons_configs cannot be empty$")
  
  # Error in converting NULL to IntegerVector:
  expect_error(validate_sons_configs(list(c(28, 30), c())), 
               class = "Rcpp::not_compatible")
  
  expect_error(validate_sons_configs(list(c(28, 30), c(-1))), 
               regexp = "age <= 0")
})



test_that("TBA", {
  library(dplyr)
  
  pedid_to_merge <- 38
  # test_males %>% 
  #   dplyr::mutate(PedOfInt = ifelse(paternalped_id == pedid_to_merge, "X", ""),
  #                 PedOfIntFounder = ifelse(paternalped_id == pedid_to_merge & is.na(pid_dad), "XX", ""))
  
  test_males %>% 
    filter(paternalped_id == pedid_to_merge)
  
  set.seed(1)
  res <- merge_pedigree(
    pids = dplyr::pull(test_males, pid),
    pids_dad = dplyr::pull(test_males, pid_dad),  
    birthyears = dplyr::pull(test_males, birthyear), 
    paternalped_ids = dplyr::pull(test_males, paternalped_id),
    pedid_to_merge = pedid_to_merge,
    sons_configs = test_sons_configs,
    no_surrogate_ancestors = 7,
    stop_birthyear = 1970,
    surr_pid_start = 5000,
    verbose = TRUE)
  res
  
  res %>% 
    left_join(test_males %>% select(pid, pid_dad, birthyear, paternalped_id),
              by = "pid")
})

