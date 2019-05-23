context("Test")

d_males <-
  structure(
    list(
      pid = c(
        13584072,
        11721524,
        11741690,
        6031700,
        16428832,
        9567688,
        9515216,
        16210893,
        1,
        19051740,
        5881475,
        3857336,
        14953427,
        15385716,
        1548940,
        975490,
        130974,
        14800761,
        18944652,
        6962050,
        418428,
        14416513
      ),
      pid_dad = c(
        NA,
        19051740,
        16428832,
        16428832,
        NA,
        NA,
        13584072,
        9515216,
        9567688,
        9567688,
        3857336,
        NA,
        6031700,
        16210893,
        6031700,
        19051740,
        5881475,
        16210893,
        NA,
        NA,
        NA,
        NA
      ),
      birthyear = c(
        1911,
        2006,
        1995,
        1992,
        1958,
        1953,
        1948,
        1979,
        1988,
        1980,
        1978,
        1950,
        2013,
        2003,
        2008,
        2009,
        2013,
        2015,
        1960,
        1973,
        2015,
        2008
      ),
      paternalped_id = c(
        12L,
        26L,
        38L,
        38L,
        38L,
        26L,
        12L,
        12L,
        26L,
        26L,
        17L,
        17L,
        38L,
        12L,
        38L,
        26L,
        17L,
        12L,
        4L,
        3L,
        2L,
        1L
      )
    ),
    class = "data.frame",
    row.names = c(NA,-22L)
  )

if (FALSE) {
  dplyr::count(d_males, paternalped_id)
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
    validate_sons_configs(sons_raw),
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
  d_males
  pedid_to_merge <- 38
  d_males %>% 
    dplyr::mutate(PedOfInt = ifelse(paternalped_id == pedid_to_merge, "X", ""),
                  PedOfIntFounder = ifelse(paternalped_id == pedid_to_merge & is.na(pid_dad), "XX", ""))
  
  res <- merge_pedigree(
    pids = dplyr::pull(d_males, pid),
    pids_dad = dplyr::pull(d_males, pid_dad),  
    birthyears = dplyr::pull(d_males, birthyear), 
    paternalped_ids = dplyr::pull(d_males, paternalped_id),
    pedid_to_merge = pedid_to_merge,
    sons_configs = test_sons_raw,
    no_surrogate_ancestors = 1,
    surr_pid_start = 50000000,
    verbose = TRUE)
  res
})
