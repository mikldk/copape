context("Util")

test_that("random_index()", {
  expect_equal(random_index(0L), 0L)
  
  for (n in seq_len(10L)) {
    x <- replicate(1000L, random_index(n))
    expect_equal(sort(unique(x)), 
                 seq_len(n) - 1L, # C++-indexing is 0-based
                 info = paste0("n = ", n))
  }
})

test_that("validate_merge_input()", {
  expect_error(
    validate_merge_input(pids = 1, 
                         pids_dad = NA, 
                         birthyears = 0, 
                         paternalped_ids = 1),
    NA) # No error
  
  # pid cannot be NA, and must be >= 1
  
  # pid_dad either NA or >= 1
  
  # birthyears free
  
  # paternalped_ids cannot be NA, and must be >= 1 
})
