context("Util")

test_that("random_index()", {
  expect_equal(random_index(0L), 0L)
  
  for (n in seq_len(10L)) {
    x <- replicate(1000L, random_index(n))
    expect_equal(sort(unique(x)), seq_len(n), info = paste0("n = ", n))
  }
})
