context("Data")

test_that("sons_dist()", {
  expect_true(all(sons_dist$cumprob > 0))
  expect_true(all(sons_dist$cumprob <= 1))
  expect_true(all(diff(sons_dist$cumprob) > 0))
  expect_equal(tail(sons_dist$cumprob, 1), 1)
})
