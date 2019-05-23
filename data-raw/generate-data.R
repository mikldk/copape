generate_data <- function() {
#   sons_dist <- list(
#     age_at_birth = list(28, c(27, 29), c(25, 27, 29), c(18, 25, 35, 45)),
#     cumprob = c(0.4, 0.3+0.4, 0.2+0.3+0.4, 0.1+0.2+0.3+0.4)
#   )
# 
#   usethis::use_data(sons_dist,
#                     internal = TRUE, overwrite = TRUE)

  test_sons_raw <- c(rep(list(30), 4), 
                     rep(list(c(27, 29)), 3),
                     rep(list(c(25, 27, 29)), 2),
                     rep(list(c(18, 25, 35, 45)), 1)
  )
  #sons_raw
  
  usethis::use_data(test_sons_raw,
                    internal = TRUE, overwrite = TRUE)
}
