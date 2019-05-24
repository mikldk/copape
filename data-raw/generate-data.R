generate_data <- function() {
  test_sons_configs <- c(rep(list(30), 4), 
                         rep(list(c(27, 29)), 3),
                         rep(list(c(25, 27, 29)), 2),
                         rep(list(c(18, 25, 35, 45)), 1)
  )
  
  test_males <-
    structure(
      list(
        pid = c(
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          1,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21,
          22,
          
          555
        ),
        pid_dad = c(
          NA,
          10,
          6,
          6,
          NA,
          NA,
          2,
          8,
          7,
          7,
          12,
          NA,
          5,
          9,
          5,
          10,
          11,
          9,
          NA,
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
          2008,
          
          1926
        ),
        paternalped_id = c(
          5L,
          6L,
          7L,
          7L,
          7L,
          6L,
          5L,
          5L,
          6L,
          6L,
          8L,
          8L,
          7L,
          5L,
          7L,
          6L,
          8L,
          5L,
          4L,
          3L,
          2L,
          1L,
          
          555L
        )
      ),
      class = "data.frame",
      row.names = c(NA,-23L)
    )
  
  usethis::use_data(test_sons_configs, 
                    test_males,
                    internal = FALSE, overwrite = TRUE)
}
