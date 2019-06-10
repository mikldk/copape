# R -d gdb -f mikl/debug.R

devtools::load_all(".")

#library(copape)

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
        14416513,
        
        555
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
        1L,
        
        555L
      )
    ),
    class = "data.frame",
    row.names = c(NA,-23L)
  )

pedid_to_merge <- 38

set.seed(1)
res <- merge_pedigree(
  pids = dplyr::pull(d_males, pid),
  pids_dad = dplyr::pull(d_males, pid_dad),  
  birthyears = dplyr::pull(d_males, birthyear), 
  paternalped_ids = dplyr::pull(d_males, paternalped_id),
  pedid_to_merge = pedid_to_merge,
  sons_configs = test_sons_raw,
  no_surrogate_ancestors = 3,
  stop_birthyear = 1970,
  surr_pid_start = 50000000,
  verbose = TRUE)

