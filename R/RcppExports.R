# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

merge_all_random <- function(pids, pids_dad, birthyears, paternalped_ids, surr_pid_start = 50000000L, max_it = -1L) {
    .Call('_copape_merge_all_random', PACKAGE = 'copape', pids, pids_dad, birthyears, paternalped_ids, surr_pid_start, max_it)
}

#' Random index between 0 and n-1 (both included)
#' 
#' @param n upper limit (not included)
random_index <- function(n) {
    .Call('_copape_random_index', PACKAGE = 'copape', n)
}
