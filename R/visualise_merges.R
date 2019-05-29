#' @export
add_dad_birthyear <- function(x) {
  z <- x %>% 
    left_join(x %>% select(pid, birthyear), 
              by = c("pid_dad" = "pid"), 
              suffix = c("", "_dad"))
  
  return(z)
}

#' @export
add_dad_is_surrogate <- function(x) {
  z <- x %>% 
    left_join(x %>% select(pid, is_surrogate), 
              by = c("pid_dad" = "pid"), 
              suffix = c("", "_dad"))
  
  return(z)
}


#' @export
insert_missing_pids <- function(d) {
  d <- d %>% mutate(sex = "male", 
                    pid_mom = NA,
                    status = 0) 
  # Slow, ok for now
  
  #d <- d_tmp %>% arrange(pid)
  
  #next_pid <- 1L + max(d$pid, d$pid_dad, d$pid_mom, na.rm = TRUE)
  new_persons <- list()
  
  for (i in 1L:nrow(d)) {

    if (!is.na(d$pid_dad[i]) && !(d$pid_dad[i] %in% d$pid)) {
      new_persons[[length(new_persons) + 1L]] <- tibble(
        pid = d$pid_dad[i],
        pid_dad = NA,
        pid_mom = NA,
        sex = "male",
        status = 0)
    }
    
    if (!is.na(d$pid_mom[i]) && !(d$pid_mom[i] %in% d$pid)) {
      new_persons[[length(new_persons) + 1L]] <- tibble(
        pid = d$pid_mom[i], 
        pid_dad = NA,
        pid_mom = NA,
        sex = "female",
        status = 0)
    }
    
    # Now, non-NA parents exists, but NA's don't and kinship2
    # forces either both or no parents
    if (is.na(d$pid_mom[i]) && !is.na(d$pid_dad[i])) {
      
      # FIXME: Stupid, but probably okay
      new_pid_mom <- 1L
      while (new_pid_mom %in% d$pid || new_pid_mom %in% d$pid_mom || new_pid_mom %in% d$pid_dad) {
        new_pid_mom <- new_pid_mom + 1L
      }
      
      d$pid_mom[i] <- new_pid_mom
      new_persons[[length(new_persons) + 1L]] <- tibble(
        pid = new_pid_mom, 
        pid_dad = NA,
        pid_mom = NA,
        sex = "female",
        status = 0)
        #status = 1)
    }
    if (!is.na(d$pid_mom[i]) && is.na(d$pid_dad[i])) {
      
      # FIXME: Stupid, but probably okay
      new_pid_dad <- 1L
      while (new_pid_dad %in% d$pid || new_pid_dad %in% d$pid_mom || new_pid_dad %in% d$pid_dad) {
        new_pid_dad <- new_pid_dad + 1L
      }
      
      d$pid_dad[i] <- new_pid_dad
      new_persons[[length(new_persons) + 1L]] <- tibble(
        pid = new_pid_dad, 
        pid_dad = NA,
        pid_mom = NA,
        sex = "male",
        status = 0)
        #status = 1)
    }
  }
  
  d_new_persons <- bind_rows(new_persons) %>% 
    distinct()
  
  d <- bind_rows(d, d_new_persons)
  return(d)
}




#' @importFrom kinship2 pedigree plot.pedigree
#' @export
visualise_merges <- function(res, highlight_pids1 = c(), highlight_pids2 = c(), cex = 0.5, ...) {
  ped_raw <- res %>% 
    insert_missing_pids()
  
  label <- ped_raw %>% 
    mutate(pid_chr = ifelse(is.na(birthyear), 
                            paste0("(", pid, ")"),
                            paste0(birthyear, "\n", "(", pid, ")",
                                   ifelse(is_surrogate, "\nSURR", "")))) %>% 
    pull(pid_chr)
  
  clr <- ped_raw %>% 
    mutate(clr = case_when(
      length(highlight_pids1) > 0 & pid %in% highlight_pids1 ~ "red",
      length(highlight_pids2) > 0 & pid %in% highlight_pids2 ~ "yellow",
      sex == "female" ~ "lightgrey",
      is_surrogate ~ "blue",
      TRUE ~ "black")) %>% 
    pull(clr)
  
  ped <- with(ped_raw, kinship2::pedigree(
    id = pid, 
    dadid = pid_dad, 
    momid = pid_mom, 
    sex = sex,
    status = status))

  p <- kinship2::plot.pedigree(ped, id = label, col = clr, cex = cex, ...)
  
  return(invisible(p))
}
