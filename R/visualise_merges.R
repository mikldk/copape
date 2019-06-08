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


#' @importFrom igraph graph_from_data_frame
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom tibble tibble
#' @importFrom dplyr left_join inner_join select rename filter mutate case_when pull
#' @importFrom ggplot2 scale_x_continuous scale_y_reverse theme element_line element_blank
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_point
#' 
#' @export
ggcopape <- function(d) {
  
  d_tmp <- d
  d_tmp <- dplyr::select(d_tmp, pid, pid_dad)
  d_tmp <- dplyr::left_join(d_tmp, dplyr::select(d, pid), 
              by = c("pid_dad" = "pid"), 
              suffix = c("_son", "_dad"))
  d_tmp <- dplyr::rename(d_tmp, pid_son = pid)
  d_tmp <- dplyr::filter(d_tmp, complete.cases(d_tmp))
  d_tmp <- dplyr::select(d_tmp, 2, 1)

  graph <- igraph::graph_from_data_frame(d_tmp, directed = TRUE)
  
  g <- tidygraph::as_tbl_graph(graph)
  
  g2 <- g
  g2 <- tidygraph::activate(g2, nodes)
  g2 <- dplyr::left_join(g2, dplyr::mutate(d, pid = as.character(pid)), by = c("name" = "pid"))
  g2 <- dplyr::mutate(g2, org_paternalped_id = as.character(org_paternalped_id))
  g2 <- dplyr::mutate(g2, org_paternalped_id = dplyr::case_when(
      is.na(org_paternalped_id) ~ "NA",
      TRUE ~ org_paternalped_id
    ))
  
  # Create layout
  ll <- ggraph::create_layout(g2, layout = 'dendrogram', 
                              circular = FALSE)
  ll2 <- ll
  ll_nms <- as.integer(as.character(ll2$name))
  ll_df <- tibble::tibble(pid = ll_nms)
  ll_df <- dplyr::inner_join(ll_df, d, by = c("pid"))
  
  
  stopifnot(isTRUE(all.equal(dplyr::pull(ll_df, pid), ll_nms)))
  ll2$y <- dplyr::pull(ll_df, birthyear)

  p <- ggraph::ggraph(ll2) + 
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::scale_y_reverse(breaks = scales::pretty_breaks(10)) +
    ggplot2::theme(
      #axis.line = element_blank(),
      axis.text.x = ggplot2::element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      #legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(size = 0.1, 
                                               colour = 'darkgrey'),
      panel.grid.minor = ggplot2::element_line(size = 0.1, 
                                               colour = 'darkgrey'),
      plot.background = ggplot2::element_blank()) 
  
  return(p)
}

