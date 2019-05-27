library(tidyverse)
library(jointprof)

devtools::load_all("~/gits/software/copape/")

dad_sons_raw <- readRDS("/home/mikl/work-aau/research-projects/2019-lineages-DK-merge/copape-01/obj-data-dad_sons_raw.Rdata")

if (!exists("dendbmales_org")) {
  dendbmales_org <- data.table::fread("/dev/shm/dendb2017males_shiny", sep = ";") %>% 
    as_tibble()
}

if (FALSE) {
  dendbmales_org %>% 
    filter(postnr == 9000, paternalped_size == 10) %>% 
    select(pid, paternalped_id)
}

pedid_to_merge <- 568656

pids_in_ped_merge <- dendbmales_org %>% 
  filter(paternalped_id == pedid_to_merge) %>% 
  pull(pid)

# Many old individuals a pedigree size of 1:
rm_paternalped_id <- c()

# Pedigrees of size <= 2 and an individual with birthyear <= 1900
rm_paternalped_id <- c(rm_paternalped_id,
                       dendbmales_org %>% 
                         filter(paternalped_size <= 2, fodselsaar <= 1900) %>% 
                         pull(paternalped_id) %>% unique())

if (pedid_to_merge %in% rm_paternalped_id) {
  stop("Cannot remove rm_paternalped_id!")
}

reduced_mat <- dendbmales_org %>% 
  filter(!(paternalped_id %in% rm_paternalped_id))


set.seed(1)

target_file <- "Rprof.out"
start_profiler(target_file)

res <- merge_pedigree(
  pids = dplyr::pull(reduced_mat, pid),
  pids_dad = dplyr::pull(reduced_mat, pid_dad),  
  birthyears = dplyr::pull(reduced_mat, fodselsaar), 
  paternalped_ids = dplyr::pull(reduced_mat, paternalped_id),
  pedid_to_merge = pedid_to_merge,
  sons_configs = dad_sons_raw,
  no_surrogate_ancestors = 6,
  stop_birthyear = 1970,
  surr_pid_start = 50000000,
  verbose = FALSE)

stop_profiler()

head(res)
dim(res)

pprof_target_file <- "Rprof.pb.gz"
profile_data <- profile::read_rprof(target_file)
profile::write_pprof(profile_data, pprof_target_file)

system2(
  find_pprof(),
  c(
    "-http",
    "localhost:8083",
    shQuote(pprof_target_file)
  ),
  wait = FALSE
)

system2("killall", c("-9", find_pprof()))
