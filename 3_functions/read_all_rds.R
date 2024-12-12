# read_all_rds

pacman::p_load(
  dplyr,
  purrr,
  stringr
)

read_all_rds <- function(path, pattern = NULL, split = '\\.') {
  paths <- list.files(path, pattern = pattern, full.names = TRUE)
  names <- list.files(path, pattern = pattern, full.names = FALSE) %>% 
    str_split_i(split, 1)
  out <- map(paths, readRDS) %>% 
    setNames(c(names))
  return(out)
}
