# Load utils so we can use install.packages()
library(utils)

# Set CRAN mirror
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})

# Install pacman if not installed already
suppressPackageStartupMessages(
  if (!requireNamespace('pacman')) {
    install.packages('pacman', dependencies = TRUE, quiet = TRUE)
  }
)

# Load conflicted for namespace conflicts
pacman::p_load(conflicted)
pacman::p_load_gh('ChrisDonovan307/projecter')

# Set conflict winners
conflicts_prefer(
  dplyr::select(),
  dplyr::filter(),
  dplyr::rename(),
  dplyr::summarize(),
  purrr::flatten(),
  base::intersect(),
  base::union(),
  .quiet = TRUE
)

# Set options
options(
  max.print = 950,
  pillar.print_max = 950,
  pillar.print_min = 950
)

# Load table of contents script
if (Sys.info()["sysname"] == "Mac") {
  tryCatch({
    system('open "table_of_contents.R"')
    cat('\nLoading Table of Contents')
  }, error = function(e) {
    cat('\nCould not open table of contents.')
  })
  
} else if (Sys.info()["sysname"] == "Windows") {
  tryCatch({
    shell.exec('table_of_contents.R')
    cat('\nLoading Table of Contents')
  }, error = function(e) {
    cat('\nCould not open table of contents.')
  })
  
} else {
  cat('\nCould not open table of contents.')
}

source("renv/activate.R")
