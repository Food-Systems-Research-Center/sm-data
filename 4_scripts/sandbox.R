# Sandbox


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr
)

# Clean, aggregated data
dat <- readRDS('2_clean/metrics.rds')
meta <- readRDS('2_clean/metadata.rds')



# Check employee numbers --------------------------------------------------


get_str(meta)
meta$variable_name

