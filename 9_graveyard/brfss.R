# Brffs
# 2025-05-12


# Description -------------------------------------------------------------

# Checking out CDC behavioral risk factor surveillance system


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  haven
)

# Load brffs
dat <- read_xpt('1_raw/cdc/2023_brfss/LLCP2023.XPT')


# Explore -----------------------------------------------------------------

get_str(dat)
