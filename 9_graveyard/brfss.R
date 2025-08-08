# BRFSS
# 2025-08-07


# Description -------------------------------------------------------------

# Checking out CDC behavioral risk factor surveillance system


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  haven
)

# Load brffs
dat <- read_xpt('1_raw/cdc/brfss/LLCP2023.XPT')
dat <- read_xpt('1_raw/cdc/brfss/LLCP2022.XPT')


# Explore -----------------------------------------------------------------

names(dat)
get_str(dat)
# Has state variable, but no county
# PLACES datasets are repackaging this, but they do include counties.
# So we are using that instead

# Check from codebook
'LSATISFY' %in% names(dat)
