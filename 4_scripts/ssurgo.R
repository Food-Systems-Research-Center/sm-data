# SSURGO
# 2025-07-14


# Description -------------------------------------------------------------

# Pulling SSURGO soil data through soilDB package


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  soilDB,
  sf
)


# Download ----------------------------------------------------------------

# Use mask polygon for northeast to download data fro Northeast states
neast_mask

downloadSSURGO(
  WHERE = neast_mask,
  exdir = '1_raw/spatial/ssurgo',
  db = 'SSURGO'
)



