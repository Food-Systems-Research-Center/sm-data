# FDA API
# 2025-06-30 update

# Pulling recall enforcement from FDA API and cleaning


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr,
  tidyr
)

results <- list()
metas <- list()

# Aggregate and Save ------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts to see if they match
check_n_records(out$result, out$meta, 'FDA, USDM')

# Save 
saveRDS(out$result, '5_objects/metrics/fda_usdm.RDS')
saveRDS(out$meta, '5_objects/metadata/fda_usdm_meta.RDS')

clear_data()
gc()
