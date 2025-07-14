#' SM Export
#'
#' @description Package and export the metrics, metadata, and utility objects.
#'   Noting that the aggregate and export scripts should probably be combined
#'   into one at some point.
#' @param update If `TRUE`, run `sm_update()` to update the `.rda` files in the
#'   project.
#' @returns Output data to "6_outputs/". "sm_data.rds" contains metrics, keys,
#'   and probably other things. "sm_spatial.rds" contains state and county
#'   polygons as well as a few other choice spatial datasets. The function also
#'   creates "metadata.csv" and "metrics.csv" which can be used to pick up
#'   analysis in other languages if necessary.
#' @keywords internal
sm_export <- function(update = TRUE, load_all = TRUE) {
  source('4_scripts/export_data.R')
  if (update) sm_update()
  if (load_all) devtools::load_all()
}
