#' Update exported datasets
#'
#' Internal function that updates the .rda files exported by the package.
#' 
#' @importFrom purrr walk2
#' @keywords internal
update_data <- function() {
  sm_data <- readRDS('6_outputs/sm_data.rds')
  walk2(sm_data, names(sm_data), function(obj, name) {
    assign(name, obj)
    do.call("use_data", list(as.name(name), overwrite = TRUE))
  })
}
