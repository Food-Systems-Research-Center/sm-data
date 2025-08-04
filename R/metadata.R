#' Sustainability Metrics Metadata
#'
#' Metadata associated with `metrics` dataset.
#'
#' @format ## `metadata` A data frame in wide format.
#' \describe{
#'   \item{variable_name}{Unique text identifier for each metric.}
#'   \item{dimension}{Framework dimension}
#'   \item{index}{Framework index}
#'   \item{indicator}{Framework indicator}
#'   \item{metric}{Framework metric}
#'   \item{axis_name}{Short name to be used in graph axis}
#'   \item{definition}{Longer form definition of the metric, usually pulled
#'   from the source itself.}
#'   \item{resolution}{Smallest geographic area that the metric can be used at.
#'   This is often county or state, but if spatial data, could also be vector
#'   or the size of raster cells (e.g., 30m).}
#'   \item{scope}{Largest geographic area that the metric applies to.}
#'   \item{updates}{How often the metric is updated from the source}
#'   \item{latest_year}{Latest year for which data are available}
#'   \item{year}{Comma separated string of all years in which the metric is
#'   available}
#'   \item{source}{Source of data}
#'   \item{url}{Website where metric can be downloaded from}
#'   \item{citation}{Citation for data source}
#'   \item{units}{Units of the metric}
#'   \item{annotation}{Describes whether there is a variance, coefficient of
#'   variation, confidence interval, or other measure associated with the 
#'   metric.}
#' }
'metadata'