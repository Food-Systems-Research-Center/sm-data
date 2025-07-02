#' Pull NASS Variable
#'
#' @description
#' Internal function to pull a particular variable from a bulk API download from NASS data. Only 
#' relevant for the `nass_compilation.R` script. Function will filter it, name it, and remove NAs
#' 
#' @param df 
#' @param sector_desc NASS sector description
#' @param commodity_desc NASS commodity description
#' @param statisticcat_desc NASS statistic category description
#' @param domain_desc NASS domain description
#' @param short_desc NASS short variable description
#' @param variable_name short, unique, camel-case identifier for variable
#' @param source_desc NASS source description
#'
#' @returns
#' @import dplyr
#' @import tidyr
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' results$total_income <- pull_variable(
#'   bulk_nass_df,
#'   sector_desc = 'ECONOMICS',
#'   commodity_desc = 'INCOME, FARM-RELATED',
#'   domain_desc = 'TOTAL',
#'   short_desc = 'INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $ / OPERATION',
#'   variable_name = 'farmIncomePF'
#' )
#' }
pull_variable <- function(df,
                          sector_desc = NULL,
                          commodity_desc = NULL, 
                          statisticcat_desc = NULL,
                          domain_desc = NULL,
                          short_desc = NULL,
                          variable_name = NULL,
                          source_desc = 'CENSUS') {
  dat <- df
  
  if (!is.null(sector_desc)) dat <- filter(dat, sector_desc == !!sector_desc)
  if (!is.null(source_desc)) dat <- filter(dat, source_desc == !!source_desc)
  if (!is.null(statisticcat_desc)) dat <- filter(dat, statisticcat_desc == !!statisticcat_desc)
  if (!is.null(commodity_desc)) dat <- filter(dat, commodity_desc == !!commodity_desc)
  if (!is.null(domain_desc)) dat <- filter(dat, domain_desc == !!domain_desc)
  if (!is.null(short_desc)) dat <- filter(dat, short_desc == !!short_desc)
  
  dat %>%
    mutate(
      variable_name = case_when(
        short_desc == !!short_desc ~ variable_name
      )
    ) %>% 
    select(any_of(c(
      'fips',
      'county_name',
      'state_name',
      'year',
      'variable_name',
      'value',
      'value_codes',
      'cv_percent',
      'domaincat_desc',
      'unit_desc'
    ))) %>% 
    filter(!is.na(variable_name))
}
