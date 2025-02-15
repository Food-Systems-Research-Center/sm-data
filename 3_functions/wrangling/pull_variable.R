# Pull variable from Census of Ag
# already filtered to NE only.

pacman::p_load(
  dplyr,
  tidyr
)

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
    # filter(
    #   is.null(sector_desc) | sector_desc == !!sector_desc,
    #   is.null(source_desc) | source_desc == !!source_desc,
    #   is.null(statisticcat_desc) | statisticcat_desc == !!statisticcat_desc,
    #   is.null(commodity_desc) | commodity_desc == !!commodity_desc,
    #   is.null(domain_desc) | domain_desc == !!domain_desc,
    #   is.null(short_desc) | short_desc == !!short_desc
    # ) %>% 
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


# Group by fips and sum
get_county_sum <- function(df) {
  df %>% 
    group_by(fips) %>% 
    mutate(value = sum(value))
}
