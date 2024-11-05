# Pull variable from Census of Ag
# already filtered to NE only.

pacman::p_load(
  dplyr,
  tidyr
)

pull_variable <- function(df,
                          sector_desc,
                          commodity_desc, 
                          domain_desc,
                          short_desc,
                          variable_name,
                          source_desc = 'CENSUS') {
  df %>%
    filter(
      is.null(sector_desc) | sector_desc == !!sector_desc,
      is.null(source_desc) | source_desc == !!source_desc,
      is.null(commodity_desc) | commodity_desc == !!commodity_desc,
      is.null(domain_desc) | domain_desc == !!domain_desc,
      is.null(short_desc) | short_desc == !!short_desc
    ) %>% 
    mutate(
      variable_name = case_when(
        short_desc == !!short_desc ~ variable_name
      )
    ) %>% 
    select(
      fips,
      county_name,
      state_name,
      year,
      variable_name,
      value,
      value_codes,
      cv_percent
    ) %>% 
    filter(!is.na(variable_name))
}


# Group by fips and sum
get_county_sum <- function(df) {
  df %>% 
    group_by(fips) %>% 
    mutate(value = sum(value))
}
