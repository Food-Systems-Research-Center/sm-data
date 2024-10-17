pacman::p_load(
  dplyr
)

lose_bad_cols <- function(df) {
  out <- df %>% 
    select(-any_of(c(
      'state_ansi',
      'watershed_code',
      'asd_code',
      'begin_code',
      'week_ending',
      'zip_5',
      'state_fips_code',
      'congr_district_code',
      'end_code',
      'country_code',
      'state_alpha',
      'county_code',
      'country_name',
      'county_ansi',
      'watershed_desc',
      'load_time',
      'asd_desc'
    ))) %>% 
    select(order(names(.)))
  return(out)
}

pull_econ_cols <- function(df) {
  out <- df %>% 
    select(
      class_desc,
      commodity_desc,
      state_name,
      domain_desc,
      domaincat_desc,
      group_desc,
      location_desc,
      prodn_practice_desc,
      short_desc,
      statisticcat_desc,
      unit_desc,
      util_practice_desc,
      Value,
      year
    ) %>% 
    select(order(names(.)))
  return(out)
}