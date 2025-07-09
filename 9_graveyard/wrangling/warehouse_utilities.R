# Warehouse utilities
# 2024-10-21

# Collection of functions to reorganize metrics and metadata from data warehouse


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  snakecase
)



# Functions ---------------------------------------------------------------


# Pull metadata from data warehouse and select relevant parts
wrangle_meta <- function(df, vars) {
  df %>% 
    select(
      variable_name,
      metric = user_friendly_variable_name,
      definition = variable_definition,
      year = years,
      updates = periodicity,
      units = format,
      source,
      url,
      citation
    ) %>% 
      mutate(
        updates = updates %>% 
          str_remove('^Every |ly updated$') %>% 
          str_replace('^yearly$', 'annual'),
        warehouse = TRUE
      ) %>% 
      filter(variable_name %in% vars) %>% 
      unique() %>% 
      arrange(variable_name)
}

# Rename relevant local sales variables according to our naming scheme
rename_local_sales_vars <- function(df) {
  df %>% 
    mutate(
      variable_name = case_when(
        str_detect(variable_name, '^agri') ~ 'agTourSalesPerc',
        str_detect(variable_name, '^d2c') ~ 'd2cSalesPerc',
        str_detect(variable_name, '^local') ~ 'localSalesPerc',
        str_detect(variable_name, '^number_csa') ~ 'nCSA',
        str_detect(variable_name, '^number_far') ~ 'nFarmersMarket',
        str_detect(variable_name, '^number_on') ~ 'nOnFarmMarket',
        variable_name == 'valueadded_farms' ~ 'nValueAddedFarms',
        variable_name == 'valueadded_farms_pct' ~ 'valueAddedFarmsPerc',
        variable_name == 'valueadded_sales' ~ 'valueAddedSales',
        variable_name == 'valueadded_sales_pct' ~ 'valueAddedSalesPerc'
      )
    )
}

# Rename access vars
rename_access_vars <- function(df) {
  df %>% 
    mutate(
      variable_name = case_when(
        variable_name == 'child_food_insecurity_rate' ~ 'foodInsecChild',
        variable_name == 'groc' ~ 'nGroc',
        variable_name == 'grocpth' ~ 'nGrocPTH',
        variable_name == 'overall_food_insecurity_rate' ~ 'foodInsecOverall',
        variable_name == 'snap_all_eligible_people_percent' ~ 'snapPercPart',
        variable_name == 'snapspth' ~ 'nSnapGrocPTH',
        variable_name == 'specspth' ~ 'nSpecGrocPTH',
        variable_name == 'wic_coverage_rate' ~ 'wicPercPart',
        variable_name == 'wic_eligibility_rate' ~ 'wicPercEligible',
        variable_name == 'wicspth' ~ 'nWicsGrocPTH',
      )
    )
}

# Rename infra vars
rename_infra_vars <- function(df) {
  df %>% 
    mutate(
      variable_name = case_when(
        variable_name == 'anaerobic_digestion_facilities' ~ 'nAnaerDigestion',
        variable_name == 'number_food_hubs' ~ 'nFoodHubs',
        variable_name == 'number_composting_facilities' ~ 'nCompost',
        variable_name == 'number_meat_processing' ~ 'nMeatProcess',
        variable_name == 'private_semi_private_refrigerated_warehouses' ~ 'nPrivFridge',
        variable_name == 'public_refrigerated_warehouses' ~ 'nPubFridge'
      )
    )
}

rename_labor_vars <- function(df) {
  df %>% 
    mutate(
      variable_name = case_when(
        str_detect(variable_name, 'median_earnings_for_male_food') ~ 'medianEarnMaleFood',
        str_detect(variable_name, 'median_earnings_for_female_food') ~ 'medianEarnFemaleFood',
        str_detect(variable_name, 'women\'s.*food') ~ 'womenEarnPercMaleFood',
        str_detect(variable_name, 'median_earnings_for_male_farm') ~ 'medianEarnMaleFarm',
        str_detect(variable_name, 'median_earnings_for_female_farm') ~ 'medianEarnFemaleFarm',
        str_detect(variable_name, 'women\'s.*farming') ~ 'womenEarnPercMaleFarm',
        
        # NAICS variables
        str_detect(variable_name, 'naics') ~ case_when(
          str_detect(variable_name, 'annual_avg_wkly_wage') ~ paste0(
            'avgWeekWageNAICS',
            str_extract(variable_name, '[0-9].*')
          ),
          str_detect(variable_name, 'total_annual') ~ paste0(
            'annualWageNaics',
            str_extract(variable_name, '[0-9].*')
          ),
          str_detect(variable_name, '^annual_avg_estabs') ~ paste0(
            'nAvgEstabsNaics',
            str_extract(variable_name, '[0-9].*')
          ),
          str_detect(variable_name, '^annual_avg_emplvl') ~ paste0(
            'avgEmpLvl',
            str_extract(variable_name, '[0-9].*')
          ),
          str_detect(variable_name, '^lq') ~ paste0(
            'lq',
            str_extract(variable_name, '[0-9].*')
          )
        ) %>% 
          snakecase::to_lower_camel_case()
      )
    )
}
