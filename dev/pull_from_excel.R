# Pull from metrics excel on OneDrive
# 2025-06-16


# Description -------------------------------------------------------------


# Pulling working copy of metrics from excel on OneDrive



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  readxl,
  openxlsx2,
  tidyr
)



# Pull Excel --------------------------------------------------------------


# Copy it locally
path <- 'C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Metrics/secondary_metrics.xlsx'
new_xl <- '2_clean/secondary_metrics.xlsx'
file.copy(path, new_xl, overwrite = TRUE)

# Pull the working excel from OneDrive to yoink variable names and figure out
# what we need to do
sheets <- excel_sheets(new_xl)[1:5]
tab <- map(sheets, ~ {
  read_excel('2_clean/secondary_metrics.xlsx', sheet = .x) %>% 
    mutate(dimension = str_to_lower(.x), .before = 'index') %>% 
    fill(c(index, indicator), .direction = 'down')
}) %>% 
  bind_rows()
get_str(tab)



# Get Summary -------------------------------------------------------------


# Summary table showing how states, counties, and years are represented by 
# each metric

# Get existing variable names and metrics
vars <- tab %>% 
  filter(variable_name != 'NONE' & !is.na(variable_name)) %>% 
  select(metric, variable_name)

# Pull them from our metrics and see which ones are fucked
existing <- metrics %>% 
  filter(variable_name %in% vars$variable_name) %>% 
  filter_fips('ne')
get_str(existing)

# Get summary table showing coverage
sum <- tab %>% 
  select(variable_name, source) %>% 
  right_join(existing) %>% 
  group_by(variable_name) %>%
  summarize(
    n_states = length(unique(fips[nchar(fips) == 2])),
    n_counties = length(unique(fips[nchar(fips) == 5])),
    n_years = length(unique(sort(year))),
    latest_year = max(unique(year)),
    source = unique(source)
    # years = paste0(unique(sort(year)), collapse = ', ')
  )
sum



# Save to excel -----------------------------------------------------------


# Join it back with the rest of the framework for viewing
out <- tab %>% 
  select(dimension, index, indicator, metric, variable_name) %>% 
  right_join(sum)
get_str(out)
out
  
# Save this summary of what we have back to excel in OneDrive. Save with date
new_path <- paste0(
  'C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Metrics/',
  Sys.Date(),
  '_metric_summary.xlsx'
)

# Second sheet for column info
info <- data.frame(
  cols = c(
    'variable_name', 
    'n_states',
    'n_counties',
    'n_years', 
    'source',
    '(other)'
  ),
  definitions = c(
    'used in code only',
    'number of states represented by metric (total 9 in Northeast)',
    'number of counties represented by metric. Total is technically 218, but 226 or 217 may also be complete because of issues with Connecticut',
    'number of years represented by metric',
    'source',
    paste(
      'Note that this workbook will be overwritten periodically, so please don\'t edit anything here.',
      'Or at least be okay with it getting erased.'
    )
  )
)
sheet_names <- list('metric_summary' = out, 'info' = info)
openxlsx2::write_xlsx(
  sheet_names, 
  new_path, 
  widths = 'auto', 
  na.strings = 'NA'
)



# Fix refined tree --------------------------------------------------------


# # Using the working copy here to fix our old refined tree
# new <- tab
# get_str(new)
# data(refined_tree)
# old <- refined_tree
# get_str(old)
# 
# # Check unique indicators in each
# new_inds <- sort(unique(new$indicator))
# old_inds <- sort(unique(old$indicator))
# setdiff(new_inds, old_inds)
# setdiff(old_inds, new_inds)
# 
# # Couple of small patches so that names align between them
# old <- old %>% 
#   mutate(indicator = case_when(
#     indicator == 'happiness index' ~ 'happiness tbd',
#     .default = indicator
#   ))
# 
# # Shorten a name or two from "new" set
# new <- new %>% 
#   mutate(indicator = case_when(
#     str_detect(indicator, 'production in') ~ 'production inputs',
#     indicator == 'happiness index' ~ 'happiness tbd',
#     .default = indicator
#   ))
# 
# new_inds <- sort(unique(new$indicator))
# old_inds <- sort(unique(old$indicator))
# additions <- setdiff(new_inds, old_inds)
# setdiff(old_inds, new_inds)
# 
# # Take "new", add an x to use column, take relevant variables.
# # Not adding new metrics or varnames, just indicators if they were missing
# new_adds <- new %>% 
#   select(dimension, index, indicator, metric) %>% 
#   filter(indicator %in% additions) %>% 
#   mutate(use = 'x', metric = 'NONE') %>% 
#   distinct() %>% 
#   mutate(variable_name = NA)
# get_str(new_adds)
# 
# # Combine the new additions to old to get fixed tree
# fixed <- old %>% 
#   select(dimension, index, indicator, metric, variable_name, use) %>% 
#   bind_rows(new_adds) %>% 
#   filter(use == 'x') %>% 
#   select(-use)
# get_str(fixed)
# 
# # Some ad-hoc fixes
# fixed <- fixed %>% 
#   mutate(
#     indicator = case_when(
#       str_detect(indicator, 'percentage of operating expenses') ~ 'NONE',
#       .default = indicator
#     ),
#     variable_name = case_when(
#       is.na(variable_name) ~ 'NONE',
#       .default = variable_name
#     ),
#     metric = case_when(
#       is.na(metric) ~ 'NONE',
#       str_detect(metric, 'NONE') ~ 'NONE',
#       .default = metric
#     )
#   )
# get_str(fixed)
# 
# # write.csv(fixed, '2_clean/trees/fixed_tree.csv')
