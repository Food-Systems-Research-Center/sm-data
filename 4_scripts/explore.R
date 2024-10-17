#' Explore
#' 2024-09-20


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  mapview,
  leaflet,
  # leafpop,
  viridisLite,
  RColorBrewer,
  stringr,
  sf,
  rlang,
  tidyr,
  ggplot2,
  plotly
)

source('3_functions/data_pipeline_functions.R')
source('3_functions/read_all_rds.R')

# Aggregated dataset by county
agg <- readRDS('2_clean/aggregated_metrics.rds')

# Metadata
meta <- readRDS('2_clean/aggregated_meta.rds')

# Spatial objects and references
counties <- read_all_rds('2_clean/spatial/', pattern = '^ne_counties_')
fips <- read_all_rds(path = '5_objects/', pattern = '^fips')
states <- read_rds('5_objects/ne_state_codes.rds')



# Check Files -------------------------------------------------------------


# See how many metrics they each list
get_str(agg)
agg_metrics <- agg$variable_name %>% 
  unique
(agg_length <- length(agg_metrics))

meta_metrics <- meta$variable_name %>% 
  unique
(meta_length <- length(meta_metrics))

agg_length == meta_length



# Test Colors -------------------------------------------------------------


brewer.pal.info
display.brewer.all()
brewer.pal(9, 'YlOrRd')


# Regression --------------------------------------------------------------


get_str(agg)
vars <- agg$variable_name %>% 
  unique %>% 
  sort
vars

# Try whole thing
# Dependent will be child_food_insecurity_rate
# Independent: wicspth, snapspth, wicspth, specspth, grocpth
vars <- str_subset(vars, 'pth$|^child_')
test <- agg %>% 
  filter(variable_name %in% vars) %>% 
  filter_to_counties() %>% 
  get_latest_year() %>% 
  make_wider()
get_str(test)

m1 <- lm(child_food_insecurity_rate_2021 ~ grocpth_2016 + specspth_2016 + 
           snapspth_2017 + wicspth_2016,
         data = test)
summary(m1)
DescTools::VIF(m1)
DHARMa::plotSimulatedResiduals(m1)
DHARMa::simulateResiduals(m1, plot = TRUE)
# This is rad as fuck



# Time Series Graph -------------------------------------------------------


get_str(agg)
# try it with child food insecurity
dat <- agg %>% 
  filter(str_detect(variable_name, '^child_'))
get_str(dat)
dat$year %>% unique

# Plot all years of food insecurity
plot <- dat %>% 
  mutate(
    across(c(year:value), as.numeric),
    state = str_sub(fips, end = 2)
  ) %>% 
  left_join(fips$fips_key, by = 'fips') %>% 
  ggplot(aes(
    x = year, 
    y = value, 
    group = fips, 
    color = state_name,
    text = paste0(
      'State: ', state_name, '\n',
      'County: ', county_name, '\n',
      'CFIR: ', round(value, 3)
    )
  )) +
  geom_line(
    lwd = 1.25,
    alpha = 0.6
  ) +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  geom_vline(
    xintercept = 2020,
    lwd = 1.25
  ) +
  annotate(
    'text',
    x = 2019.5,
    y = 0.28,
    label = 'COVID',
    col = 'black',
    fontface = 2
  ) +
  labs(
    x = 'Year',
    y = 'Child Food Insecurity Rate',
    color = 'State',
    title = 'Child Food Insecurity Rate by County, 2011-2021'
  ) +
  scale_color_manual(values = brewer.pal(6, 'Dark2'))
plot
# Pretty neat

# Test plotly
test <- ggplotly(plot, tooltip = 'text')
test
# Looks great



# Time Series Vars --------------------------------------------------------


get_str(agg)

agg %>% 
  filter(str_detect(variable_name, 'wic_coverage_rate')) %>% 
  pull(year) %>% 
  unique
agg %>% 
  filter(str_detect(variable_name, 'wic_eligib')) %>% 
  pull(year) %>% 
  unique
agg %>% 
  filter(str_detect(variable_name, 'median_earnings')) %>% 
  pull(year) %>% 
  unique
