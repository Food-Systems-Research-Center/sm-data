# Explore GHGs
# 2024-09-10

# Poking around with EPA GHG explorer
# https://cfpub.epa.gov/ghgdata/inventoryexplorer/index.html#iagriculture/entiresector/allgas/category/all


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  ggplot2,
  tidyr
)

raw <- read.csv('1_raw/epa_ghg.csv')


# Wrangle -----------------------------------------------------------------


get_str(raw)
# Need to make this long format

dat <- raw %>% 
  pivot_longer(cols = 2:ncol(dat), names_to = 'year', values_to = 'ghgs') %>% 
  rename(state = 1) %>% 
  mutate(year = as.numeric(str_remove(year, 'X')))

get_str(dat)



# Graph -------------------------------------------------------------------


dat %>% 
  ggplot(aes(x = year, y = ghgs, color = state)) +
  geom_line() +
  theme_bw() +
  labs(
    x = 'Year',
    y = 'GHGs',
    color = 'State',
    title = 'Agricultural Emissions by State, 1990-2020'
  )
