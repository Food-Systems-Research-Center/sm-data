# LULC
# 2024-09-13

# Wrangling 2022 VT Open Data Base Land Cover
# https://geodata.vermont.gov/pages/ba998c98930f474c97aaf3bd44f1f694


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  sf,
  stars,
  dplyr,
  mapview,
  raster,
  terra,
  stringr
)



# Load and Wrangle --------------------------------------------------------


raw <- read_stars('1_raw/spatial/Landlandcov_BaseLC2022/Landlandcov_BaseLC2022.tif')

# Explore
raw
st_crs(raw)
plot(raw)



# Raster Package ----------------------------------------------------------


dat <- rast('1_raw/spatial/Landlandcov_BaseLC2022/Landlandcov_BaseLC2022.tif')
# It is too big to run ratify...

# Check categorical outcomes
is.factor(dat)
levels(dat)

# Check
dat
get_str(dat)
plot(dat)

# Explore
res(dat)
dim(dat)
ext(dat)

# Check cells
cells <- dim(dat)[1] * dim(dat)[2]
format(cells, big.mark = ',')
# 159 billion cells

# freq(dat) # DONT RUN THIS - can't count to 159 billion

# Summary. This is what we want. Sampling out of 1,000,000 for sanity
set.seed(42)
sum <- dat %>% 
  summary(size = 1000000) %>% 
  as.data.frame() %>% 
  filter(str_detect(Freq, "NA\'s", negate = TRUE)) %>%
  select(freq = Freq) %>% 
  mutate(
    class = str_split_i(freq, ':', 1) %>% str_trim(),
    freq = str_split_i(freq, ':', 2) %>% str_trim() %>% as.numeric(),
    perc = (freq / sum(freq, na.rm = TRUE) * 100) %>% round(2),
    .keep = 'none'
  ) %>% 
  select(class, freq, perc)
sum
# Railroads, bare soil, and buildings are lumped into Other
# NOTE: Should remove water from calculations
# Also, is "Open" land just everything that isn't trees or development?