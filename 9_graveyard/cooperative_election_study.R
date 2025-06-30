# Cooperative Election Study
# https://cces.gov.harvard.edu/

# Has tons of questions about voting, civic participation, government


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr
)

dat <- read.table(
  '1_raw/temp/CCES23_Common_OUTPUT.tab',
    header = TRUE, 
    sep = "\t"
)

dat <- read.csv('1_raw/temp/CCES22_Common_OUTPUT_vv_topost.csv')
fips_key <- readRDS('5_objects/fips_key.rds')



# Explore -----------------------------------------------------------------


dim(dat)
get_str(dat)
# 60,000 responses

# Check NE states
ne <- dat %>% 
  filter(inputstate %in% c(9, fips_key$fips))
dim(ne)
# 2611 in New England

# Check by State
ne %>% 
  group_by(inputstate) %>% 
  summarize(count = n())
# Not half bad

# Relevant questions:
#   CC22_423 How much do you trust the federal gov in handling nations problems
#   CC22_424 How much do you trust the state gov in handling nations problems
# Really only second one would be relevant 

# Pull those questions
ne <- ne %>% 
  select(inputstate, CC22_423, CC22_424)
get_str(ne)

# Check full set for coding error??
dat$CC22_423 %>% get_table()
# So 8 is fucking 4 for some reason. I hate this

# Check those two questions
ne$CC22_423 %>% get_table()
ne$CC22_424 %>% get_table()
