# Sm Repo Test
# 2025-01-26


# Description -------------------------------------------------------------


# Testing out how we can load sm metrics into database


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  conflicted,
  RMariaDB,
  DBI,
  dbplyr,
  purrr,
  furrr,
  parallelly
)

# Edit system environment for user or project
# usethis::edit_r_environ('user')
# usethis::edit_r_environ('project')

# Check username from user environment
readRenviron('.Renviron')
# Sys.getenv('WEBDB_CDONOV12_ADMIN')
# Sys.getenv('WEBDB_CDONOV12_ADMIN_PW')
# Sys.getenv('WEBDB_HOST')

con <- dbConnect(
  RMariaDB::MariaDB(),
  user = Sys.getenv('WEBDB_CDONOV12_ADMIN'),
  password = Sys.getenv('WEBDB_CDONOV12_ADMIN_PW'),
  host = Sys.getenv('WEBDB_HOST'),
  dbname = 'CDONOV12_sm_repo'
)



# Explore -----------------------------------------------------------------


# Best practice: password = rstudioapi::askForPassword()
# To be used interactively
summary(con)

dbListTables(con)
dbListFields(con, 'test_table')
dbListFields(con, 'metrics')

query <- 'SELECT * FROM metrics LIMIT 10'
result <- dbGetQuery(con, query)


# Test uploading df -------------------------------------------------------


# Load all output data
sm_data <- readRDS('6_outputs/sm_data.rds')
names(sm_data)

# Pull out just metrics data
metrics <- sm_data$metrics
get_str(metrics)

# Put year and value into numeric. Also make it small to test
metrics_df <- metrics %>% 
  mutate(across(c(year, value), as.numeric)) %>% 
  slice_sample(n = 1000)
get_str(metrics_df)

# Both create table and upload df to table
dbWriteTable(
  con, 
  name = "metrics", 
  value = metrics_df, 
  overwrite = TRUE
)

# Check it
dbListFields(con, 'metrics')
dbGetQuery(con, 'describe metrics')
dbGetQuery(con, 'select * from metrics limit 10')
dbGetQuery(con, 'select count(*) from metrics')
# Works!

# Remove table
dbRemoveTable(con, 'metrics')
dbListTables(con)



# Upload all metrics ------------------------------------------------------


# Load all output data
sm_data <- readRDS('6_outputs/sm_data.rds')
metrics_full <- sm_data$metrics %>% 
  mutate(across(c(year, value), as.numeric)) %>% 
  filter(!is.na(fips))
  # slice_sample(n = 25000)
get_str(metrics_full)

# Save it as csv to test in vscode
readr::write_csv(metrics_full, 'webdb/metrics.csv')

# Check the table 
dbGetQuery(con, 'describe metrics')

# Load in chunks
chunk_size <- 10000
num_chunks <- ceiling(nrow(metrics_full) / chunk_size)

get_time()
for (i in seq_len(num_chunks)) {
  start <- (i - 1) * chunk_size + 1
  end <- min(i * chunk_size, nrow(data))
  chunk <- metrics_full[start:end, ]
  
  # Append each chunk
  dbWriteTable(con, "metrics", chunk, append = TRUE, row.names = FALSE)
  
  # Print where we're at
  get_time(c('Finished chunk ', i, ' at'))
}

# Check
dbListTables(con)
dbGetQuery(con, 'select count(*) from metrics')
dbGetQuery(con, 'select * from metrics limit 10')


# Load all 500k rows
dbWriteTable(
  con, 
  name = "metrics", 
  value = metrics_full
  # overwrite = TRUE
)



# Furrr -------------------------------------------------------------------


# Load all output data
sm_data <- readRDS('6_outputs/sm_data.rds')
set.seed(42)
metrics_full <- sm_data$metrics %>% 
  mutate(across(c(year, value), as.numeric)) %>% 
  filter(!is.na(fips)) %>% 
  unique()
get_str(metrics_full)

# Check for dupes
unique_check <- !duplicated(metrics_full[c("fips", "year", "variable_name")])
non_unique_rows <- metrics_full[!unique_check, ]
get_str(non_unique_rows)
non_unique_rows$variable_name %>% unique
# Problem is only waterIrrSrcOffFarmNOpsWithExp?
# nAnaerDigestion also

# Filter down metrics full to ditch that
metrics_full <- metrics_full %>% 
  filter(! variable_name %in% c('waterIrrSrcOffFarmNOpsWithExp', 'nAnaerDigestion'))
dim(metrics_full)
get_str(metrics_full)
    
# Reset table
dbRemoveTable(con, 'metrics')
dbExecute(
  con, 
  'create table metrics (
    fips varchar(5),
    year int,
    variable_name varchar(255),
    value double,
    primary key (fips, year, variable_name)
  )'
)

# Check table
dbListTables(con)
dbGetQuery(con, 'describe metrics')
dbGetQuery(con, 'select count(*) from metrics')

# Set up variables
chunk_size <- 10000
n_chunks <- ceiling(nrow(metrics_full) / chunk_size)
# config <- furrr_options(
#   seed = TRUE,
#   globals = TRUE,
#   packages = c('DBI', 'RMariaDB', 'projecter')
# )

# Set up parallel
# cores <- availableCores()
# plan(multisession, workers = cores - 2)

get_time()
walk(1:n_chunks, ~ {
  # Define start and end rows
  start <- (.x - 1) * chunk_size + 1
  end <- min(.x * chunk_size, nrow(metrics_full))
  chunk <- metrics_full[start:end, ]
  
  # Append each chunk
  dbWriteTable(con, "metrics", chunk, append = TRUE, row.names = FALSE)
  
  # Print where we're at
  get_time(c('Finished chunk ', .x, ' at'))
}, .progress = TRUE)

# plan(sequential)

# Check table
dbGetQuery(con, 'describe metrics')
dbGetQuery(con, 'select count(*) from metrics')
test <- dbGetQuery(con, 'select * from metrics')
test <- dbGetQuery(con, 'select value from metrics limit 100')
get_str(test)
# works fine without future

