#' Add NHANES data
#' 2024-08-06



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  conflicted,
  RMySQL,
  DBI,
  dbplyr,
  purrr,
  haven,
  stringr
)

# Edit system environment for user or project
usethis::edit_r_environ('user')
usethis::edit_r_environ('project')

# Check username from user environment
readRenviron('~/.Renviron')
Sys.getenv('MYSQL_USER')
Sys.getenv('MYSQL_PASSWORD')
Sys.getenv('MYSQL_HOST')



# Connect -----------------------------------------------------------------


# Connect to NHANES db
con <- dbConnect(
  MySQL(),
  user = Sys.getenv('MYSQL_USER'),
  password = Sys.getenv('MYSQL_PASSWORD'),
  host = Sys.getenv('MYSQL_HOST'),
  dbname = 'CDONOV12_nhanes'
)

summary(con)



# Upload ------------------------------------------------------------------


# Load NHANES datasets
names <- list.files('webdb/datasets/', pattern = '.XPT')
files <- list.files('webdb/datasets/', pattern = '.XPT', full.names = TRUE)
dfs <- map(files, read_xpt) %>% 
  setNames(str_split_i(names, '\\.', 1))
names(dfs)

# Check tables
dbListTables(con)

# Map over list to upload them all
iwalk(dfs, ~ {
  dbWriteTable(
    con,
    value = .x,
    name = .y,
    overwrite = FALSE
  )
})

dbListTables(con)
# Noice



# Explore -----------------------------------------------------------------


bm <- dbReadTable(con, 'P_BMX')
get_str(bm)

# Try joining BMX and DEMO by SEQN
query <- '
SELECT * FROM P_DEMO
INNER JOIN P_BMX ON P_DEMO.SEQN = P_BMX.SEQN;
'

dat <- dbGetQuery(con, query)
get_str(dat)

# RIDAGEYR