#' Testing out WebDB MySQL database
#' 2024-08-02



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  conflicted,
  RMySQL,
  DBI,
  dbplyr,
  purrr
)

# Edit system environment for user or project
# usethis::edit_r_environ('user')
# usethis::edit_r_environ('project')

# Check username from user environment
readRenviron('~/.Renviron')
# Sys.getenv('MYSQL_USER')
# Sys.getenv('MYSQL_PASSWORD')
# Sys.getenv('MYSQL_HOST')



# OFRN DB on FSRC Silk Test -----------------------------------------------


# Sys.getenv('fsrc_reader')
# Sys.getenv('fsrc_reader_pass')
# Sys.getenv('webdb_host')

con <- dbConnect(
  MySQL(),
  user = Sys.getenv('fsrc_reader'),
  password = Sys.getenv('fsrc_reader_pass'),
  host = Sys.getenv('webdb_host'),
  dbname = 'FSRC_OFRN'
)

summary(con)
dbListTables(con)
# This works



# Create New DB -----------------------------------------------------------


con <- dbConnect(
  MySQL(),
  user = Sys.getenv('MYSQL_USER'),
  password = Sys.getenv('MYSQL_PASSWORD'),
  host = Sys.getenv('MYSQL_HOST')
)

summary(con)
# Connected, but not to a specific database

dbListObjects(con)
# Cannot check objects without being in a database

# Create new database
dbSendQuery(con, 'CREATE DATABASE nhanes')
# No luck here - permissions. Have to do it in workbench or command line?



# Connect to Test DB ------------------------------------------------------


con <- dbConnect(
  MySQL(),
  user = Sys.getenv('MYSQL_USER'),
  password = Sys.getenv('MYSQL_PASSWORD'),
  host = Sys.getenv('MYSQL_HOST'),
  dbname = 'CDONOV12_test'
)

# Best practice: password = rstudioapi::askForPassword()
# To be used interactively
summary(con)

# Check open connections
dbListConnections(MySQL())


# Exploring Existing Table ------------------------------------------------


# Check objects and tables in database
dbListObjects(con)
dbListTables(con)

# Check fields in specific table
dbListFields(con, 'iris_test')



# Read and Query ----------------------------------------------------------


# Read whole table
iris_df <- dbReadTable(con, 'iris_test')

# Send query
query <- 
'
SELECT * FROM iris_test
WHERE sepal_length > 5
'

results <- dbSendQuery(con, query)
db_results <- dbFetch(results)
db_results

# Note that results have to be cleared
dbClearResult(results)
rm(db_results)



# Better Querying ---------------------------------------------------------


query <- 'SELECT * FROM iris_test'
result <- dbGetQuery(con, query)
result
# This is better, combines steps, and doesn't make you clear the result



# Create Table Manually ---------------------------------------------------


# Check if mtcars table already exists.
dbListObjects(con)
dbListTables(con)

# Manual query to create a new table
query <- 
'
CREATE TABLE manual_table (
  mpg NUMERIC,
  cyl INTEGER,
  disp INTEGER,
  hp INTEGER
)
'

# Send query to make table
dbSendQuery(con, query)

# Check it
dbListTables(con)
# Noice

# See what is there
dbReadTable(con, 'test_mtcars')
# It is empty
# Add data from here if desired

# Delete it
dbSendQuery(con, 'DROP TABLE manual_table')
dbListTables(con)
# Noice



# Send DF to Table --------------------------------------------------------


# Now just upload the DF
dbListTables(con)
dbWriteTable(con, 
             name = "mtcars", 
             value = mtcars, 
             overwrite = TRUE)

# Check
dbListTables(con)
query <- 'SELECT * FROM mtcars;'
result <- dbGetQuery(con, query)

# Or just read whole table
result2 <- dbReadTable(con, 'mtcars')

# Are they the same
identical(result, result2)
# Nope

# Check
result
result2
# Using dbReadTable gave us row names, while dbGetQuery made names a column

dbRemoveTable(con, 'mtcars')



# dbplyr ------------------------------------------------------------------


# Check tables
dbListTables(con)

# Copy mtcars into db with dplyr
dplyr::copy_to(
  dest = con, 
  df = mtcars,
  name = 'mtcars'
)
dbListTables(con)
# Hmm this doesn't work. It exists but is empty

# Get rid of it and move on with the old way
dbRemoveTable(con, 'mtcars')

# Old way
dbWriteTable(con, 
             name = "mtcars", 
             value = mtcars, 
             overwrite = TRUE)
dbListTables(con)

# Pull a tibble version to work with
dat <- dplyr::tbl(con, 'mtcars')
dat
class(dat)
# tbl_mysql connection, dbi, sql, lazy tibble

# Query with dplyr
dat %>% 
  select(mpg, cyl, hp) %>% 
  filter(hp > 200)
# Note that $ extraction does not work
# Neither does names(). Have to use colnames()

# Get SQL translation
query <- mtcars_tbl %>% 
  select(mpg, cyl, hp) %>% 
  filter(hp > 200)

show_query(query)
explain(query)
# This is very cool

dbListTables(con)
# Keep working with mtcars



# Alter Table -------------------------------------------------------------


# Check out columns of mtcars
dbExecute(con, 'statement') # Try this out



# Remove Table ------------------------------------------------------------


dbRemoveTable(con, 'test_mtcars_upload')



# Disconnect --------------------------------------------------------------


# Check open connections
dbListConnections(MySQL())

# Disconnect from current
dbDisconnect(con)

# Try closing the first one
dbDisconnect(dbListConnections(MySQL())[[1]])
dbListConnections(MySQL())

# Loop through and disconnect from all
map(dbListConnections(MySQL()), dbDisconnect)
dbListConnections(MySQL())
# Noice

