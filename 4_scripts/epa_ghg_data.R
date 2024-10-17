# EPA GHG data
# 2024-09-10

# https://cfpub.epa.gov/ghgdata/inventoryexplorer/


# Houskeeping -------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  rvest
)



# Explore -----------------------------------------------------------------


# Example:
# https://cfpub.epa.gov/ghgdata/inventoryexplorer/#agriculture/entiresector/allgas/category/all
full_url <- 'https://cfpub.epa.gov/ghgdata/inventoryexplorer/#agriculture/entiresector/allgas/category/all'
base_url <- 'https://cfpub.epa.gov/ghgdata/inventoryexplorer'

out <- GET(full_url) %>% 
  content('text') %>% 
  read_html()

out
get_str(out)

html_text(out$node)

## another workflow
link <- 'https://cfpub.epa.gov/ghgdata/inventoryexplorer/#agriculture/entiresector/allgas/category/all'
page <- read_html(link)
table <- html_element(page, "table.sortable") %>%
  html_table()


## And another
page <- read_html(link)
tables <- html_elements(page, 'table')
html_table(tables[2])
# Booo

