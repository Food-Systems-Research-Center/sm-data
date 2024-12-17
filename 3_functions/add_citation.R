# get citation
# quick function to paste together source with URL to get APA style citation

pacman::p_load(
  dplyr,
  lubridate
)

add_citation <- function(df, 
                         source_col = source, 
                         url_col = url,
                         access_date = NULL) {
  if (is.null(access_date)) {
    date_of_access <- format(Sys.Date(), "%B %d, %Y")
  } else {
    date_of_access <- format(ymd(access_date), "%B %d, %Y")
  }
  
  df %>%
    mutate(citation = paste0(
      {{ source_col }},
      # '. ',
      ' Retrieved from ',
      {{ url_col }},
      ', accessed on ',
      date_of_access,
      '.'
    ))
}

# Make this better
add_citation_2 <- function(df, 
                           source_col = source, 
                           url_col = url,
                           api_url = NULL
                           ) {
  if (is.null(api_url)) {
    out <- df %>%
      mutate(citation = paste0(
        {{ source_col }},
        ', ',
        {{ url_col }},
        ', accessed on ',
        format(Sys.Date(), "%B %d, %Y"),
        '.'
      ))
  } else {
    out <- df %>%
      mutate(citation = paste0(
        {{ source_col }},
        ', <',
        {{ api_url }},
        '>, accessed on ',
        format(Sys.Date(), "%B %d, %Y"),
        '.'
      ))
  }
}
