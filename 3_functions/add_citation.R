# get citation
# quick function to paste together source with URL to get APA style citation

add_citation <- function(df, source_col = source, url_col = url) {
  df %>%
    mutate(citation = paste0(
      {{ source_col }},
      ' Retrieved from ',
      {{ url_col }}
    ))
}
