#' Metadata: Add citation column
#'
#' @description Function to be used in metadata pipeline after metadata df is
#'   created. Pipe it through to add a new citation column which builds on the
#'   source column and either the URL column, or, if the user adds an api_url,
#'   it builds on that.
#'
#' @details Note that we need to add an option for the access date, because if
#'   it is a journal or something, we shouldn't have a date retrieved. But most
#'   data are coming from websites or APIs anyway.
#'
#' @param df A metadata df
#' @param date Date in long format (e.g. June 15, 2025). If none given, will
#'   default to the current day.
#' @param source_col Name of column that contains the source without quotes.
#' @param url_col Name of column with URL without quotes.
#' @param api_url If the API URL is different from the listed source, add it
#'   here and it will be appropriated formatted.
#'
#' @returns The metadata df with a new citation column
#' @import dplyr
#' @keywords internal
meta_citation <- function(df, 
                          date = format(Sys.Date(), "%B %d, %Y"),
                          source_col = source, 
                          url_col = url,
                          api_url = NULL) {
  if (is.null(api_url)) {
    out <- df %>%
      mutate(citation = paste0(
        {{ source_col }},
        ', ',
        {{ url_col }},
        ', accessed on ',
        date,
        '.'
      ))
  } else {
    out <- df %>%
      mutate(citation = paste0(
        {{ source_col }},
        ', <',
        {{ api_url }},
        '>, accessed on ',
        date,
        '.'
      ))
  }
}