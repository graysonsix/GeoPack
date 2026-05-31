#' Filter Data to a Single Country
#'
#' A helper function that filters a data frame to rows matching a given country.
#'
#' @param data A data frame containing a column called country_name.
#' @param country A string with the name of the country to filter to.
#'
#' @return A filtered data frame.
#' @importFrom dplyr filter
filter_country <- function(data, country) {
  if (!country %in% data$country_name) {
    stop(paste0("'", country, "' was not found in the data."))
  }
  dplyr::filter(data, country_name == country)
}
