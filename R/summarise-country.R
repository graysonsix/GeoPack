#' Summarise Economic and Political Data for a Country
#'
#' Returns a summary table of GDP growth, unemployment, and regime changes
#' for a given country over time.
#'
#' @param data A data frame containing country data with columns
#'   country_name, year, and regime_category.
#' @param country A string with the name of the country to summarise.
#'
#' @return A tibble with yearly economic and political data for the country.
#' @importFrom dplyr filter select arrange any_of
#' @export
summarise_country <- function(data, country) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.character(country)) stop("`country` must be a string.")

  result <- filter_country(data, country)

  result |>
    dplyr::select(dplyr::any_of(c("year", "gdp_growth", "unemployment_rate", "regime_category"))) |>
    dplyr::arrange(-year)
}
