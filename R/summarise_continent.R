#' Summarise Economic and Political Data by Continent
#'
#' Groups the dataset by continent and returns the number of distinct
#' countries, average GDP growth, average unemployment rate, and the percentage
#' of country-years classified as democratic.
#'
#' @param data A data frame returned by load_data().
#' @param year_range An optional numeric vector of length 2 giving the start
#'   and end years, e.g. c(2000, 2020). If NULL (default) all years are used.
#'
#' @return A tibble with one row per continent sorted by descending mean GDP
#'   growth, with columns continent, n_countries, mean_gdp_growth,
#'   mean_unemployment, and pct_democratic.
#' @importFrom dplyr mutate group_by summarise filter n_distinct desc arrange
#' @importFrom countrycode countrycode
#' @export
summarise_continent <- function(data, year_range = NULL) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  if (!is.null(year_range)) {
    if (length(year_range) != 2 || !is.numeric(year_range)) {
      stop("`year_range` must be a numeric vector of length 2, e.g. c(2000, 2020).")
    }
    if (year_range[1] > year_range[2]) {
      stop("First element of `year_range` must be <= second element.")
    }
    data <- data |>
      dplyr::filter(year >= year_range[1], year <= year_range[2])
  }

  data |>
    dplyr::mutate(
      continent = countrycode::countrycode(
        country_name, "country.name", "continent",
        warn = FALSE
      )
    ) |>
    dplyr::filter(!is.na(continent)) |>
    dplyr::group_by(continent) |>
    dplyr::summarise(
      n_countries       = dplyr::n_distinct(country_name),
      mean_gdp_growth   = round(mean(gdp_growth,       na.rm = TRUE), 2),
      mean_unemployment = round(mean(unemployment_rate, na.rm = TRUE), 2),
      pct_democratic    = round(mean(is_democracy,      na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_gdp_growth))
}
