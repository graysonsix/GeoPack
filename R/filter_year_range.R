#' Filter Data to a Year Range
#'
#' A wrangling helper that subsets the dataset to rows within a given
#' year range and drops rows with missing values in key economic and
#' political columns. Mirrors the filtering pattern used throughout
#' the GeoPack analyses.
#'
#' @param data A data frame returned by load_data().
#' @param start_year The first year to include.
#' @param end_year The last year to include.
#'
#' @return A filtered data frame.
#' @importFrom dplyr filter
#' @export
filter_year_range <- function(data, start_year, end_year) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("`start_year` and `end_year` must be numeric.")
  }
  if (start_year > end_year) {
    stop("`start_year` must be less than or equal to `end_year`.")
  }

  data |>
    dplyr::filter(
      year >= start_year,
      year <= end_year,
      !is.na(gdp_nominal),
      !is.na(unemployment_rate),
      !is.na(is_democracy)
    )
}
