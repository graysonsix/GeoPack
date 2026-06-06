#' Analyse the Relationship Between Democracy, Unemployment, and GDP
#'
#' Fits a linear regression of log GDP on unemployment rate, democracy status,
#' their interaction, and year. Returns a tidy coefficient table with
#' 95% confidence intervals. The year 2020 is excluded by default due to
#' COVID-19 economic disruptions.
#'
#' @param data A data frame returned by load_data().
#' @param start_year The first year to include. Default is 1990.
#' @param end_year The last year to include. Default is 2019.
#' @param exclude_2020 Logical. If TRUE (default), the year 2020 is excluded.
#'
#' @return A tibble with columns term, estimate, std.error, statistic,
#'   p.value, conf.low, and conf.high.
#'
#' @importFrom dplyr filter
#' @importFrom broom tidy
#' @export
analyse_gdp_democracy <- function(data,
                                  start_year = 1990,
                                  end_year   = 2019,
                                  exclude_2020 = TRUE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  model_data <- data |>
    dplyr::filter(
      year >= start_year,
      year <= end_year,
      !is.na(gdp_nominal),
      gdp_nominal > 0,
      !is.na(unemployment_rate),
      !is.na(is_democracy)
    )

  if (exclude_2020) {
    model_data <- dplyr::filter(model_data, year != 2020)
  }

  if (nrow(model_data) < 10) {
    stop("Fewer than 10 complete observations in the specified year range.")
  }

  model <- lm(
    log(gdp_nominal) ~ unemployment_rate * is_democracy + year,
    data = model_data
  )

  broom::tidy(model, conf.int = TRUE)
}
