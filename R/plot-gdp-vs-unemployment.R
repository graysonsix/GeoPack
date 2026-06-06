#' Plot Unemployment Rate by Log GDP
#'
#' Creates a scatterplot of unemployment rate by log GDP, grouped by whether
#' the country is classified as a democracy.
#'
#' @param data A data frame containing democracy and economic data with columns
#'   country_name, year, regime_category_index, log_gdp, and unemployment_rate.
#' @param start_year The first year to include in the plot.
#' @param end_year The last year to include in the plot.
#' @param democracy_only Logical. If TRUE, only democratic observations are included.
#'   If FALSE, both democracies and non-democracies are included.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual labs theme_minimal theme
#' @importFrom dplyr filter mutate
#' @export
plot_gdp_vs_unemployment <- function(data,
                                     start_year = 2000,
                                     end_year = 2020,
                                     democracy_only = FALSE) {

  plot_data <- data |>
    dplyr::filter(
      year >= start_year,
      year <= end_year,
      !is.na(gdp_per_capita_nominal),
      !is.na(unemployment_rate),
      !is.na(is_democracy)
    ) |>
    dplyr::mutate(
      log_gdp = log(gdp_per_capita_nominal),
      democracy_status = dplyr::case_when(
        is_democracy == TRUE ~ "Democracy",
        is_democracy == FALSE ~ "Non-democracy"
      )
    )

  if (democracy_only == TRUE) {
    plot_data <- plot_data |>
      dplyr::filter(democracy_status == "Democracy")
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = log_gdp,
      y = unemployment_rate,
      color = democracy_status
    )
  ) +
    ggplot2::geom_point(alpha = 0.6, size = 2) +
    ggplot2::scale_color_manual(
      values = c(
        "Democracy" = "#2C7BB6",
        "Non-democracy" = "#D7191C"
      )
    ) +
    ggplot2::labs(
      title = "Unemployment Rate by Log GDP per Capita",
      subtitle = paste0(start_year, " to ", end_year),
      x = "Log GDP per Capita",
      y = "Unemployment Rate",
      color = "Regime Type"
    ) +
    ggplot2::theme_minimal()
}
