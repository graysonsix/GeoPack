#' Plot Regime Changes by Country
#'
#' Creates a horizontal bar chart of the most politically unstable countries.
#'
#' @param data A data frame containing democracy data with columns
#'   country_name, regime_category_index, and year.
#' @param top_n Number of countries to display. Default is 10.
#' @param color_by Column to color bars by. Default is "continent".
#'
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual labs theme_minimal theme element_text element_blank
#' @importFrom dplyr arrange mutate group_by summarise slice_max
#' @importFrom forcats fct_reorder
#' @importFrom countrycode countrycode
#' @export
plot_regime_changes <- function(data, top_n = 10, color_by = "continent") {


  # validate inputs
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.numeric(top_n) || top_n < 1) stop("`top_n` must be a positive number.")

  continent_cols <- c(
    "Africa"   = "#E63946",
    "Americas" = "#F4A261",
    "Asia"     = "#2A9D8F",
    "Europe"   = "#457B9D",
    "Oceania"  = "#9B5DE5"
  )

  plot_data <- data |>
    dplyr::arrange(country_name, year) |>
    dplyr::group_by(country_name) |>
    dplyr::summarise(
      n_changes = sum(regime_category_index != dplyr::lag(regime_category_index), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::slice_max(n_changes, n = top_n, with_ties = FALSE) |>
    dplyr::mutate(
      continent    = countrycode::countrycode(country_name, "country.name", "continent"),
      country_name = forcats::fct_reorder(country_name, n_changes),
      continent    = factor(continent, levels = unique(continent))
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(
    x    = n_changes,
    y    = country_name,
    fill = continent
  )) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::scale_fill_manual(values = continent_cols, na.value = "gray70") +
    ggplot2::labs(
      title    = paste("Top", top_n, "Most Politically Unstable Countries"),
      subtitle = "Total regime changes, 1950-2022",
      x        = "Number of Regime Changes",
      y        = NULL,
      fill     = "Continent"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title          = ggplot2::element_text(face = "bold", size = 16),
      panel.grid.major.y  = ggplot2::element_blank(),
      panel.grid.minor    = ggplot2::element_blank()
    )
}
