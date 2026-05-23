#' Load Democracy Data
#'
#' Loads the democracy dataset from the TidyTuesday repository.
#'
#' @return A data frame containing democracy data from 1950-2022.
#' @importFrom readr read_csv
#' @export
load_data <- function(){
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-11-05/democracy_data.csv",
    show_col_types = FALSE
  )
}
