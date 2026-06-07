#' Load Democracy Data
#'
#' Loads the full merged dataset from the package's inst/ folder.
#' Uses arrow for efficient file reading.
#'
#' @return A data frame containing democracy and economic data from 1950-2022.
#' @importFrom arrow read_csv_arrow
#' @export
load_data <- function() {
  path <- system.file("extdata", "masterSet.csv", package = "GeoPack")
  arrow::read_csv_arrow(path)
}
