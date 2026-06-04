#' Load Democracy Data
#'
#' Loads the democracy dataset from the TidyTuesday repository.
#'
#' @return A data frame containing democracy data from 1950-2022.
#' @importFrom readr read_csv
#' @export
load_data <- function(){
  path <- system.file("extdata", "masterSet.csv", package = "GeoPack")
  data <- read_csv(path)
}
