test_that("summarise_country returns a data frame", {
  masterSet <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-11-05/democracy_data.csv",
    show_col_types = FALSE
  )

  result <- summarise_country(masterSet, "Thailand")

  expect_s3_class(result, "data.frame")
  expect_true("year" %in% names(result))
  expect_true("regime_category" %in% names(result))
})

test_that("summarise_country errors on bad input", {
  masterSet <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-11-05/democracy_data.csv",
    show_col_types = FALSE
  )

  expect_error(summarise_country(masterSet, "Fake Country Name"))
  expect_error(summarise_country("not a data frame", "Thailand"))
})
