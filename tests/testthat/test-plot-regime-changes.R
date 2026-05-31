test_that("plot_regime_changes returns a ggplot", {
  dat <- load_data()

  plot_default <- plot_regime_changes(dat)
  plot_top5    <- plot_regime_changes(dat, top_n = 5)

  expect_s3_class(plot_default, "ggplot")
  expect_s3_class(plot_top5,    "ggplot")
})

test_that("plot_regime_changes errors on bad input", {
  expect_error(plot_regime_changes("not a data frame"))
  expect_error(plot_regime_changes(load_data(), top_n = -1))
})
