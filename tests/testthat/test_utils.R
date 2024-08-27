library(testthat)
library(ggplot2)

test_that("plot_truncated_normal produces a ggplot object", {
  p1 <- plot_truncated_normal(cov = 0.5, lower = c(-1, -1), upper = c(1, 1), y = 0.5, plot_type = "scatter")
  p2 <- plot_truncated_normal(cov = 0.5, lower = c(-1, -1), upper = c(1, 1), y = 0.5, plot_type = "hist")
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_truncated_normal handles invalid input gracefully", {
  expect_error(
    plot_truncated_normal(cov = "invalid", lower = c(-1, -1), upper = c(1, 1), y = 0.5, plot_type = "scatter"),
    "Covariance must be a numeric value."
  )
})
