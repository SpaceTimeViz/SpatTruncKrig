library(testthat)

if (!testthat:::on_cran()) {
  library(SpatTruncKrig)
  test_check("SpatTruncKrig")
}
