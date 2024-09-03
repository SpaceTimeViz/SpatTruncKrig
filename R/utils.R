
#' Matern Covariance Function
#'
#' @param locs A matrix of locations (grid points).
#' @param rho The range parameter.
#' @param sigma2 The variance parameter.
#' @param nu The smoothness parameter.
#' @return A covariance matrix.
#' @export
matern_cov <- function(locs, rho = 0.2, sigma2 = 1, nu = 1.5) {
  dist_matrix <- as.matrix(dist(locs))
  cov_matrix <- sigma2 * (1 + sqrt(3) * dist_matrix / rho) * exp(-sqrt(3) * dist_matrix / rho)
  return(cov_matrix)
}
