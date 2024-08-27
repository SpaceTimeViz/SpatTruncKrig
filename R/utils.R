#' Plot Truncated Normal Distribution
#'
#' This function generates plots for a truncated bivariate normal distribution.
#' It can either create a scatter plot of the generated data or a histogram of
#' one of the variables with its corresponding density curve.
#'
#' @param cov Numeric. The covariance between the two variables.
#' @param lower Numeric vector of length 2. The lower bounds for the truncation.
#' @param upper Numeric vector of length 2. The upper bounds for the truncation.
#' @param y Numeric. The upper limit for the y-axis in the histogram plot.
#' @param plot_type Character. The type of plot to generate. Either "scatter" for
#'   a scatter plot of the truncated data or "hist" for a histogram with a density curve.
#'
#' @details The function generates data from a truncated bivariate normal distribution
#'   using the `rtmvnorm` function from the `tmvtnorm` package. It then plots the data
#'   based on the specified plot type using `ggplot2`.
#'
#' @examples
#' plot_truncated_normal(cov = 0.5, lower = c(-2, -2), upper = c(2, 2), y = 0.5, plot_type = "scatter")
#' plot_truncated_normal(cov = 0.3, lower = c(-1, -1), upper = c(1, 1), y = 0.5, plot_type = "hist")
#'
#' @importFrom tmvtnorm rtmvnorm
#' @importFrom truncnorm dtruncnorm
#' @importFrom ggplot2 ggplot geom_point geom_histogram geom_line labs theme_minimal
#' @export
#' @importFrom graphics par
#' @importFrom ggplot2 aes theme_bw theme element_text unit ylim guides guide_legend
plot_truncated_normal <- function(cov, lower, upper, y, plot_type = "scatter") {
  if (!is.numeric(cov) || length(cov) != 1) {
    stop("Covariance must be a numeric value.")
  }
  
  if (det(matrix(c(1, cov, cov, 1), ncol = 2)) <= 0) {
    stop("Covariance matrix must be positive definite.")
  }
  # Calculate the determinant
  det_value <- det(matrix(c(1, cov, cov, 1), ncol = 2))
  
  # Check if determinant is NA, NaN, or non-positive
  if (is.na(det_value) || is.nan(det_value) || det_value <= 0) {
    stop("Covariance matrix must be positive definite.")
  }
  
  # Generate data from a truncated normal distribution
  tnorm_data <- rtmvnorm(
    n = 100000, 
    mean = c(0, 0), 
    sigma = matrix(c(1, cov, cov, 1), ncol = 2), 
    lower = lower, 
    upper = upper
  )
  
  tnorm_df <- as.data.frame(tnorm_data)
  colnames(tnorm_df) <- c("V1", "V2")
  
  main_expr <- substitute(
    expression(paste(sigma[12]^2, " = ", cov_val, ", ", a[1], " = ", lower1, 
                     ", ", a[2], " = ", lower2, ", ", b[1], " = ", upper1, 
                     ", ", b[2], " = ", upper2)),
    list(cov_val = cov, lower1 = lower[1], lower2 = lower[2], upper1 = upper[1], upper2 = upper[2])
  )
  
  if (plot_type == "scatter") {
    p <- ggplot2::ggplot(tnorm_df, ggplot2::aes(x = V1, y = V2)) +
      ggplot2::geom_point(size = 0.001, color = "blue") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold")
      ) +
      ggplot2::labs(title = eval(main_expr), x = "x1", y = "x2")
  } else if (plot_type == "hist") {
    tnorm_density <- truncnorm::dtruncnorm(seq(lower[1], upper[1], length.out = 300), 
                                           a = lower[1], 
                                           b = upper[1], 
                                           mean = 0, 
                                           sd = 1)
    p <- ggplot2::ggplot(data.frame(x = tnorm_df$V1), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(aes(y = ..density..), bins = 100, fill = "gray", color = "black") +
      ggplot2::geom_line(aes(x = seq(lower[1], upper[1], length.out = 300), y = tnorm_density), 
                         color = "blue", linewidth = 1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold")
      ) +
      ggplot2::labs(title = eval(main_expr), x = "Values", y = "Probability") +
      ggplot2::ylim(0, y) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Density"))
  }
  
  return(p)
}
