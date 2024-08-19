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
plot_truncated_normal <- function(cov, lower, upper, y, plot_type = "scatter") {
  # Input validation
  if (!is.numeric(cov) || length(cov) != 1 || is.na(cov)) {
    stop("Invalid input: 'cov' must be a numeric value of length 1.")
  }
  if (!is.numeric(lower) || length(lower) != 2 || any(is.na(lower))) {
    stop("Invalid input: 'lower' must be a numeric vector of length 2.")
  }
  if (!is.numeric(upper) || length(upper) != 2 || any(is.na(upper))) {
    stop("Invalid input: 'upper' must be a numeric vector of length 2.")
  }
  if (!is.numeric(y) || length(y) != 1 || is.na(y)) {
    stop("Invalid input: 'y' must be a numeric value of length 1.")
  }
  if (!plot_type %in% c("scatter", "hist")) {
    stop("Invalid input: 'plot_type' must be either 'scatter' or 'hist'.")
  }
  
  old_par <- par(mar = c(5, 5, 5, 5))  # Set margins to 5 lines, can be adjusted as needed
  
  # Generate data from a truncated normal distribution
  tnorm_data <- rtmvnorm(
    n = 100000, 
    mean = c(0, 0), 
    sigma = matrix(c(1, cov, cov, 1), ncol = 2), 
    lower = lower, 
    upper = upper
  )
  
  # Dynamically generate the main title expression using `substitute`
  main_expr <- substitute(
    expression(paste(sigma[12]^2, " = ", cov_val, ", ", a[1], " = ", lower1, 
                     ", ", a[2], " = ", lower2, ", ", b[1], " = ", upper1, 
                     ", ", b[2], " = ", upper2)),
    list(cov_val = cov, lower1 = lower[1], lower2 = lower[2], upper1 = upper[1], upper2 = upper[2])
  )
  
  if (plot_type == "scatter") {
    # Create scatter plot
    p <- ggplot(data = as.data.frame(tnorm_data), aes(x = V1, y = V2)) +
      geom_point(color = "blue", size = 0.1) +
      labs(title = eval(main_expr), x = "x1", y = "x2") +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(rep(5, 4), "lines")
      )
    
    par(old_par)  # Restore original margin settings
    return(p)  # Return the ggplot object
  } else if (plot_type == "hist") {
    # Calculate density data
    tnorm_density <- truncnorm::dtruncnorm(seq(lower[1], upper[1], length.out = 300), 
                                           a = lower[1], 
                                           b = upper[1], 
                                           mean = 0, 
                                           sd = 1)
    
    # Create histogram and density curve plot
    p <- ggplot(data = as.data.frame(tnorm_data[, 1]), aes(x = V1)) +
      geom_histogram(aes(y = ..density..), bins = 100, fill = "gray", color = "black") +
      geom_line(aes(x = seq(lower[1], upper[1], length.out = 300), y = tnorm_density), 
                color = "blue", linewidth = 1) +  # Use linewidth instead of size
      labs(title = eval(main_expr), x = "Values", y = "Probability") +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(rep(5, 4), "lines")
      ) +
      ylim(0, y) +
      guides(fill = guide_legend(title = "Histogram", color = "Density"))
    
    par(old_par)  # Restore original margin settings
    return(p)  # Return the ggplot object
  }
}
