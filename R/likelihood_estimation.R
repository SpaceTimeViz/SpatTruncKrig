#' Fit Maximum Likelihood Estimation for Truncated Multivariate Normal
#'
#' @param locs A matrix of locations (grid points).
#' @param y_matrix A matrix of observed data.
#' @param lower_bound A vector of lower truncation bounds.
#' @param upper_bound A vector of upper truncation bounds.
#' @param basis_fn A matrix representing the basis functions.
#' @param initial_params A vector of initial parameter values (variance, range, beta).
#' @param lower_bounds A vector of lower bounds for the parameters. Default is c(0, 0, -Inf).
#' @param upper_bounds A vector of upper bounds for the parameters. Default is c(Inf, Inf, Inf).
#' @param max_call Maximum number of function evaluations. Default is 5000.
#' @param maxit Maximum number of iterations. Default is 1000.
#' @param threshold_stop Convergence threshold. Default is 1e-6.
#' @param temperature Starting temperature for the GenSA algorithm. Default is 1e4.
#' @param max_time Maximum time allowed for the optimization (in seconds). Default is 600.
#' @param verbose Logical. If TRUE, provides verbose output. Default is TRUE.
#' @return A vector of estimated parameters.
#' @export
fit_mle <- function(
    locs,
    y_matrix,
    lower_bound,
    upper_bound,
    basis_fn,
    initial_params,
    lower_bounds = c(0, 0, -Inf),
    upper_bounds = c(Inf, Inf, Inf),
    max_call = 1000,
    maxit = 1000,
    threshold_stop = 1e-6,
    temperature = 1e4,
    max_time = 100,
    verbose = TRUE
) {
  
  obj_fun_gensa <- function(params) {
    -log_likelihood_truncated(
      params,
      locs = as.matrix(locs),
      y_matrix = t(y_matrix),
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      basis_fn = basis_fn
    )
  }
  
  result_gensa <- GenSA(
    par = initial_params, 
    fn = obj_fun_gensa, 
    lower = lower_bounds, 
    upper = upper_bounds,
    control = list(
      max.call = max_call,            
      maxit = maxit,               
      threshold.stop = threshold_stop,      
      temperature = temperature,          
      max.time = max_time,             
      verbose = verbose
    )
  )
  
  return(result_gensa$par)
}

#' Log-Likelihood for Truncated Multivariate Normal
#'
#' @param params A vector of parameters (variance, range, beta).
#' @param locs A matrix of locations (grid points).
#' @param y_matrix A matrix of observed data.
#' @param lower_bound A vector of lower truncation bounds.
#' @param upper_bound A vector of upper truncation bounds.
#' @param basis_fn A matrix representing the basis functions.
#' @return The negative log-likelihood value.
#' @export
log_likelihood_truncated <- function(params, locs, y_matrix, lower_bound, upper_bound, basis_fn) {
  var <- params[1]
  range <- params[2]
  beta <- params[3]
  covParms <- c(var, range)
  
  mu <- as.vector(basis_fn %*% beta)
  log_lik <- 0
  
  n <- nrow(locs)
  
  for (i in 1:nrow(y_matrix)) {
    y <- y_matrix[i, ]
    
    est_Vecc <- tryCatch({
      VeccTMVN::pmvn(
        lower = lower_bound, 
        upper = upper_bound, 
        mean = mu, 
        locs = as.matrix(locs),  
        covName = "matern15_isotropic",  
        covParms = covParms, 
        m = 10,  
        verbose = FALSE
      )
    }, error = function(e) {
      return(NA)
    })
    
    if (any(is.na(est_Vecc)) || any(est_Vecc <= 0) || any(!is.finite(est_Vecc))) {
      return(Inf)
    }
    
    log_lik <- log_lik - sum(log(est_Vecc))
  }
  
  if (!is.finite(log_lik)) {
    return(Inf)
  }
  
  return(log_lik)
}
