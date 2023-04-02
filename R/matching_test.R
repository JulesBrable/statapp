# Required package
library(Matching)

# Updated function to estimate the average treatment effect
# using Abadie-Imbens (2006) matching method
est_ATE_Abadie_Imbens <- function(data, treat_var, outcome_var, M, caliper) {
  
  # Perform matching using Euclidean distance
  matching_output <- Match(Y = data[[outcome_var]],
                           Tr = data[[treat_var]],
                           X =  data[, setdiff(names(data), c(outcome_var, treat_var)), with = FALSE],
                           M = M,
                           caliper = caliper)
  
  # Compute the average treatment effect
  tau <- summary(matching_output)$est
  
  # Extract matched pairs data
  matched_data <- data[unlist(matching_output$index.treated), ]
  matched_controls <- data[unlist(matching_output$index.control), ]
  
  # Compute the sum of squared differences of the matched pairs
  sum_sq_diff_matched <- sum((matched_data[[outcome_var]] - matched_controls[[outcome_var]])^2)
  
  # Compute the estimated covariance of the average treatment effect
  S_hat <- sum_sq_diff_matched / (nrow(matched_data) * (nrow(matched_data) - M))
  
  # Compute the average outcome of the treated units
  Y1_mean <- mean(matched_data[[outcome_var]])
  
  # Compute the sum of the squared differences between treated unit outcomes and the average treated unit outcome
  sum_sq_diff_treated <- sum((matched_data[[outcome_var]] - Y1_mean)^2)
  
  # Compute the estimated variance of the treated group
  sigma2_treated <- (sum_sq_diff_treated / (nrow(matched_data) - 1)) - S_hat
  
  # Compute the standard error and t-statistic of the estimate
  se <- sqrt(S_hat)
  t_stat <- tau / se
  
  # Return the estimated ATE, its standard error, t-statistic, and the estimated variance of the treated group
  return(list(tau = tau, se = se, t_stat = t_stat, sigma2_treated = sigma2_treated))
}

df <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))

res <- est_ATE_Abadie_Imbens(
  df, "rf_fedrg", "dbwt", M = 1, caliper = 0.2
  )

