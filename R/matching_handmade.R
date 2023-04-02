library(tidyverse)
library(Rcpp)
library(doParallel)
library(foreach)

path <- "data"
df <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))

# -----------------------------------------------------------------

cppFunction('List find_closest_matches(NumericMatrix mat1, NumericMatrix mat2, int start_index, int end_index) {
  int n1 = mat1.nrow();
  int n2 = mat2.nrow();

  NumericVector min_distances(n1, R_PosInf);
  IntegerVector min_indices(n1, -1);

  for (int i = 0; i < n1; i++) {
    for (int j = start_index; j <= end_index; j++) {
      double dist_ij = 0.0;
      for (int k = 0; k < mat1.ncol(); k++) {
        double diff = double(mat1(i, k) - mat2(j, k));
        dist_ij += diff * diff;
      }
      double dist = sqrt(dist_ij);
      
      if (dist < min_distances[i]) {
        min_distances[i] = dist;
        min_indices[i] = j;
      }
    }
  }

  return List::create(Named("min_distances") = min_distances, Named("min_indices") = min_indices);
}')

df_control <- df %>% filter(rf_fedrg == 0) %>% select(-rf_fedrg, -dbwt)
df_treatment <- df %>% filter(rf_fedrg == 1) %>% select(-rf_fedrg, -dbwt)

# Set up parallel processing
registerDoParallel(cores = detectCores())

# Define the chunk size (adjust as needed)
chunk_size <- 1000

# Split the control group into chunks
n_control <- nrow(df_control)
n_chunks <- ceiling(n_control / chunk_size)
chunk_indices <- split(0:(n_control - 1), rep(1:n_chunks, each = chunk_size, length.out = n_control))

# Match each chunk of the control group to the treatment group
results <- foreach(chunk_index = 1:n_chunks, .combine = 'c', .packages = c("foreach")) %dopar% {
  start_index <- min(chunk_indices[[chunk_index]])
  end_index <- max(chunk_indices[[chunk_index]])
  res <- find_closest_matches(as.matrix(df_treatment), as.matrix(df_control[start_index:end_index, ]), 0, nrow(df_control) - 1)
  res[["min_indices"]] + start_index - 1
}

M <- 1  # number of matches per unit
N1 <- sum(df$rf_fedrg == 1)

Yhat_0 <- numeric(length(df$dbwt))
Yhat_1 <- numeric(length(df$dbwt))

for (i in 1:length(df$dbwt)) {
  W_i <- df$rf_fedrg[i]
  Y_i <- df$dbwt[i]
  
  if (W_i == 0) {
    Yhat_0[i] <- Y_i
    Yhat_1[i] <- mean(df$dbwt[results[i] + 1])
  } else {
    Yhat_0[i] <- mean(df$dbwt[results[i] + 1])
    Yhat_1[i] <- Y_i
  }
}

tau_t_M <- sum((df$rf_fedrg - (1 - df$rf_fedrg) * 1/M) * (Yhat_1 - Yhat_0)) / N1
tau_t_M

# Compute the conditional variance
J <- 1
conditional_var <- sapply(1:length(df$dbwt), function(i) {
  W_i <- df$rf_fedrg[i]
  if (W_i == 1) {
    Yi <- df$dbwt[i]
    Y_same_treatment <- df$dbwt[results[i] + 1]
    return((J / (J + 1)) * (Yi - mean(Y_same_treatment))^2)
  } else {
    return(0)
  }
})

# Overall variance estimation
KM <- rep(1, length(df$dbwt))
var_tau_t_M <- sum(((df$rf_fedrg - (1 - df$rf_fedrg) * KM / M)^2) * conditional_var) / (N1^2)

# Test statistic and p-value
t_stat <- tau_t_M / sqrt(var_tau_t_M)
df <- N1 - 1
p_value <- 2 * pt(-abs(t_stat), df)

# Display the p-value
p_value
