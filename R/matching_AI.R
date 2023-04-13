# Function to estimate the ATE/ATT
# using Abadie-Imbens (2006) matching method
est_Abadie_Imbens <- function(data, treat_var, outcome_var, ...) {
  
  Match(Y = data[[outcome_var]],
        Tr = data[[treat_var]],
        X =  data[, setdiff(names(data), c(outcome_var, treat_var)),
                  with = FALSE],
        ...)
}
