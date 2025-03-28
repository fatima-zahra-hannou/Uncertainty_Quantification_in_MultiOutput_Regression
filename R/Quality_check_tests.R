# Example testing
y_pred <- matrix(c(3.5, 2.1, 4.0), ncol = 1)
y_true <- matrix(c(3.7, 2.0, 3.8), ncol = 1)

# Calculate residual scores
residuals <- absolute_residual_scores(y_pred, y_true)

# Simultaneous coverage
sim_cov <- simultaneous_coverage(y_true, matrix(c(3.0, 1.8, 3.7), ncol = 1), matrix(c(3.8, 2.5, 4.2), ncol = 1))

# Get sorted scores
sorted_scores <- get_sorted_scores(residuals)

# Get Beta quantiles
beta_quantiles <- get_Beta_quantiles(sorted_scores, 0.1)

# Get prediction bounds
bounds <- get_prediction_bounds(beta_quantiles, y_pred)
