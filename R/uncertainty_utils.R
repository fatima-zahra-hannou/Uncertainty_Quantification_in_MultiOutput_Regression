# uncertainty_utils.R

#' Absolute Residual Scores
#'
#' Calculate nonconformity scores based on absolute differences between the predicted and true values.
#'
#' @param y_pred Numeric vector or matrix. The predictions made by the model.
#' @param y_true Numeric vector or matrix. The true target values.
#'
#' @return Numeric vector or matrix. Residual normalized scores for each prediction.
#' @export
absolute_residual_scores <- function(y_pred, y_true) {
  return(abs(y_pred - y_true))
}


#' Simultaneous Coverage Calculation
#'
#' Calculate the empirical simultaneous coverage, which measures the percentage of cases where all true values
#' fall within their respective prediction intervals.
#'
#' @param y_true Numeric matrix. The true target values.
#' @param y_lower Numeric matrix. The lower bound of the prediction interval.
#' @param y_upper Numeric matrix. The upper bound of the prediction interval.
#'
#' @return Numeric value. The empirical simultaneous coverage.
#' @export
simultaneous_coverage <- function(y_true, y_lower, y_upper) {
  y_true <- as.matrix(y_true)
  y_lower <- as.matrix(y_lower)
  y_upper <- as.matrix(y_upper)

  list_fall_or_not <- rep(1, nrow(y_true))

  for (i in seq_len(nrow(y_true))) {
    for (j in seq_len(ncol(y_true))) {
      if (y_lower[i, j] > y_true[i, j] || y_upper[i, j] < y_true[i, j]) {
        list_fall_or_not[i] <- 0
      }
    }
  }

  emp_sim_coverage <- sum(list_fall_or_not) / nrow(y_true)

  cat("We have", sum(list_fall_or_not), "curves falling inside all their prediction intervals simultaneously\n")
  cat("For a total of", nrow(y_true), "curves\n")
  cat("Which leads to an empirical simultaneous coverage of", emp_sim_coverage, "\n")

  return(emp_sim_coverage)
}


#' Get Sorted Scores
#'
#' Sort the score matrix for each target dimension in ascending order.
#'
#' @param Scores Numeric matrix. The score matrix to be sorted.
#'
#' @return Numeric matrix. The sorted scores for each target dimension.
#' @export
get_sorted_scores <- function(Scores) {
  # Check if Scores is a data.table, if so use proper indexing
  if (inherits(Scores, "data.table")) {
    n <- nrow(Scores)
    n_dim <- ncol(Scores)
    sorted_scores <- data.table(matrix(0, nrow = n, ncol = n_dim))

    for (i in seq_len(n_dim)) {
      sorted_scores[[i]] <- sort(Scores[[i]])
    }
    return(sorted_scores)
  } else {
    # If Scores is a matrix, proceed with matrix sorting
    n <- nrow(Scores)
    n_dim <- ncol(Scores)
    sorted_scores <- matrix(0, nrow = n, ncol = n_dim)

    for (i in seq_len(n_dim)) {
      sorted_scores[, i] <- sort(Scores[, i])
    }
    return(sorted_scores)
  }
}


#' Get Beta Quantiles
#'
#' Calculate the Beta quantiles from the sorted score matrix for each target dimension.
#'
#' @param sorted_Scores Numeric matrix. The sorted score matrix (from lowest to highest score values).
#' @param Beta Numeric value. The error rate to determine the quantile.
#'
#' @return Numeric vector. The Beta quantiles for each target dimension.
#' @export
get_Beta_quantiles <- function(sorted_Scores, Beta) {
  sorted_Scores <- as.matrix(sorted_Scores)  # Ensure it's a matrix

  n <- nrow(sorted_Scores)
  n_dim <- ncol(sorted_Scores)
  quantile_values <- numeric(n_dim)

  for (i in seq_len(n_dim)) {
    # Calculate rank correctly, ensuring it's always within valid bounds
    rank <- min(max(ceiling((1 - Beta) * (n+1)), 1), n)  # Ensures it's in [1, n]

    # Extract the Beta quantile value for the current dimension
    quantile_values[i] <- sorted_Scores[rank, i]
  }

  return(quantile_values)
}

#' Get Prediction Bounds
#'
#' Calculate the lower and upper prediction bounds given the Beta quantiles and predicted values.
#'
#' @param quantile Numeric vector. The Beta quantiles.
#' @param y_pred Numeric matrix. The predicted values for each target dimension.
#'
#' @return A list containing:
#' \item{y_lower}{Numeric matrix. The lower prediction bounds.}
#' \item{y_upper}{Numeric matrix. The upper prediction bounds.}
#' @export
get_prediction_bounds <- function(quantile, y_pred) {
  # Ensure quantile is a vector
  quantile <- as.vector(quantile)

  # Expand quantile into a matrix with the same number of rows as y_pred
  quantile_matrix <- matrix(quantile, nrow = nrow(y_pred), ncol = length(quantile), byrow = TRUE)

  # Compute the prediction bounds
  y_lower <- y_pred - quantile_matrix
  y_upper <- y_pred + quantile_matrix

  return(list(y_lower = y_lower, y_upper = y_upper))
}


#' Dichotomy Function to Find the Optimal Beta
#'
#' This function implements the dichotomy method to find the optimal Beta value that
#' minimizes the difference between the empirical simultaneous coverage and a target value.
#' It performs binary search over a specified range of Beta values and iterates for a given
#' number of iterations to find the Beta that best satisfies the target coverage.
#'
#' @param target A numeric value representing the desired target for the empirical simultaneous coverage.
#' @param xmin The minimum value for Beta (the lower bound of the search interval).
#' @param xmax The maximum value for Beta (the upper bound of the search interval).
#' @param n_iter An integer indicating the number of iterations to perform for the binary search.
#' @param Eval_simultaneous_coverage_beta A function that takes a Beta value as input and returns
#' the empirical simultaneous coverage for that Beta.
#'
#' @return A numeric value representing the Beta that minimizes the difference between
#' the empirical simultaneous coverage and the target value.
#'
#' @examples
#' # Example usage:
#' target_coverage <- 0.95
#' Beta_optimal <- dichotomie(target = target_coverage, xmin = 0, xmax = 1, n_iter = 10, Eval_simultaneous_coverage_beta)
#' print(Beta_optimal)
#'
#' @export
dichotomie <- function(target, xmin, xmax, n_iter, Eval_simultaneous_coverage_beta) {
  a <- xmin
  b <- xmax
  for (i in 1:n_iter) {
    c <- (a + b) / 2
    y <- Eval_simultaneous_coverage_beta(c)
    if (y > target) {
      a <- c
    } else {
      b <- c
    }
  }

  if (abs(Eval_simultaneous_coverage_beta(a) - target) < abs(Eval_simultaneous_coverage_beta(b) - target)) {
    return(a)
  } else {
    return(b)
  }
}
