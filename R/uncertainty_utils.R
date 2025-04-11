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


#' Get Beta Quantiles (Vectorized)
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
  # Compute rank once, shared across all columns
  rank <- min(max(ceiling((1 - Beta) * (n + 1)), 1), n)

  # Extract the values at the rank across all columns in a vectorized way
  quantile_values <- sorted_Scores[rank, ]

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
  y_pred <- as.matrix(y_pred)  # Ensure y_pred is a matrix
  quantile <- as.vector(quantile)  # Ensure quantile is a vector

  # Check if dimensions of y_pred and quantile align
  if (ncol(y_pred) != length(quantile)) {
    stop("Mismatch between number of target dimensions and length of quantile vector.")
  }

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
#' target_coverage <- 0.90
#'
#' # Simulated Eval_simultaneous_coverage_beta function (for the example only)
#' Eval_simultaneous_coverage_beta <- function(Beta) {
#'   return(0.9)  # Simulated coverage, replace with actual logic
#' }
#'
#' # Running dichotomy to find the optimal Beta
#' Beta_optimal <- dichotomie(target = target_coverage, xmin = 0, xmax = 1, n_iter = 10, Eval_simultaneous_coverage_beta = Eval_simultaneous_coverage_beta)
#' print(Beta_optimal)
#'
#' @export
dichotomie <- function(target, xmin, xmax, n_iter, Eval_simultaneous_coverage_beta) {
  a <- xmin
  b <- xmax

  for (i in 1:n_iter) {
    c <- (a + b) / 2
    y <- Eval_simultaneous_coverage_beta(c)

    cat(sprintf("Iteration %d: Beta = %.4f, Empirical Simultaneous Coverage = %.4f\n", i, c, y))

    # Check for NA/NULL
    if (is.na(y) || is.null(y)) {
      warning(sprintf("Empirical coverage is NA or NULL at Beta = %.4f", c))
      next
    }

    if (y > target) {
      a <- c
    } else {
      b <- c
    }
  }

  # Check if both values are valid
  a_cov <- Eval_simultaneous_coverage_beta(a)
  b_cov <- Eval_simultaneous_coverage_beta(b)

  if (is.na(a_cov) || is.null(a_cov)) {
    warning("Coverage at 'a' is NA or NULL, using 'b'.")
    return(b)
  }
  if (is.na(b_cov) || is.null(b_cov)) {
    warning("Coverage at 'b' is NA or NULL, using 'a'.")
    return(a)
  }

  # Returning the optimal Beta based on closer coverage
  if (abs(a_cov - target) < abs(b_cov - target)) {
    return(a)
  } else {
    return(b)
  }
}


###############################################################################################################"
# Special functions only for max rank method

#' Column-wise Rank
#'
#' Returns a matrix of the same size where each element represents
#' the column-wise rank of the corresponding element in the input matrix.
#'
#' @param matrix A numeric matrix of size n x m.
#'
#' @return A matrix of size n x m containing column-wise ranks.
#' @export
column_wise_rank <- function(matrix) {
  apply(matrix, 2, rank, ties.method = "first")
}


#' R_max Vector
#'
#' Computes the row-wise maximum rank for each observation in the rank matrix.
#'
#' @param rank_matrix A numeric matrix of ranks (output from column_wise_rank).
#'
#' @return A numeric vector of length n containing the max rank per row.
#' @export
R_max_vector <- function(rank_matrix) {
  apply(rank_matrix, 1, max)
}

#' Compute r_max Scalar
#'
#' Gets the scalar value corresponding to the (1 - alpha) quantile
#' of the R_max vector.
#'
#' @param R_max A numeric vector containing max ranks for each observation.
#' @param alpha A numeric value (between 0 and 1) representing the error rate.
#'
#' @return A scalar integer corresponding to the quantile threshold.
#' @export
get_r_max_scalar <- function(R_max, alpha) {
  n <- length(R_max)
  sorted_data <- sort(R_max)
  rank <- ceiling((1 - alpha) * (n + 1))
  rank <- min(rank, n)  # Cap rank at n for safety
  r_max <- sorted_data[rank]
  return(r_max)
}

#' Max Rank Quantiles
#'
#' Compute the r_max-th quantiles from a sorted score matrix.
#'
#' @param r_max Integer, the r_max threshold (from Max Rank method).
#' @param sorted_score_matrix A sorted (in ascending order) score matrix (columns = dimensions).
#'
#' @return A vector of quantiles per dimension (column).
max_rank_quantiles <- function(r_max, sorted_score_matrix) {
  apply(sorted_score_matrix, 2, function(sorted_column) {
    if (r_max > length(sorted_column)) {
      stop("r_max exceeds number of elements in the column.")
    }
    return(sorted_column[r_max])
  })
}


###############################################################################################################"
# Special functions only for max rank beta optim method

#' Simultaneous Coverage Evaluation (bis version)
#'
#' This function evaluates the proportion of elements in the R_max vector that are
#' less than or equal to the threshold `ceil(Beta * (n + 1))`, where `n` is the
#' number of elements in the R_max vector. This is a modified version of the standard
#' simultaneous coverage that uses a dynamic threshold based on `Beta` and the size of `R_max`.
#'
#' @param R_max A numeric vector of maximum ranks for each element in the score matrix.
#'              This represents the rank of residuals or error terms used for uncertainty estimation.
#' @param Beta A numeric value between 0 and 1. It represents the coverage parameter, which
#'             controls the desired level of confidence in the prediction intervals.
#'
#' @return A numeric value representing the proportion of the elements in `R_max` that are
#'         less than or equal to the threshold `ceil(Beta * (n + 1))`. This is the coverage
#'         estimate for the uncertainty method.
#'
#' @examples
#' R_max <- c(1, 2, 3, 4, 5)  # Example R_max vector
#' Beta <- 0.8  # Example Beta value
#' coverage <- simultaneous_coverage_bis(R_max, Beta)
#' cat("Simultaneous Coverage:", coverage, "\n")
#' @export
simultaneous_coverage_bis <- function(R_max, Beta) {
  # Step 1: Calculate the threshold value as ceil((1 - Beta) * n)
  n <- length(R_max)  # number of elements in R_max
  threshold <- ceiling((1 - Beta) * n)

  # Step 2: Count how many elements of R_max are below or equal to the threshold
  count_below_threshold <- sum(R_max <= threshold)

  # Step 3: Calculate the proportion
  proportion_below_threshold <- count_below_threshold / n

  return(proportion_below_threshold)
}
















