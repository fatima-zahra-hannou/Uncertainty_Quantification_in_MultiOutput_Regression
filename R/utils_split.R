#' Split data into training, calibration, and test sets (with indices)
#'
#' @param X Input features
#' @param Y Output targets
#' @param n_train Number of training samples (default 1000)
#' @param n_test Number of test samples (default 1000)
#' @param n_calib Number of calibration samples (user-defined)
#' @return A list: data splits + row indices
#' @export
train_test_calib_split <- function(X, Y, n_train = 1000, n_test = 1000, n_calib) {
  n <- nrow(X)

  # Check if the dataset has enough data
  if (n < (n_train + n_test + n_calib)) {
    stop("Not enough data to split into the requested sizes. Reduce n_train, n_test, or n_calib.")
  }

  # Shuffle indices
  idx_all <- sample(n)

  # Assign training and test sets
  idx_train <- idx_all[1:n_train]
  idx_test <- idx_all[(n_train + 1):(n_train + n_test)]

  # Assign calibration set randomly from remaining samples
  remaining_idx <- setdiff(idx_all, c(idx_train, idx_test))
  idx_calib <- sample(remaining_idx, n_calib)

  list(
    X_train = X[idx_train, , drop = FALSE],
    y_train = Y[idx_train, , drop = FALSE],
    X_calib = X[idx_calib, , drop = FALSE],
    y_calib = Y[idx_calib, , drop = FALSE],
    X_test = X[idx_test, , drop = FALSE],
    y_test = Y[idx_test, , drop = FALSE],
    idx_train = idx_train,
    idx_calib = idx_calib,
    idx_test = idx_test
  )
}
