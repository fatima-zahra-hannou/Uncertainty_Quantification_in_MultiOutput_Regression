#' Split data into training, calibration and test sets (with indices)
#'
#' @param X Input features
#' @param Y Output targets
#' @param test_size Size of test set (default 0.1)
#' @param calib_size Size of calibration set (default 0.2)
#' @return A list: data splits + row indices
#' @export
train_test_calib_split <- function(X, Y, test_size = 0.1, calib_size = 0.2) {
  n <- nrow(X)
  idx_all <- sample(n)

  n_test <- floor(test_size * n)
  n_calib <- floor(calib_size * n)
  n_train <- n - n_test - n_calib

  idx_train <- idx_all[1:n_train]
  idx_calib <- idx_all[(n_train + 1):(n_train + n_calib)]
  idx_test  <- idx_all[(n_train + n_calib + 1):n]

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
