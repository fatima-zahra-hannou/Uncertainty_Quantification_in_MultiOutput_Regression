#' Absolute Residual Scores
#'
#' Calculate nonconformity scores based on absolute differences.
#'
#' @param y_pred Numeric matrix. Predictions made by the model.
#' @param y_true Numeric matrix. True target values.
#' @return Numeric matrix. Residual normalized scores for each prediction.
#' @export
absolute_residual_scores <- function(y_pred, y_true) {
  abs(y_pred - y_true)
}

#' Simultaneous Coverage Calculation
#'
#' Calculate empirical simultaneous coverage and print it.
#'
#' @param y_true Numeric matrix. True target values.
#' @param y_lower Numeric matrix. Lower prediction bounds.
#' @param y_upper Numeric matrix. Upper prediction bounds.
#' @return Numeric value. Global coverage over prediction intervals.
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

  message("We have ", sum(list_fall_or_not), " curves falling inside all their prediction intervals simultaneously")
  message("For a total of ", nrow(y_true), " curves")
  message("Which leads to an empirical simultaneous coverage of ", emp_sim_coverage)

  return(emp_sim_coverage)
}

#' Coverage and Interval Width Calculation
#'
#' Calculate the empirical coverage and the average interval width for each target dimension.
#'
#' @param y_true Numeric matrix. True target values.
#' @param y_low Numeric matrix. Lower prediction bounds.
#' @param y_up Numeric matrix. Upper prediction bounds.
#' @return List containing:
#'   \item{coverage}{Numeric vector. Coverage per target dimension.}
#'   \item{interval_width}{Numeric vector. Average interval width per target dimension.}
#' @export
get_coverage_and_intWidth <- function(y_true, y_low, y_up) {
  y_true <- as.matrix(y_true)
  y_low <- as.matrix(y_low)
  y_up <- as.matrix(y_up)

  n_dim <- ncol(y_true)
  list_coverage <- numeric(n_dim)
  list_interval_width <- numeric(n_dim)

  for (i in seq_len(n_dim)) {
    y_true_dim <- y_true[, i]
    ylow <- y_low[, i]
    ytop <- y_up[, i]

    coverage <- sum(ylow <= y_true_dim & y_true_dim <= ytop) / length(y_true_dim)
    width <- mean(ytop - ylow)

    list_coverage[i] <- coverage
    list_interval_width[i] <- width
  }

  return(list(coverage = list_coverage, interval_width = list_interval_width))
}

#' Sorted Scores Calculation
#'
#' Sort the score for each dimension.
#'
#' @param Scores Numeric matrix. Score matrix.
#' @return Numeric matrix. Sorted scores.
#' @export
get_sorted_scores <- function(Scores) {
  apply(Scores, 2, sort)
}

#' Beta Quantile Calculation
#'
#' Get the Beta score quantile for each dimension.
#'
#' @param sorted_Scores Numeric matrix. Sorted score matrix from lowest to highest.
#' @param Beta Numeric value. Error rate.
#' @return Numeric vector. Beta score quantiles.
#' @export
get_Beta_quantiles <- function(sorted_Scores, Beta) {
  n <- nrow(sorted_Scores)
  n_dim <- ncol(sorted_Scores)
  quantile_values <- numeric(n_dim)

  for (i in seq_len(n_dim)) {
    rank <- max(min(ceiling((1 - Beta) * (n + 1)), n) - 1, 0)
    quantile_values[i] <- sorted_Scores[rank + 1, i]  # Adjust for R's 1-based indexing
  }

  return(quantile_values)
}

#' Compute Prediction Bounds
#'
#' Compute the lower and upper prediction bounds.
#'
#' @param quantile Numeric vector. Beta quantiles.
#' @param y_pred Numeric matrix. Model predictions.
#' @return List containing:
#'   \item{y_lower}{Numeric matrix. Lower prediction bounds.}
#'   \item{y_upper}{Numeric matrix. Upper prediction bounds.}
#' @export
get_prediction_bounds <- function(quantile, y_pred) {
  y_lower <- y_pred - quantile
  y_upper <- y_pred + quantile

  return(list(y_lower = y_lower, y_upper = y_upper))
}


################# ModelUncertainty class


ModelUncertainties <- R6::R6Class("ModelUncertainties",
                                  public = list(
                                    model = NULL,
                                    X_calibration = NULL,
                                    Y_calibration = NULL,
                                    uncertainty_method = NULL,
                                    Global_alpha = NULL,
                                    sorted_scores = NULL,
                                    beta_optim = NULL,

                                    initialize = function(model, X_calibration, Y_calibration, uncertainty_method = "Beta_opt", Global_alpha = 0.1) {
                                      self$model <- model
                                      self$X_calibration <- X_calibration
                                      self$Y_calibration <- Y_calibration
                                      self$uncertainty_method <- uncertainty_method
                                      self$Global_alpha <- Global_alpha
                                    },

                                    fit = function() {
                                      y_pred_calib <- self$model$predict(self$X_calibration)
                                      scores <- abs(y_pred_calib - self$Y_calibration)
                                      self$sorted_scores <- apply(scores, 2, sort)

                                      if (self$uncertainty_method == "Beta_opt") {
                                        self$beta_optim <- self$dichotomy(target = 1 - self$Global_alpha, xmin = 0, xmax = self$Global_alpha, n_iter = 10)
                                      } else {
                                        stop("Unsupported uncertainty method")
                                      }
                                    },

                                    predict = function(X_test) {
                                      y_pred_test <- self$model$predict(X_test)
                                      beta_quantiles <- self$get_Beta_quantiles(self$sorted_scores, self$beta_optim)
                                      y_lower <- y_pred_test - beta_quantiles
                                      y_upper <- y_pred_test + beta_quantiles
                                      return(list(lower = y_lower, upper = y_upper))
                                    },

                                    Eval_simultaneous_coverage_beta = function(Beta) {
                                      beta_quantile <- self$get_Beta_quantiles(self$sorted_scores, Beta)
                                      y_lower <- self$model$predict(self$X_calibration) - beta_quantile
                                      y_upper <- self$model$predict(self$X_calibration) + beta_quantile
                                      coverage <- mean(rowSums(self$Y_calibration >= y_lower & self$Y_calibration <= y_upper) == ncol(self$Y_calibration))
                                      return(coverage)
                                    },

                                    dichotomy = function(target, xmin, xmax, n_iter) {
                                      a <- xmin
                                      b <- xmax
                                      for (i in seq_len(n_iter)) {
                                        c <- (a + b) / 2
                                        y <- self$Eval_simultaneous_coverage_beta(c)
                                        if (y > target) {
                                          a <- c
                                        } else {
                                          b <- c
                                        }
                                      }
                                      return(ifelse(abs(self$Eval_simultaneous_coverage_beta(a) - target) < abs(self$Eval_simultaneous_coverage_beta(b) - target), a, b))
                                    },

                                    get_Beta_quantiles = function(sorted_scores, Beta) {
                                      n <- nrow(sorted_scores)
                                      rank <- pmax(pmin(ceiling((1 - Beta) * (n + 1)), n) - 1, 1)
                                      return(sorted_scores[rank, ])
                                    }
                                  )
)

# Example Usage:
# model <- MLModel$new(X_train, y_train, method = "gradient_boosting")
# model$fit()
# uncertainty_model <- ModelUncertainties$new(model, X_calibration, Y_calibration, "Beta_opt", 0.1)
# uncertainty_model$fit()
# pred_intervals <- uncertainty_model$predict(X_test)
# print(pred_intervals$lower)
# print(pred_intervals$upper)
