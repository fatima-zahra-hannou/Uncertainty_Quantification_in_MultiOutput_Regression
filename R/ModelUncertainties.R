# ModelUncertainties.R

source("R/uncertainty_utils.R")

ModelUncertainties <- R6::R6Class("ModelUncertainties",
                                  public = list(
                                    model = NULL,
                                    X_calibration = NULL,
                                    Y_calibration = NULL,
                                    uncertainty_method = NULL,
                                    Global_alpha = NULL,
                                    Beta_optim = NULL,

                                    # Constructor
                                    initialize = function(model, X_calibration, Y_calibration, uncertainty_method, Global_alpha) {
                                      self$model <- model
                                      self$X_calibration <- X_calibration
                                      self$Y_calibration <- Y_calibration
                                      self$uncertainty_method <- uncertainty_method
                                      self$Global_alpha <- Global_alpha
                                      self$Beta_optim <- NULL
                                    },

                                    # Fit method to compute Beta quantiles
                                    fit = function() {
                                      # Get predictions on the calibration set
                                      y_pred_calib <- self$model$predict(self$X_calibration)

                                      # Compute the score matrix
                                      S <- absolute_residual_scores(y_pred_calib, self$Y_calibration)

                                      # Get the sorted score matrix
                                      sorted_S <- get_sorted_scores(S)  # assuming sorted_S is a matrix

                                      # Define the function to evaluate the simultaneous coverage for a given Beta value
                                      Eval_simultaneous_coverage_beta <- function(Beta) {
                                        # Get Beta quantiles
                                        Beta_quantile <- get_Beta_quantiles(sorted_S, Beta)

                                        # Get the corresponding lower and upper prediction bounds
                                        bounds <- get_prediction_bounds(Beta_quantile, y_pred_calib)

                                        # Compute the empirical simultaneous coverage
                                        Sim_coverage_Beta <- simultaneous_coverage(self$Y_calibration, bounds$y_lower, bounds$y_upper)

                                        return(Sim_coverage_Beta)
                                      }

                                      # Use dichotomy to find the optimal Beta
                                      self$Beta_optim <- dichotomie(target = 1 - self$Global_alpha, xmin = 0, xmax = self$Global_alpha, n_iter = 10, Eval_simultaneous_coverage_beta)
                                    },

                                    # Predict method to get prediction intervals
                                    predict = function(X_test) {
                                      # Get predictions on the test set
                                      y_pred_test <- self$model$predict(X_test)

                                      # Compute the Beta-optim quantiles
                                      y_pred_calib <- self$model$predict(self$X_calibration)  # You might want to use calibration set predictions
                                      S <- absolute_residual_scores(y_pred_calib, self$Y_calibration)
                                      sorted_S <- get_sorted_scores(S)
                                      Beta_optim_quantiles <- get_Beta_quantiles(sorted_S, self$Beta_optim)

                                      # Get the corresponding lower and upper prediction bounds
                                      bounds <- get_prediction_bounds(Beta_optim_quantiles, y_pred_test)

                                      return(list(y_lower = bounds$y_lower, y_upper = bounds$y_upper))
                                    }
                                  )
)
