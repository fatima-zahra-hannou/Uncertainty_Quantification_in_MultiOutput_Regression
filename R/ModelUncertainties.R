# Source the custom utility functions
source("R/uncertainty_utils.R", local = TRUE)

#' @title ModelUncertainties Class
#' @description This class encapsulates the process of fitting uncertainty models and making predictions with uncertainty intervals.
#' The class supports multiple uncertainty estimation methods such as "Beta_Optim" and "Max_Rank".
#' @export

ModelUncertainties <- R6::R6Class("ModelUncertainties",
                                  public = list(
                                    #' @field model The machine learning model (e.g., regression, classifier).
                                    model = NULL,

                                    #' @field X_calibration Calibration dataset features.
                                    X_calibration = NULL,

                                    #' @field Y_calibration Calibration dataset targets.
                                    Y_calibration = NULL,

                                    #' @field uncertainty_method The method for uncertainty estimation (e.g., "Beta_Optim").
                                    uncertainty_method = NULL,

                                    #' @field Global_alpha The significance level for uncertainty estimation.
                                    Global_alpha = NULL,

                                    #' @field Beta_optim The optimized Beta value used for uncertainty estimation.
                                    Beta_optim = NULL,

                                    #' @field quantiles The quantiles used for uncertainty intervals.
                                    quantiles = NULL,


                                    # Constructor
                                    #' @description Constructor for the ModelUncertainties class
                                    #' @param model The machine learning model (e.g., regression, classifier).
                                    #' @param X_calibration Calibration dataset features.
                                    #' @param Y_calibration Calibration dataset targets.
                                    #' @param uncertainty_method The method for uncertainty estimation (e.g., "Beta_Optim").
                                    #' @param Global_alpha The significance level for uncertainty estimation.
                                    initialize = function(model, X_calibration, Y_calibration, uncertainty_method, Global_alpha) {
                                      self$model <- model
                                      self$X_calibration <- X_calibration
                                      self$Y_calibration <- Y_calibration
                                      self$uncertainty_method <- uncertainty_method
                                      self$Global_alpha <- Global_alpha
                                      self$Beta_optim <- NULL
                                      self$quantiles <- NULL
                                    },

                                    # Fit method to compute Beta quantiles based on the choosen uncertainty method
                                    #' @description Fits the uncertainty model based on the chosen method.
                                    #' @return None
                                    fit = function() {
                                      cat("Fitting uncertainty model with method:", self$uncertainty_method, "\n")

                                      # Get predictions on the calibration set
                                      y_pred_calib <- self$model$predict(self$X_calibration)

                                      # Compute the score matrix
                                      S <- uncertainty_utils::absolute_residual_scores(y_pred_calib, self$Y_calibration)

                                      # Get the sorted score matrix
                                      sorted_S <- uncertainty_utils::get_sorted_scores(S)  # assuming sorted_S is a matrix

                                      ##############################################################
                                      ### Beta Optim Method

                                      if (self$uncertainty_method == "Beta_Optim") {
                                        # Define the function to evaluate the simultaneous coverage for a given Beta value
                                        Eval_simultaneous_coverage_beta <- function(Beta) {
                                          # Get Beta quantiles
                                          Beta_quantile <- uncertainty_utils::get_Beta_quantiles(sorted_S, Beta)

                                          # Get the corresponding lower and upper prediction bounds
                                          bounds <- uncertainty_utils::get_prediction_bounds(Beta_quantile, y_pred_calib)

                                          # Compute the empirical simultaneous coverage
                                          Sim_coverage_Beta <- uncertainty_utils::simultaneous_coverage(self$Y_calibration, bounds$y_lower, bounds$y_upper)

                                          cat(sprintf("Beta = %.4f => Coverage = %.4f\n", Beta, Sim_coverage_Beta))

                                          return(Sim_coverage_Beta)
                                        }

                                        # Use dichotomy to find the optimal Beta
                                        cat("Running dichotomy to find optimal Beta...\n")
                                        self$Beta_optim <- uncertainty_utils::dichotomie(target = 1 - self$Global_alpha,
                                                                                         xmin = 0,
                                                                                         xmax = self$Global_alpha,
                                                                                         n_iter = 10,
                                                                                         Eval_simultaneous_coverage_beta = Eval_simultaneous_coverage_beta)
                                        cat("Optimal Beta found:", self$Beta_optim, "\n")

                                        # Save quantiles for later use in predict()
                                        self$quantiles <- uncertainty_utils::get_Beta_quantiles(sorted_S, self$Beta_optim)
                                      }

                                      ##############################################################
                                      ### Max Rank Method
                                      else if (self$uncertainty_method == "Max_Rank") {
                                        # Step 1: Compute rank matrix
                                        rank_matrix <- uncertainty_utils::column_wise_rank(S)

                                        # Step 2: Compute R_max vector
                                        R_max <- uncertainty_utils::R_max_vector(rank_matrix)

                                        # Step 3: Get r_max scalar based on the desired global alpha
                                        r_max <- uncertainty_utils::get_r_max_scalar(R_max, self$Global_alpha)

                                        # Step 4: Compute final quantiles per dimension using r_max
                                        self$quantiles <- uncertainty_utils::max_rank_quantiles(r_max, sorted_S)

                                        cat("Max Rank method fitted successfully. r_max =", r_max, "\n")
                                      }

                                      ##############################################################
                                      ### Fast Beta Optim Method
                                      else if (self$uncertainty_method == "Fast_Beta_Optim") {
                                        # Step 1: Compute rank matrix
                                        rank_matrix <- uncertainty_utils::column_wise_rank(S)

                                        # Step 2: Compute R_max vector
                                        R_max <- uncertainty_utils::R_max_vector(rank_matrix)

                                        # Adapted Eval_simultaneous_coverage function
                                        Eval_simultaneous_coverage_beta_max_rank <- function(Beta) {

                                          # Compute the empirical simultaneous coverage using the new function
                                          Sim_coverage_Beta <- uncertainty_utils::simultaneous_coverage_bis(R_max, Beta)

                                          cat(sprintf("Beta = %.4f => Coverage = %.4f\n", Beta, Sim_coverage_Beta))

                                          return(Sim_coverage_Beta)
                                        }

                                        # Use dichotomy to find the optimal Beta
                                        cat("Running dichotomy to find optimal Beta...\n")
                                        self$Beta_optim <- uncertainty_utils::dichotomie(target = 1 - self$Global_alpha,
                                                                                         xmin = 0,
                                                                                         xmax = self$Global_alpha,
                                                                                         n_iter = 10,
                                                                                         Eval_simultaneous_coverage_beta = Eval_simultaneous_coverage_beta_max_rank)
                                        cat("Optimal Beta found:", self$Beta_optim, "\n")

                                        # Save quantiles for later use in predict()
                                        self$quantiles <- uncertainty_utils::get_Beta_quantiles(sorted_S, self$Beta_optim)

                                      }

                                      ##############################################################
                                      ### If the choosen method doesn't exist
                                      else {
                                        available_methods <- c("Beta_Optim", "Max_Rank", "Fast_Beta_Optim")
                                        stop(paste0(
                                          "Unknown uncertainty method: '", self$uncertainty_method, "'.\n",
                                          "Available methods are: ", paste(available_methods, collapse = ", ")
                                        ))
                                      }

                                    },

                                    # Predict method to get prediction intervals

                                    #' @description Predicts uncertainty bounds for the given test data.
                                    #' @param X_test Features of the test dataset.
                                    #' @return A list with two elements: `y_lower` and `y_upper`, representing the lower and upper bounds.

                                    predict = function(X_test) {
                                      # Test to check the user has fit before predict
                                      if (is.null(self$quantiles)) {
                                        stop("Quantiles not computed. Did you forget to call fit()?")
                                      }

                                      # Only check Beta_optim for methods that use it
                                      if (self$uncertainty_method == "Beta_Optim" && is.null(self$Beta_optim)) {
                                        stop("Beta_optim is NULL. Did you forget to call fit() before predict()?")
                                      }

                                      # Get predictions on the test set
                                      y_pred_test <- self$model$predict(X_test)

                                      # Get the corresponding lower and upper prediction bounds
                                      bounds <- uncertainty_utils::get_prediction_bounds(self$quantiles, y_pred_test)

                                      return(list(y_lower = bounds$y_lower, y_upper = bounds$y_upper))
                                    }
                                  )
)
